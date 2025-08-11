package journey

import "base:intrinsics"
import "core:sys/linux"
import "core:fmt"


/*
   What is the problem (informal):
   We want to figure out a way to organize entity's components in a way where it is possible to always read/write from a full cache line and use all of it.
   In most cases we only use only a small subset of data (not reading from all field in a struct) and the rest is wasted. We want the end user to think
   about the notion that there is data, which in this case is the component and there is a transform, which is the system and just optimize that.
   Rather than think that each entity is an Object and than operate each object differently (which is easier to reason about) and thus causing waste in most cases. We want a easy way for 
   user to use the notion of data and transform. Something that doesn't cause high friction in thier project and still reap the optimization benefits.
   The problem this library is trying to solve is to organize the data in a way there the end user just query all the data it needs and write the transfom and it is done.

   Input:
   Component
   Register: World, Data_Type, Int
   Store: World, Entities, Data_Type_Identifiers
   Remove: Word, Entities, Data_Type_Identifiers
   Query: Word, Data_Types


   Entity
   Add: World, Int
   Remove: World, Int_Collection

   Output:
   Component
   Register: Data_Type_Identifiers
   Store: Nothing
   Remove: Nothing
   Query: Query
   
   Entity
   Add: Int_Collection
   Remove: Nothing


   Design:
   
   Queries will be by specific indices that are returned when calling register_components, If we query by typeid it will not distingush the different interpreted Position data types.
   (using position as an example) It will fetch all the Position data, which is not what we want. The interperation of the data is up to the user. They may specify a Position != Position
   if the user interprets the data differently (example Player Position, Decal Position) or a data can indeed be Position == Position.

   --------------------------------------------------------------------------------------------
   Limitation:

   The specified memory requirement set by the user can only be multiple of page sizes in this library.
   Because the library will assume all operation is correct and the specified memory requirement set is sufficient it will not do any resizing. (don't see this as a weakness)
   
   Storing indices of the data will be invalid/incorrect every time there is a change in the specified collection (Adding, Removing, and Querying).

   Storing pointer of other data will mostly be invalid/incorrect when the library changes the specified collection (don't see this as a weakness)

   Removing and adding data while transforming the data is undefined behaviour and may cause a crash.

   The library will handle data in bulk, so single changes will be slower
   eg CreateEntities(&world, 60) will be faster than CreateEntities(&world, 1)

   The data can only be struct

   Header data and meta data maybe stored in memory to avoid using generic in this library.

   No notion of events (hooks, observer, observable, etc...) buitin the library (don't see this as a weakness)
   --------------------------------------------------------------------------------------------
   Constraint:
   
   Explicit memory utilization (Size, and Capacity) requirement set by the user.
   
   There will be no shared components. This will be up to the user
   
   There will be no resources. This will be up to the user

   It is best not to do any data structure changes in the transform. (Adding/Removing data)

   There will not be any dependency on the data. The user can create depenency of the data if they want in the transform (if statement).

   majority of the procedure of this library will be done in bulk

   The library will not a builtin component events (such as on value change, on component added, on component removed, etc....). This will force
   the user to pay higher memory usage and performance even though they may not use it. The end user can implement it over the library if needed.
   --------------------------------------------------------------------------------------------
   Assumption:

   The library will assume each data is unique for example you can register more than on Position. The interpretation of the data is up to the user not the library.
   eg. The user might want to seperate Npc Position from Enemy Position. Even though they are both Position. Instead of having one Position, and creating two struct
   one Npc struct and the other Enemy struct which distingush the difference of each Position struct.

   The library will assume each operation it does is correct and will not do any checks, so invalid use will be undefined.

   The amount of components (array of data) and entity (index) that will be stored can be really high, so memory usage is important.

   We will assume that the each unique data will be stored in homogenous collection.

   We will assume that manipulating the organization of the data will happen less frequently than the actual system.
   eg. Adding Removing or Querying the data will happen less frequently than transforming the data.
   --------------------------------------------------------------------------------------------
   Goals:

   Data organization and easy query sets of data for transform.

   The Data and Transforms can operate on different thread and be thread safe. This does not mean sync primitive, but rather it must be implementated
   where overlapping writes and overlapping write than read on different threads is possible.
   
   The implementation must not cause any Cache, Memory, and Performance issues on the end user (must be fast and efficient).

   We need to organize the data in a way where the actual transform implemented by the user is set up to be really fast and optimized because of how the data is organized.

   We need to organize the data in a way to allow the end user to use both SIMD and single types on the data when implementing the transform.

 */

 BYTE :: distinct u8
 WORD :: distinct u16
 DWORD :: distinct u32
 QWORD :: distinct u64

 PAGE_SIZE :: 4096
 PAGE_BIT_SIZE :: PAGE_SIZE * 8

 TILE_SIZE :: 8

 //What is the access order (Hot first, Cold last) are the data meaning full for the structure (for the computer)
 //For destroying the world we will use a null Dealloc (let the OS reclaim the pages after the application ends). Thus we will not store data
 //for deallocation
 World :: struct{
	 data_storage : [^]DataStorage,
	 indices : [^]QWORD, 
 }

 //What is the access order (Hot first, Cold last) are the data meaning full for the structure (for the computer)
 DataStorageIMM :: struct{
	 blob : rawptr, //[[data,....................], [entity,.....................]]
	 bytes_offset : QWORD, // the end of the [data] and the start of the [entity] in bytes
 }

 //What is the access order (Hot first, Cold last) are the data meaning full for the structure (for the computer)
 DataStorageMUT :: struct{
	 //TODO: We need something more primitive here. Remeber we are discarding meaning from data and just treating it as data.
	 current_byte_offset : QWORD,
	 grouping : [2]DWORD,
 }

 //What is the access order (Hot first, Cold last) are the data meaning full for the structure (for the computer)
 DataStorage :: struct {
	 imm : DataStorageIMM,
	 mut : DataStorageMUT,
 }
 
 @(optimization_mode="favor_size") 
 create_world :: #force_inline proc ($indices_capacity : QWORD, $unique_data_capacity : QWORD) -> World
	where indices_capacity > 0 && unique_data_capacity > 0{
		world : World = ---

		TARGET_UNIQUE_DATA_CAPACITY :: unique_data_capacity  * size_of(DataStorage)
		TARGET_INDICES_CAPACITY :: (indices_capacity + 0x08) 

		{
			world.data_storage = transmute([^]DataStorage)intrinsics.syscall(
				linux.SYS_mmap,
				0x00,
				uintptr((TARGET_UNIQUE_DATA_CAPACITY + 0xFFF) & 0xFFFFFFFFFFFFF000),
				uintptr(0x03),
				uintptr(0x21),
				~uintptr(0),
				uintptr(0))

			//idea: first byte will be used for recycled entities each bit that is 1 will represent which index an entity has been removed the entity bit can be determined by using BMI possibly.
			 world.indices = transmute([^]QWORD)intrinsics.syscall(
				 linux.SYS_mmap,
				 0x00,
				 uintptr((TARGET_INDICES_CAPACITY + 0x7FFF) / PAGE_BIT_SIZE * PAGE_SIZE),
				 uintptr(0x03),
				 uintptr(0x21),
				 ~uintptr(0),
				 uintptr(0))

			 world.indices[0x00] = 0x40
		 
		}

		//TODO:We may give advise how the allocation is used or something else. We don't really know the access pattern yet.
		return world
	 	  
 }


 @(optimization_mode="favor_size") 
 register_data_storage :: proc(world : ^World, $data_typeid : typeid, $data_storage_index : QWORD, $indices_capacity : QWORD)
	where intrinsics.type_is_struct(data_typeid) #no_bounds_check{

		//Should we reserve the first index in the data storage?  What will we keep in it?
		INDICES_SIZE :: indices_capacity * size_of(QWORD)
		DATA_SIZE :: (indices_capacity * size_of(#soa[TILE_SIZE]data_typeid)) / TILE_SIZE

		TOTAL_SIZE :: (INDICES_SIZE + DATA_SIZE + 0xFFF) & 0xFFFFFFFFFFFFF000

		{
			world.data_storage[data_storage_index].imm = {
				transmute(rawptr)intrinsics.syscall(
					linux.SYS_mmap,
					uintptr(0x00),
					uintptr(TOTAL_SIZE),
					uintptr(0x03),
					uintptr(0x21),
					~uintptr(0),
					uintptr(0)),
				DATA_SIZE,
			}

		}

		//TODO:We may give advise how the allocation is used or something else. We don't really know the access pattern yet.
 }



//- When create a procedure and calling a procedure think of these following question;
//1) Do i get a valid value back everytime?
//2) What type of value do I get back?
//3) Can i use the value without checking?
//4) Can i run the fuction repeatedly and produce the same effect without allocating memory per call?
 //TODO:Khal Disassemble Me and optimize me!
@(optimization_mode="favor_size",enable_target_feature = "bmi,bmi2")
create_indices :: proc(world : ^World, $indices_count : QWORD){

	/*
	   Reminders

	   Write code that operate in batches (When there is one there is many)
	   Reserve the zero index for header or info
	   Reduce code path because every new path can represents a new possibility of code failure

	   write for readability
	   don't make the code pessimistic
	   design around data layout and data flow first
	   be explicit rather than implicit. This also applies to the user
	   Primitive type and Explicit size
	   error are just data
	   where does read and write occur what is mutated?
	   When reading use 64 bytes when possible
	   When write use 32 bytes when possible
	   Use scope rather than function calls
	 */
	full_slots :: indices_count / 0x40
	partial_bits :: indices_count & 63

	occupying_bits : QWORD = ---
	current_slot : QWORD = ---
	total_used_bits : QWORD = ---
	carry_over_index : QWORD = --- 

	occupying_bits = world.indices[0x00] + partial_bits
	current_slot = world.indices[0x00] / 0x40

	total_used_bits = (full_slots * 0x40) + occupying_bits
	carry_over_index = total_used_bits / 0x40

	for i in 0..<full_slots{
		world.indices[current_slot + i] = 0xFFFFFFFFFFFFFFFF
	}

	world.indices[current_slot + full_slots] = 0xFFFFFFFFFFFFFFFF

	world.indices[carry_over_index] = (1 << (total_used_bits % 64)) - 1


	world.indices[0x00] = total_used_bits
}

 //Used for testing.
 main :: proc(){

	 world : World = ---

	 a :: struct{
		 c : f32,
	 }

	 world = create_world(700, 500)
	 register_data_storage(&world, a, 0, 200)
	 create_indices(&world, 500)
	 create_indices(&world, 200)
	 create_indices(&world, 100)

	 b := world.indices[0x00] / 0x40 

	 for i in 1..=b{
		 fmt.println("index: ",world.indices[i])
	 }


 }
