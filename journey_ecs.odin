package journey

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
   --------------------------------------------------------------------------------------------
   Constraint:
   
   Explicit memory utilization (Size, and Capacity) requirement set by the user.
   
   There will be no shared components. This will be up to the user
   
   There will be no resources. This will be up to the user

   It is best not to do any data structure changes in the transform. (Adding/Removing data)

   There will not be any dependency on the data. The user can create depenency of the data if they want in the transform (if statement).

   majority of the procedure of this library will be done in bulk
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

 
 World :: struct{
	 entities : [^]BYTE,
	 data_sparse : [^]DataSparse,
 }

 DataSparse :: struct{
	 //Others.
	 component_blob : rawptr,
	 entity_blob : rawptr,
 }


 //Do I get a valid value back everytime?
 //What type of value do I get back?
 //Can I use the value without any check?
 //Can I run the function repeatly and procduce the same effect  
 init_world :: proc(world : ^World, $entity_capacity : DWORD, $unique_component_capacity : DWORD){

	 //Entity Init
	 //TODO:Khal Do we need to reserve the first element for some header or meta data?
	 {		 
		 entity_page_count_required : DWORD = ---

		 entity_page_count_required = (entity_capacity + 32767) / PAGE_BIT_SIZE
		 ptr, _ := linux.mmap(0x00, uint(entity_page_count_required * PAGE_SIZE), {.READ, .WRITE}, {.ANONYMOUS}, linux.Fd(-1), 0)
		 world.entities = cast([^]BYTE)ptr
	 }

	 //Data Store Init
	 {



	 }
 }









 //Used for testing.
 main :: proc(){

	 world : World

	 init_world(&world, 4095, 200)

 }
