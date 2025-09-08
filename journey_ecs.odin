package journey


import "base:intrinsics"
import "core:sys/linux"


//Debug use
import "base:runtime"
import "core:fmt"
import "core:strings"


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



 BYTES_BIT_SIZE :: 8
 WORD_BIT_SIZE :: 16
 DWORD_BIT_SIZE :: 32
 QWORD_BIT_SIZE :: 64

 PAGE_SIZE :: 4096
 PAGE_BIT_SIZE :: PAGE_SIZE * 8

 TILE_SIZE :: 8

 DUMP :: #config(CSV_DUMP, false)

when ODIN_DEBUG && DUMP{

    //We don't care about perf here. We are using this for dumping.
    
    CSVColumn ::struct{
        header : string,
        data : [dynamic]QWORD,
    }

    CSVDump :: struct{
        cols : []CSVColumn,
        index : map[string]QWORD,
        highest : QWORD,
    }


    csv_global : CSVDump
    csv_index : QWORD = 0
    @(init)
    setup_csv :: proc(){
        csv_global.cols = make([]CSVColumn, 64)
        csv_global.index = make_map(map[string]QWORD)

     }

    append_csv :: proc($header : string, data : QWORD){

        occupied := header in csv_global.index


        if !occupied{
            csv_global.index[header] = csv_global.highest
            csv_global.highest += 1
        }


        index := csv_global.index[header]
       
        if csv_global.cols[index].header == "" {
            csv_global.cols[index] = { header, make([dynamic]QWORD, 0, 64)}
        }

        runtime.append_elem(&csv_global.cols[index].data, data)


    }

    dump_csv_to_file :: proc($path : cstring){
        
        max_data_length := 0

        builder := strings.builder_make()

        
        for index in 0..=csv_global.highest{
            if index < csv_global.highest{
                strings.write_string(&builder, csv_global.cols[index].header)

                strings.write_string(&builder, ", ")
            }else{
                strings.write_string(&builder, csv_global.cols[index].header)
                strings.write_string(&builder, "\n")
            }


            if len(csv_global.cols[index].data) > max_data_length{
                max_data_length = len(csv_global.cols[index].data)
            }

        }

        for i in 0..<max_data_length{


            for index in 0..<csv_global.highest{
                
                current_col := &csv_global.cols[index]

                if (len(current_col.data) - 1) < i{
                    strings.write_string(&builder, "-1")
                }else{

                    strings.write_u64(&builder, u64(current_col.data[i]))                    
                }

                if index != (csv_global.highest - 1){

                    strings.write_string(&builder, ", ")
                }else{
                    strings.write_string(&builder, "\n")
                    //fmt.println(index, "end")
                }
                

            }

        }


        file_handle, _ := linux.open(path, {.RDWR, .CREAT}, {.IWUSR, .IRUSR})

        linux.truncate(path, 0)
        linux.write(file_handle, builder.buf[:])
    }
}




//Dont have this in my version of Odin
@(private, default_calling_convention = "none")
foreign _ {
	@(link_name = "llvm.x86.bmi.bzhi.32")
	bzhi_u32 :: proc(a, index: u32) -> u32 ---
	@(link_name = "llvm.x86.bmi.bzhi.64")
	bzhi_u64 :: proc(a, index: u64) -> u64 ---
}

 //What is the access order (Hot first, Cold last) are the data meaning full for the structure (for the computer)
 //For destroying the world we will use a null Dealloc (let the OS reclaim the pages after the application ends). Thus we will not store data
 //for deallocation
World :: struct{
	 data_storage : [^]DataStorage,
	 indices : [^]QWORD,
     //very cold data for handling adding and removing of data_storage (blob) adding and removing data and indices at runtime will be considered
     //extremely rarely. 
 }


DataDetail :: struct {
    current_bytes_offset : QWORD,
    indices_bytes_offset : QWORD, 
    data_size : QWORD,
}

 //What is the access order (Hot first, Cold last) are the data meaning full for the structure (for the computer)
DataStorage :: struct{
     blob : rawptr,
    using detail : DataDetail,
 }

 @(optimization_mode="favor_size") 
 create_world :: proc ($indice_bit_capacity : QWORD, $unique_data_capacity : QWORD) -> World
	where indice_bit_capacity > 0 && unique_data_capacity > 0{
		world : World = ---

		TARGET_UNIQUE_DATA_CAPACITY :: unique_data_capacity  * size_of(DataStorage)
		TARGET_INDICES_CAPACITY :: (indice_bit_capacity + 0x08) 

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


        when ODIN_DEBUG && DUMP{

            append_csv("w i/r indices_bit_capacity", indice_bit_capacity)
            append_csv("w i/r unique_data_capacity", unique_data_capacity)
            append_csv("w l/r target_unique_data_capcity", TARGET_UNIQUE_DATA_CAPACITY)
            append_csv("w l/r target_indices_capacity", TARGET_INDICES_CAPACITY)
            append_csv("w r target_unique_data_page_size", (TARGET_UNIQUE_DATA_CAPACITY + 0xFFF) & 0xFFFFFFFFFFFFF000)
            append_csv("w r target_indices_page_size", (TARGET_INDICES_CAPACITY + 0x7FFF) / PAGE_BIT_SIZE * PAGE_SIZE)
            append_csv("w o/w data_storage_address", QWORD(uintptr(world.data_storage)))
            append_csv("w o/w indices_address", QWORD(uintptr(world.indices)))
            append_csv("w o/w world_address", QWORD(uintptr(&world)))
        }
        
        

		//TODO:We may give advise how the allocation is used or something else. We don't really know the access pattern yet.
		return world
	 	  
 }


//TODO:Khal indices_size will change since we are using a low..high 
@(optimization_mode="favor_size") 
 register_data_storage :: proc(world : ^World, $data_typeid : typeid, $data_storage_index : QWORD, $indices_capacity : QWORD)
	where intrinsics.type_is_struct(data_typeid){

        //TODO:Khal we need to make sure that INDICE_SIZE is aligned to 32 for optimal SIMD 
		INDICES_SIZE :: indices_capacity * size_of(QWORD) * 2 / TILE_SIZE 
		DATA_SIZE :: (indices_capacity * size_of(#soa[TILE_SIZE]data_typeid)) / TILE_SIZE

        TOTAL_SIZE :: (INDICES_SIZE + DATA_SIZE + 0xFFF) & 0xFFFFFFFFFFFFF000

		{

            data_storage : ^DataStorage = &world.data_storage[data_storage_index]

            data_storage.blob = transmute(rawptr)intrinsics.syscall(
                linux.SYS_mmap,
				uintptr(0x00),
				uintptr(TOTAL_SIZE),
				uintptr(0x03),
				uintptr(0x21),
				~uintptr(0),
				uintptr(0),
            )


            data_storage.detail = DataDetail{
                0, INDICES_SIZE, size_of(data_typeid)
            }

	    }
		//TODO:We may give advise how the allocation is used or something else. We don't really know the access pattern yet.
        

        when ODIN_DEBUG && DUMP{

            append_csv("r i/rw world_address", QWORD(uintptr(world)))
            append_csv("r i/r data_storage_index", data_storage_index)
            append_csv("r i/r indices_capacity", indices_capacity)
            append_csv("r l/r indices_size", INDICES_SIZE)
            append_csv("r l/r data_size", DATA_SIZE)
            append_csv("r l/r total_size", TOTAL_SIZE)
            append_csv("r o/w current_bytes_offset", 0)
            append_csv("r o/w indice_bytes_offset", INDICES_SIZE)
            append_csv("r o/w data_size", size_of(data_typeid))
            

        }
}

//TODO:Khal optimize me
 //Recycling is not implemented. (may not be implemented)
@(optimization_mode="favor_size", enable_target_feature = "bmi2")
create_indices :: proc(world : ^World, $bits_to_use : QWORD) -> QWORD{
	NUMBER_OF_QWORD_OCCUPIED :: bits_to_use / 0x40
	ALIGNED64_BITS_TO_USE :: NUMBER_OF_QWORD_OCCUPIED * 0x40

	indices_buffer : [^]QWORD = ---
	current_bit_used : QWORD = ---
	remaining_bit_mask : QWORD = ---
	
	current_bit_used = world.indices[0x00]

	indices_buffer = world.indices[current_bit_used / QWORD_BIT_SIZE:]
	remaining_bit_mask = QWORD(bzhi_u64(u64(0xFFFFFFFFFFFFFFFF), u64(current_bit_used + bits_to_use) % QWORD_BIT_SIZE))//(1 << (total_bit_used % QWORD_BIT_SIZE)) - 1	

    when NUMBER_OF_QWORD_OCCUPIED > 0{
	    for i in 0..<NUMBER_OF_QWORD_OCCUPIED{
		    indices_buffer[i] = 0xFFFFFFFFFFFFFFFF
        }
    }

	indices_buffer[NUMBER_OF_QWORD_OCCUPIED] = 0xFFFFFFFFFFFFFFFF

	if !transmute(b64)(((current_bit_used + bits_to_use) / QWORD_BIT_SIZE) - ((current_bit_used + ALIGNED64_BITS_TO_USE) / QWORD_BIT_SIZE)){
		indices_buffer[NUMBER_OF_QWORD_OCCUPIED] = remaining_bit_mask
	}

	indices_buffer[NUMBER_OF_QWORD_OCCUPIED + 1] = remaining_bit_mask

	world.indices[0x00] += bits_to_use


    
    when ODIN_DEBUG && DUMP{

        append_csv("i i/rw world_address", QWORD(uintptr(world)))
        append_csv("i i/r bits_to_use", bits_to_use)
        append_csv("i l/r qword_occupied", NUMBER_OF_QWORD_OCCUPIED)
        append_csv("i l/r aligned64_bits_to_use", ALIGNED64_BITS_TO_USE)
        append_csv("i l/r current_bit_used", current_bit_used)
        append_csv("i l/r current_indice_index", current_bit_used / 0x40)
        append_csv("i l/r remaining_bits", remaining_bit_mask)
        carry_over := !transmute(b64)(((current_bit_used + bits_to_use) / QWORD_BIT_SIZE) - ((current_bit_used + ALIGNED64_BITS_TO_USE) / QWORD_BIT_SIZE))
        append_csv("i l/r is_bits_carried_over", QWORD(carry_over))
        append_csv("i o/w target_bit_used", world.indices[0x00])
        append_csv("i o/w target_indice_index", world.indices[0x00] / 0x40)
        append_csv("i o/w indice", (bits_to_use * 0x100000000) | (current_bit_used - 0x40))


    }
    
	return (bits_to_use * 0x100000000) | (current_bit_used - 0x40)
}

@(optimization_mode="favor_size")
bind_indices_to_data :: proc(world : ^World, $storage_index : QWORD, indices : [$N]QWORD) #no_bounds_check {

    data_storage : ^DataStorage = ---
    blob_identifier_ptr : [^]QWORD = ---
    
    data_storage = &world.data_storage[storage_index]
    blob_identifier_ptr = transmute([^]QWORD)(uintptr(data_storage.blob) + uintptr(data_storage.current_bytes_offset))
    
    //i < indices_count
    for i : QWORD = 0; transmute(b64)(i - N); i+=1{
        current_indice : QWORD = ---

        current_indice = indices[i]

        //i * 2
        blob_identifier_ptr[0x00] = QWORD(DWORD(current_indice))
        blob_identifier_ptr[0x01] = QWORD(current_indice / 0x100000000)

        blob_identifier_ptr = transmute([^]QWORD)(uintptr(blob_identifier_ptr) + 0x10)
    }

    data_storage.current_bytes_offset += (N * 0x10)


    when ODIN_DEBUG && DUMP{

        append_csv("b i/r world_address", QWORD(uintptr(world)))
        append_csv("b i/r storage_index", storage_index)
        for i in 0..<N{
            append_csv("b i/r indices_array", indices[i])
            append_csv("b o/r current_bit_used_indices", QWORD(DWORD(indices[i])))
            append_csv("b o/r current_bit_to_use_indices", QWORD(indices[i] / 0x100000000))
        }

        append_csv("b l/rw blob", QWORD(uintptr(data_storage.blob)))
        append_csv("b l/rw end blob", QWORD(uintptr(blob_identifier_ptr)))
        append_csv("b l/rw data_storage_address", QWORD(uintptr(data_storage)))
        append_csv("b o/rw previous_data_storage_current_byte_offset", data_storage.current_bytes_offset - (N * 0x10))
        append_csv("b o/rw data_storage_current_byte_offset", data_storage.current_bytes_offset)
    }
}

 //Used for testing. Remove when fully implemented.
 main :: proc(){

     HEALTH_STORAGE_INDEX :: 0
     NPC_POSITION_STORAGE_INDEX :: 1
     ENEMY_POSITION_STORAGE_INDEX :: 2
     PROP_STORAGE_INDEX :: 7

     PropData :: struct{
         foo : QWORD,
         bar : DWORD,
         baz : DWORD,
     }
     
	 Health :: struct{
		 bar : f32,
	 }

     Position :: struct{
         x : f32,
         y : f32,
     }


     world : World = ---

     
	 world = create_world(900, 30)
     world = create_world(100,57)
     world = create_world(575, 123)
     world = create_world(236, 353)
     world = create_world(345, 512)
     world = create_world(1, 2)
     world = create_world(53, 24)
     world = create_world(999, 1)
     world = create_world(1, 999)
     
     /*
     //Register Health "component" in the world. (max of 100 "entities")
	 register_data_storage(&world, Health, HEALTH_STORAGE_INDEX, 100)

     //Register (NPC) Position "component" in the world. (max of 200 "entities")
     register_data_storage(&world, Position, NPC_POSITION_STORAGE_INDEX, 200)

     //prop data
     register_data_storage(&world, PropData, PROP_STORAGE_INDEX, 123)
     
     //Register (Enemy) Position "component" in the world. (max of 300 "entities")
     register_data_storage(&world, Position, ENEMY_POSITION_STORAGE_INDEX, 300)

     props := create_indices(&world, 53)
     //Create 20 "entities"
	 enemies := create_indices(&world, 20)
     //Create 50 "entities"
	 npc := create_indices(&world, 50)
     npc_merchant := create_indices(&world, 37)

     bind_indices_to_data(&world, PROP_STORAGE_INDEX, [1]QWORD{props})
     bind_indices_to_data(&world, HEALTH_STORAGE_INDEX, [2]QWORD{enemies, npc})
     bind_indices_to_data(&world, NPC_POSITION_STORAGE_INDEX, [1]QWORD{npc})
     bind_indices_to_data(&world, ENEMY_POSITION_STORAGE_INDEX, [1]QWORD{enemies})
     bind_indices_to_data(&world, NPC_POSITION_STORAGE_INDEX, [1]QWORD{npc_merchant})
     bind_indices_to_data(&world, HEALTH_STORAGE_INDEX, [1]QWORD{npc_merchant})
     */
     
     

     when ODIN_DEBUG && DUMP{

         dump_csv_to_file("/home/khalid/Documents/GitHub/Journey_ECS/dump.csv")

     }


     
     //TODO:Khal Procedure to work on:
     //release_indices_from_data (this will be slow), since we will assume it will rarely be called at runtime.
     //Get identifier with datas
     //get identifier from data
     //Has data?
     //Get Data bulk?
     //Set Data bulk?
     //Get All Data?
     //Removing individual Data will be really slow
     //Removing bulk data will be faster.
     //Fetch Alive entities
     //Query
     //Run
     //Recylce "entity" (Way later... or possible no implemented)
    
 }
