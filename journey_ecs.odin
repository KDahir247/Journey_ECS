package journey


import "base:intrinsics"
import "core:sys/linux"

//Debug use
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

   There will not be deleting data or indices after we bind it to a blob. We may reuse it (free list), but there will be no structural changes after binding to the blob. 
   Except for querying and grouping. This will not happen every frame rather it will happen in the initialization. 
 
   Deleting data or removing data will be implemented by the user on top of this 
   For example the may hold a free list for each type of data and indices that need to be reused because of world streaming. If dealing with
   multiple thread than the can hold a free list per thread for each chunk which will hold the reusable when in this case doing world streaming.
   In other words composition over this primitive implementation to cater their use case.
--------------------------------------------------------------------------------------------
   Assumption:

   The library will assume each data is unique for example you can register more than on Position. The interpretation of the data is up to the user not the library.
   eg. The user might want to seperate Npc Position from Enemy Position. Even though they are both Position. Instead of having one Position, and creating two struct
   one Npc struct and the other Enemy struct which distingush the difference of each Position struct.

   The library will assume each operation it does is correct and will not do any checks, so invalid use will be undefined.

   The amount of components (array of data) and entity (index) that will be stored can be really high, so memory usage is important.

   We will assume that the each unique data will be stored in homogenous collection.

   We will assume that manipulating the organization of the data will happen less frequently than the actual system.
   eg. Adding, Querying the data will happen less frequently than transforming the data. Removing the data will never happen in this implementation
   Reusing data may happen but the implementation is up to the user and should not cause structural change.

--------------------------------------------------------------------------------------------
   Goals:

   Data organization and easy query sets of data for transform.

   The Data and Transforms can operate on different thread and be thread safe. This does not mean sync primitive, but rather it must be implementated
   where overlapping writes and overlapping write than read on different threads is possible.
   
   The implementation must not cause any Cache, Memory, and Performance issues on the end user (must be fast and efficient).

   We need to organize the data in a way where the actual transform implemented by the user is set up to be really fast and optimized because of how the data is organized.

   We need to organize the data in a way to allow the end user to use both SIMD and single types on the data when implementing the transform. 

   ECS contains simple filter for example checking if a "entity" is within two columnar pages and it usually stops there.
   Can we add more filter while still making it fast for example some of the SQL filter that are reasonable and frequently used in games

 */

//Assume user has atleast 8, 12, or 16. 
//Assume user support SSE2, SSE3, SSSE3, SSE4.1, SSE4.2, AVX, AVX2
//Assume user has 16, 32 gb, and 64 gb of ram

BYTE :: distinct u8
WORD :: distinct u16
DWORD :: distinct u32
QWORD :: distinct u64
SIGNED32 :: distinct i32
SIGNED64 :: distinct i64

BYTES_BIT_SIZE :: 8
WORD_BIT_SIZE :: 16
DWORD_BIT_SIZE :: 32
QWORD_BIT_SIZE :: 64

CACHE_LINE :: 64
PAGE_SIZE :: 4096
PAYLOAD_SIZE :: 3904
PAGE_BIT_SIZE :: PAGE_SIZE * 8

XMM_BYTES :: 16
YMM_BYTES :: 32
ZMM_BYTES :: 64


 DUMP :: #config(CSV_DUMP, false)
 LOGICAL_CORE_COUNT :: #config(CORE, 8)
 PHYSICAL_CORE_COUNT :: LOGICAL_CORE_COUNT / 2


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
                }

            }

        }


        file_handle, _ := linux.open(path, {.RDWR, .CREAT}, {.IWUSR, .IRUSR})

        linux.truncate(path, 0)
        linux.write(file_handle, builder.buf[:])
    }
}

PageThreadAccess :: enum{
    ThreadLocal,
    ThreadReadonly,
    ThreadReadWrite,
}

PageStructuralOperation :: enum{
    StructuralChange,
    NoStructuralChange,
}

PageDataAccess :: enum{
    InputOutput,
    Input,
    Output,
}

PageMode :: enum{
    SIMD8,
    SIMD4,
    MASK8,
    MASK4, 
    SCALAR,
}

StructuralOperation :: enum u8{
    PUSH, 
    POP,
    GROW,
    SHRINK,
}


World :: struct($PAGE_COUNT : QWORD, $THREAD_COUNT : QWORD) #align(64){
    header : [^]TableHeader,
    columnar_table : [^]ColumnarPage(PAGE_COUNT),
    owner_thread_id : QWORD,
    ds_ops : [^]DSOperation,
    dso_buffer : [^]BYTE,
    //TODO:Khal structure layout is not done yet.
    free_list : [^]FreeList,
    sync_frame_gen : QWORD,
    _padding_ : QWORD,
    
    sync_point : [4]AtomicSynchronization,
}

PushOp :: struct{
    //implement me
}

PopOp :: struct{
    //implement me
}


GrowOp :: struct{ 
    //implement me
}

ShrinkOp :: struct{
    //implement me
}

//TODO:Khal we might shrink this to be 4 byte struct (each enum is u8)
TableHeader :: struct{
    thread_access : PageThreadAccess,
    structural_op : PageStructuralOperation,
    data_access : PageDataAccess,
    page_mode : PageMode,
}

FreeList :: struct{
    temp : QWORD,
    //implement me
}


ColumnarHeader :: struct #align(64){
    allocated_data_bytes : QWORD,
    data_bytes_per_core : QWORD,
    reserved_data_bytes : QWORD,
    reserved_data_bytes_per_core : QWORD,
    data_size : QWORD,
    data_alignment : QWORD,
    start_indices : QWORD,
    end_indices : QWORD,
}


ColumnarPage :: struct($PAGE_COUNT: QWORD){
    header : ColumnarHeader,
    //used too zero out payload data for unused "entity" (eg. when "entity is dead the bit_zero_mask specific position is set to zero" than the bit_zero_mask is converted to a simd mask to mask out the deleted entities payload
    bit_zero_mask : [PAGE_COUNT * 128]BYTE,
    payload : [(PAGE_COUNT * 3904) + ((PAGE_COUNT - 1) * size_of(ColumnarHeader))]BYTE
}


//To get the base we need (65536 * thread_id + per_thread_cursor)
DSOperation :: struct{
    op : StructuralOperation,
    thread_id : u8, 
}


AtomicSynchronization :: struct #align(64){
    atomic_bitmask : DWORD,
    //TODO:Khal add sync metadata if needed
}

//Happy
@(optimization_mode="favor_size") 
init_world :: proc (world : ^World($page_count, $thread_count), $unique_data_capacity : QWORD)
where page_count > 0 && thread_count <= 4 #no_bounds_check{

    {
        buffer_address : uintptr = ---

        HEADER :: TableHeader
        COLUMNAR :: ColumnarPage(page_count)

        TOTAL_COLUMNAR_BYTES :: size_of(COLUMNAR) * unique_data_capacity

        TOTAL_HEADER_SIZE :: (size_of(TableHeader) * unique_data_capacity + 0xFFF) & 0xFFFFFFFFFFFFF000

        TOTAL_SIZE :: TOTAL_COLUMNAR_BYTES + TOTAL_HEADER_SIZE

        buffer_address = intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(TOTAL_SIZE),
            uintptr(0x03),
            uintptr(0x21),
            ~uintptr(0),
            uintptr(0),
        )

        world.header = cast(^HEADER)(buffer_address)
        world.columnar_table = cast([^]COLUMNAR)(buffer_address + uintptr(TOTAL_HEADER_SIZE))
    }

    world.owner_thread_id = QWORD(intrinsics.syscall(linux.SYS_gettid))
    
    {
        DEFAULT_DS_SIZE :: 65536
      
        TOTAL_DS_OP_SIZE :: (DEFAULT_DS_SIZE * thread_count / 2 + 0xFFF) & 0xFFFFFFFFFFFFF000
        TOTAL_DS_BUFFER_SIZE :: (DEFAULT_DS_SIZE * thread_count + 0xFFF) & 0xFFFFFFFFFFFFF000
        
        world.ds_ops = cast([^]DSOperation)intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(TOTAL_DS_OP_SIZE),
            uintptr(0x03),
            uintptr(0x21),
            ~uintptr(0),
            uintptr(0),
        )       

        world.dso_buffer = cast([^]BYTE)intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(TOTAL_DS_BUFFER_SIZE),
            uintptr(0x03),
            uintptr(0x21),
            ~uintptr(0),
            uintptr(0),
        )         
    }

 }


//TODO:Khal we need to reimplement this
@(optimization_mode="favor_size", enable_target_feature="avx,avx2")
register_columnar :: proc(world : ^World($page_count, $thread_count), $data_typeid : typeid, $table_index : QWORD, $lane_count : QWORD, $start : QWORD, $end : QWORD)
	where intrinsics.type_is_struct(data_typeid) && end > start &&  (lane_count & 0x01) == 0 && page_count > 0 && thread_count <= 4{

        //We are still only using 24 bytes in the world.
        {
            //TODO:Khal make the naming generic. Remove Data
            INDICES_CAPACITY :: end - start
            WORKING_CHUNK :: (INDICES_CAPACITY + lane_count - 0x01) / lane_count
            EVEN_WORKING_CHUNK :: (WORKING_CHUNK + PHYSICAL_CORE_COUNT - 1) / PHYSICAL_CORE_COUNT * PHYSICAL_CORE_COUNT
            DATA_NEEDED_RAW_BYTES :: size_of(#soa[lane_count]data_typeid) * EVEN_WORKING_CHUNK
            ALIGN_CACHE_DATA_RAW_BYTES :: (DATA_NEEDED_RAW_BYTES + 0x3F) & 0xFFFFFFFFFFFFFFC0
            OCCUPIED_CACHE_LINE :: ALIGN_CACHE_DATA_RAW_BYTES / 0x40
            EVEN_CACHE_LINE :: (OCCUPIED_CACHE_LINE + PHYSICAL_CORE_COUNT - 1) / PHYSICAL_CORE_COUNT * PHYSICAL_CORE_COUNT
            TARGET_DATA_RAW_BYTES :: EVEN_CACHE_LINE * 0x40

            //Bottleneck on pagefault.
            
            world.columnar_table[table_index].header = {
                TARGET_DATA_RAW_BYTES,
                TARGET_DATA_RAW_BYTES / PHYSICAL_CORE_COUNT,
                (PAYLOAD_SIZE * QWORD(PAGE_COUNT)) - TARGET_DATA_RAW_BYTES,
                (PAYLOAD_SIZE * QWORD(PAGE_COUNT) - TARGET_DATA_RAW_BYTES) / PHYSICAL_CORE_COUNT,
                size_of(data_typeid),
                align_of(data_typeid),
                start,
                end,
            }
        }

        {
            when align_of(data_typeid) == 8{
                world.header.meta_list[table_index].page_mode = PageMode.SIMD4
            }else when align_of(data_typeid) > 8 || align_of(data_typeid) < 4{
                world.header.meta_list[table_index].page_mode = PageMode.SCALAR
            }else when align_of(data_typeid) == 4{
                world.header.meta_list[table_index].page_mode = PageMode.SIMD8
            }
        }
        
        //TODO Set up the free list
        {
            

        }
    }


//Runtime change on the columnar metadata require sync after call
commit_to_columnar :: proc(world : ^$T/World, $table_index : QWORD, $commit_count : QWORD){
    commited_data_bytes : QWORD = ---

    
    columnar := &world.columnar_table[table_index]

    COMMIT_GRANULARITY :: PHYSICAL_CORE_COUNT * CACHE_LINE
    
    committed_data_bytes := ((columnar.data_size * commit_count) + (COMMIT_GRANULARITY - 1)) / COMMIT_GRANULARITY * COMMIT_GRANULARITY

    //if not_the_owning_thread_for_the_world{
    //    sync_block for write

    //    write to the deferred structual command buffer


    //    return
    //}
    

    if committed_data_bytes < columnar.reserved_data_bytes {

        //TODO:khal sync lock and than do operation below. We than append command buffer so chunk can be rebuilt on the main thread.
        
        columnar.allocated_data_bytes += committed_data_bytes
        columnar.data_bytes_per_core +=  (committed_data_bytes / PHYSICAL_CORE_COUNT)
        columnar.reserved_data_bytes -= committed_data_bytes
        columnar.reserved_data_bytes_per_core -= (committed_data_bytes / PHYSICAL_CORE_COUNT)
        columnar.end += commit_count
    }
}

//TODO:Khal any runtime changes in the columnar metadata will require synchronization between threads.
sync :: proc(){



}

//0..53, 53..73, 73..92, 92..160
indices_intersect_blob :: proc(){
    
}


query :: proc(){



}


//TODO:Khal if we pass the responsiblity to the user to reuse the indice/s than we need to create a proc that get all the data that the indice has.
//This will be done in bulk. This will do no structual change and should be thread safe, since we are reading from the indices section.


//Paralllel loop implementation?????????


 //Used for testing. Remove when fully implemented.
 main :: proc(){

     HEALTH_STORAGE_INDEX :: 0
     NPC_POSITION_STORAGE_INDEX :: 1
     ENEMY_POSITION_STORAGE_INDEX :: 2
     PROP_STORAGE_INDEX :: 4

     PropData :: struct{
         foo : QWORD,
         bar : DWORD,
         baz : DWORD,
         t : DWORD,
     }
     
	 Health :: struct{
		 bar : f32,
	 }

     Position :: struct{
         x : f32,
         y : f32,
     }


 	 world : World(1,4) = ---

     //Create a world with 24 columnar page where each columnar page is a single page size
     init_world(&world, 24)

          
     //Register (NPC) Position "component" in the world. 
     //register_columnar(world, Position, NPC_POSITION_STORAGE_INDEX, 200)

     
     when ODIN_DEBUG && DUMP{

         dump_csv_to_file("/home/khalid/Documents/GitHub/Journey_ECS/dump.csv")

     }


     //Maybe add a free list to handle "remove" indices from the blob.
     

     //TODO:Khal Procedure to work on:
     //Remove
     //Get identifier with datas
     //get identifier from data
     //Has data?
     //Get Data bulk?
     //Set Data bulk?
     //Get All Data?
     //Query
     //Run
     //Recylce "entity" (Way later... or possible no implemented)
     //defrag indices
    
 }
