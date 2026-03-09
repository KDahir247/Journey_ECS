package journey


import "base:intrinsics"
import "core:sys/linux"

//Debug use
import "core:fmt"
import "core:strings"
import "base:runtime"
/*
   What is the problem (informal):
   - We want to figure out a way to organize entity's components in a way where it is possible to always read/write from a full cache line and use all of it.
   In most cases we only use only a small subset of data (not reading from all field in a struct) and the rest is wasted. We want the end user to think
   about the notion that there is data, which in this case is the component and there is a transform, which is the system and just optimize that. 

   - Rather than think that each entity is an Object and than operate each object differently (which is easier to reason about) and thus causing waste in most cases. We want a easy way for 
   user to use the notion of data and transform. Something that doesn't cause high friction in thier project and still reap the optimization benefits.
   The problem this library is trying to solve is to organize the data in a way there the end user just query all the data it needs and write the transfom and it is done. 
 
   - We want to make the implementation by default thread safe when the user implement the system. This doesn't really mean excessive locking or syncing like how typical games or games engine are done. We want to create a way to avoid all that 
   when possible, 


Design:
   
   Queries will be by specific indices that are specificed by the user when calling register_components, If we query by typeid it will not distingush the different interpreted Position data types.
   (using position as an example) It will fetch all the Position data, which is not what we really want. The interperation of the data is up to the user. They may specify a Position != Position
   if the user interprets the data differently (example Player Position, Decal Position) or a data can indeed be Position == Position depending on the user interperation.

   Data will be stored in a ColumnarPage which is a multiple of page sizes. Within a ColumnarPage there is a global metadata for all the LogicalPage. The LogicalPage contains a bit_mask which is used to zero out component that 
   has no "entity" assign to it or if the "entity" is deleted (there is actually no representation of entity in our implementation) 
   So for example the system will still do the logic for a deleted entity component let say we move the entity with Position, so even though the data doesn't contain a mapping to an entity or it is deleted it will still do the logic 
   but on the final pass of the system it will do a AND operator with the zero_bit_mask which will zero out the data for the component with no entity. Exactly like SIMD MASKING
   --------------------------------------------------------------------------------------------
   Limitation:

   The specified memory requirement set by the user can only be multiple of page sizes in this library.
   Because the library will assume all operation is correct and the specified memory requirement set is sufficient it will not do any resizing. (don't see this as a weakness)
   
   Storing indices of the data will be invalid/incorrect every time there is a change in the specified collection (Adding, Removing, and Querying).

   Storing pointer of other data will mostly be invalid/incorrect when the library changes the specified collection (don't see this as a weakness)

   Removing and adding data while transforming the data is undefined behaviour and may cause a crash.

   The library will handle data in bulk, so single changes will be slower 

   The data can only be struct

   Header data and meta data maybe stored in memory to avoid using generic in this library.

   No notion of events (hooks, observer, observable, etc...) buitin the library (don't see this as a weakness) 
 
   Each LogicalPage can hold at maximum 1024 "entites", so if more "entities" are required create more LogicalPage for the specific ColumnarPage
   --------------------------------------------------------------------------------------------
   Constraint:
   
   Explicit memory utilization (Size, and Capacity) requirement set by the user.
   
   There will be no shared components. This will be up to the user
   
   There will be no resources. This will be up to the user

   There will not be any dependency on the data. The user can create depenency of the data if they want in the transform (if statement).

   majority of the procedure of this library will be done in bulk

   The library will not a builtin component events (such as on value change, on component added, on component removed, etc....). This will force
   the user to pay higher memory usage and performance even though they may not use it. The end user can implement it over the library if needed. 

   There will not be deleting data or indices after we bind it to a columnarPage. We may reuse it (free list), but there will be no structural changes after binding to the ColumnarPage.  
 
   Deleting data or removing data will be implemented by using a Deferred Operation Buffer and a Sync point. All deferred operation will be done on the main thread and no other threads 
   deferred operations on other threads instead of the main thread will cause some undefined behaviour. 

   Querying and Grouping will be very fast and will use alot of assumption, so reordering the data in the ColumnarPage will break it and break the logic
   --------------------------------------------------------------------------------------------
   Assumption:

   The library will assume each data is unique for example you can register more than on Position. The interpretation of the data is up to the user not the library.
   eg. The user might want to seperate Npc Position from Enemy Position. Even though they are both Position. Instead of having one Position, and creating two struct
   one Npc struct and the other Enemy struct which distingush the difference of each Position struct.

   The library will assume each operation it does is correct and will not do any checks, so invalid use will be undefined.

   The amount of components (array of data) and entity (index) that will be stored can be really high, so memory usage is important.

   We will assume that the each unique data will be stored in homogenous collection.

   We will assume that manipulating the organization of the data will happen less frequently than the actual system.
   eg. Adding, Querying the data will happen less frequently than transforming the data. 

  We will assume that there will be no structual change at all in the implementation
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

SINGLE :: distinct f32
DOUBLE :: distinct f64
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


//TODO:Khal we need to incorperate a way to make each new procedure call go in a new row, currently it just add to the column 
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

        occupied : bool = header in csv_global.index


        if !occupied{
            csv_global.index[header] = csv_global.highest
            csv_global.highest += 1
        }


        index : QWORD = csv_global.index[header]
       
        if csv_global.cols[index].header == "" {
            csv_global.cols[index] = { header, make([dynamic]QWORD, 0, 64)}
        }

        runtime.append_elem(&csv_global.cols[index].data, data)
    }
    dump_csv_to_file :: proc($path : cstring){
        
        max_data_length : int = 0

        builder : strings.Builder = strings.builder_make()
        
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
                
                current_col : ^CSVColumn = &csv_global.cols[index]

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

sysm_proc :: #type proc(data_buffer : [^]BYTE, data_meta : DataMeta) 

DataMeta :: struct{
    lane_count : QWORD,
    total_indices : QWORD,
}

SysmValue :: union{
    SINGLE,
    DOUBLE,
    BYTE,
    WORD,
    DWORD,
    QWORD,
}

CombinatorOperation :: enum BYTE{
    NIL = 0,
    AND,
    OR,
    //NOT,
    //XOR,
    //NAND
}

ComparisionOperation :: enum BYTE{
    GT,
    GTE,
    EQ,
    NEQ,
    LT,
    LTE,
    NOTIN,
    IN,
}

SystemPredicate :: struct{
    val : QWORD,
    offset : WORD,
    cmp_op : ComparisionOperation,
    combinator_op : CombinatorOperation,
}

PageThreadAccess :: enum u8{
    ThreadLocal,
    ThreadReadonly,
    ThreadReadWrite,
}

PageStructuralOperation :: enum u8{
    StructuralChange,
    NoStructuralChange,
}

PageDataAccess :: enum u8{
    InputOutput,
    Input,
    Output,
}

PageMode :: enum u8{
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
    header : [^]ColumnarHeader,
    columnar_table : [^]ColumnarPage(PAGE_COUNT),
    table_count : QWORD,
    owner_thread_id : QWORD,
    ds_ops : [^]StructuralOperation,
    dso_buffer : [^]BYTE,
    //TODO:Khal structure layout is not done yet.
    free_list : [^]FreeList,
    sync_frame_gen : QWORD,
    
    
    sync_point : [4]AtomicSynchronization,
}

//The size of the Op structs can not be greater than 32 bytes
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

ColumnarHeader :: struct{
    allocated_data_bytes : QWORD,
    reserved_data_bytes : QWORD,
    
    data_size : DWORD,
    start_indices : DWORD,
    end_indices : DWORD,

    thread_access : PageThreadAccess,
    structural_op : PageStructuralOperation,
    data_access : PageDataAccess,
    page_mode : PageMode,
}


FreeList :: struct{
    temp : QWORD,
    //implement me
}

ColumnarPage :: struct($PAGE_COUNT: QWORD){
    bit_zero_mask : [16 * PAGE_COUNT]QWORD,
    payload : [3968 * PAGE_COUNT]BYTE,
}


AtomicSynchronization :: struct #align(64){
    atomic_bitmask : DWORD,
    //TODO:Khal add sync metadata if needed
}


@(optimization_mode="favor_size") 
init_world :: proc (world : ^World($page_count, $thread_count), $unique_data_capacity : QWORD)
where page_count > 0 && thread_count <= 4 && thread_count & 1 == 0 {

    when ODIN_DEBUG && DUMP{
        append_csv("init_world", 0)
        append_csv("World address input w", QWORD(uintptr(world)))
        append_csv("page_count constant r", QWORD(page_count))
        append_csv("thread_count constant r", QWORD(thread_count))
        append_csv("unique_data_count constant r", QWORD(unique_data_capacity))
     }

    {
        buffer_address : uintptr = ---

        HEADER :: ColumnarHeader
        COLUMNAR :: ColumnarPage(page_count)

        REQUIRED_HEADER_BYTES : QWORD : (size_of(HEADER) * unique_data_capacity + 0xFFF) & 0xFFFFFFFFFFFFF000
        REQUIRED_COLUMNAR_BYTES : QWORD : size_of(COLUMNAR) * unique_data_capacity

        REQUIRED_TOTAL_BYTES : QWORD : REQUIRED_HEADER_BYTES + REQUIRED_COLUMNAR_BYTES

        buffer_address = intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(REQUIRED_TOTAL_BYTES),
            uintptr(0x03),
            uintptr(0x21),
            ~uintptr(0),
            uintptr(0),
        )

        world.header = cast(^HEADER)(buffer_address)
        world.columnar_table = cast([^]COLUMNAR)(buffer_address + uintptr(REQUIRED_HEADER_BYTES))
        world.table_count = unique_data_capacity
        when ODIN_DEBUG && DUMP{
            append_csv("total_columnar_bytes local r", REQUIRED_COLUMNAR_BYTES)
            append_csv("total_header_bytes local w", REQUIRED_HEADER_BYTES)
            append_csv("total_size local w", REQUIRED_TOTAL_BYTES)
            append_csv("buffer_address local w", QWORD(uintptr(buffer_address)))
            append_csv("world_header_address input w", QWORD(uintptr(world.header)))
            append_csv("world_columnar_table input w", QWORD(uintptr(world.columnar_table)))
        }
        
    }

    world.owner_thread_id = QWORD(intrinsics.syscall(linux.SYS_gettid))
    
    {
        REQUIRED_DS_OP_BYTES : QWORD : 4096 //assuming the op enum to be 1 byte so we can issue 4096
        REQUIRED_DS_BUFFER_BYTES : QWORD : 131072 //assuming limit of op struct to be 32 byte so (131072 / 32) is 4096, so we can issue 4096 
         
        world.ds_ops = cast([^]StructuralOperation)intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(REQUIRED_DS_OP_BYTES),
            uintptr(0x03),
            uintptr(0x21),
            ~uintptr(0),
            uintptr(0),
        )
        
        world.dso_buffer = cast([^]BYTE)intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(REQUIRED_DS_BUFFER_BYTES),
            uintptr(0x03),
            uintptr(0x21),
            ~uintptr(0),
            uintptr(0),
        )

        when ODIN_DEBUG && DUMP{
            append_csv("ds_op_bytes constant r", QWORD(REQUIRED_DS_OP_BYTES))
            append_csv("ds_buffer_bytes constant r", QWORD(REQUIRED_DS_BUFFER_BYTES))
            
            append_csv("world_ds_ops_address input w", QWORD(uintptr(world.ds_ops)))
            append_csv("world_ds_buffer_address input w", QWORD(uintptr(world.dso_buffer)))
        }
    }

    //TODO:Khal add FreeList initialization
    {





    }

    when ODIN_DEBUG && DUMP{
        append_csv("world_owner_thread_id input w", QWORD(world.owner_thread_id))
    }
 }


@(optimization_mode="favor_size", enable_target_feature="avx,avx2")
register_columnar :: proc(world : ^World($page_count, $thread_count), $data_typeid : typeid, $table_index : QWORD, $start : QWORD, $end : QWORD)
where intrinsics.type_is_struct(data_typeid) && end > start &&  page_count > 0 && thread_count <= 4 && thread_count & 1 == 0{

    when align_of(data_typeid) == 8{
        LANE_COUNT : QWORD : 4
        PAGE_MODE : PageMode : .SIMD4
    }else when align_of(data_typeid) > 8 || align_of(data_typeid) < 4{
        LANE_COUNT : QWORD : 1
        PAGE_MODE : PageMode : .SCALAR
    }else when align_of(data_typeid) == 4{
        LANE_COUNT : QWORD : 8
        PAGE_MODE : PageMode : .SIMD8
    }
            
    INDICES_CAPACITY : QWORD : end - start

    SIMD_PADDED_INDICES_CAPACITY : QWORD : (INDICES_CAPACITY + LANE_COUNT - 0x01) / LANE_COUNT * LANE_COUNT
    REQUIRED_SIMD_RAW_BYTES : QWORD : size_of(data_typeid) * SIMD_PADDED_INDICES_CAPACITY
            
    CACHE_ALIGNED_RAW_BYTES : QWORD : (REQUIRED_SIMD_RAW_BYTES + 0x3F) & 0xFFFFFFFFFFFFFFC0
    OCCUPIED_CACHE_LINE : QWORD : CACHE_ALIGNED_RAW_BYTES / 0x40
            
    EVEN_DISTRIBUTED_CACHE_LINE : QWORD : (OCCUPIED_CACHE_LINE + thread_count - 1) / thread_count * thread_count
    TARGET_RAW_BYTES : QWORD : EVEN_DISTRIBUTED_CACHE_LINE * 0x40
    
    TARGET_DATA_COUNT : QWORD : TARGET_RAW_BYTES / size_of(data_typeid)

    when ODIN_DEBUG && DUMP{
        append_csv("register_columnar", 0)
        append_csv("world_address input rw", QWORD(uintptr((world))))
        append_csv("page_count constant r", QWORD(page_count))
        append_csv("thread_count constant r", QWORD(thread_count))
        //append_csv("typeid_constant r", QWORD(data_typeid))
        append_csv("table_index constant r", table_index)
        append_csv("start_constant r", start)
        append_csv("end constant r", end)
        append_csv("data_size constant r", size_of(data_typeid))
        append_csv("data_alignment constant r", align_of(data_typeid))
        append_csv("lane_count constant r", QWORD(LANE_COUNT))
        append_csv("indices_capacity constant r", QWORD(INDICES_CAPACITY))
        append_csv("simd_indices_capcity constant r", QWORD(SIMD_PADDED_INDICES_CAPACITY))
        append_csv("required_soa_aos_bytes constant r", QWORD(REQUIRED_SIMD_RAW_BYTES))
        append_csv("cacheline_aligned_soa_aos_bytes constant r", QWORD(CACHE_ALIGNED_RAW_BYTES))
        append_csv("occupying cacheline constant r", QWORD(OCCUPIED_CACHE_LINE))
        append_csv("even cacheline constant r", QWORD(EVEN_DISTRIBUTED_CACHE_LINE))
        append_csv("required_target_bytes constant r", QWORD(TARGET_RAW_BYTES))
        append_csv("required_target_byter_per_core constant r", QWORD(TARGET_RAW_BYTES) / 4)
        append_csv("required_data_count constant r", QWORD(TARGET_DATA_COUNT))
    }

    
    {
        //Bottleneck on pagefault.
        world.header[table_index] = {
            TARGET_RAW_BYTES,
            (page_count * 4096) - TARGET_RAW_BYTES,
            size_of(data_typeid),
            DWORD(start),
            DWORD(end),
                .ThreadLocal,
                .StructuralChange,
                .InputOutput,
            PAGE_MODE,
        }
                      
    }

    
    {
        TARGET_DATA_BIT_COUNT :: TARGET_DATA_COUNT >> 6
        TARGET_DATA_BIT_REMAINING :: TARGET_DATA_COUNT & 63
        for i in 0..<TARGET_DATA_BIT_COUNT{
            world.columnar_table[table_index].bit_zero_mask[i] = 0xFFFFFFFFFFFFFFFF 
        }

        world.columnar_table[table_index].bit_zero_mask[TARGET_DATA_BIT_COUNT] = (1 << TARGET_DATA_BIT_REMAINING) -1 
    }

    //TODO:Khal Set up the free list elements
    {
            

    }
}


/*
Brief Problem we are trying to solve:

We want to create a iterator that will run through the payload (BYTE buffer) and chunk the buffer
When iterating we may want some fine specific type of data within the payload or columnar_table/s
For example we may want Indices that contain both Velocity and Gravity, thus we need to filter out indices
that only contain one or the other or neither of it.

I also want to add data predicate which will be similar to most SQL database query syntax
which are the following; AND, OR, WHERE, NOT,BETWEEN(A and B), IN(A, field_name), FILTER(EQ,LT,GT,LTE, GTE)

This procedure should be really extremely fast due to it being called every frame (that will be 240 times per second for a 240 fps
                                                                                   and 144 times per second for 144 fps and 60 times
                                                                                   per second for a 60 fps (lowest_target))
Thus we need to cache as much as possible on what we can, since it is using SIMD layout internally for the payload layout
the implementation should avoid using branching for both the indices filter and the data predicate by using masking

The implementation shouldn't extensively rely on caching since the data predicate result may change, for example
a value change to be greater than let say 5 but the data predicate is for less than 5. This make caching data predicate
very cumbersome when there is any changes in the payload

Input:


Output:


Goals:


Limit:



Assumption:

The type is not known until it is converted to the the specific type by the user in the system procedure.

We know the data may change each frame thus the predicate result may change each frame, but the sequence of instruction
from the predicate will be the same for each run




*/


run_0 :: proc(world : ^World($page_count, $thread_count), $table_index : QWORD, sysm : sysm_proc, predicates : ..SystemPredicate){

    //This must be fast (Time critical) happens per frame.
    for pred in predicates{
        
        
        
        


    }


    
    
    sysm(raw_data(world.columnar_table[table_index].payload[:]), {})
    
}


//TODO:Khal refer to sql update for inspiration
update :: proc(){


}

//Paralllel loop implementation?????????

  Position :: struct{
         x : f32,
         y : f32,
     }

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

   

     Bar :: struct{
         x : WORD
     }

     Baz :: struct{
         x : BYTE
     }


  	 world : World(2,4) = ---

     init_world(&world, 28)
     
     //Register (NPC) Position "component" in the world. 
     register_columnar(&world, Position, 0,0, 67)

     set_data :: proc(buf : [^]BYTE, meta : DataMeta){
         data : [^]#soa[4]Position = transmute([^]#soa[4]Position)buf

         for i in 0..<1{
             (^#simd[4]f32)(&data[i].x)^ = {2,2,2,2}
         }
         
     }

     print_data :: proc(buf : [^]BYTE, meta : DataMeta){
         data : [^]#soa[4]Position = transmute([^]#soa[4]Position)buf

         for i in 0..<1{
             fmt.println(data[i])
         }
         
     }
     
     run_0(&world, 0, set_data, {})
     run_0(&world, 0, print_data, {})
     
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
     //Sync
     //Deferred operation (recylce entities)
 }
