package journey

import "base:intrinsics"
import "core:sys/linux"
import "base:runtime"

//Debug use
import "core:fmt"
import "core:strings"

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

BYTES_BIT_SIZE : QWORD : 8
WORD_BIT_SIZE : QWORD : 16
DWORD_BIT_SIZE : QWORD : 32
QWORD_BIT_SIZE : QWORD : 64

CACHE_LINE : QWORD : 64
PAGE_SIZE : QWORD : 4096
PAYLOAD_SIZE : QWORD : 3904
PAGE_BIT_SIZE : QWORD : PAGE_SIZE * 8

XMM_BYTES : QWORD : 16
YMM_BYTES : QWORD : 32
ZMM_BYTES : QWORD : 64

DUMP : bool : #config(CSV_DUMP, false)

LOGICAL_CORE_COUNT : QWORD : #config(CORE, 8)
PHYSICAL_CORE_COUNT : QWORD : LOGICAL_CORE_COUNT / 2

FIRST_THREAD : QWORD : 0
SECOND_THREAD : QWORD : 1
THIRD_THREAD : QWORD : 2
FOURTH_THREAD : QWORD : 3

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
    setup_csv :: proc "contextless"(){
        context = runtime.default_context()
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

sysm_proc :: #type proc "contextless" (data_buffer : [^]BYTE, data_meta : DataMeta) 

DataMeta :: struct{
    //lane_count : QWORD,
    //data_size : DWORD,
    total_indices : DWORD,
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
    value_handle : QWORD,
    byte_offset : WORD,
    byte_stride : WORD,
    padding_0 : WORD,
    cmp_op : ComparisionOperation,
    combinator_op : CombinatorOperation,

}


THREAD_LOCAL : DWORD : 0b0000_0000_0000_0001
THREAD_READONLY : DWORD : 0b0000_0000_0000_0010
THREAD_READWRITE : DWORD : 0b0000_0000_0000_0011

STRUCTURAL_CHANGE : DWORD : 0b0000_0000_0001_0000
NO_STRUCTURAL_CHANGE : DWORD : 0b0000_0000_0010_0000
STRUCTURAL_DYNAMIC : DWORD : 0b0000_0000_0011_0000

OUTPUT : DWORD : 0b0000_0001_0000_0000
INPUT : DWORD : 0b0000_0010_0000_0000
INPUT_OUTPUT : DWORD : 0b0000_0011_0000_0000

SIMD_8 : DWORD : 0b0011_0000_0000_0000
SIMD_4 : DWORD : 0b0001_0000_0000_0000
SCALAR : DWORD : 0b0010_0000_0000_0000

World :: struct($THREAD_COUNT : DWORD){
    // 1 Cacheline (Base)
    header : [^]ColumnarHeader,
    columnar_blocks : [^]ColumnarBlock,

    read_write_columnar_block_bytes : QWORD,
    read_columnar_block_bytes : QWORD,

    //Should we replace this with workspace and Query Cache.
    //Worksapce bytes should be the maximum threshold of 1 page because we will be working with ColumnarPage chunks?
    //Actually should we make this, 1 page * number of thread so each thread get it own page, which will match the ColumnarBlock.
    workspace : [^]BYTE,
    workspace_bytes_capacity : QWORD,

    ds_ops : [^]StructuralOperation,
    dso_buffer : [^]BYTE,
    //

    //some Cacheline (Filtering engine)
    _unused_1 : [7]QWORD,
    filter_cache : [^]BYTE, //TODO:Khal not yet sure of the structure and data layout.
    //

    // 1 Cacheline (Main Sync)
    owner_thread_id : QWORD,
    sync_frame_gen : QWORD,
    _unused_ : [6]QWORD,
    //

    //thread count number cache line (Thread Sync)
    sync_point : [THREAD_COUNT]AtomicSynchronization,
    //
    
    //1 Cache line (Debug/Reflection)
    //Eg. which components a indices has
    //tracking?
    unused_2 : [8]QWORD,
    //

}

//TODO:Khal we need to better orgranize this. We don't really know the access pattern nor do we actually know which data we
//would need to store. 
//We know we need to query_mask, to skip the bit_zero_mask (coarse query)
//We need the start_indices_index and end_indices_index to determine the "entity id range" eg. enitity 5 to 10
ColumnarHeader :: struct{
    query_mask : DWORD,
    indices_index : DWORD,
    indices_per_page : DWORD,
    struct_padding_offset : DWORD, 
}

ColumnarBlock :: struct{
    block : [4]ColumnarPage
}

ColumnarPage :: struct #align(4096){
    bit_zero_mask : [8]QWORD,
    payload : [4032]BYTE,
}


StructuralOperation :: enum BYTE{
    PUSH, 
    POP,
    GROW,
    SHRINK,
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


AtomicSynchronization :: struct #align(64){
    atomic_bitmask : DWORD,
    //TODO:Khal implement me and add sync metadata if needed
}

Foo :: struct{
    a : [16]DWORD,
    b : [8]QWORD,
    c : [64]BYTE

}



Lambda :: struct($T : typeid){
	call :  proc "contextless" (type_array : [^]#soa[0x40 / align_of(T)]T),
}



@(optimization_mode="favor_size") 
init_world :: proc (world : ^World($thread_count), $unique_data_capacity : QWORD, $unique_read_data_capacity : QWORD)
where thread_count <= 4 && thread_count & 1 == 0 && (unique_data_capacity + unique_read_data_capacity) > 0 {

    when ODIN_DEBUG && DUMP{
        append_csv("World address input w", QWORD(uintptr(world)))
        append_csv("thread_count constant r", QWORD(thread_count))
        append_csv("unique_data_count constant r", QWORD(unique_data_capacity))
        append_csv("unique_read_data_count constant r", QWORD(unique_read_data_capacity))
     }

    READ_WRITE : QWORD : 0x03
    PRIVATE_ANON : QWORD : 0x21
    ADVISE_POPULATE_READ : QWORD : 0x16
    ADVISE_POPULATE_WRITE : QWORD : 0x17
    ADVISE_COLLAPSE : QWORD : 0x19
    
    buffer_address : uintptr = ---

    //The ColumnarBlock Allocation
    {	

	when (unique_data_capacity & 0x7F) == 0{
	    UNIQUE_RW_DATA_CAPACITY_GRANULARITY : QWORD : unique_data_capacity + 0x80
	}else{
	    UNIQUE_RW_DATA_CAPACITY_GRANULARITY : QWORD : (unique_data_capacity + 0x7F) & 0xFFFFFFFFFFFFFF80
	}

	when (unique_read_data_capacity & 0x7F) == 0{
	    UNIQUE_R_DATA_CAPACITY_GRANULARITY : QWORD : unique_read_data_capacity + 0x80
	}else{
	    UNIQUE_R_DATA_CAPACITY_GRANULARITY : QWORD : (unique_read_data_capacity + 0x7F) & 0xFFFFFFFFFFFFFF80
	}

	REQUIRED_RW_HEADER_BYTES : QWORD : size_of(ColumnarHeader) * UNIQUE_RW_DATA_CAPACITY_GRANULARITY
	REQUIRED_R_HEADER_BYTES : QWORD : size_of(ColumnarHeader) * UNIQUE_R_DATA_CAPACITY_GRANULARITY

        REQUIRED_RW_BLOCK_BYTES_2MIB : QWORD : size_of(ColumnarBlock) * UNIQUE_RW_DATA_CAPACITY_GRANULARITY 
        REQUIRED_R_BLOCK_BYTES_2MIB : QWORD : size_of(ColumnarBlock) * UNIQUE_R_DATA_CAPACITY_GRANULARITY 

        //TODO:Khal read the syscall implementations to see what it actually does.
       
        REQUIRED_COL_BYTES_2MIB : QWORD : REQUIRED_RW_BLOCK_BYTES_2MIB + REQUIRED_R_BLOCK_BYTES_2MIB
	REQUIRED_HEADER_BYTES_4KIB : QWORD : (REQUIRED_RW_HEADER_BYTES + REQUIRED_R_HEADER_BYTES + 0xFFF) & 0xFFFF_FFFF_FFFF_F000

	UNUSED_PADDED_BYTES : QWORD : ((UNIQUE_RW_DATA_CAPACITY_GRANULARITY + UNIQUE_R_DATA_CAPACITY_GRANULARITY) - (unique_data_capacity + unique_read_data_capacity)) * size_of(ColumnarBlock)

	//TODO:Khal relook at and re-architecture it.  
	//TODO:Khal how will we handle over utilization of deffered operation from some thread, while other thread are under utilizing deffered operations?
        REQUIRED_DS_OP_BYTES : QWORD : 4096 //assuming the op enum to be 1 byte so we can issue 4096 ops
        
	REQUIRED_DSO_BUFFER_BYTES : QWORD : 65536 //assuming limit of op struct to be 16 bytes so (65536 / 16) is 4096, so we can issue 4096 
        TOTAL_REQUIRED_DEFFERED_OPS_BYTES : QWORD : REQUIRED_DS_OP_BYTES + REQUIRED_DSO_BUFFER_BYTES

	//size sanity check
	{
		#assert((REQUIRED_RW_BLOCK_BYTES_2MIB  & 0x1FFF_FF) == 0)
 		#assert((REQUIRED_R_BLOCK_BYTES_2MIB & 0x1FFF_FF) == 0)
		#assert((REQUIRED_COL_BYTES_2MIB  & 0x1FFF_FF) == 0)

 		#assert((REQUIRED_RW_HEADER_BYTES & 0x7FF) == 0)
 		#assert((REQUIRED_R_HEADER_BYTES & 0x7FF) == 0)
		#assert((REQUIRED_HEADER_BYTES_4KIB & 0xFFF) == 0)

 		#assert(UNUSED_PADDED_BYTES > 0)
 		#assert(UNUSED_PADDED_BYTES % align_of(ColumnarBlock) == 0)	
	}

        buffer_address = intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(REQUIRED_HEADER_BYTES_4KIB),
            uintptr(READ_WRITE),
            uintptr(PRIVATE_ANON),
            ~uintptr(0),
            uintptr(0),
        )

        world.header = cast([^]ColumnarHeader)buffer_address

        buffer_address = intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(REQUIRED_COL_BYTES_2MIB),
            uintptr(READ_WRITE),
            uintptr(PRIVATE_ANON),
            ~uintptr(0),
            uintptr(0),
        )

        intrinsics.syscall(
            linux.SYS_madvise,
            buffer_address,
            uintptr(REQUIRED_COL_BYTES_2MIB),
            uintptr(ADVISE_POPULATE_WRITE)
        )
 
	world.columnar_blocks = cast([^]ColumnarBlock)(buffer_address + uintptr(UNUSED_PADDED_BYTES))

	world.read_write_columnar_block_bytes = size_of(ColumnarBlock) * unique_data_capacity
	world.read_columnar_block_bytes = size_of(ColumnarBlock) * unique_read_data_capacity

	world.workspace = cast([^]BYTE)buffer_address
	world.workspace_bytes_capacity = UNUSED_PADDED_BYTES

	//Set the workspace and the query cache
	//Workspace is a temporary Allocation and query cache is a fast cache for 
	//same indices in multiple ColumnarBlock.


        buffer_address = intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(TOTAL_REQUIRED_DEFFERED_OPS_BYTES),
            uintptr(READ_WRITE),
            uintptr(PRIVATE_ANON),
            ~uintptr(0),
            uintptr(0),
        )

        world.ds_ops = cast([^]StructuralOperation)(buffer_address)
        world.dso_buffer = cast([^]BYTE)(buffer_address + uintptr(REQUIRED_DS_OP_BYTES))

        when ODIN_DEBUG && DUMP{
            append_csv("total_header_bytes local w", REQUIRED_HEADER_BYTES_4KIB)
            
            append_csv("total_columnar_bytes local r", REQUIRED_COL_BYTES_2MIB)
            
            append_csv("buffer_address local w", QWORD(uintptr(buffer_address)))
            append_csv("world_header_address input w", QWORD(uintptr(world.header)))
            append_csv("world_columnar_table input w", QWORD(uintptr(world.columnar_blocks)))

            append_csv("ds_op_bytes constant r", QWORD(REQUIRED_DS_OP_BYTES))
            append_csv("dso_buffer_bytes constant r", QWORD(REQUIRED_DSO_BUFFER_BYTES))
            append_csv("total_size constant r", QWORD(TOTAL_REQUIRED_DEFFERED_OPS_BYTES))
            
            append_csv("buffer_address local w", QWORD(uintptr(buffer_address)))
            append_csv("world_ds_ops_address input w", QWORD(uintptr(world.ds_ops)))
            append_csv("world_ds_buffer_address input w", QWORD(uintptr(world.dso_buffer)))
        }
    }

    
     //Filtering Engine
    {
	//TODO:Khal Filter parameter are currently _unused due do not knowing the layout and implementation yet.
    }

    //Main Synchronization
    {
	world.owner_thread_id = QWORD(intrinsics.syscall(linux.SYS_gettid))

	//TODO:Khal World Sync parameter are currently _unused due do not knowing the layout and implementation yet.
	when ODIN_DEBUG && DUMP{
	    append_csv("world_owner_thread_id input w", QWORD(world.owner_thread_id))
	} 
    }

    //Thread Synchronization
    {
	for thread_index in 0..<thread_count{
		//TODO:Khal Thread Sync parameter are currently _unused due do not knowing the layout and implementation yet.
	}
    }


    //Debug and Reflection
    {
	//TODO:Khal Debug & Reflection parameter are currently _unused due do not knowing the layout and implementation yet.
    }
    

}

@(optimization_mode="favor_size")
lock_readonly_blocks :: proc(world : ^World($thread_count)){

    when ODIN_DEBUG && DUMP{
	

    }

    READONLY : QWORD : 0x01
    ADVISE_COLLAPSE : QWORD : 0x19

    readonly_columnar_address : uintptr = ---

    readonly_columnar_address = uintptr(world.columnar_blocks) + uintptr(world.read_write_columnar_block_bytes)

    intrinsics.syscall(
        linux.SYS_mprotect,
	readonly_columnar_address,
	uintptr(world.read_columnar_block_bytes),
        uintptr(READONLY),
    )

    intrinsics.syscall(
	linux.SYS_madvise,
	readonly_columnar_address,
        uintptr(world.read_columnar_block_bytes),
        uintptr(ADVISE_COLLAPSE)
    )
}

//We need to distinguish read and read and write pages. We can encode that to table_index or we can pass another constant parameter
@(optimization_mode="favor_size")
register_columnar :: proc(world : ^World($thread_count), $data_typeid : typeid, $table_index : QWORD, $read_only : DWORD, $start_indice : DWORD, $number_of_components : DWORD)
where intrinsics.type_is_struct(data_typeid) && size_of(data_typeid) >= 8 && start_indice >= 0 && number_of_components > 0 #no_bounds_check{
	
    when ODIN_DEBUG && DUMP{
        append_csv("register_columnar", 0)
        append_csv("world_address input rw", QWORD(uintptr((world))))
        append_csv("thread_count constant r", QWORD(thread_count))
        append_csv("data_size constant r", size_of(data_typeid))
        append_csv("data_alignment constant r", align_of(data_typeid))
        append_csv("table_index constant r", table_index)
	append_csv("read_only constant r", QWORD(read_only))
        append_csv("start_indice r", QWORD(start_indice))
        append_csv("number_of_components r", QWORD(number_of_components))
    }

    DATA_TYPEID_FIELD_COUNT : QWORD : intrinsics.type_struct_field_count(data_typeid)
    PAYLOAD_MAX_BYTES_SIZE : DWORD : 4032

    struct_padding_offset : DWORD 

    when (number_of_components & 3) == 0{
	NUMBER_OF_INDICES_PER_COLUMNAR : DWORD : number_of_components >> 2 
    }else{
	NUMBER_OF_INDICES_PER_COLUMNAR : DWORD : ((number_of_components + 0x03) & 0xFFFF_FFFC) >> 2
    }

    //We are checking if the number of component bytes, which are divided up to the four columnar page
    //are less than the max columnar payload size (4032) bytes. Otherwise it is an overflow.
    #assert((NUMBER_OF_INDICES_PER_COLUMNAR * size_of(data_typeid)) < PAYLOAD_MAX_BYTES_SIZE)


    {
	types : [^]^runtime.Type_Info = ---
	
	types = type_info_of(data_typeid).variant.(runtime.Type_Info_Named).base.variant.(runtime.Type_Info_Struct).types
	
	for i in 0..<DATA_TYPEID_FIELD_COUNT{
	    struct_padding_offset += (0x40 - DWORD(types[i].size)) 	
	}

    }
    

    //TODO:Khal we need to get the fields from the struct once we get each field we need to get it's alignment.
    //we than need to divide 0x40 / alignment for each field and sum the result.
    //we than need to get the field count of the struct and multiply it by 0x40.
    //We than need to get the different between (field_count * 0x40) / (0x40 / each_field_alignment) 
    //that will be the remaining_data_stride in the ColumnarHeader of that specific data type.
    //Do we need to get the alignment or the size???? and this will just help end of struct alignment not field alignment.





    /*
    when size_of(data_typeid) > 1 && align_of(data_typeid) == 1{
	//The user used #packed directive. Refection is need to fetch the highest field size. 
	type_info_ptr : ^runtime.Type_Info = ---
	struct_info : runtime.Type_Info_Struct = ---
	largest_field_size : DWORD = 0

	type_info_ptr = type_info_of(data_typeid).variant.(runtime.Type_Info_Named).base
	struct_info = type_info_ptr.variant.(runtime.Type_Info_Struct)

	for i in 0..<DATA_TYPEID_FIELD_COUNT{
		type_info_ptr = struct_info.types[i]

		if largest_field_size < DWORD(type_info_ptr.size){
			largest_field_size = DWORD(type_info_ptr.size)
		}
	}

	array_size = 0x40 / largest_field_size

    }else{
	array_size = 0x40 / align_of(data_typeid)
    }
    */
 
    //Logic goes here.

 

    //Assignment goes here.
    when read_only != 0{
	 	world.header[table_index] = {
		0,
		start_indice,
		NUMBER_OF_INDICES_PER_COLUMNAR,
		struct_padding_offset,
	}
    }else{
	
	world.readonly_header[table_index] = {
		0,
		start_indice,
		NUMBER_OF_INDICES_PER_COLUMNAR,
		struct_padding_offset,
	}
    }

    //TODO:Khal we must set the specfic ColumnarHeader's query_mask and the specific ColumnarPage's bit_zero_mask field in the ColumnarBlock
    when ODIN_DEBUG && DUMP{
	

    }


}


@(optimization_mode="favor_size")
compute_struct_wasted_space :: #force_inline proc (world : ^World($thread_count), $table_index : QWORD) -> f32{
	struct_padding : DWORD = ---
	cacheline_struct_bytes : DWORD = ---
	struct_padding = world.header[table_index].struct_padding_offset
	cacheline_struct_bytes = (struct_padding + 0x3F) & 0xFFFF_FFC0

	if b32(struct_padding | cacheline_struct_bytes){ 
	    return f32(struct_padding) / f32(cacheline_struct_bytes) * 100.0 
	}

	return 0
}



   /*
      How will we have invariant on SIMD alignment for the payload bytes. How can we guarentee SIMD alignment. Even on different size field struct. Should it be Implicit (Where it adds padding to guarentee alignment) or should it be Explict (Where it up to the user struct layout that determine alignment). This will break the invariant.
 	#Solution we can create a function that will guarentee the alignment to allow SIMD operation.
	This may need to handle field alignment and end of struct alignment.
	We can do this by implementing alignment on simd_load and simd_store function.

     */




//TODO:Khal make this better.
simd_load :: #force_inline proc "contextless" (array : $A/[$LANE]$E) -> #simd[LANE]E{
	//TODO:Khal we will load 64 bytes chunk so u8 lane will be 64 and u16 will be 32, etc...
	return transmute(#simd[LANE]E)array

}


simd_store :: proc{simd_store_simd, simd_store_array}


@(enable_target_feature="avx,avx2")
simd_store_simd :: #force_inline proc "contextless" (slice : [^]$E, value : #simd[$LANE]E){
	//TODO:Khal we need to align the since to be aligned to 64 byte boundary
	//and possibly make the $LANE to by 64 / size_of(E), so u8 is 64, u16 is 32, u32 is 16, u64 is 8.
	(cast(^#simd[LANE]E)slice)^ = value
}


@(enable_target_feature="avx,avx2")
simd_store_array :: #force_inline proc "contextless" (slice : [^]$E, value : $A/[$LANE]E){

	(cast(^#simd[LANE]E)slice)^ = simd_load(value)
}



/*
brief informal statement of the problem:
We need to support filtering multiple instances of a single component type based on their fields or flags  

System level issue:
Filtering instances of a single component type based on a specific field can be expensive due data layout requiring you to load more data than is required by the filter predicate, which
increases the cost even more as the filter complexity increases.


What must be controlled:

Even though we can't control data layout for user defined components. We must control how the data layout of the components is stored in memory,
since filtering multiple instances of a single component can be one of the major bottleneck due to data access inefficiencies.

We must control layout and the system of all the filter in a way to eliminate all contradictions, redundancy, and possibly merge filters together to reduce
the number of passes. The sorting order of the filters must be controlled as well, since it will reduce data access and improve early rejections. 

contradiction -> the work (if a > 5 && a < 5)
redundancy -> unnecessary work (if a > 5 && a > 4)
merging -> reduce repeatable data access
ordering -> reduce unnecessary data access

Even though we can't control the filter evaluation, since it is dynamic and dependent on the user defined data and transformation
We must manage how control flow is handled to reduce divergences during evaluation to increase predicatability and overall performance (plus maintainability/readability).   



Latency/Throughput targets:

/////////MEMORY//////////

ram base clock frequency

3000 mts
3200 mts
3600 mts

Bandwidth (upper bound ignoring timing limitation)
3_000_000_000 * 64 bit_lane * 2 (dual_channel) = 384_000_000_000 bits or 48_000_000_000 byte per sec (384 gbps or 48GB/s)
3_200_000_000 * 64 * 2 = 409_600_000_000 bits or 51_200_000_000 bytes per sec (409.6 gbps or 51.2 GB/s)
3_600_000_000 * 64 * 2 = 460_800_000_000 bits or 57_600_000_000 bytes per sec (460.8 gbps 57.6 GB/s)


(upper bound)

3000 mts

12_000_000_000 struct_per_sec = 48_000_000_000 bytes / 4 bytes (single field struct DWORD/SINGLE)
6_000_000_000 struct_per_sec = 48_000_000_000 bytes / 8 bytes (single field struct QWORD/DOUBLE)

200_000_000 4_bytes_struct_per_60_fps = 12_000_000_000 * (1/60)
100_000_000 8_bytes_struct_per_60_fps = 6_000_000_000 * (1/60)

83_333_333 4_bytes_struct_per_144_fps = 12_000_000_000 * (1/144)
41_666_666 8_bytes_struct_per_144_fps = 6_000_000_000 * (1/144)

50_000_000 4_bytes_struct_per_240_fps = 12_000_000_000 * (1/240)
25_000_000 8_bytes_struct_per_240_fps = 6_000_000_000 * (1/240)

////////

3200 mts


12_800_000_000 struct_per_sec = 51_200_000_000 bytes / 4 bytes (single field struct DWORD/SINGLE)
6_400_000_000 struct_per_sec = 51_200_000_000 bytes / 8 bytes (single field struct QWORD/DOUBLE)

213_333_333 4_bytes_struct_per_60_fps = 12_800_000_000 * (1/60)
106_666_666 8_bytes_struct_per_60_fps = 6_400_000_000 * (1/60)

88_888_888 4_bytes_struct_per_144_fps = 12_800_000_000 * (1/144)
44_444_444 8_bytes_struct_per_144_fps = 6_400_000_000 * (1/144)

53_333_333 4_bytes_struct_per_240_fps = 12_800_000_000 * (1/240)
26_666_666 8_bytes_struct_per_240_fps = 6_400_000_000 * (1/240)


////////

3600 mts

14_400_000_000 struct_per_sec = 57_600_000_000 bytes / 4 bytes (single field struct DWORD/SINGLE)
7_200_000_000 struct_per_sec = 57_600_000_000 bytes / 8 bytes (single field struct QWORD/DOUBLE)

240_000_000 4_bytes_struct_per_60_fps = 14_400_000_000 * (1/60)
120_000_000 8_bytes_struct_per_60_fps = 7_200_000_000 * (1/60)

100_000_000 4_bytes_struct_per_144_fps = 14_400_000_000 * (1/144)
50_000_000 8_bytes_struct_per_144_fps = 7_200_000_000 * (1/144)

60_000_000 4_bytes_struct_per_240_fps = 14_400_000_000 * (1/240)
30_000_000 8_bytes_struct_per_240_fps = 7_200_000_000 * (1/240)

////////


We will target 3200 mts as the medium and the range of fps will be 144 to 240
so 

88_888_888 4_bytes_struct_per_144_fps = 12_800_000_000 * (1/144)
44_444_444 8_bytes_struct_per_144_fps = 6_400_000_000 * (1/144)

53_333_333 4_bytes_struct_per_240_fps = 12_800_000_000 * (1/240)
26_666_666 8_bytes_struct_per_240_fps = 6_400_000_000 * (1/240)

or for 64 bytes struct

5_555_555 cache_line_struct_per_144_fps
3_333_333 cache_line_struct_per_240_fps

is our theoretical limit

We are using testingmlp to get our ram latency which is 102.2 ns so the range we will use is +-10
from 92.2 to 112.2 ns latency

Around 16 MLP (memory level parallelism) is when the scaling start to diminish from # of lanes 

so pointer chasing (memory load dependency) = (MLP * cacheline) / latency
range(11.1, 9.12), = (16 * 64 bytes) / range(92.2 ns, 112.2 ns) 

so roughly 11.1 gbps to 9.12 gbps is the limit for pointer chasing (memory load dependency)

2_775_000_000 4_bytes_struct_per_second = 11_100_000_000 bytes / 4 bytes (single field struct DWORD/SINGLE)
1_387_500_000 8_bytes_struct_per_second = 11_100_000_000 bytes / 8 bytes (single field struct QWORD/DOUBLE)

19_270_833 4_bytes_struct_per_144_fps = 2_775_000_000 * (1/144)
9_635_416 8_bytes_struct_per_144_fps = 1_387_500_000 * (1/144)

11_562_500 4_bytes_struct_per_240_fps = 2_775_000_000 * (1/240)
5_781_250 8_bytes_struct_per_240_fps = 1_387_500_000 * (1/240)

2_280_000_000 4_bytes_struct_per_second = 9_120_000_000 bytes / 4 bytes (single field struct DWORD/SINGLE)
1_140_000_000 8_bytes_struct_per_second = 9_120_000_000 bytes / 8 bytes (single field struct QWORD/DOUBLE)


from 48, 51.2, 57.6 GB/s depending on 3000 mts, 3200 mts or 3600 mts to 9.12 GIB/s to 11.1 GIB/s

It looks like 6.5GIB/s is our limit when doing a simd load and store using memcpy of 1 GIB of cold data
but without the temporal we can only achieve 4 GIB/s so we will use this as our limit to handle worst case

We achieve this with 4 xmm load, store or 2 ymm load, store.
This look correct since our target is 9.12 GIB/s to 11.1 GIB/s and the operation we are doing is
1 load from dst, 1 load from src, and 1 store from dst

so 9.12 GIB/s / 3 memory operation and 11.1 GIB/s / 3 memory operation, which will be 3.04 GIB/s to 3.7 GIB/s

so we will have a lower limit this is loading 2 data, and writing it to another data this will be
3.04 GIB/s to 3.7 GIB/s 

for just loading two 1GIB buffer in any predicatable offset we get 6 GIB/s as the number of different buffer grows
the 6 GIB/s start decreasing 

for just loading one 1GIB buffer in sequential order we get around 14.4 GIB/s to 13.37 GIB/s

This is the model that fits for my PC

The lower bound is 3.04 to 3.7 GIB/s for load buffer, load diff buffer, store (Write heavy)
Fair bound is 4.0 to 5.0 GIB/s for loading 3 to 6 buffer and minimal store
Ideal bound is 6.0 to 7.0 GIB/s for loading 2 buffer and minimal store
High bound is 13.37 to 14.4 GIB/s for loading 1 buffer and accessing it in sequence (no store) 


/////////CACHE//////////
ws      cpu clock
64    	80
128	    30
256     60
512 	60
1024	90
2048	101
4096	140
8192	371
16384	391

There seem to be noise from 64 to 16KIB but when we get to 32KIB to RAM there seem to be an average of 2.5 times increase on the cpu clock
compared to 1.3 (There is noise so this should be a lot less)

Cpu start degrading fron 4096 to 8192 and a steady 2.0x after 16KIB. until we reach memory access which is closely to 3.0x 

When there is an L3 hit, the line is invalidated from
the L3 if the access was a store. It is invalidated from the L3 if the access was a load and the line was
read by just one core.

So we won't really rely on the L3 cache, but rather L1 and L2 cache in this guideline

We know the fetch rate from L1 to L2 cache is 32 bytes per cycle,

Most cpu l1 data cache is 32KIB, 8 ways, 64 set. This is an assumption that we will believe
So we can use any of the 8 lanes  of cache line, but only one of the particular 64 set can be used for storing the memory
So if each set there is 8 "lanes" and in each line is a cache line. to determine the set it is ((address / cache line) % number of set)

to find the critical stride we will do critical_stride = total_cache_size / number_of_ways

If we use the L1 data cache and assume it is 32KIB, 8 ways, 64 set for l1 data cache. Than the critical stride would be
(32 * 1024) / 8 == 4096 bytes. This is exactly a page size. We also know that in each set there are 8 ways and in each way it hold 1 cache line
so we can do 8 page size before it start to evict anything from the l1 data cache (this is assuming there isn't any other data loaded or stored)

If we use the L2 cache and assume it is 512KIB, 8 ways, 1024 set for the L2 cache. Than the critical stride would be
(512 * 1024) / 8 == 65536 bytes. This is exactly 16 times the page size. We also know that in each set there is 8 ways and in each way it hold 1 cache line
so we can do 128 page size before it start evicting (This is assuming there is no instruction cached in the L2 cache and it is empty)

iteration test
Way	microseconds
1	115.797
2	76.753
4	56.806
8	51.846
16	74.71
32	83.285
64	84.157
128 261.762
256 435.729
512 817.687

For each 64 bytes it is stored in a set. Which mean 64 is stored in set 0, 128 in set 1, 192 in set 3, etc....
If we use let say only set 0 and not any other set by saturating the ways then we are under utilizing the cache.
This can be caused by having critcal stride offset.

AMD l1 data cache is most likely VIPT (Virtually indexed, physically tagged) meaning that the set and the individual data in cache is fetched using virtual address, but the tag
to determine the specific cache in the set (which way in the set) uses the physical address rather than the virtual address.

Following layout AMD representation on how the address is used to access the cache

[Tag field,    Index field,    Offset field]

The offset field is used to index into the cache line and we will assume the cache line is 64 bytes, so the offset field can't be greater than
6 bits (1 << 6) == 64

The index field is used to get the set index in the cache. We will assume the number of set in a l1 data cache is 64 set (zen1 to zen3)
so the Index field should be greater than 64. This can be captured by 1 << 6

[Tag field, Index field (6 bits), Offset field (6 bits)]

This also show why critical_stride = total_cache_size / number_of_ways  will map the the same set.

Which DC banks are accessed is determined by address bits 5:3 (multiple load or store in the same bank is not good)

[Tag field, Index field (6 bits),  Bank(5:3) normal offset (2:0)   Offset field (6 bits)]

We can assume AMD cpu contains the following type of HW prefetchers;
L1 Stream HW Prefetcher L1 Stride Prefetcher  L1 Region Prefetcher  L2 Stream HW Prefetcher L2 up/Down Prefetcher

This means that accessing data sequentially and in constant stride is good in the l1 data cache.
We can assume that hardware prefetching stop at page boundaries (this is an assumption), so trying to cross a page boundaries
will have a cache miss if the data isn't already in the cache. We can track 24 outstanding cache miss from l1 data cache (zen3) and
22 outstanding cache miss from l1 data cache (zen2)

NOTE should we just make the lane size size_of(type) * LANE == 64 bytes? so if we fiter or use a single field type than we are
using a full cache line. We can do two ymm0 load by smashing them. Intsead of size_of(type) * lane_count == 32 byte if ymm0 or size_of(type) * lane_count == 16 bytes if xmm

Ideally the solution to the problem (filtering) shouldn't cause alot of contention to the cpu cache, so cache eviction and contention
should be avoid by using all fields in the data sequentially and multiple times. The repacked layout of the data we are trying to filter must not 
cause large strides when filtering. From single filter on single fields, multiple fiters on single field, multiple filters on multiple fields.
but there will only be one filter type that will be tied to a field. A single filter type can't be tied to multiple field. Eg if position.x < 5
This will be tied to the x field a never the y field. If we are looking at cores. We want to avoid the possibility of loading the same cache line to different cores.

We know that if there a data miss it fetches in cache line granularity (64 bytes), so we can either organize data in the following

We know that each core contains it own l1 cache and l2 cache

(assuming x and y a f32)
aos (x,y,x,y,x,y,x,y,x,y) so if we filter on just x we are only utilizing 50 percent cache line, stride size would be 4 bytes
soa ([x,x,x,x,x,x,x,.........], [y,y,y,y,y,y,..............]) so if we just filter on just x we utilize 100 percent cache line, stride size would be 0 bytes
soa_aos_simd ([x,x,x,x,x,x,x,x], [y,y,y,y,y,y,y,y], [x,x,x,x,x,x,x,x], [y,y,y,y,y,y,y,y], etc....) so if we just filter on just x we only utilize 50 percent cache line, stride size would be 32 bytes
soa_aos_cache_line ([x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x],[y,y,y,y,y,y,y,y,y,y,y,y,y,y,y,y], etc...) so if we just filter on just x we utilize 100 percent of the cache line. stride size would be 64 bytes


Lower bound is we are bouncing fields on filter and only filtering a single field, which load unnecessary data in cache.
Fair bound is we organize the fields in a way where it a N array where N < 64 and N is a SIMD lane count. This will still load unnecessary data
We also know the N array to be either 32 bytes or 16 bytes depending on xmm or ymm
Ideal bound is we organize the fields in a way where it a N array where N == 64 (cache line). This will store each field in different cache line so filtering will only use a cache line and stride are predicatable
We also know the N array to be 64 bytes so for f32 that would be an array of 16 for f64 that would be 8
High bound is we organize the fields in a way where it a SOA array where all the fields are stored as a array. The SOA array length must be determined by the struct field count and field size so we can't really know unless
know the struct type


/////////TLB//////////

We know that each core get it own dedicated MMU (L1 data TLB, l1 instruction TLB and L2 unified TLB)
We know that the lower 12 or 21 bits of any virtual address is the offset of the physical address, so we can gurantee that accessing a page
sequentially will also access the physical address sequentially.

We want to prevent loading the same page to multiple core. We want each core's tlb to have different pages for write, but sharing pages on read is fine.
Sharing a page across cores may result in a first time access tlb miss on other cores if it is first time access even though a single core has already access it.

amd 19h family optimization (zen3 to zen4): If a 16-Kbyte aligned block of four consecutive 4-Kbyte pages are also consecutive and 16-Kbyte
aligned in physical address space and have identical page attributes, the processor may
opportunistically store them in a single TLB entry resulting in increased effective capacity for both
L1 and L2 DTLB and ITLB. We can try to ahieve this by allocating large block in mmap for example 16KIB. We can do better though.
    Especially on linux by utilizing THP (Transparent Huge Page) if enabled (default). We can allocate in 2MIB granularity and possibly madvise.


4kIB Page Structure
9 bits     9 bits       9 bits        9 bits       12 bits
PMLE4  ->  PDPE    ->   PDE     ->    PTE     ->   offset 

2MIB Page Structure
9 bits     9 bits       9 bits        21 bits      
PMLE4  ->  PDPE    ->   PDE     ->    offset

L2 TLB can hold 1536 4KIB or 2 MIB pages as well as PDE (second last table for 4KIB walk skipping 3 table heirarchy, but last table for 2MIB walk skipping 3 table heirarchy) 

The cpu processor has either 2 (zen to zen2) to 6 (zen3 to zen4) page walker that can handle l2 tlb miss. Miss can start speculatively from the data or instruction
The table walker contains a 64 entry cache (Page Directory Cache) that hold PMLE4 and PDPE entries, which may increase virtual to physical translation by skipping the first and second table 

Since TLB miss can start speculatively we can assume that there is some sort of prefetching done in the MMU. We are not quite sure which type, but
Sequential access should be optimal and is assumed to be supported. 

from AMD arch manual

A change to any paging data-structure entry is not automatically reflected in the TLB. Software must invalidate the TLB entry of a modified translation-table entry so that the
change is reflected in subsequent address translations.
If a table entry is updated and does not remove a permission violation, it is unpredictable whether the old or updated entry will be
used until an invalidation is performed. 

so we can say that there will be a tlb shootdown when we change from r/w to only read only or even better if we are even more strict with the
permission we can say that there will be a tlb shootdown (invalidation of the entry in the TLB).



/////////INSTRUCTION//////////

Zen 3 CPU int unit path:
3 ALU
1 ALU BR

3 AGU

1 BR

Zen 3 CPU float unit path:
MUL MAC
ADD
MUL MAC
ADD

F2I ST
F2I ST

So we can issue a maximum of 6 instruction per cycle due to dispatch

1 jump maximum per 16 code bytes 

loop counter <= 64

//TODO:Khal add Instruction bench and write low and high bounds and target

max displacement is 32 bits (DWORD)
it looks like most of the instruction immediate only work for i8, i16, and i32
would branching reduce how good the harware will prefetch for the instruction cache?
For most instructions, the default operand size in 64-bit mode is 32 bits


build_sym_predicate :: proc($data_typeid : typeid, $field_name : string, $cmp_op : ComparisionOperation, val : ^$N, $comb_op : CombinatorOperation, sysm_predicate : ^SystemPredicate)
where intrinsics.type_is_struct(data_typeid) && (intrinsics.type_is_float(N) || intrinsics.type_is_integer(N)) && intrinsics.type_field_type(data_typeid, field_name) == N{
    FIELD_OFFSET : WORD : WORD(intrinsics.type_field_index_of(data_typeid, field_name))
    FIELD_TYPE : typeid : intrinsics.type_field_type(data_typeid, field_name)
    FIELD_COUNT : WORD : intrinsics.type_struct_field_count(data_typeid)

    offset_bytes : WORD  = 0
    
    when intrinsics.type_struct_has_implicit_padding(data_typeid){
	    LANE_COUNT : WORD : 1
    }else{
        FIELD_IS_UNIFORM : bool : (align_of(data_typeid) * intrinsics.type_struct_field_count(data_typeid)) == size_of(data_typeid)
        
        when FIELD_IS_UNIFORM{
            ELEMENT_SIZE : DWORD : size_of(data_typeid) / intrinsics.type_struct_field_count(data_typeid)

            //We are currently using YMM SIMD 256
            when ELEMENT_SIZE == 8{
		    LANE_COUNT : WORD : 4
            }else when ELEMENT_SIZE == 4{
		    LANE_COUNT : WORD : 8
            }else {
		    LANE_COUNT : WORD : 1
            }
            
        }else{
		LANE_COUNT : WORD : 1
        }

	STRIDE_BYTES : WORD : (size_of(data_typeid) * LANE_COUNT) - (size_of(FIELD_TYPE) * LANE_COUNT)

	when FIELD_OFFSET > 0 && LANE_COUNT == 1{
		base_info : ^runtime.Type_Info = ---
		struct_info : runtime.Type_Info_Struct = ---

		base_info = type_info_of(data_typeid).variant.(runtime.Type_Info_Named).base
		struct_info = base_info.variant.(runtime.Type_Info_Struct)

		for i in 0..<FIELD_OFFSET{
			offset_bytes += struct_info.types[i].size
		}
	}else{

		offset_bytes = size_of(FIELD_TYPE) * LANE_COUNT * FIELD_OFFSET
	}
    }
    
    sysm_predicate^ = {
	transmute(QWORD)val,
	offset_bytes,
	STRIDE_BYTES,
	0,
	cmp_op,
	comb_op,
    }
    
    
    
}



/*
Brief Problem we are trying to solve:

We want to create a iterator that will run through the payload (BYTE buffer) and chunk the buffer
When iterating we may want some fine specific type of data within the payload or columnar_table/s
For example we may want Indices that contain both Velocity and Gravity, thus we need to filter out indices
that contains both Velocity and Gravity.

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

Since this procedure is time sensitive we need to reduce random unpredictable branching. This can be done by using a mask.
If we use a mask we need to store the previous buffer/s in some temp storage and than update the values of the columnar payload in the system and
from there do a mask like simd using the old previous value and the new values. If the predicate passes than we will use the new values otherwise revert to the old values
the system will operate on all the data though and is oblivous on this masking step. The masking step is only done internally in the run_x procedure

We are going to make this procedure contextless to force user to pass system procedure that are contextless. In transformation there should not be any allocation or logging
It should just be computation.

Input:
World pointer read from, table_index read from, thread_index read from. callback, predicate to filter on struct field granularity

Output:


Goals:


Limit:



Assumption:

The type is not known until it is converted to the the specific type by the user in the system procedure.

We know the data may change each frame thus the predicate result may change each frame, but the sequence of instruction
from the predicate will be the same for each run




*/

@(optimization_mode="favor_size")
run_0 :: proc  (world : ^World($thread_count), $table_index : QWORD, thread_index : QWORD, sysm : sysm_proc, predicates : ..SystemPredicate)
{


    
    
    
    //Should we filter only the number of component specified in register_columnar???? or should we just let the user iterate over all the data type?
    sysm(raw_data(world.columnar_table[table_index].payload[:]), {})
    
}



//TODO:Khal refer to sql update for inspiration
update :: proc(){


}
*/
//Paralllel loop implementation?????????
 //Used for testing. Remove when fully implemented.
@(enable_target_feature="avx,avx2")
main :: proc(){
	
     HEALTH_STORAGE_INDEX :: 0
     NPC_POSITION_STORAGE_INDEX :: 1
     ENEMY_POSITION_STORAGE_INDEX :: 2
     PACKED_DIRECTIVE_STORAGE_INDEX :: 3

     PropData :: struct{
         foo : QWORD,
         bar : DWORD,
         baz : DWORD,
         t : DWORD,
     }
     
	 Health :: struct{
		 val : f32,
	 }

     Position :: struct {
         x : DWORD,
         y : DWORD,
	 z : BYTE,
	 //padding : [3]BYTE,
     }

     player_health : Health = {100}


     @(enable_target_feature="avx,avx2")
     foo_proc :: proc "contextless" (name : [^]#soa[16]Position){

	     a : #simd[16]DWORD = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}

	     b : #simd[16]DWORD = {2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2}
	     simd_store(raw_data(name[0].y[:]),a)
	     
	     position_y := simd_load(name[0].y)
	     simd_store(raw_data(name[0].x[:]), a+position_y)
	     //position_z := simd_load(name[0].z)

	     //position_x^ = {3,3,3,3}
	     
     }

     //


    f : Foo = Foo{}
    world : World(4) = ---

    init_world(&world, 129,5)

     p := transmute([^]#soa[16]Position)(raw_data(world.columnar_blocks[0].block[0].payload[:]))

     //fmt.println(world.columnar_blocks[0].block[0].payload[0:128])

     a : Lambda(Position) = ---
     a.call = foo_proc
     a.call(p)

     
     //fmt.println("size of lambda ", size_of(Lambda(Position)), align_of(Lambda(Position)))

     //fmt.println(world.columnar_blocks[0].block[0].payload[0:128])
    lock_readonly_blocks(&world)
    
    //Register (NPC) Position "component" in the world. 
    //register_columnar(&world, Position, NPC_POSITION_STORAGE_INDEX, 0x01, 21, 301)
    register_columnar(&world, Foo, PACKED_DIRECTIVE_STORAGE_INDEX, 0x01, 5, 19)
    waste := compute_struct_wasted_space(&world, PACKED_DIRECTIVE_STORAGE_INDEX) 
    fmt.println("waste, ", waste)
    /*

     
     readjust_npc_position :: proc "contextless" (buf : [^]BYTE, meta : DataMeta){
         data : [^]#soa[8]Position = transmute([^]#soa[8]Position)buf

         for i in 0..<1{
             (^#simd[8]f32)(&data[i].x)^ = {2,2,2,2,2,2,2,2}
         }
         
     }

     //npc_pos_overlapping_x : SystemPredicate
     //build_sym_predicate(Position, "x", .EQ, &player_position.x, .OR, &npc_pos_overlapping_x)
     //npc_pos_overlapping_y : SystemPredicate 
     //build_sym_predicate(Position, "y", .EQ, &player_position.y, .NIL, &npc_pos_overlapping_y)

     //This system will run for all the npc positions only if the player position is overlapping (either x or y) 
     //run_0(&world, NPC_POSITION_STORAGE_INDEX, SECOND_THREAD, readjust_npc_position, npc_pos_overlapping_x, npc_pos_overlapping_y)
     
     when ODIN_DEBUG && DUMP{
         dump_csv_to_file("/home/khalid/Documents/GitHub/Journey_ECS/dump.csv")

     }


     bench_proc :: #type proc "contextless" (dst : [^]BYTE, src : [^]BYTE, size : QWORD) ->DOUBLE
     
     ksize :: 1 << 30
     kcount :: 8

     src:= transmute([^]#simd[16]u32)intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(ksize),
            uintptr(0x03),
            uintptr(0x22),
            ~uintptr(0),
            uintptr(0),
        )

    linux.madvise(src, ksize,.HUGEPAGE)


     dst := transmute([^]#simd[16]u32)intrinsics.syscall(
            linux.SYS_mmap,
            0x00,
            uintptr(ksize),
            uintptr(0x03),
            uintptr(0x22),
            ~uintptr(0),
            uintptr(0),
        )


     b := src[0]+dst[0]



     GetTick :: proc "contextless"() -> QWORD{
         ts,_ := linux.clock_gettime(.MONOTONIC)
         return QWORD(ts.time_sec * 1000000000 + ts.time_nsec)

     }

     RDTSC :: proc "contextless"() -> QWORD{
         return QWORD(intrinsics.read_cycle_counter())
     }

     GetFreq ::proc() -> QWORD{
         return 1000000000
     }


     memcpy_simple :: proc "contextless" (dst : [^]BYTE, src : [^]BYTE, size : QWORD){
         intrinsics.mem_copy(dst,src,int(size))

     }


    memcpy_non_overlap :: proc "contextless" (dst : [^]BYTE, src : [^]BYTE, size : QWORD){
         intrinsics.mem_copy_non_overlapping(dst,src,int(size))

     }

     @(enable_target_feature="avx,avx2")
     copy_unroll :: proc "contextless" (#no_alias dst, src: [^]BYTE, size: QWORD, $UNROLL: QWORD) -> DOUBLE {

         dstv : [^]#simd[8]f32 = transmute([^]#simd[8]f32)dst
         srcv : [^]#simd[8]f32 = transmute([^]#simd[8]f32)src

         vec_count : QWORD = size / size_of(#simd[8]f32)

         //We are doing 16 because our MLP is 16ish
         t1 : #simd[8]f32 = ---
         t2 : #simd[8]f32 = ---
         t3 : #simd[8]f32 = ---
         t4 : #simd[8]f32 = ---
         t5 : #simd[8]f32 = ---
         t6 : #simd[8]f32 = ---
         t7 : #simd[8]f32 = ---
         t8 : #simd[8]f32 = ---
         t9 : #simd[8]f32 = ---
         t10 : #simd[8]f32 = ---
         t11 : #simd[8]f32 = ---
         t12 : #simd[8]f32 = ---
         t13 : #simd[8]f32 = ---
         t14 : #simd[8]f32 = ---
         t15 : #simd[8]f32 = ---
         t16 : #simd[8]f32 = ---

         for i :QWORD= 0; i + 15 < vec_count; i+=16{

             t1 = dstv[i]
             t2 = dstv[i+1]

             t3 = dstv[i+2]
             t4 = dstv[i+3]

             t5 = dstv[i+4]
             t6 = dstv[i+5]
        
             t7 = dstv[i+6]
             t8 = dstv[i+7]
        
             t9 = srcv[i]
             t10 = srcv[i+1]
        
             t11 = srcv[i+2]
             t12 = srcv[i+3]
        
             t13 = srcv[i+4]
             t14 = srcv[i+5]
        
             t15 = srcv[i+6]
             t16 = srcv[i+7]
         }
         f := (t1+t2)+(t3+t4)+(t5+t6)+(t7+t8)+(t9+t10)+(t11+t12)+(t13+t14)+(t15+t16)

         return 0.0
     }


     copy_4  :: proc "contextless" (#no_alias d, s: [^]BYTE, sz: QWORD) -> DOUBLE {return copy_unroll(d,s,sz,4) }
     copy_8  :: proc "contextless" (#no_alias d, s: [^]BYTE, sz: QWORD) -> DOUBLE {return copy_unroll(d,s,sz,8) }
     copy_16  :: proc "contextless" (#no_alias d, s: [^]BYTE, sz: QWORD) -> DOUBLE{return copy_unroll(d,s,sz,16) }
     copy_32 :: proc "contextless" (#no_alias d, s: [^]BYTE, sz: QWORD) -> DOUBLE {return copy_unroll(d,s,sz,32) }
     copy_64 :: proc "contextless" (#no_alias d, s: [^]BYTE, sz: QWORD) -> DOUBLE {return copy_unroll(d,s,sz,64) }


     set_bench :: proc "contextless"(#no_alias d,s : [^]BYTE, num_way : QWORD) -> DOUBLE {

         load : BYTE = ---

         for i in 0..<(4096 / num_way){
             for j in 0..<num_way{
                 load = s[j * 4096]
             }
         }

         d[0] = load

         return 0.0



         
         
         
     }


     
     bank_same :: proc "contextless"(#no_alias d,s : [^]BYTE, page_count : QWORD) -> DOUBLE {

         dstv : [^]DWORD = transmute([^]DWORD)d
         srctv : [^]WORD = transmute([^]WORD)s


         load_a : DWORD = ---
         load_b : DWORD = ---

         for i in 0..<4096{
             base := i * 2
             load_a = DWORD(srctv[base]) 
             load_b = DWORD(srctv[base+1])
         }

         dstv[0] = load_a + load_b 

         return 0.0
         

     }



     bank_different :: proc "contextless"(#no_alias d,s : [^]BYTE, page_count : QWORD) -> DOUBLE {

         dstv : [^]DWORD = transmute([^]DWORD)d
         srctv : [^]WORD = transmute([^]WORD)s


         load_a : DWORD = ---
         load_b : DWORD = ---

         for i in 0..<4096{
             base := i * 2

             load_a = DWORD(srctv[base])
             load_b = DWORD(srctv[base+4])


         }

         dstv[0] = load_a + load_b 

         return 0.0

     }
     
     tlb_4096 :: proc "contextless"(#no_alias d,s : [^]BYTE, page_count : QWORD) -> DOUBLE {
         t2 : QWORD = ---
         t1 : QWORD = ---
      
         load : BYTE = ---

         
         t1 = GetTick()

         for _ in 0..<4{
             for i in 0..<page_count{
                 for j in 0..<64{
                     load += s[(i * 4096) + QWORD(j)]
                 }
             }
         }


         t2 = GetTick()
         

         d[0] = load

         return DOUBLE(t2- t1)

         



     }


     memory_aliasing :: proc "contextless"(#no_alias d,s : [^]BYTE, stride_read_write : QWORD) -> DOUBLE {

         temp : BYTE = 0


         for i in 0..<((1<<30) / 8192){
             base := QWORD(i)*4096

             d[base] = 0x32
             temp = s[base + stride_read_write]
             


         }


         //d[0] = temp


         return 0.0;
         
         
     }
     
     
     warm_up :: proc(dst : [^]BYTE, src : [^]BYTE, size : QWORD){

         for i in 0..<(size/4096){
             base := i * 4096
             dst[base] = 5
             src[base] = 2

         }
        
     }
     
     
     bench :: proc(dst : [^]BYTE, src : [^]BYTE, $size : QWORD, bench : bench_proc){

         t1 := GetTick()
         for size in 0..<kcount{
             bench(dst,src,ksize)
         }
         
         t2 := GetTick()
         seconds := f64(t2-t1)/ f64(GetFreq())

         bytes := f64(ksize * kcount)
         gib   := bytes / (1024.0 * 1024.0 * 1024.0)
         bw    := gib / seconds

         
         fmt.printf("%.30f GIB/s \n",bw)
     }

     bench_set :: proc(dst : [^]BYTE, src : [^]BYTE, size : QWORD, bench : bench_proc){

         time1 := GetTick()
         for iter in 0..<kcount{
             bench(dst,src, size)
         }
         time2 := GetTick()

         second := f64(time2-time1)/ f64(GetFreq())

         fmt.printf("%v ways, seconds : %.10f \n",size, second)

     }
     bench_tlb :: proc(dst : [^]BYTE, src : [^]BYTE, $size : QWORD, bench : bench_proc){

         dst_timing := transmute([^]DOUBLE)dst

         page_sizes :[]QWORD= {1,2,4,8,16,32,60,61,62,63,64,65,128,256,512,1024,2046,2047,2048,2049,4096,8192,16384,32768,65536,131072,262144}
         
         for i in 0..<len(page_sizes){
             dst_timing[i] = bench(dst,src, page_sizes[i])
         }
         
         for i in 0..<len(page_sizes){
             working_set := page_sizes[i] * 4096
             fmt.printf("page_count %v, working_set %v, seconds %.10f \n",page_sizes[i], working_set, dst_timing[i] / DOUBLE(GetFreq()))
         }
     }

     //there seem to be a false dependency on store followed by load with 4096 strides
     bench_memory_false_dependecy :: proc(dst : [^]BYTE, src : [^]BYTE, $size : QWORD, bench : bench_proc){
         time1 := GetTick()
         for size in 0..<kcount{
             bench(dst,src,0)
         }
         time2 := GetTick()

         time1_4096 := GetTick()
         for size in 0..<kcount{
             bench(dst,src,4096)
         }
         time2_4096 := GetTick()

         time1_3584 := GetTick()
         for size in 0..<kcount{
             bench(dst,src,4096-512)
         }
         time2_3584 := GetTick()

         time1_4608 := GetTick()
         for size in 0..<kcount{
             bench(dst,src,4096+512)
         }
         time2_4608 := GetTick()

         second := f64(time2-time1)/ f64(GetFreq())
         second_4096 := f64(time2_4096-time1_4096)/ f64(GetFreq())
         second_3584 := f64(time2_3584-time1_3584)/ f64(GetFreq())
         second_4608 := f64(time2_4608-time1_4608)/ f64(GetFreq())
         fmt.printf("0 offset write read %.7f,\n4096 offset write read %.7f,\n3584 offset write read %.7f\n4608 offset write read %.7f\n",second,second_4096, second_3584, second_4608)

     }


     bench_bank :: proc(dst : [^]BYTE, src : [^]BYTE){
         timed_begin := GetTick()
         for size in 0..<kcount{
             bank_different(dst,src,0)
         }

         timed_end := GetTick()


         times_begin := GetTick()
         
         for size in 0..<kcount{
             bank_same(dst,src,0)
         }

         times_end := GetTick()

         second_d := f64(timed_end-timed_begin)/ f64(GetFreq())
         second_s := f64(times_end-times_begin)/ f64(GetFreq())

         fmt.printf("same bank load: %.7f, different bank load: %.7f", second_s, second_d)



     }

     
     //warm_up(dst,src, 4096 * 8)
     //bench_bank(dst,src)
     

     
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
     */
}
