package journey

import "core:slice"
import "core:runtime"
import "core:intrinsics"
import "core:mem"
import "core:hash"
//TODO:khal avoid generic type, such as int, uint use explicit sizes. We are targeting x64 
//     where cache line size is 64 byte.

////////////////////////////// ECS Constant /////////////////////////////

DEFAULT_STORE_CAPACITY :: 32
DEFAULT_COMPONENT_SPARSE :: 32

UINT_BIT_SIZE :: intrinsics.count_trailing_zeros(uint(size_of(int)))
ENTITY_BIT_SIZE :: size_of(int) << UINT_BIT_SIZE
PAGE_SIZE :uint: 64
PAGE_BIT :uint: intrinsics.count_trailing_zeros(PAGE_SIZE)
PAGE_INDEX :uint: PAGE_SIZE - 1

////////////////////////////// ECS Utility /////////////////////////////

//return 0 for all negative and 1 for all postive and zero.
@(private, optimization_mode="speed")
normalize_value :: #force_inline proc "contextless" (val : int) -> int{
    return (val >> 63) + 1 //arithemtic shift
}
///////////////////////////////////////////////////////////////////////

////////////////////////// ECS World //////////////////////////////////

ComponentInfo :: struct{
    component_raw : rawptr,
    size : int,
    alignment : int,
    type_id : typeid,
}

World :: struct{
    entities_stores : EntityStore,
    component_stores : ComponentStore,
}

init_world :: proc($component_count : int) -> ^World{
    world := new(World)

    world.entities_stores = init_entity_store()
    world.component_stores = init_component_store(component_count)

    return world
}

deinit_world :: proc(world : $W/^$World){
    deinit_entity_store(&world.entities_stores)
    deinit_component_store(&world.component_stores)
    free(world)
}

register :: proc(world : $W/^$World, $component_type : typeid, size : int, allocator : mem.Allocator) -> int 
where intrinsics.type_is_struct(component_type){
    return internal_register_component(&world.component_stores, component_type, size, allocator)
}

set_component :: proc(world : $W/^$World, entity : uint, storage_index, component : $E)
where intrinsics.type_is_struct(E){
    internal_sparse_put(&world.component_stores.component_sparse[storage_index], entity, component)
}

get_component :: proc(world : $W/^$World, entity : uint, storage_index : int, $component_type : typeid) -> ^component_type
where intrinsics.type_is_struct(component_type){
    return internal_sparse_get(&world.component_stores.component_sparse[storage_index], entity, component_type) 
}

has_component :: proc(world : $W/^$World, entity : uint, storage_index : int) -> bool{
    return internal_sparse_has(&world.component_stores.component_sparse[storage_index], entity) >= 0
}

add_component ::  proc(world : $W/^$World, entity : uint, storage_index : int, component : $E)
where intrinsics.type_is_struct(E){ 
    //handle group structual change.
    internal_sparse_push(&world.component_stores.component_sparse[storage_index], entity,  component)
}

remove_component :: proc(world : $W/^$World, entity : uint, storage_index : int, $component_type : typeid)
where intrinsics.type_is_struct(component_type){
    //handle group structual change.
    internal_sparse_remove(&world.component_stores.component_sparse[storage_index], entity, component_type)
}

get_components_with_entities :: proc(world : $W/^$World, storage_index : int, $component_type : typeid) -> (components : []component_type, entities : []uint)
where intrinsics.type_is_struct(component_type){
    components = internal_sparse_fetch_components(&world.component_stores.component_sparse[storage_index], component_type)
    entities = internal_sparse_fetch_entities(&world.component_stores.component_sparse[storage_index])

    return
}

get_entities_with_component :: proc(world : $W/^$World, storage_index : int, $component_type : typeid) -> []uint
where intrinsics.type_is_struct(component_type){
    return internal_sparse_fetch_entities(&world.component_stores.component_sparse[storage_index])
} 

get_components :: proc(world : $W/^$World, storage_index : int, $component_type :typeid) -> []component_type
where intrinsics.type_is_struct(component_type){
    return internal_sparse_fetch_components(& world.component_stores.component_sparse[storage_index], component_type)
}

get_components_from_entity :: proc(world : $W/^$World, entity : uint, allocator : mem.Allocator = context.temp_allocator) -> []ComponentInfo{
    component_collection := make([dynamic]ComponentInfo,allocator)

    for &component_sparse, index in world.component_stores.component_sparse{
        if internal_sparse_has(&component_sparse, entity) >= 0{
            component_info := world.component_stores.sparse_info[index]
            component_raw_ptr := internal_sparse_get_raw(&component_sparse, entity, component_info.size)

            append(&component_collection, ComponentInfo{
                component_raw = component_raw_ptr,
                size = component_info.size,
                alignment = component_info.alignment,
                type_id = component_info.type_id,  
            })

        }
    }

    return component_collection[:]
}

create_entity :: proc(world : $W/^$World) -> uint{
    entity := internal_create_entity(&world.entities_stores)
    return entity
}

clear_entity :: proc(world : $W/^$World, entity : uint){
    for &component_sparse, index in world.component_stores.component_sparse{
        if internal_sparse_has(&component_sparse, entity) >= 0{
            component_info := world.component_stores.sparse_info[index]
            //handle group structual change.
            internal_sparse_remove(&component_sparse, entity, component_info.size)
        }
    }
}

remove_entity :: proc(world : $W/^$World, entity : uint){
    clear_entity(world, entity)
    internal_recylce_entity(&world.entities_stores,entity)
}

get_alive_entites :: proc(world : $W/^$World, allocator := context.temp_allocator) -> []uint{
    return interanl_fetch_alive_entites(&world.entities_stores, allocator)
}

//allocated_memory in bytes, used_memory in bytes
get_memory_usage :: proc(world : $W/^$World, storage_index : int) -> [2]int{
    component_sparse := world.component_stores.component_sparse[storage_index]
    component_info := world.component_stores.sparse_info[storage_index]

    len := internal_sparse_len(&component_sparse)
    cap := component_info.count

    total_bytes_sparse := 0

    used_bytes_component := len * component_info.size
    allocated_bytes_component := cap * component_info.size

    used_bytes_entity := len * size_of(uint)
    allocated_bytes_entity := cap * size_of(uint)

    for page in component_sparse.sparse_array.sparse{
        if page != nil{
            total_bytes_sparse += int(PAGE_INDEX) * size_of(int)
        }
    }

    return [2]int{(used_bytes_component + used_bytes_entity) + (total_bytes_sparse + size_of(ComponentSparse)), (allocated_bytes_component + allocated_bytes_entity) + (total_bytes_sparse + size_of(ComponentSparse))}
}

//allocated_memory in bytes, used_memory in bytes
get_all_memory_usage :: proc(world : $W/^$World) -> [2]int{
    total_allocated_bytes := 0
    total_used_bytes := 0

    for storage_index in 0..<world.component_stores.len{
        component_memory := get_memory_usage(world, storage_index)

        total_allocated_bytes += component_memory.x
        total_used_bytes += component_memory.y
    }

    return [2]int{total_allocated_bytes, total_used_bytes}
}

get_component_len :: proc(world : $W/^$World, storage_index : int) -> int{
    return internal_sparse_len(&world.component_stores.component_sparse[storage_index])
}

get_component_cap :: proc(world : $W/^$World, storage_index : int) -> int{
    component_info := world.component_stores.sparse_info[storage_index]
    return component_info.count
}

///////////////////////////////////////////////////////////////////

//////////////////////// Entity Store /////////////////////////////
EntityStore :: struct { 
    entities : [dynamic]int,
    removed_indicies : []uint,
    entity_index : uint,
}

@(private)
init_entity_store :: proc() -> EntityStore{
    recycled_entity_container := make([]uint, DEFAULT_STORE_CAPACITY)
    (transmute(^runtime.Raw_Slice)&recycled_entity_container).len = 0

    entity_store := EntityStore{
        entities = make([dynamic]int, 4, DEFAULT_STORE_CAPACITY),
        removed_indicies = recycled_entity_container,
    }
    
    return entity_store
}

@(private)
deinit_entity_store :: proc(entity_store : $E/^$EntityStore){
    delete(entity_store.entities)
    delete(entity_store.removed_indicies)
}

@(private)
internal_create_entity :: proc(entity_store : $E/^$EntityStore) -> uint #no_bounds_check{
    if len(entity_store.removed_indicies) <= 0{

        target_entity_id := entity_store.entity_index
        
        wrap_aroung_entity_id := entity_store.entity_index & PAGE_INDEX
        current_index := entity_store.entity_index >> 6
        
        remainder_bit := PAGE_SIZE - wrap_aroung_entity_id

        resize_dynamic_array(&entity_store.entities, int(remainder_bit + entity_store.entity_index) >> 6)

        entity_store.entities[current_index] |= 1 << wrap_aroung_entity_id
        entity_store.entity_index += 1 

        return target_entity_id
    }

    //TODO:khal implement me
    unimplemented("Using Recylced entity is not implemented yet.")
}

@(private)
internal_recylce_entity :: proc(entity_store : $E/^$EntityStore, entity : uint) #no_bounds_check{
    page := internal_fetch_page(entity)
    page_index := internal_fetch_page_index(entity)
    already_recycled_page := false

    if len(entity_store.removed_indicies) >= DEFAULT_STORE_CAPACITY{
        (transmute(^runtime.Raw_Slice)&entity_store.removed_indicies).len = 0
    }

    //Linear search. It shouldn't be to bad, since the removed_indicies is capped at 32 and once reach it will permanently remove all recycled entities 
    for x in entity_store.removed_indicies{
        if x == page{
            already_recycled_page = true
            break
        }
    }
   
    if !already_recycled_page{
        current_len := len(entity_store.removed_indicies)
        (transmute(^runtime.Raw_Slice)&entity_store.removed_indicies).len += 1
        entity_store.removed_indicies[current_len] = page
    }

    entity_store.entities[page] &= ~(1 << page_index)
}

@(private, optimization_mode = "size", enable_target_feature="lzcnt,popcnt")
interanl_fetch_alive_entites :: proc(entity_store : $E/^$EntityStore, allocator : mem.Allocator) -> []uint #no_bounds_check{

    alive_count_0 := 0
    alive_count_1 := 0
    alive_count_2 := 0
    alive_count_3 := 0

    load_entity_index := 0;
    for load_entity_index < len(entity_store.entities) {
        alive_count_0 += intrinsics.count_ones(entity_store.entities[load_entity_index])
        alive_count_1 += intrinsics.count_ones(entity_store.entities[load_entity_index + 1])
        alive_count_2 += intrinsics.count_ones(entity_store.entities[load_entity_index + 2])
        alive_count_3 += intrinsics.count_ones(entity_store.entities[load_entity_index + 3])
        load_entity_index += 4;
    }

    total_alive_entites := (alive_count_0 + alive_count_1) + (alive_count_2 + alive_count_3) + 1

    entity_slice := make_slice([]uint, total_alive_entites, allocator)

    store_entity_index := 1

    for i := 0; i < load_entity_index; i += 4{
        entity_offset_0 := i << PAGE_BIT
        entity_offset_1 := (i + 1) << PAGE_BIT
        entity_offset_2 := (i + 2) << PAGE_BIT
        entity_offset_3 := (i + 3) << PAGE_BIT

        current_bit_0 := entity_store.entities[i]
        current_bit_1 := entity_store.entities[i + 1]
        current_bit_2 := entity_store.entities[i + 2]
        current_bit_3 := entity_store.entities[i + 3]

        for bit_count in 0..<64{
            target_entity_bit_0 := (current_bit_0 & -current_bit_0)
            target_entity_bit_1 := (current_bit_1 & -current_bit_1)
            target_entity_bit_2 := (current_bit_2 & -current_bit_2)
            target_entity_bit_3 := (current_bit_3 & -current_bit_3)

            target_bit_0 :=int(PAGE_INDEX) - intrinsics.count_leading_zeros(target_entity_bit_0)
            target_bit_1 :=int(PAGE_INDEX) - intrinsics.count_leading_zeros(target_entity_bit_1)
            target_bit_2 :=int(PAGE_INDEX) - intrinsics.count_leading_zeros(target_entity_bit_2)
            target_bit_3 :=int(PAGE_INDEX) - intrinsics.count_leading_zeros(target_entity_bit_3)

            index_mask_0 := target_bit_0 >> PAGE_INDEX
            index_mask_1 := target_bit_1 >> PAGE_INDEX
            index_mask_2 := target_bit_2 >> PAGE_INDEX
            index_mask_3 := target_bit_3 >> PAGE_INDEX
            
            current_bit_0 &= ~target_entity_bit_0
            current_bit_1 &= ~target_entity_bit_1
            current_bit_2 &= ~target_entity_bit_2
            current_bit_3 &= ~target_entity_bit_3

            entity_slice[store_entity_index] = uint(target_bit_0 + entity_offset_0)
            entity_slice[(store_entity_index + 1)] = uint(target_bit_1 + entity_offset_1)
            entity_slice[(store_entity_index + 2)] = uint(target_bit_2 + entity_offset_2)
            entity_slice[(store_entity_index + 3) ] = uint(target_bit_3 + entity_offset_3)

            store_entity_index += (index_mask_0 + 1) + (index_mask_1 + 1) + (index_mask_2 + 1) + (index_mask_3 + 1)
        }
    }

    return entity_slice[1:]
}

@(private)
internal_fetch_page_index :: #force_inline proc "contextless" (entity : uint) -> uint{
    return entity & PAGE_INDEX
}

@(private)
internal_fetch_page  ::  #force_inline proc "contextless" (entity : uint)  -> uint{
    return entity >> PAGE_BIT
}

/////////////////////////////////////////////////////////////////

////////////////////// ECS Query Cache //////////////////////////
QueryCache :: struct{
    hash : int,
    count : int,
}

/////////////////////////////////////////////////////////////////

///////////////////// Component Store ///////////////////////////
SparseInfo :: struct{
    size : int,
    alignment : int,
    count : int,
    type_id : typeid,
}

ComponentStore :: struct{
    component_sparse :[]ComponentSparse,
    sparse_info : []SparseInfo,
    query_cache : []QueryCache,
    len : int,
}

@(private)
init_component_store :: proc($component_count : int) -> ComponentStore where component_count > 0 #no_bounds_check{
    component_store := ComponentStore{
        component_sparse = make([]ComponentSparse, component_count),
        sparse_info = make([]SparseInfo, component_count),
        query_cache = make([]QueryCache, component_count),
    }

    return component_store
}

@(private)
internal_register_component :: proc(component_store : $C/^$ComponentStore, $component_type : typeid, size : int, allocator : mem.Allocator) -> int #no_bounds_check{
    current_len := component_store.len

    component_store.component_sparse[current_len] = init_component_sparse(component_type, size, allocator)
    
    component_store.sparse_info[current_len] = SparseInfo{
        size = size_of(component_type),
        alignment = align_of(component_type),
        count = size,
        type_id = typeid_of(component_type),
    }

    component_store.len += 1
   
    return current_len
}

@(private)
deinit_component_store :: proc(component_store : $C/^$ComponentStore){
    for &sparse in component_store.component_sparse{
        deinit_component_sparse(&sparse)
    }
    
    delete(component_store.component_sparse)
    delete(component_store.groups)
}

//////////////////////// Sparse Set //////////////////////////
SparseArray :: struct{
    sparse : [dynamic]rawptr
}

@(private)
internal_sparse_init :: proc() -> SparseArray{
    sparse := make([dynamic]rawptr)

    return SparseArray{
        sparse = sparse,
    }
}

internal_sparse_deinit :: proc(sparse_array :  $SA/^$SparseArray){
    for sparse_page in sparse_array.sparse{
        if sparse_page != nil{
            free(sparse_page)
        }
    }

    delete(sparse_array.sparse)
}

@(private)
internal_sparse_allocate_at :: proc(sparse_array : $SA/^$SparseArray, entity : uint) #no_bounds_check{
    page := internal_fetch_page(entity)

    if page < len(sparse_array.sparse){
        return
    }

    resize(&sparse_array.sparse, int(page + 1))
    sparse_array.sparse[page],_ = mem.alloc(int(PAGE_SIZE) << UINT_BIT_SIZE)
    runtime.memset(sparse_array.sparse[page], -1 , int(PAGE_SIZE) << UINT_BIT_SIZE) //can we avoid memset here. I want zero to be initialization.
}

@(private)
internal_sparse_has_index :: #force_inline proc(sparse_array : $SA/^$SparseArray, entity : uint) -> int #no_bounds_check{
    page := internal_fetch_page(entity)
    page_index := internal_fetch_page_index(entity)

    if page >= len(sparse_array.sparse){
        return -1
    }

    return ([^]int)(sparse_array.sparse[page])[page_index]
}

@(private)
internal_sparse_get_index :: #force_inline proc(sparse_array : $SA/^$SparseArray, entity : uint) -> int #no_bounds_check{
    page := internal_fetch_page(entity)
    page_index := internal_fetch_page_index(entity)

    return ([^]int)(sparse_array.sparse[page])[page_index]
}

@(private)
internal_sparse_put_index :: #force_inline proc(sparse_array : $SA/^$SparseArray, entity : uint, value : int) #no_bounds_check{
    page := internal_fetch_page(entity)
    page_index := internal_fetch_page_index(entity)

    ([^]int)(sparse_array.sparse[page])[page_index] = value 
}

@(private)
internal_sparse_swap_index ::  proc(sparse_array : $SA/^$SparseArray, #any_int dst_entity, src_entity : uint) #no_bounds_check{
    dst_page_index := internal_fetch_page_index(dst_entity)
    dst_page := internal_fetch_page(dst_entity)

    src_page_index := internal_fetch_page_index(src_entity)
    src_page := internal_fetch_page(src_entity)

    dst_sparse_page := ([^]uint)(sparse_array.sparse[dst_page])
    src_sparse_page := ([^]uint)(sparse_array.sparse[src_page])
    
    dst_sparse_page[dst_page_index], src_sparse_page[src_page_index] = src_sparse_page[src_page_index], dst_sparse_page[dst_page_index]
}

ComponentSparse :: struct {
    sparse_array : SparseArray,
    component_blob : rawptr, 
    entity_blob : rawptr,
    len : int, 
}

@(private)
init_component_sparse :: proc($type : typeid, size : int, allocator : mem.Allocator) -> ComponentSparse{
    component_blob,_ := mem.alloc(size_of(type) * size, align_of(type), allocator)
    entity_blob,_ := mem.alloc(size_of(uint) * size, align_of(uint), allocator)

    return ComponentSparse{
        sparse_array = internal_sparse_init(),
        component_blob = component_blob,
        entity_blob = entity_blob,
    }
}

deinit_component_sparse :: proc(component_sparse : $S/^$ComponentSparse){
    internal_sparse_deinit(&component_sparse.sparse_array)

    free(component_sparse.component_blob)
    free(component_sparse.entity_blob)
}

@(private)
internal_sparse_push :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, component : $T){
    current_len := component_sparse.len

    internal_sparse_allocate_at(&component_sparse.sparse_array, entity)
    internal_sparse_put_index(&component_sparse.sparse_array, entity, current_len)

    ([^]T)(component_sparse.component_blob)[current_len] = component
    ([^]uint)(component_sparse.entity_blob)[current_len] = entity

    component_sparse.len = current_len + 1
}

@(private)
internal_sparse_get_raw :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, component_size : int) -> rawptr{
    dense_index := internal_sparse_get_index(&component_sparse.sparse_array, entity)
    return rawptr(uintptr(component_sparse.component_blob) + uintptr(dense_index * component_size))
}

@(private)
internal_sparse_get :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, $component_type : typeid) ->  ^component_type{
    dense_index := internal_sparse_get_index(&component_sparse.sparse_array, entity)

    component_blob_data := ([^]component_type)(component_sparse.component_blob)
    return &component_blob_data[dense_index]
}

@(private)
internal_sparse_index_component :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : uint ,$component_type : typeid) -> ^component_type{
    component_blob_data := ([^]component_type)(component_sparse.component_blob)
    return &component_blob_data[index]
} 

@(private)
internal_sparse_index_entity :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : uint) -> uint{
    return ([^]uint)(component_sparse.entity_blob)[index] 
}

@(private)
internal_sparse_put :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, component : $T) {
    dense_index := internal_sparse_get_index(&component_sparse.sparse_array, entity)

    component_blob_data := ([^]T)(component_sparse.component_blob)
    component_blob_data[dense_index] = component
}

@(private)
internal_sparse_remove :: proc{internal_sparse_remove_with_meta,internal_sparse_remove_with_type}

@(private)
internal_sparse_remove_with_meta :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, component_size : int) #no_bounds_check{
    dense_index := internal_sparse_get_index(&component_sparse.sparse_array, entity)
    entity_component_blob := ([^]uint)(component_sparse.entity_blob)
    target_len := component_sparse.len - 1
    
    current_index := component_size * dense_index
    last_index := component_size * target_len

    current_component := rawptr(uintptr(component_sparse.component_blob) + uintptr(current_index))
    last_component := rawptr(uintptr(component_sparse.component_blob) + uintptr(last_index))

    internal_sparse_put_index(&component_sparse.sparse_array, entity_component_blob[target_len], dense_index)
    internal_sparse_put_index(&component_sparse.sparse_array, entity, -1)
    
    intrinsics.mem_copy(current_component, last_component, component_size)
    entity_component_blob[dense_index] = entity_component_blob[target_len]

    component_sparse.len = target_len
}

@(private)
internal_sparse_remove_with_type :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, $component_type : typeid){
    dense_index := internal_sparse_get_index(&component_sparse.sparse_array, entity)
    component_blob_data := ([^]component_type)(component_sparse.component_blob)
    entity_component_blob := ([^]uint)(component_sparse.entity_blob)
    target_len := component_sparse.len - 1

    internal_sparse_put_index(&component_sparse.sparse_array, entity_component_blob[target_len], dense_index)
    internal_sparse_put_index(&component_sparse.sparse_array, entity, -1)

    intrinsics.mem_copy(&component_blob_data[dense_index],&component_blob_data[target_len], size_of(component_type))
    entity_component_blob[dense_index] = entity_component_blob[target_len]

    component_sparse.len = target_len
}

@(private)
internal_sparse_has :: #force_inline proc(component_sparse : $S/^$ComponentSparse, entity : uint) -> int{
    return internal_sparse_has_index(&component_sparse.sparse_array, entity)
}

@(private)
internal_sparse_fetch_components :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid) -> []component_type{
    component_blob_data := ([^]component_type)(component_sparse.component_blob)
    return component_blob_data[:component_sparse.len]
}

@(private)
internal_sparse_fetch_component_upto :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid, len : int) -> []component_type{
    component_blob_data := ([^]component_type)(component_sparse.component_blob)
    return component_blob_data[:len]
}

@(private)
internal_sparse_fetch_entities :: #force_inline proc(component_sparse : $S/^$ComponentSparse) -> []uint {
    return ([^]uint)(component_sparse.entity_blob)[:component_sparse.len]
}

@(private)
internal_sparse_fetch_entities_upto :: #force_inline proc(component_sparse : $S/^$ComponentSparse, len : int) -> []uint {
    return ([^]uint)(component_sparse.entity_blob)[:len]
}

@(private)
internal_sparse_swap :: proc(component_sparse : $S/^$ComponentSparse, #any_int dst_entity, src_entity : int, mask : int = -1, component_size : int){
    dst_index := internal_sparse_get_index(&component_sparse.sparse_array, uint(dst_entity)) 
    src_index := internal_sparse_get_index(&component_sparse.sparse_array, uint(src_entity))
   
    target_dst_entity := dst_entity & mask
    target_src_entity := src_entity & mask
    target_dst_index := dst_index & mask
    target_src_index := src_index & mask

    dst_component_blob_data := rawptr(uintptr(component_sparse.component_blob) + uintptr(target_dst_index * component_size))
    src_component_blob_data := rawptr(uintptr(component_sparse.component_blob) + uintptr(target_src_index * component_size))

    entity_blob_data := ([^]uint)(component_sparse.entity_blob)

    internal_sparse_swap_index(&component_sparse.sparse_array, target_dst_entity, target_src_entity)
    slice.ptr_swap_overlapping(dst_component_blob_data, src_component_blob_data, component_size)
    entity_blob_data[target_dst_index], entity_blob_data[target_src_index] = entity_blob_data[target_src_index], entity_blob_data[target_dst_index]
}

@(private)
internal_sparse_len :: #force_inline proc(component_sparse : $S/^$ComponentSparse) -> int{
    return component_sparse.len
}

///////////////////////////////////////////////////////////

///////////////////////// Systems /////////////////////////

@(private)
query_1 :: proc(world : $W/^$World, storage_index : int) -> int #no_bounds_check{
    return internal_sparse_len(&world.component_stores.component_sparse[storage_index])
}

@(private)
query_2 :: proc(world : $W/^$World, storage_index : int, storage_index_1 : int, $chunk_size : int) -> int 
    where chunk_size > 2 && chunk_size % 2 == 0 #no_bounds_check  {
        query_index := 0

        sparse_set_a : ^ComponentSparse = raw_data(world.component_stores.component_sparse)[storage_index:]
        sparse_set_b : ^ComponentSparse = raw_data(world.component_stores.component_sparse)[storage_index_1:]

        total_storage := (storage_index + storage_index_1)
        total_storage_len := (sparse_set_a.len + sparse_set_b.len)
    
        hash_identifier := int(hash.fnv64a(([^]byte)(&total_storage)[:8])) ~ int(hash.fnv64a(([^]byte)(&total_storage_len)[:8]))

        if hash_identifier != world.component_stores.query_cache[storage_index].hash{
            component_info_a := world.component_stores.sparse_info[storage_index]
            component_info_b := world.component_stores.sparse_info[storage_index_1]

            entities_a := internal_sparse_fetch_entities(sparse_set_a)
            entities_b := internal_sparse_fetch_entities(sparse_set_b)

            minimum_entites := len(entities_a) < len(entities_b) ? entities_a : entities_b 

            for len(minimum_entites) > 0{
                target_chunk_size := min(len(minimum_entites), chunk_size)
                current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
                minimum_entites = next_entity_chunks

                for entity in current_entity_chunks{
                    sparse_index_a := internal_sparse_get_index(&sparse_set_a.sparse_array, entity)
                    sparse_index_b := internal_sparse_get_index(&sparse_set_b.sparse_array, entity)

                    is_valid := min(sparse_index_a | sparse_index_b, 1)
                    a_mask := (query_index - sparse_index_a) | 0x7FFF_FFFF_FFFF_FFFF
                    b_mask := (query_index - sparse_index_b) | 0x7FFF_FFFF_FFFF_FFFF

                    internal_sparse_swap(sparse_set_a,entities_a[query_index], entity, a_mask, component_info_a.size)
                    internal_sparse_swap(sparse_set_b,entities_b[query_index], entity, b_mask, component_info_b.size)

                    query_index += is_valid
                }
            }
    }

    world.component_stores.query_cache[storage_index] = QueryCache{
        hash = hash_identifier,
        count = query_index

    }
    world.component_stores.query_cache[storage_index_1] = QueryCache{
        hash = hash_identifier,
        count = query_index

    }
    
    return query_index
}

// @(private)
// query_3 :: proc(world : $W/^$World,$a : typeid, $b : typeid, $c : typeid, $chunk_size : int) -> Query_3(a, b, c)
//     where chunk_size > 2 && chunk_size % 2 == 0 && c != a && c != b #no_bounds_check{ 
//         ab_query := query_2(world, a, b, chunk_size)

//         component_info_c := world.component_stores.component_info[c]

//         defer internal_component_sparse_mod_zeroed(&world.component_stores.component_sparse[component_info_c.sparse_index])

//         target_count := ab_query.a_sparse_index + ab_query.b_sparse_index + component_info_c.sparse_index

//         sparse_set_a := world.component_stores.component_sparse[ab_query.a_sparse_index]
//         sparse_set_b := world.component_stores.component_sparse[ab_query.b_sparse_index]

//         sparse_set_c := world.component_stores.component_sparse[component_info_c.sparse_index]

//         total_modification_count := sparse_set_a.modification_count + sparse_set_b.modification_count + sparse_set_c.modification_count 

//         sub_group_index := component_info_c.group_indices[1]

//         if world.component_stores.groups[sub_group_index].count != target_count{
//             removed_sub_group_index := internal_unregister_group(&world.component_stores, Group_Type.SubGroup, {a,b,c})
//             sub_group_index = internal_register_group(&world.component_stores, Group_Type.SubGroup, {a,b,c}, removed_sub_group_index)

//             total_modification_count += 1
//         } 
        
//         sub_group := &world.component_stores.groups[sub_group_index]
//         sub_group.count = target_count

//         if total_modification_count > 0 && ab_query.len > 0{
//             sub_group.start = 0

//             entities_a := internal_sparse_fetch_entities_upto(&sparse_set_a, ab_query.len)
//             entities_b := internal_sparse_fetch_entities_upto(&sparse_set_b, ab_query.len)
            
//             entities_c := internal_sparse_fetch_entities(&sparse_set_c, len(component_info_c.field_sizes))
//             minimum_entites := len(entities_c) < len(entities_a) ? entities_c : entities_a

//             for len(minimum_entites) > 0{
//                 target_chunk_size := min(len(minimum_entites), chunk_size)
//                 current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
//                 minimum_entites = next_entity_chunks
                
//                 for entity in current_entity_chunks{

//                     sparse_group_index := internal_sparse_get_index(&sparse_set_a.sparse_array, entity)
//                     sparse_sub_group_index := internal_sparse_get_index(&sparse_set_c.sparse_array, entity)

//                     is_valid := -normalize_value(sparse_group_index | sparse_sub_group_index)

//                     sub_group_start_entity_a := entities_a[sub_group.start]
//                     sub_group_start_entity_b := entities_b[sub_group.start]
    
//                     sub_group_start_entity_c := entities_c[sub_group.start]

//                     internal_sparse_swap(&sparse_set_a, sub_group_start_entity_a, entity, a, is_valid)
//                     internal_sparse_swap(&sparse_set_b, sub_group_start_entity_b, entity, b, is_valid)
//                     internal_sparse_swap(&sparse_set_c, sub_group_start_entity_c, entity, c, is_valid)
    
//                     sub_group.start -= is_valid

//                 }
//             }
//         }

//         return Query_3(a, b, c){
//             world = world,
//             index = 0,
//             len = sub_group.start,

//             a_sparse_index = ab_query.a_sparse_index,
//             b_sparse_index = ab_query.b_sparse_index,
//             c_sparse_index = component_info_c.sparse_index,
//         }
// }

// @(private)
// query_4 :: proc(world : $W/^$World,$a : typeid, $b : typeid, $c : typeid, $d : typeid, $chunk_size : int) -> Query_4(a,b,c,d) 
//     where chunk_size > 2 && chunk_size % 2 == 0 #no_bounds_check{
//         ab_query := query_2(world, a,b, chunk_size)
//         cd_query := query_2(world, c,d, chunk_size)

//         component_info_d := world.component_stores.component_info[d]

//         target_count := (ab_query.a_sparse_index + ab_query.b_sparse_index) + (cd_query.a_sparse_index + cd_query.b_sparse_index)

//         sparse_set_a := world.component_stores.component_sparse[ab_query.a_sparse_index]
//         sparse_set_b := world.component_stores.component_sparse[ab_query.b_sparse_index]

//         sparse_set_c := world.component_stores.component_sparse[cd_query.a_sparse_index]
//         sparse_set_d := world.component_stores.component_sparse[cd_query.b_sparse_index]

//         total_modification_count := (sparse_set_a.modification_count + sparse_set_b.modification_count) + (sparse_set_c.modification_count + sparse_set_d.modification_count)

//         sub_group_index := component_info_d.group_indices[1]

//         if world.component_stores.groups[sub_group_index].count != target_count{
//             removed_sub_group_index := internal_unregister_group(&world.component_stores, Group_Type.SubGroup, {a,b,c,d})
//             sub_group_index = internal_register_group(&world.component_stores, Group_Type.SubGroup, {a,b,c,d}, removed_sub_group_index)

//             total_modification_count += 1
//         }

//         sub_group := &world.component_stores.groups[sub_group_index]
//         sub_group.count = target_count

//         if total_modification_count > 0 && ab_query.len > 0 && cd_query.len > 0 {
//             sub_group.start = 0

//             entities_a := internal_sparse_fetch_entities_upto(&sparse_set_a, ab_query.len)
//             entities_b := internal_sparse_fetch_entities_upto(&sparse_set_b, ab_query.len)

//             entities_c := internal_sparse_fetch_entities_upto(&sparse_set_c, cd_query.len)
//             entities_d := internal_sparse_fetch_entities_upto(&sparse_set_d, cd_query.len)
            
//             minimum_entites := ab_query.len < cd_query.len ? entities_a : entities_c

//             for len(minimum_entites) > 0{
//                 target_chunk_size := min(len(minimum_entites), chunk_size)
//                 current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
//                 minimum_entites = next_entity_chunks

//                 for entity in current_entity_chunks{
//                     group_a_sparse_index := internal_sparse_get_index(&sparse_set_a.sparse_array, entity)
//                     group_b_sparse_index := internal_sparse_get_index(&sparse_set_c.sparse_array, entity)

//                     //we negate it since sparse swap mask represent -1 as true and 0 as false
//                     is_valid := -normalize_value(group_a_sparse_index | group_b_sparse_index)
    
//                     sub_group_start_entity_a := entities_a[sub_group.start]
//                     sub_group_start_entity_b := entities_b[sub_group.start]
    
//                     sub_group_start_entity_c := entities_c[sub_group.start]
//                     sub_group_start_entity_d := entities_d[sub_group.start]
    
//                     internal_sparse_swap(&sparse_set_a, sub_group_start_entity_a, entity, a, is_valid)
//                     internal_sparse_swap(&sparse_set_b, sub_group_start_entity_b, entity, b, is_valid)
//                     internal_sparse_swap(&sparse_set_c, sub_group_start_entity_c, entity, c, is_valid)
//                     internal_sparse_swap(&sparse_set_d, sub_group_start_entity_d, entity, d, is_valid)
    
//                     sub_group.start -= is_valid

//                 }
//             }
//         }

//         return Query_4(a,b,c,d){
//             world = world,
//             index = 0,
//             len = sub_group.start,

//             a_sparse_index = ab_query.a_sparse_index,
//             b_sparse_index = ab_query.b_sparse_index,
//             c_sparse_index = cd_query.a_sparse_index,
//             d_sparse_index = cd_query.b_sparse_index,
//         }
// }

query :: proc{query_1,query_2  } //query_3, query_4

//////////////////////////////////////////////////////////

