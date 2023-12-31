package journey

import "core:slice"
import "core:runtime"
import "core:intrinsics"
import "core:mem"
////////////////////////////// ECS Constant /////////////////////////////

DEFAULT_STORE_CAPACITY :: 32
DEFAULT_COMPONENT_SPARSE :: 32

UINT_BIT_SIZE :: intrinsics.count_trailing_zeros(uint(size_of(int)))
ENTITY_BIT_SIZE :: size_of(int) << UINT_BIT_SIZE
PAGE_SIZE :uint: 64
PAGE_BIT :uint: intrinsics.count_trailing_zeros(PAGE_SIZE)
PAGE_INDEX :uint: PAGE_SIZE - 1

////////////////////////////// ECS Resource ////////////////////////////
Resources :: struct{
    //
}
////////////////////////////////////////////////////////////////////////

////////////////////////////// ECS Utility /////////////////////////////

//return 0 for all negative and 1 for all postive and zero.
@(private)
normalize_value :: #force_inline proc "contextless" (val : int) -> int{
    return (val >> 63) + 1 //arithemtic shift
}

@(private)
fetch_raw_soa_footer :: #force_inline proc(raw_soa : rawptr, field_count : int) -> (footer : ^runtime.Raw_SOA_Footer_Dynamic_Array){
    raw_field_count := uintptr(field_count)
    footer = (^runtime.Raw_SOA_Footer_Dynamic_Array)(uintptr(raw_soa) + raw_field_count * size_of(rawptr))
    return
} 
///////////////////////////////////////////////////////////////////////

////////////////////////// ECS World //////////////////////////////////
SOAType :: struct($T : typeid){}

World :: struct{
    entities_stores : EntityStore,
    component_stores : ComponentStore,
}

init_world :: proc() -> ^World{
    world := new(World)

    world.entities_stores = init_entity_store()
    world.component_stores = init_component_store()

    return world
}

deinit_world :: proc(world : $W/^$World){
    deinit_entity_store(&world.entities_stores)
    deinit_component_store(&world.component_stores)
    free(world)
}

register :: proc(world : $W/^$World, $component_type : typeid) 
where intrinsics.type_is_struct(component_type){
    internal_register_component(&world.component_stores, component_type)
}

set_soa_component :: proc(world : $W/^$World, entity : uint, component : $E)
where intrinsics.type_is_struct(E){
    sparse_index := world.component_stores.component_info[E].sparse_index
    internal_sparse_put(&world.component_stores.component_sparse[sparse_index],entity,component)
}

get_soa_component :: proc(world : $W/^$World, entity : uint, $component_type : typeid) -> component_type
where intrinsics.type_is_struct(component_type){
    sparse_index := world.component_stores.component_info[component_type].sparse_index 
    return internal_sparse_get(&world.component_stores.component_sparse[sparse_index], entity, component_type) 
}

has_soa_component :: proc(world : $W/^$World, entity : uint, $component_type : typeid) -> bool
where intrinsics.type_is_struct(component_type){
    sparse_index := world.component_stores.component_info[component_type].sparse_index 
    return internal_sparse_has(&world.component_stores.component_sparse[sparse_index], entity) >= 0

}

add_soa_component :: proc(world : $W/^$World, entity : uint, component : $E)
where intrinsics.type_is_struct(E){ 
    component_info := world.component_stores.component_info[E]
    internal_sparse_push(&world.component_stores.component_sparse[component_info.sparse_index], entity, len(component_info.field_sizes), component)
   
}

remove_soa_component :: proc(world : $W/^$World, entity : uint, $component_type : typeid)
where intrinsics.type_is_struct(component_type){
    sparse_index := world.component_stores.component_info[component_type].sparse_index 
    internal_sparse_remove(&world.component_stores.component_sparse[sparse_index],entity,component_type)
}


get_soa_component_with_id :: proc(world : $W/^$World, $component_type : typeid/SOAType($E)) -> (entity_slice: []uint,soa_slice :# soa[]E, length : int)
where intrinsics.type_is_struct(E){
    soa_slice, length = get_soa_components(world, component_type)
    entity_slice = get_id_soa_components(world, E)
    return
}

get_id_soa_components :: proc(world : $W/^$World, $component_type : typeid) -> []uint
where intrinsics.type_is_struct(component_type){
    component_info := world.component_stores.component_info[component_type]
    return internal_sparse_fetch_entities(&world.component_stores.component_sparse[component_info.sparse_index], len(component_info.field_sizes))
} 


get_soa_components :: proc(world : $W/^$World, $component_type : typeid/SOAType($E)) -> (soa_slice :# soa[]E, length : int) 
where intrinsics.type_is_struct(E){
    sparse_index := world.component_stores.component_info[E].sparse_index 
    soa_slice, length = internal_sparse_fetch_components(& world.component_stores.component_sparse[sparse_index], component_type)
    return
}

create_entity :: proc(world : $W/^$World) -> uint{
    return internal_create_entity(&world.entities_stores)
}

remove_entity :: proc(world : $W/^$World, entity : uint){
    for type, component_info in world.component_stores.component_info{
        if internal_sparse_has(&world.component_stores.component_sparse[component_info.sparse_index], entity) >= 0{ // Should we even check?
            internal_sparse_remove(&world.component_stores.component_sparse[component_info.sparse_index], component_info.field_sizes, entity)
        }
    }

    internal_remove_entity(&world.entities_stores,entity)
}

//allocated_memory, used_memory, len
get_memory_usage :: proc(world : $W/^$World, component_type : typeid) -> [3]int{
    component_info := world.component_stores.component_info[component_type] 

    field_size := len(component_info.field_sizes)

    len := internal_sparse_len(&world.component_stores.component_sparse[component_info.sparse_index], field_size)
    cap := internal_sparse_cap(&world.component_stores.component_sparse[component_info.sparse_index], field_size)

    component_sparse := world.component_stores.component_sparse[component_info.sparse_index]

    total_struct_size := 0
    total_bytes_sparse := 0

    for size in component_info.field_sizes{
        total_struct_size += size
    }

    used_bytes_component := len * total_struct_size
    used_bytes_entity := len * size_of(uint)

    allocated_bytes_component := cap * total_struct_size
    allocated_bytes_entity := cap * size_of(uint)

    for page in component_sparse.sparse_array.sparse{
        if page != nil{
            total_bytes_sparse += int(PAGE_INDEX) * size_of(int)
        }
    }

    return [3]int{(used_bytes_component + used_bytes_entity) + (total_bytes_sparse + size_of(ComponentSparse)), (allocated_bytes_component + allocated_bytes_entity) + (total_bytes_sparse + size_of(ComponentSparse)), len}
}

//allocated_memory, used_memory
get_all_memory_usage :: proc(world : $W/^$World) -> [2]int{
    total_allocated_bytes := 0
    total_used_bytes := 0

    for component_type, _ in world.component_stores.component_info{
        component_memory := get_memory_usage(world, component_type)

        total_allocated_bytes += component_memory[0]
        total_used_bytes += component_memory[1]
    }

    return [2]int{total_allocated_bytes, total_used_bytes}
}

get_soa_component_len :: proc(world : $W/^$World, $component_type : typeid) -> int{
    component_info := world.component_stores.component_info[component_type] 
    return internal_sparse_len(&world.component_stores.component_sparse[component_info.sparse_index], len(component_info.field_sizes))
}

get_soa_component_cap :: proc(world : $W/^$World, $component_type : typeid) -> int{
    component_info := world.component_stores.component_info[component_type] 
    return internal_sparse_cap(&world.component_stores.component_sparse[component_info.sparse_index], len(component_info.field_sizes))
}
///////////////////////////////////////////////////////////////////

//////////////////////// Entity Store /////////////////////////////
EntityStore :: struct { 
     entities : [dynamic]int,
     removed_indicies : [dynamic]uint,
     current_index : uint,
}

@(private)
init_entity_store :: proc() -> EntityStore{

    //DEFAULT_STORE_CAPACITY * 64 can be added before resizing entities 
    //removed_indices will be reserve a small size of the dynamic array, since DEFAULT_STORE_CAPACITY(32) * 64 is large and uncommon to delete 2048 entities
    entity_store := EntityStore{
        entities = make([dynamic]int, 1,DEFAULT_STORE_CAPACITY),
        removed_indicies = make([dynamic]uint, 0, DEFAULT_STORE_CAPACITY >> 4),
        current_index = 0,
    }
    
    return entity_store
}

@(private)
deinit_entity_store :: proc(entity_store : $E/^$EntityStore){
    delete(entity_store.entities)
    delete(entity_store.removed_indicies)
}

@(private)
internal_create_entity :: proc(entity_store : $E/^$EntityStore) -> uint{
    if entity_store.entities[entity_store.current_index] == -1{
        append_nothing(&entity_store.entities)
        entity_store.current_index += 1
    }

    if len(entity_store.removed_indicies) <= 0{
        entity_bits := entity_store.entities[entity_store.current_index]

        entity_id :  = ENTITY_BIT_SIZE - uint(intrinsics.count_leading_zeros(entity_bits))
        target_entity_bit := 1 << entity_id

        entity_store.entities[entity_store.current_index] |= target_entity_bit

        target_entity_offset := entity_store.current_index << PAGE_BIT
        return entity_id + target_entity_offset
    }else{
        removed_entity_index := entity_store.removed_indicies[0]
        entity_bits := entity_store.entities[removed_entity_index]

        trailing_entity_bit := intrinsics.count_trailing_zeros(entity_bits) - 1
        invert_trailing_entity_bit := intrinsics.count_trailing_zeros(~entity_bits)

        recycled_entity_id := uint(trailing_entity_bit >= 0 ? trailing_entity_bit : invert_trailing_entity_bit)
        recycled_entity_bit := 1 << recycled_entity_id

        entity_store.entities[removed_entity_index] |= recycled_entity_bit

        if entity_store.entities[removed_entity_index] == -1{
            unordered_remove(&entity_store.removed_indicies, 0)
        }

        recycled_entity_offset := removed_entity_index << PAGE_BIT
        return recycled_entity_id + recycled_entity_offset
    }
}

@(private)
internal_remove_entity :: proc(entity_store : $E/^$EntityStore, entity : uint){
    page := internal_fetch_page(entity)
    page_index := internal_fetch_page_index(entity)
    entity_store.entities[page] &= ~(1 << page_index)

    //Linear search. It shouldn't be to bad for small amount of entity 4096 lower, but higher will need more iteration
    //Iteration amount is (EntityID + 1) / 64, but this is ok since deleting entity should be called sparingly in a ecs solution
    if !slice.contains(entity_store.removed_indicies[:], page){
        append(&entity_store.removed_indicies, page)
    }
}

@(private)
internal_fetch_page_index :: #force_inline proc(entity : uint) -> uint{
    return entity & PAGE_INDEX
}

@(private)
internal_fetch_page :: #force_inline proc(entity : uint) -> uint{
    return entity >> PAGE_BIT
}

/////////////////////////////////////////////////////////////////

///////////////////////// ECS Group /////////////////////////////
Group_Type :: enum int{
    Group = 0,
    SubGroup = 1
}

Group :: struct{
    start : int,
    count : int,
}

/////////////////////////////////////////////////////////////////

///////////////////// Component Store ///////////////////////////
ComponentInfo :: struct{
    sparse_index : int,
    group_indices : [2]int,
    
    field_sizes : []int,
}

ComponentStore :: struct{
    component_info : map[typeid]ComponentInfo, 
    component_sparse : [dynamic]ComponentSparse, 
    groups : [dynamic]Group,
}

@(private)
init_component_store :: proc() -> ComponentStore #no_bounds_check{
    //groups will be reserve a small size of the dynamic array, since groups is only used for query.
    component_store := ComponentStore{
        component_info = make_map(map[typeid]ComponentInfo, DEFAULT_STORE_CAPACITY),
        component_sparse = make([dynamic]ComponentSparse, 0, DEFAULT_STORE_CAPACITY),
        groups = make([dynamic]Group, 1, DEFAULT_STORE_CAPACITY >> 2),
    }

    component_store.groups[0] = Group{
        start=0,
        count=-1,
    }

    return component_store
}

@(private)
internal_register_group :: proc(component_store : $C/^$ComponentStore, $group_type :Group_Type, structure_types : []typeid, index : int) -> int #no_bounds_check{
    group_len := len(component_store.groups)
    mask_index := normalize_value(index)

    target_group_index := 1 + index // if it is zero then we know that we need to append a new group since there is no freed space in the groups collection
    invert_mask_index := 1 - mask_index

    group_index := (group_len * invert_mask_index) + (target_group_index * mask_index)

    for id in structure_types{
        component_info := &component_store.component_info[id]
        component_info.group_indices[group_type] = group_index
    }
    
    if mask_index == 0{
        append_nothing(&component_store.groups)
    }

    return group_index
}

@(private)
internal_unregister_group :: proc(component_store : $C/^$ComponentStore, $group_type :Group_Type, structure_types : []typeid)  -> int #no_bounds_check{
    component_info := component_store.component_info[structure_types[0]]
    group_index := component_info.group_indices[group_type]

    component_store.groups[group_index] = {}

    for id in structure_types{
        component_info := &component_store.component_info[id]
        component_info.group_indices[group_type] = 0
    }

    return group_index - 1
}

//TODO:khal maybe add internal_register_component_bulk to allow multiple component registerd. Look at the odin lang https://github.com/odin-lang/Odin/blob/master/core/runtime/core_builtin.odin#L410 for reference.
@(private)
internal_register_component :: proc(component_store : $C/^$ComponentStore, $component_type : typeid) #no_bounds_check{
    if component_type not_in component_store.component_info{
        field_count :: intrinsics.type_struct_field_count(component_type)

        struct_var_sizes := make([]int, field_count)

        {
            // We lose alot of detail in the component sparse so we must cache it in component_info
            // When registering component. This function is called per unique struct type to register so it ok.

            type_info := type_info_of(typeid_of(component_type))
            type_info = runtime.type_info_base(type_info)
            struct_info := type_info.variant.(runtime.Type_Info_Struct)
    
            for type, index in struct_info.types{
                struct_var_sizes[index] = type.size
            }
        }

        component_store.component_info[component_type] = ComponentInfo{
            sparse_index = len(component_store.component_sparse),
            field_sizes = struct_var_sizes,
        }

        append(&component_store.component_sparse, init_component_sparse(component_type))
    }
}

@(private)
deinit_component_store :: proc(component_store : $C/^$ComponentStore){
    for i in 0..<len(component_store.component_sparse){
        deinit_component_sparse(&component_store.component_sparse[i])
    }

    delete(component_store.component_sparse)

    for _,component in component_store.component_info{
        delete(component.field_sizes)
    }

    delete(component_store.component_info)
    delete(component_store.groups)
}

//////////////////////// Sparse Set //////////////////////////
SparseArray :: struct{
    sparse : [dynamic]rawptr
}

@(private)
internal_sparse_init :: proc() -> SparseArray{
    return SparseArray{
        sparse = make([dynamic]rawptr),
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

//TODO:khal maybe add internal_sparse_allocate_at_bulk to handle internal_sparse_push_bulk implementation
@(private)
internal_sparse_allocate_at :: proc(sparse_array : $SA/^$SparseArray, entity : uint){
    page := internal_fetch_page(entity)

    if page >= len(sparse_array.sparse){
        resize(&sparse_array.sparse, int(page + 1))
    } 

    if sparse_array.sparse[page] == nil{
        sparse_array.sparse[page],_ = mem.alloc(int(PAGE_INDEX) << UINT_BIT_SIZE)
    }
}

@(private)
internal_sparse_has_index :: #force_inline proc(sparse_array : $SA/^$SparseArray, entity : uint) -> int{
    page := internal_fetch_page(entity)
    page_id := internal_fetch_page_index(entity)

    if  page < len(sparse_array.sparse){
        sparse_page := sparse_array.sparse[page]

        return sparse_page != nil ? ([^]int)(sparse_page)[page_id] - 1 : -1
    }

    return -1
}

@(private)
internal_sparse_get_index :: #force_inline proc(sparse_array : $SA/^$SparseArray, entity : uint) -> int{
    page := internal_fetch_page(entity)
    page_id := internal_fetch_page_index(entity)

    sparse_id := ([^]int)(sparse_array.sparse[page])[page_id]
    return sparse_id - 1
}

@(private)
internal_sparse_put_index :: #force_inline proc(sparse_array : $SA/^$SparseArray, entity : uint, value : int){
    page := internal_fetch_page(entity)
    page_id := internal_fetch_page_index(entity)

    ([^]int)(sparse_array.sparse[page])[page_id] = value
}

@(private)
internal_sparse_swap_index ::  proc(sparse_array : $SA/^$SparseArray, dst_entity : uint, src_entity : uint){
    dst_page_id := internal_fetch_page_index(dst_entity)
    src_page_id := internal_fetch_page_index(src_entity)

    dst_page := internal_fetch_page(dst_entity)
    src_page := internal_fetch_page(src_entity)

    dst_sparse_page := sparse_array.sparse[dst_page]
    src_sparse_page := sparse_array.sparse[src_page]

    ([^]uint)(dst_sparse_page)[dst_page_id], ([^]uint)(src_sparse_page)[src_page_id] = ([^]uint)(src_sparse_page)[src_page_id], ([^]uint)(dst_sparse_page)[dst_page_id]
}

ComponentSparse :: struct {
    sparse_array : SparseArray,
    component_blob : rawptr, 
    entity_blob : rawptr,
    modification_count : int, 
}

@(private)
init_component_sparse :: proc($type : typeid) -> ComponentSparse{
    component_soa_dense :^#soa [dynamic]type = new(#soa[dynamic]type) 
    reserve_soa(component_soa_dense, DEFAULT_COMPONENT_SPARSE)
    entity_blob,_ := mem.alloc(DEFAULT_COMPONENT_SPARSE << UINT_BIT_SIZE)

    return ComponentSparse{
        sparse_array = internal_sparse_init(),
        entity_blob = entity_blob,
        component_blob = (^rawptr)(component_soa_dense),
        modification_count = 0,
    }
}

deinit_component_sparse :: proc(component_sparse : $S/^$ComponentSparse){
    internal_sparse_deinit(&component_sparse.sparse_array)

    mem.free(component_sparse.entity_blob)

    free((^rawptr)(component_sparse.component_blob)^)
    free(component_sparse.component_blob)
}

@(private)
internal_component_sparse_mod_zeroed  :: #force_inline proc(component_sparse : $S/^$ComponentSparse){
    component_sparse.modification_count = 0
}

//TODO:khal maybe add internal_sparse_push_bulk to allow multiple entities and component add together. Look at the odin lang https://github.com/odin-lang/Odin/blob/master/core/runtime/core_builtin.odin#L410 for reference.

@(private)
internal_sparse_push :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, field_count : int, component : $T) #no_bounds_check{
    internal_sparse_allocate_at(&component_sparse.sparse_array, entity)

    raw_footer := fetch_raw_soa_footer(component_sparse.component_blob, field_count)

    current_cap := raw_footer.cap
    current_len := raw_footer.len

    soa_component_array := cast(^#soa[dynamic]T)(component_sparse.component_blob)
    append_soa_elem(soa_component_array, component)

    if current_cap <= raw_footer.len{
        component_sparse.entity_blob,_ = mem.resize(component_sparse.entity_blob, current_cap << UINT_BIT_SIZE, raw_footer.cap << UINT_BIT_SIZE)
    }

    ([^]uint)(component_sparse.entity_blob)[current_len] = entity

    internal_sparse_put_index(&component_sparse.sparse_array, entity, raw_footer.len)
    
    component_sparse.modification_count += 1
}

@(private)
internal_sparse_get :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, $component_type : typeid) ->  component_type #no_bounds_check {
    dense_index := internal_sparse_get_index(&component_sparse.sparse_array, entity)
    return (cast(^#soa[dynamic]component_type)(component_sparse.component_blob))[dense_index]
}

@(private)
internal_sparse_index_component :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : uint ,$component_type : typeid) -> component_type{
    return (cast(^#soa[dynamic]component_type)(component_sparse.component_blob))[index]
} 

@(private)
internal_sparse_index_entity :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : uint) -> uint{
    return ([^]uint)(component_sparse.entity_blob)[index] 
}

@(private)
internal_sparse_put :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, component : $T) {
    dense_id := internal_sparse_get_index(&component_sparse.sparse_array, entity)

    soa_component_array := cast(^#soa[dynamic]T)(component_sparse.component_blob)
    soa_component_array[dense_id] = component
}

@(private)
internal_sparse_remove :: proc{internal_sparse_remove_with_meta,internal_sparse_remove_with_type}

@(private)
internal_sparse_remove_with_meta :: proc(component_sparse : $S/^$ComponentSparse, field_sizes : []int, entity : uint) #no_bounds_check{
    dense_id := internal_sparse_get_index(&component_sparse.sparse_array, entity)

    raw_footer := fetch_raw_soa_footer(component_sparse.component_blob, len(field_sizes))
    raw_footer.len -= 1
    
    {
        downcasted_soa_raw := (^rawptr)(component_sparse.component_blob)^

        soa_offset := 0
        for current_struct_elem_size in field_sizes{
        
            dst_elem_index := current_struct_elem_size * dense_id
            src_elem_index := current_struct_elem_size * raw_footer.len
            
            dst := rawptr(uintptr(downcasted_soa_raw) + uintptr(soa_offset + dst_elem_index))
            src := rawptr(uintptr(downcasted_soa_raw) + uintptr(soa_offset + src_elem_index))

            runtime.mem_copy(dst, src, current_struct_elem_size)

            soa_offset += current_struct_elem_size * raw_footer.cap
        }
    }
    
    last_entity := ([^]uint)(component_sparse.entity_blob)[raw_footer.len]
    ent_ptr :^uint = ([^]uint)(component_sparse.entity_blob)[dense_id:]
    ent_ptr^ = last_entity

    internal_sparse_put_index(&component_sparse.sparse_array, last_entity, dense_id + 1)
    internal_sparse_put_index(&component_sparse.sparse_array, entity, 0)

    component_sparse.modification_count += 1
}

@(private)
internal_sparse_remove_with_type :: proc(component_sparse : $S/^$ComponentSparse, entity : uint, $component_type : typeid) #no_bounds_check{
    dense_id := internal_sparse_get_index(&component_sparse.sparse_array, entity)

    soa_component_array := cast(^#soa[dynamic]component_type)(component_sparse.component_blob)
    raw_footer := raw_soa_footer_dynamic_array(soa_component_array)
    raw_footer.len -= 1

    soa_component_array[dense_id] = soa_component_array[raw_footer.len]
    
    last_entity := ([^]uint)(component_sparse.entity_blob)[raw_footer.len]
    ent_ptr :^uint = ([^]uint)(component_sparse.entity_blob)[dense_id:]
    ent_ptr^ = last_entity

    internal_sparse_put_index(&component_sparse.sparse_array, last_entity, dense_id + 1)
    internal_sparse_put_index(&component_sparse.sparse_array, entity, 0)

    component_sparse.modification_count += 1
}

@(private)
internal_sparse_has :: #force_inline proc(component_sparse : $S/^$ComponentSparse, entity : uint) -> int{
    return internal_sparse_has_index(&component_sparse.sparse_array, entity)
}

@(private)
internal_sparse_fetch_components :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid/SOAType($E)) -> (soa_slice :#soa[]E, length: int){
    soa_slice = (cast(^#soa[]E)(component_sparse.component_blob))^
    length = len(soa_slice)
    return
}

@(private)
internal_sparse_fetch_component_upto :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid/SOAType($E), len : int) -> (soa_slice :#soa[]E){
    soa_slice = (cast(^#soa[]E)(component_sparse.component_blob))^
    soa_slice = soa_slice[:len]
    return
}

@(private)
internal_sparse_fetch_entities :: #force_inline proc(component_sparse : $S/^$ComponentSparse, count : int) -> []uint {
    raw_footer := fetch_raw_soa_footer(component_sparse.component_blob, count)
    return ([^]uint)(component_sparse.entity_blob)[:raw_footer.len]
}

@(private)
internal_sparse_fetch_entities_upto :: #force_inline proc(component_sparse : $S/^$ComponentSparse, len : int) -> []uint {
    return ([^]uint)(component_sparse.entity_blob)[:len]
}

@(private)
internal_sparse_swap :: proc(component_sparse : $S/^$ComponentSparse, dst_entity, src_entity : uint, $component_type : typeid, neg_mask : int = -1) #no_bounds_check{
    soa_component_slice := (cast(^#soa[]component_type)(component_sparse.component_blob))
    entity_slice := ([^]uint)(component_sparse.entity_blob)

    dst_id := internal_sparse_get_index(&component_sparse.sparse_array, dst_entity) 
    src_id := internal_sparse_get_index(&component_sparse.sparse_array,src_entity)
    
    target_src_entity := int(src_entity) & neg_mask
    target_dst_entity := int(dst_entity) & neg_mask
    target_src_id := src_id & neg_mask
    target_dst_id := dst_id & neg_mask

    internal_sparse_swap_index(&component_sparse.sparse_array, uint(target_dst_entity),uint(target_src_entity))

    soa_component_slice[target_dst_id], soa_component_slice[target_src_id] = soa_component_slice[target_src_id], soa_component_slice[target_dst_id]
    entity_slice[target_dst_id], entity_slice[target_src_id] = entity_slice[target_src_id], entity_slice[target_dst_id]
}

@(private)
internal_sparse_len :: #force_inline proc(component_sparse : $S/^$ComponentSparse, count : int) -> int{
    raw_footer := fetch_raw_soa_footer(component_sparse.component_blob, count)
    return raw_footer.len
}

@(private)
internal_sparse_cap :: #force_inline proc(component_sparse : $S/^$ComponentSparse, count : int) -> int{
    raw_footer := fetch_raw_soa_footer(component_sparse.component_blob, count)
    return raw_footer.cap
}

///////////////////////////////////////////////////////////

///////////////////////// Systems /////////////////////////
Query_1 :: struct($a : typeid){
    world : ^World,
    index : int,
    len : int,

    a_sparse_index : int,
}

Query_2 :: struct($a : typeid, $b : typeid){
    world : ^World,
    index : int,
    len : int,

    a_sparse_index : int,
    b_sparse_index : int,
}

Query_3 :: struct($a : typeid, $b : typeid, $c : typeid){
    world : ^World,
    index : int,
    len : int,

    a_sparse_index : int,
    b_sparse_index : int,
    c_sparse_index : int,
}

Query_4 :: struct($a : typeid, $b : typeid, $c : typeid, $d : typeid){
    world : ^World,
    index : int,
    len : int,

    a_sparse_index : int,
    b_sparse_index : int,
    c_sparse_index : int,
    d_sparse_index : int,
}

Iter_1 :: struct($a : typeid){
    entities : []uint,
    component_a : #soa[]a,  
}

Iter_2 :: struct($a : typeid, $b : typeid){
    entities : []uint,
    component_a : #soa[]a,
    component_b : #soa[]b, 
}

Iter_3 :: struct($a : typeid, $b : typeid, $c : typeid){
    entities : []uint,
    component_a : #soa[]a,
    component_b : #soa[]b,
    component_c : #soa[]c,
}

Iter_4 :: struct($a : typeid, $b : typeid, $c : typeid, $d : typeid){
    entities : []uint,
    component_a : #soa[]a,
    component_b : #soa[]b,
    component_c : #soa[]c,
    component_d : #soa[]d,
}

@(private)
query_1 :: proc(world : $W/^$World, $a : typeid) -> Query_1(a) #no_bounds_check{
    component_info_a := world.component_stores.component_info[a]
    sparse_set_a := world.component_stores.component_sparse[component_info_a.sparse_index]

    return Query_1(a){
        world = world,
        index = 0,
        len = internal_sparse_len(&sparse_set_a,len(component_info_a.field_sizes)),

        a_sparse_index = component_info_a.sparse_index,
    } 
}

  
@(private)
query_2 :: proc(world : $W/^$World,$a : typeid, $b : typeid, $chunk_size : int) -> Query_2(a,b) 
    where chunk_size > 2 && chunk_size % 2 == 0 && a != b #no_bounds_check  {
        component_info_a := world.component_stores.component_info[a]
        component_info_b := world.component_stores.component_info[b]

        defer {
            internal_component_sparse_mod_zeroed(&world.component_stores.component_sparse[component_info_a.sparse_index])
            internal_component_sparse_mod_zeroed(&world.component_stores.component_sparse[component_info_b.sparse_index])
        }

        group_count_target := component_info_a.sparse_index + component_info_b.sparse_index

        sparse_set_a := world.component_stores.component_sparse[component_info_a.sparse_index]
        sparse_set_b := world.component_stores.component_sparse[component_info_b.sparse_index]

        total_modification_count := sparse_set_a.modification_count + sparse_set_b.modification_count

        group_index := component_info_a.group_indices[0]

        if world.component_stores.groups[group_index].count != group_count_target{
            removed_group_index := internal_unregister_group(&world.component_stores,Group_Type.Group, {a,b})
            group_index = internal_register_group(&world.component_stores,Group_Type.Group, {a,b}, removed_group_index)

            total_modification_count += 1
        }

        
        group := &world.component_stores.groups[group_index]
        group.count = group_count_target
   
        if total_modification_count > 0{
            group.start = 0
            entities_a := internal_sparse_fetch_entities(&sparse_set_a, len(component_info_a.field_sizes))
            entities_b := internal_sparse_fetch_entities(&sparse_set_b, len(component_info_b.field_sizes))
            minimum_entites := len(entities_a) < len(entities_b) ? entities_a : entities_b 

            for len(minimum_entites) > 0{
                target_chunk_size := min(len(minimum_entites), chunk_size)
                current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
                minimum_entites = next_entity_chunks

                for entity in current_entity_chunks{
                    group_start_entity_a := entities_a[group.start]
                    group_start_entity_b := entities_b[group.start]

                    sparse_index_a :=internal_sparse_get_index(&sparse_set_a.sparse_array, entity)
                    sparse_index_b := internal_sparse_get_index(&sparse_set_b.sparse_array, entity)

                    is_valid := normalize_value(sparse_index_a|sparse_index_b)

                    a_mask := -normalize_value((sparse_index_a & -is_valid) +(-1 - group.start))
                    b_mask := -normalize_value((sparse_index_b & -is_valid) +(-1 - group.start))
               
                    //fmt.println(entity, sparse_index_a, sparse_index_b, is_valid )
                    internal_sparse_swap(&sparse_set_a, group_start_entity_a, entity, a, a_mask)
                    internal_sparse_swap(&sparse_set_b, group_start_entity_b, entity, b, b_mask)

                    group.start += is_valid
                }
            }
    }
    
    return Query_2(a,b){
        world = world,
        index = 0,
        len = group.start,

        a_sparse_index = component_info_a.sparse_index,
        b_sparse_index = component_info_b.sparse_index,
    }
}

@(private)
query_3 :: proc(world : $W/^$World,$a : typeid, $b : typeid, $c : typeid, $chunk_size : int) -> Query_3(a, b, c)
    where chunk_size > 2 && chunk_size % 2 == 0 && c != a && c != b #no_bounds_check{ 
        ab_query := query_2(world, a, b, chunk_size)

        component_info_c := world.component_stores.component_info[c]

        defer internal_component_sparse_mod_zeroed(&world.component_stores.component_sparse[component_info_c.sparse_index])

        target_count := ab_query.a_sparse_index + ab_query.b_sparse_index + component_info_c.sparse_index

        sparse_set_a := world.component_stores.component_sparse[ab_query.a_sparse_index]
        sparse_set_b := world.component_stores.component_sparse[ab_query.b_sparse_index]

        sparse_set_c := world.component_stores.component_sparse[component_info_c.sparse_index]

        total_modification_count := sparse_set_a.modification_count + sparse_set_b.modification_count + sparse_set_c.modification_count 

        sub_group_index := component_info_c.group_indices[1]

        if world.component_stores.groups[sub_group_index].count != target_count{
            removed_sub_group_index := internal_unregister_group(&world.component_stores, Group_Type.SubGroup, {a,b,c})
            sub_group_index = internal_register_group(&world.component_stores, Group_Type.SubGroup, {a,b,c}, removed_sub_group_index)

            total_modification_count += 1
        } 
        
        sub_group := &world.component_stores.groups[sub_group_index]
        sub_group.count = target_count

        if total_modification_count > 0 && ab_query.len > 0{
            sub_group.start = 0

            entities_a := internal_sparse_fetch_entities_upto(&sparse_set_a, ab_query.len)
            entities_b := internal_sparse_fetch_entities_upto(&sparse_set_b, ab_query.len)
            
            entities_c := internal_sparse_fetch_entities(&sparse_set_c, len(component_info_c.field_sizes))
            minimum_entites := len(entities_c) < len(entities_a) ? entities_c : entities_a

            for len(minimum_entites) > 0{
                target_chunk_size := min(len(minimum_entites), chunk_size)
                current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
                minimum_entites = next_entity_chunks
                
                for entity in current_entity_chunks{

                    sparse_group_index := internal_sparse_get_index(&sparse_set_a.sparse_array, entity)
                    sparse_sub_group_index := internal_sparse_get_index(&sparse_set_c.sparse_array, entity)

                    is_valid := -normalize_value(sparse_group_index | sparse_sub_group_index)

                    sub_group_start_entity_a := entities_a[sub_group.start]
                    sub_group_start_entity_b := entities_b[sub_group.start]
    
                    sub_group_start_entity_c := entities_c[sub_group.start]

                    internal_sparse_swap(&sparse_set_a, sub_group_start_entity_a, entity, a, is_valid)
                    internal_sparse_swap(&sparse_set_b, sub_group_start_entity_b, entity, b, is_valid)
                    internal_sparse_swap(&sparse_set_c, sub_group_start_entity_c, entity, c, is_valid)
    
                    sub_group.start -= is_valid

                }
            }
        }

        return Query_3(a, b, c){
            world = world,
            index = 0,
            len = sub_group.start,

            a_sparse_index = ab_query.a_sparse_index,
            b_sparse_index = ab_query.b_sparse_index,
            c_sparse_index = component_info_c.sparse_index,
        }
}

@(private)
query_4 :: proc(world : $W/^$World,$a : typeid, $b : typeid, $c : typeid, $d : typeid, $chunk_size : int) -> Query_4(a,b,c,d) 
    where chunk_size > 2 && chunk_size % 2 == 0 #no_bounds_check{
        ab_query := query_2(world, a,b, chunk_size)
        cd_query := query_2(world, c,d, chunk_size)

        component_info_d := world.component_stores.component_info[d]

        target_count := (ab_query.a_sparse_index + ab_query.b_sparse_index) + (cd_query.a_sparse_index + cd_query.b_sparse_index)

        sparse_set_a := world.component_stores.component_sparse[ab_query.a_sparse_index]
        sparse_set_b := world.component_stores.component_sparse[ab_query.b_sparse_index]

        sparse_set_c := world.component_stores.component_sparse[cd_query.a_sparse_index]
        sparse_set_d := world.component_stores.component_sparse[cd_query.b_sparse_index]

        total_modification_count := (sparse_set_a.modification_count + sparse_set_b.modification_count) + (sparse_set_c.modification_count + sparse_set_d.modification_count)

        sub_group_index := component_info_d.group_indices[1]

        if world.component_stores.groups[sub_group_index].count != target_count{
            removed_sub_group_index := internal_unregister_group(&world.component_stores, Group_Type.SubGroup, {a,b,c,d})
            sub_group_index = internal_register_group(&world.component_stores, Group_Type.SubGroup, {a,b,c,d}, removed_sub_group_index)

            total_modification_count += 1
        }

        sub_group := &world.component_stores.groups[sub_group_index]
        sub_group.count = target_count

        if total_modification_count > 0 && ab_query.len > 0 && cd_query.len > 0 {
            sub_group.start = 0

            entities_a := internal_sparse_fetch_entities_upto(&sparse_set_a, ab_query.len)
            entities_b := internal_sparse_fetch_entities_upto(&sparse_set_b, ab_query.len)

            entities_c := internal_sparse_fetch_entities_upto(&sparse_set_c, cd_query.len)
            entities_d := internal_sparse_fetch_entities_upto(&sparse_set_d, cd_query.len)
            
            minimum_entites := ab_query.len < cd_query.len ? entities_a : entities_c

            for len(minimum_entites) > 0{
                target_chunk_size := min(len(minimum_entites), chunk_size)
                current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
                minimum_entites = next_entity_chunks

                for entity in current_entity_chunks{
                    group_a_sparse_index := internal_sparse_get_index(&sparse_set_a.sparse_array, entity)
                    group_b_sparse_index := internal_sparse_get_index(&sparse_set_c.sparse_array, entity)

                    //we negate it since sparse swap mask represent -1 as true and 0 as false
                    is_valid := -normalize_value(group_a_sparse_index | group_b_sparse_index)
    
                    sub_group_start_entity_a := entities_a[sub_group.start]
                    sub_group_start_entity_b := entities_b[sub_group.start]
    
                    sub_group_start_entity_c := entities_c[sub_group.start]
                    sub_group_start_entity_d := entities_d[sub_group.start]
    
                    internal_sparse_swap(&sparse_set_a, sub_group_start_entity_a, entity, a, is_valid)
                    internal_sparse_swap(&sparse_set_b, sub_group_start_entity_b, entity, b, is_valid)
                    internal_sparse_swap(&sparse_set_c, sub_group_start_entity_c, entity, c, is_valid)
                    internal_sparse_swap(&sparse_set_d, sub_group_start_entity_d, entity, d, is_valid)
    
                    sub_group.start -= is_valid

                }
            }
        }

        return Query_4(a,b,c,d){
            world = world,
            index = 0,
            len = sub_group.start,

            a_sparse_index = ab_query.a_sparse_index,
            b_sparse_index = ab_query.b_sparse_index,
            c_sparse_index = cd_query.a_sparse_index,
            d_sparse_index = cd_query.b_sparse_index,
        }
}


run_1 :: proc(query : ^Query_1($a)) -> (iterator : Iter_1(a), idx : int, cond : bool) #no_bounds_check {
    iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
    iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], SOAType(a), query.len)

    if cond = query.index < query.len; cond{
        idx = query.index
        query.index += 1
    }

    //Reset
    if !cond{
        query.index = 0
    }
    return
}

run_2 :: proc(query : ^Query_2($a, $b)) -> (iterator : Iter_2(a,b), idx : int, cond : bool) #no_bounds_check{    
    iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
    iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], SOAType(a), query.len)
    iterator.component_b = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.b_sparse_index], SOAType(b), query.len)

    if cond = query.index < query.len; cond{
        idx = query.index
        query.index += 1
    }

    //Reset
    if !cond{
        query.index = 0
    }
    return
}

run_3 :: proc(query : ^Query_3($a, $b, $c)) -> (iterator : Iter_3(a,b,c), idx : int, cond : bool) #no_bounds_check{
    iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
    iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], SOAType(a), query.len)
    iterator.component_b = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.b_sparse_index], SOAType(b), query.len)
    iterator.component_c = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.c_sparse_index], SOAType(c), query.len)


    if cond = query.index < query.len; cond{
        idx = query.index
        query.index += 1
    }


    //Reset
    if !cond{
        query.index = 0
    }
    return
}

run_4 :: proc(query : ^Query_4($a, $b, $c, $d)) -> (iterator : Iter_4(a,b,c,d), idx : int, cond : bool) #no_bounds_check{
     iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
     iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], SOAType(a), query.len)
     iterator.component_b = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.b_sparse_index], SOAType(b), query.len)
     iterator.component_c = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.c_sparse_index], SOAType(c), query.len)
     iterator.component_d = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.d_sparse_index], SOAType(d), query.len)


     if cond = query.index < query.len; cond{
        idx = query.index
        query.index += 1
    }

    //Reset
    if !cond{
        query.index = 0
    }
    return
}

query :: proc{query_1, query_2, query_3, query_4}
run :: proc{run_1, run_2, run_3, run_4}

//////////////////////////////////////////////////////////
