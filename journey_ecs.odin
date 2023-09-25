package journey

import "core:slice"
import "core:intrinsics"
import "core:mem"

////////////////////////////// ECS Constant /////////////////////////////

DEFAULT_CAPACITY :: 32
DEFAULT_COMPONENT_SPARSE :: 32
DEFAULT_GROUP :: 32
DEFAULT_ENTITY_PAGE :: 64
////////////////////////////// ECS Resource ////////////////////////////
Resources :: struct{
    //
}

SOAType :: struct($T : typeid){}
////////////////////////////////////////////////////////////////////////

////////////////////////////// ECS Utility /////////////////////////////

//return 0 for all negative and 1 for all postive and zero.
@(private)
normalize_value :: #force_inline proc "contextless" (val : int) -> int{
    return (val >> 63) + 1 //arithemtic shift
}

///////////////////////////////////////////////////////////////////////

////////////////////////// ECS World //////////////////////////////////
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
    // will be negative and array index cant be negative so it will terminate code if entity is invalid
    sparse_index := world.component_stores.component_info[E].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity) 
    internal_sparse_put(&world.component_stores.component_sparse[sparse_index],entity,component)
}

get_soa_component :: proc(world : $W/^$World, entity : uint, $component_type : typeid) -> component_type
where intrinsics.type_is_struct(component_type){
    // will be negative and array index cant be negative so it will terminate code if entity is invalid
    sparse_index := world.component_stores.component_info[component_type].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity) 
    return internal_sparse_get(&world.component_stores.component_sparse[sparse_index], entity, component_type) 
}

has_soa_component :: proc(world : $W/^$World, entity : uint, $component_type : typeid) -> bool
where intrinsics.type_is_struct(component_type){
    // will be negative and array index cant be negative so it will terminate code if entity is invalid
    sparse_index := world.component_stores.component_info[component_type].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity) 
    return internal_sparse_has(&world.component_stores.component_sparse[sparse_index], entity) == 1 

}

add_soa_component :: proc(world : $W/^$World, entity : uint, component : $E)
where intrinsics.type_is_struct(E){ 
    // will be negative and array index cant be negative so it will terminate code if entity is invalid
    sparse_index := world.component_stores.component_info[E].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity)
    internal_sparse_push(&world.component_stores.component_sparse[sparse_index], entity, component)
   
}

remove_soa_component :: proc(world : $W/^$World, entity : uint, $component_type : typeid)
where intrinsics.type_is_struct(component_type){
    // will be negative and array index cant be negative so it will terminate code if entity is invalid
    sparse_index := world.component_stores.component_info[component_type].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity)
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
    sparse_index := world.component_stores.component_info[component_type].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity)
    return internal_sparse_fetch_entities(&world.component_stores.component_sparse[sparse_index])
} 


get_soa_components :: proc(world : $W/^$World, $component_type : typeid/SOAType($E)) -> (soa_slice :# soa[]E, length : int) 
where intrinsics.type_is_struct(E){
    sparse_index := world.component_stores.component_info[E].sparse_index * internal_entity_valid_mask(&world.entities_stores, entity)
    soa_slice, length = internal_sparse_fetch_components(& world.component_stores.component_sparse[sparse_index], component_type)
    return
}


create_entity :: proc(world : $W/^$World) -> uint{
    return internal_create_entity(&world.entities_stores)
}


remove_entity :: proc(world : $W/^$World, entity : u32){
    unimplemented("TODO: khal implement this")
}

///////////////////////////////////////////////////////////////////

//////////////////////// Entity Store /////////////////////////////

EntityStore :: struct { 
     entities : [dynamic]int,
     current_page : uint,
    // entities : [dynamic]u32,
    // available_to_recycle : int,
    // next_recycle : u32,
    // __padding1__ : u32,
    // __padding2__ : u32,
}

@(private)
init_entity_store :: proc() -> EntityStore{

    entity_store := EntityStore{
        entities = make([dynamic]int, 1,DEFAULT_CAPACITY),
        current_page = 0,
    }
    
    return entity_store
}

//GOOD
@(private)
deinit_entity_store :: proc(entity_store : $E/^$EntityStore){
    delete(entity_store.entities)
}

@(private)
internal_create_entity :: proc(entity_store : $E/^$EntityStore) -> uint{
    if entity_store.entities[entity_store.current_page] == -1{
        append(&entity_store.entities, 0)
        entity_store.current_page += 1
    }
    
    offset := entity_store.current_page * DEFAULT_ENTITY_PAGE
        
    entity_bits := entity_store.entities[entity_store.current_page]

    entity_id :uint= uint(64 - intrinsics.count_leading_zeros(entity_bits))

    entity_store.entities[entity_store.current_page] |= 1 << entity_id

    return entity_id + offset
}

@(private)
internal_entity_valid_mask :: #force_inline proc(entity_store : $E/^$EntityStore, entity : uint) -> int{
    target_page := entity >> 6

    mask := (entity_store.entities[target_page] >> entity) & 1
    remap_mask := (mask << 1) - 1

    return remap_mask
}

// @(private)
// internal_create_entity :: proc(entity_store : $E/^$EntityStore) -> u32{
//     entity : u32 = entity_store.next_recycle

//    if entity_store.available_to_recycle > 0{

//     entity_store.next_recycle = entity_store.entities[entity] >> 16
//     entity_store.entities[entity] = (entity + 1)  << 16
//     entity_store.available_to_recycle -= 1

//    }else{
//     entity = u32(len(entity_store.entities)) //TODO:khal optimize

//     append(&entity_store.entities, (entity + 1) << 16)
//    }

//    return entity
// }

// @(private)
// internal_destroy_entity :: proc(entity_store : $E/^$EntityStore, entity : u32) #no_bounds_check{
//     entity_store.entities[entity] += 1
//     entity_store.available_to_recycle += 1

//     entity_store.next_recycle = entity 
// }


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
    group_indices : [2]int,
    sparse_index : int,
}

ComponentStore :: struct{
    component_sparse : [dynamic]ComponentSparse, 
    component_info : map[typeid]ComponentInfo, 
    groups : [dynamic]Group,
}

@(private)
init_component_store :: proc() -> ComponentStore #no_bounds_check{
    component_store := ComponentStore{
        component_sparse = make([dynamic]ComponentSparse, 0, DEFAULT_CAPACITY),
        component_info = make_map(map[typeid]ComponentInfo, DEFAULT_CAPACITY),
        groups = make([dynamic]Group, 1, DEFAULT_GROUP),
    }

    component_store.groups[0] = Group{
        start=0,
        count=-1,
    }

    return component_store
}

@(private)
internal_register_group :: proc(component_store : $C/^$ComponentStore,$group_type :Group_Type, structure_types : []typeid, index : int) -> int #no_bounds_check{
    group : Group

    //if mask index is -1 it mean that there is so free up space so we need to append, thus getting the length of the groups collection
    mask_index := normalize_value(index)

    groups_length := len(component_store.groups)

    invert_mask_index := 1 - mask_index
    target_group_index := 1 + index // if it is zero then we know that we need to append a new group since there is no freed space in the groups collection

    group_index := (groups_length * invert_mask_index) + (target_group_index * mask_index)

    for id in structure_types{
        component_info := &component_store.component_info[id]
        component_info.group_indices[group_type] = group_index
    }
    
    if mask_index == 0{
        append(&component_store.groups, group)
    }else{
        component_store.groups[group_index] = group
    }

    return group_index
}

@(private)
internal_unregister_group :: proc(component_store : $C/^$ComponentStore, $group_type :Group_Type, structure_types : []typeid)  -> int #no_bounds_check{
    
    target_group_index := 0

    for id in structure_types{
        component_info := component_store.component_info[id]
        group_index := component_info.group_indices[group_type]
        component_store.groups[group_index].start = 0
        component_store.groups[group_index].count = 0

        target_group_index = max(target_group_index, group_index)
    }


    return target_group_index - 1
}

//TODO:khal maybe add internal_register_component_bulk to allow multiple component registerd. Look at the odin lang https://github.com/odin-lang/Odin/blob/master/core/runtime/core_builtin.odin#L410 for reference.
// Also should we remove check (component_type not_in component_store.component_info)?
@(private)
internal_register_component :: #force_inline proc(component_store : $C/^$ComponentStore, $component_type : typeid) #no_bounds_check{
    if component_type not_in component_store.component_info{

        component_store.component_info[component_type] = ComponentInfo{
            sparse_index = len(component_store.component_sparse),
        }

        component_sparse := init_component_sparse(component_type)
        append(&component_store.component_sparse, component_sparse)

    }
}


@(private)
deinit_component_store :: proc(component_store : $C/^$ComponentStore){
    for comp in component_store.component_sparse{
        deinit_component_sparse(comp)
    }

    delete(component_store.component_sparse)

    delete(component_store.component_info)

    delete(component_store.groups)
}

//////////////////////// Sparse Set //////////////////////////
ComponentSparse :: struct { 
    component_blob : rawptr, 
    entity_blob : rawptr,
    sparse_blob : rawptr, 
    len : int, 
    modification_count : int, 
}

@(private)
init_component_sparse :: proc($type : typeid) -> ComponentSparse{

    component_soa_dense :^#soa [dynamic]type = new(#soa[dynamic]type) 
    reserve_soa(component_soa_dense, DEFAULT_COMPONENT_SPARSE)
    
    component_soa_blob := (^rawptr)(component_soa_dense)

    entity_blob,_ := mem.alloc(DEFAULT_COMPONENT_SPARSE << 2)
    sparse_blob,_ := mem.alloc(524280)//TODO: khal high allocation maybe do pagination. Look at the pro, cons, and design....

    return ComponentSparse{
        sparse_blob = sparse_blob,
        entity_blob = entity_blob,
        component_blob = component_soa_blob,
        len = 0,
        modification_count = 0,
    }
}

deinit_component_sparse :: proc(component_sparse :ComponentSparse){

    mem.free_with_size(component_sparse.sparse_blob,524280)
    //entity_blob contains u32
    mem.free(component_sparse.entity_blob)

    free((^rawptr)(component_sparse.component_blob)^)
    free(component_sparse.component_blob)
}

//Used by the query system 
@(private)
internal_component_sparse_mod_zeroed  :: #force_inline proc(component_sparse : $S/^$ComponentSparse){
    component_sparse.modification_count = 0
}

@(private)
internal_sparse_get_index :: #force_inline proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int) -> int{
    return ([^]int)(component_sparse.sparse_blob)[entity] - 1
}

@(private)
internal_sparse_put_index :: #force_inline proc(component_sparse : $S/^$ComponentSparse, entity : int,  value : int) {
    sparse_ptr : ^int = ([^]int)(component_sparse.sparse_blob)[entity:]
    sparse_ptr^ = value
}

@(private)
internal_sparse_resize :: proc(component_sparse : $S/^$ComponentSparse, capacity : int){
    new_capacity := (capacity << 1) + 8
    // size of u32 is 4, so 1 << 2 == 1 * 4
    component_sparse.entity_blob,_ = mem.resize(component_sparse.entity_blob, capacity << 2, new_capacity << 2, 4)
}

//TODO:khal maybe add internal_sparse_push_bulk to allow multiple entities and component add together. Look at the odin lang https://github.com/odin-lang/Odin/blob/master/core/runtime/core_builtin.odin#L410 for reference.


@(private)
internal_sparse_push :: proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int, component : $T) #no_bounds_check
{
    next_sparse_len := component_sparse.len + 1
    soa_component_array := cast(^#soa[dynamic]T)(component_sparse.component_blob)
    soa_capacity := cap(soa_component_array)

    if soa_capacity <= next_sparse_len{
        internal_sparse_resize(component_sparse, soa_capacity)
    }

    append_soa_elem(soa_component_array, component)

    entity_data := ([^]u32)(component_sparse.entity_blob)
    entity_data[component_sparse.len] = u32(entity)
    component_sparse.len = next_sparse_len

    sparse_ptr : ^int = ([^]int)(component_sparse.sparse_blob)[entity:]
    sparse_ptr^ = next_sparse_len

    component_sparse.modification_count += 1
}

@(private)
internal_sparse_get :: proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int, $component_type : typeid) ->  component_type #no_bounds_check {
    dense_index := internal_sparse_get_index(component_sparse, entity)
    return (cast(^#soa[dynamic]component_type)(component_sparse.component_blob))[dense_index]
}

@(private)
internal_sparse_index_component :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : int ,$component_type : typeid) -> component_type{
    return (cast(^#soa[dynamic]component_type)(component_sparse.component_blob))[index]
} 

@(private)
internal_sparse_index_entity :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : int) -> u32{
    return ([^]u32)(component_sparse.entity_blob)[index] 
}

@(private)
internal_sparse_put :: proc(component_sparse : $S/^$ComponentSparse,#any_int entity : int, component : $T) {
    dense_id := internal_sparse_get_index(component_sparse, entity)
    soa_component_array := cast(^#soa[dynamic]T)(component_sparse.component_blob)
    soa_component_array[dense_id] = component
}


@(private)
internal_sparse_remove :: proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int, $component_type : typeid) #no_bounds_check{
    dense_id := internal_sparse_get_index(component_sparse, entity)

    soa_component_array := cast(^#soa[dynamic]component_type)(component_sparse.component_blob)
    raw_footer := raw_soa_footer_dynamic_array(soa_component_array)
    
    raw_footer.len -= 1
    component_sparse.len -= 1

    last_component := soa_component_array[raw_footer.len]
    soa_component_array[dense_id] = last_component
    
    last_entity := ([^]u32)(component_sparse.entity_blob)[component_sparse.len]
    ent_ptr :^u32 = ([^]u32)(component_sparse.entity_blob)[dense_id:]
    ent_ptr^ = last_entity

    sparse_ptr : ^int = ([^]int)(component_sparse.sparse_blob)[last_entity:]
    sparse_ptr^ = dense_id + 1
    
    internal_sparse_put_index(component_sparse, entity, 0)

    component_sparse.modification_count += 1
}

//GOOD
@(private)
internal_sparse_has :: #force_inline proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int) -> int{
    sparse_val := internal_sparse_get_index(component_sparse, entity) 
    return 1 - (sparse_val >> 31) & 1
}

@(private)
internal_sparse_fetch_components :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid/SOAType($E)) -> (soa_slice :#soa[]E, length: int){
    soa_slice = (cast(^#soa[]E)(component_sparse.component_blob))^
    length = component_sparse.len
    return
}

@(private)
internal_sparse_fetch_component_upto :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid/SOAType($E), len : int) -> (soa_slice :#soa[]E){
    soa_slice = (cast(^#soa[]E)(component_sparse.component_blob))^
    soa_slice = soa_slice[:len]
    return
}

@(private)
internal_sparse_fetch_entities :: #force_inline proc(component_sparse : $S/^$ComponentSparse) -> []u32 {
    return ([^]u32)(component_sparse.entity_blob)[:component_sparse.len]
}

@(private)
internal_sparse_fetch_entities_upto :: #force_inline proc(component_sparse : $S/^$ComponentSparse, len : int) -> []u32 {
    return ([^]u32)(component_sparse.entity_blob)[:len]
}

@(private)
internal_sparse_swap :: #force_inline proc(component_sparse : $S/^$ComponentSparse, #any_int dst_entity, src_entity : int, $component_type : typeid, neg_mask : int = -1) #no_bounds_check{
    soa_component_slice := (cast(^#soa[]component_type)(component_sparse.component_blob))
    sparse_slice := ([^]int)(component_sparse.sparse_blob)
    entity_slice := ([^]u32)(component_sparse.entity_blob)

    dst_id := internal_sparse_get_index(component_sparse, dst_entity) 
    src_id := internal_sparse_get_index(component_sparse, src_entity)
    
    target_src_entity := src_entity & neg_mask
    target_dst_entity := dst_entity & neg_mask
    target_src_id := src_id & neg_mask
    target_dst_id := dst_id & neg_mask

    sparse_slice[target_dst_entity], sparse_slice[target_src_entity] = sparse_slice[target_src_entity], sparse_slice[target_dst_entity]

    //It would be nice if we can use ptr_swap_non_overlapping, but not sure how that will work with a soa slice, so this will do.
    soa_component_slice[target_dst_id], soa_component_slice[target_src_id] = soa_component_slice[target_src_id], soa_component_slice[target_dst_id]
    entity_slice[target_dst_id], entity_slice[target_src_id] = entity_slice[target_src_id], entity_slice[target_dst_id]
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
    entities : []u32,
    component_a : #soa[]a,  
}

Iter_2 :: struct($a : typeid, $b : typeid){
    entities : []u32,
    component_a : #soa[]a,
    component_b : #soa[]b, 
}

Iter_3 :: struct($a : typeid, $b : typeid, $c : typeid){
    entities : []u32,
    component_a : #soa[]a,
    component_b : #soa[]b,
    component_c : #soa[]c,
}

Iter_4 :: struct($a : typeid, $b : typeid, $c : typeid, $d : typeid){
    entities : []u32,
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
        len = sparse_set_a.len,

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

        //Group_a and Group_b indicies are shared so we can use either.
        group_index := component_info_a.group_indices[0]

        //Creating and re-grouping components.
        if world.component_stores.groups[group_index].count != group_count_target{
            removed_group_index := internal_unregister_group(&world.component_stores,Group_Type.Group, {a,b})
            group_index = internal_register_group(&world.component_stores,Group_Type.Group, {a,b}, removed_group_index)

            total_modification_count += 1
        }

        
        group := &world.component_stores.groups[group_index]
        group.count = group_count_target
   
        if total_modification_count > 0{
            group.start = 0
  
            entities_a := internal_sparse_fetch_entities(&sparse_set_a)
            entities_b := internal_sparse_fetch_entities(&sparse_set_b)
            minimum_entites := len(entities_a) < len(entities_b) ? entities_a : entities_b 

            for len(minimum_entites) > 0{
                target_chunk_size := min(len(minimum_entites), chunk_size)
                current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
                minimum_entites = next_entity_chunks

                //Potential Hot Path
                for entity in current_entity_chunks{
                    group_start_entity_a := entities_a[group.start]

                    group_start_entity_b := entities_b[group.start]

                    sparse_index_a := internal_sparse_get_index(&sparse_set_a, entity)
                    sparse_index_b := internal_sparse_get_index(&sparse_set_b, entity)

                    is_valid := normalize_value(sparse_index_a | sparse_index_b)
                    a_mask := -normalize_value((sparse_index_a & -is_valid) +(-1 - group.start))
                    b_mask := -normalize_value((sparse_index_b & -is_valid) +(-1 - group.start))
               
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

        component_info_a := world.component_stores.component_info[a]
        component_info_b := world.component_stores.component_info[b]
        component_info_c := world.component_stores.component_info[c]

        defer internal_component_sparse_mod_zeroed(&world.component_stores.component_sparse[component_info_c.sparse_index])

        target_count := component_info_a.sparse_index + component_info_b.sparse_index + component_info_c.sparse_index

        sparse_set_a := world.component_stores.component_sparse[component_info_a.sparse_index]
        sparse_set_b := world.component_stores.component_sparse[component_info_b.sparse_index]

        sparse_set_c := world.component_stores.component_sparse[component_info_c.sparse_index]

        total_modification_count := sparse_set_a.modification_count + sparse_set_b.modification_count + sparse_set_c.modification_count 

        sub_group_index := component_info_c.group_indices[1]

        if world.component_stores.groups[sub_group_index].count != target_count{
            removed_sub_group_index := internal_unregister_group(&world.component_stores, Group_Type.SubGroup, {a,b,c})
            sub_group_index = internal_register_group(&world.component_stores, Group_Type.SubGroup,{a,b,c},removed_sub_group_index)

            total_modification_count += 1
        } 
        
        sub_group := &world.component_stores.groups[sub_group_index]
        sub_group.count = target_count

        if total_modification_count > 0 && ab_query.len > 0{
            sub_group.start = 0

            entities_a := internal_sparse_fetch_entities_upto(&sparse_set_a, ab_query.len)
            entities_b := internal_sparse_fetch_entities_upto(&sparse_set_b, ab_query.len)
            
            entities_c := internal_sparse_fetch_entities(&sparse_set_c)
            minimum_entites := len(entities_c) < len(entities_a) ? entities_c : entities_a

            for len(minimum_entites) > 0{
                target_chunk_size := min(len(minimum_entites), chunk_size)
                current_entity_chunks, next_entity_chunks := slice.split_at(minimum_entites, target_chunk_size)
                minimum_entites = next_entity_chunks
                
                for entity in current_entity_chunks{

                    sparse_group_index := internal_sparse_get_index(&sparse_set_a, entity)
                    sparse_sub_group_index := internal_sparse_get_index(&sparse_set_c,entity)
                    //we negate it since sparse swap mask represent -1 as true and 0 as false
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

        component_info_a := world.component_stores.component_info[a]
        component_info_b := world.component_stores.component_info[b]

        component_info_c := world.component_stores.component_info[c]
        component_info_d := world.component_stores.component_info[d]

        target_count := (component_info_a.sparse_index + component_info_b.sparse_index) + (component_info_c.sparse_index + component_info_d.sparse_index)

        sparse_set_a := world.component_stores.component_sparse[component_info_a.sparse_index]
        sparse_set_b := world.component_stores.component_sparse[component_info_b.sparse_index]

        sparse_set_c := world.component_stores.component_sparse[component_info_c.sparse_index]
        sparse_set_d := world.component_stores.component_sparse[component_info_d.sparse_index]

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
                    group_a_sparse_index := internal_sparse_get_index(&sparse_set_a, entity)
                    group_b_sparse_index := internal_sparse_get_index(&sparse_set_c, entity)

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
    
    //TODO:khal optimize this so we only have to get the component dense and entity dense just once
    iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
    iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index],SOAType(a), query.len)

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
    //TODO:khal optimize this so we only have to get the component dense and entity dense just once
    iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
    iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index],SOAType(a), query.len)
    iterator.component_b = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.b_sparse_index],SOAType(b), query.len)

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
    //TODO:khal optimize this so we only have to get the component dense and entity dense just once
    iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
    iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index],SOAType(a), query.len)
    iterator.component_b = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.b_sparse_index],SOAType(b), query.len)
    iterator.component_c = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.c_sparse_index],SOAType(c), query.len)


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
     //TODO:khal optimize this so we only have to get the component dense and entity dense just once
     iterator.entities = internal_sparse_fetch_entities_upto(&query.world.component_stores.component_sparse[query.a_sparse_index], query.len)
     iterator.component_a = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.a_sparse_index],SOAType(a), query.len)
     iterator.component_b = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.b_sparse_index],SOAType(b), query.len)
     iterator.component_c = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.c_sparse_index],SOAType(c), query.len)
     iterator.component_d = internal_sparse_fetch_component_upto(&query.world.component_stores.component_sparse[query.d_sparse_index],SOAType(d), query.len)


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
