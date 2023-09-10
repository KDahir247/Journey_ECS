package journey

import "core:slice"
import "core:runtime"
import "core:fmt"
import "core:intrinsics"
import "core:mem"
//TODO:Khal should I take into account of user adding or removing component while iterating over it
// If so then we need to find a way to handle invalidation. One way is to iterate from last to first
// So inserting will add to the last and won't enter the iteration (since it at the end)
// and removing the currently iterate will be swap by the last which won't invalidate the iteration (removing must be done at the end after manipulation)
// Or should i pass the responsiblity to the user where user have to use  #reverse on iteration on dense.
//TODO:Khal should I take into account of user adding or removing component while iterating over it
// If so then we need to find a way to handle invalidation. One way is to iterate from last to first
// So inserting will add to the last and won't enter the iteration (since it at the end)
// and removing the currently iterate will be swap by the last which won't invalidate the iteration (removing must be done at the end after manipulation)
// Or should i pass the responsiblity to the user where user have to use  #reverse on iteration on dense.


////////////////////////////// ECS Constant /////////////////////////////

DEFAULT_CAPACITY :: 32
DEFAULT_COMPONENT_SPARSE :: 32
DEFAULT_GROUP :: 32

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

///////////////////////////////////////////////////////////////////////

////////////////////////// ECS World //////////////////////////////////

ComponentGroup :: struct{
    component_ids : []typeid,   
}

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

//This just clear all the world's resources. User is required to free world ptr
deinit_world :: proc(world : $W/^$World){
    deinit_entity_store(&world.entities_stores)
    deinit_component_store(&world.component_stores)
}

subgroup_registry :: proc(world : $W/^$World, component_groups : ..ComponentGroup){
    internal_register_sub_group(&world.component_stores, component_groups)  
}

group_registry :: proc(world : $W/^$World, component_ids : []typeid)
{
    internal_register_group(&world.component_stores,component_ids)
}

register :: proc(world : $W/^$World, $component_type : typeid){
    internal_register_component(&world.component_stores, component_type)
}

set_component :: proc(world : $W/^$World, entity : u32, component : $T){
    if intrinsics.expect(internal_entity_is_alive(&world.entities_stores, entity), true){ //TODO:khal remove for something better.
        internal_increment_version(&world.entities_stores, entity)//TODO: khal swap for something better.
        sparse_index := world.component_stores.component_info[T].sparse_index
        internal_sparse_put(&world.component_stores.component_sparse[sparse_index],entity,component)
    }
}

get_component :: proc(world : $W/^$World, entity : u32, $component_type : typeid) -> ^component_type{
    if intrinsics.expect(internal_entity_is_alive(&world.entities_stores, entity), true){ //TODO:khal remove for something better.
        internal_increment_version(&world.entities_stores, entity)//TODO: khal swap for something better.
        sparse_index := world.component_stores.component_info[component_type].sparse_index
        return internal_sparse_get(&world.component_stores.component_sparse[sparse_index], entity, component_type) 
    }

    return nil
}

has_component :: proc(world : $W/^$World, entity : u32, $component_type : typeid) -> bool{
    if intrinsics.expect(internal_entity_is_alive(&world.entities_stores, entity), true){ //TODO:khal remove for something better. 
        internal_increment_version(&world.entities_stores, entity)//TODO: khal swap for something better.
        sparse_index := world.component_stores.component_info[component_type].sparse_index
        return internal_sparse_has(&world.component_stores.component_sparse[sparse_index], entity) == 1 
    }

    return false
}

get_entities_with_component :: proc(world : $W/^$World, $component_type : typeid) -> []u32{
    sparse_index := world.component_stores.component_info[component_type].sparse_index
    return internal_sparse_fetch_entities(&world.component_stores.component_sparse[sparse_index])
} 

get_components_with_id :: proc(world : $W/^$World, $component_type : typeid) -> []component_type{
    sparse_index := world.component_stores.component_info[component_type].sparse_index
    return internal_sparse_fetch_components(&world.component_stores.component_sparse[sparse_index], component_type)
}

add_component :: proc(world : $W/^$World, entity : u32, component : $T){
    if intrinsics.expect(internal_entity_is_alive(&world.entities_stores, entity), true){ //TODO: khal remove for something better.
        internal_increment_version(&world.entities_stores, entity) //TODO: khal swap for something better.
        internal_add_component_group(&world.component_stores, entity, component)
    }
}

remove_component :: proc(world : $W/^$World, entity : u32, $component_type : typeid){
    if intrinsics.expect(internal_entity_is_alive(&world.entities_stores, entity), true){ //TODO:khal remove for something better.
        internal_increment_version(&world.entities_stores, entity)//TODO: khal swap for something better.
        internal_remove_component_group(&world.component_stores, entity, component_type)
    }
}

query :: proc(world : $W/^$World,  query_desc : ..typeid) -> []u32{
    panic("Not implemented yet!")
}

create_entity :: proc(world : $W/^$World) -> u32{
    return internal_create_entity(&world.entities_stores)
}


remove_entity :: proc(world : $W/^$World, entity : u32){
    panic("Not implemented yet!")
}

///////////////////////////////////////////////////////////////////

//////////////////////// Entity Store /////////////////////////////

EntityStore :: struct { 
    entities : [dynamic]u32,
    available_to_recycle : int,
    next_recycle : u32,
    __padding1__ : u32,
    __padding2__ : u32,
}

@(private)
init_entity_store :: proc() -> EntityStore{

    entity_store := EntityStore{
        entities = make([dynamic]u32, 0,DEFAULT_CAPACITY),
    }
    
    return entity_store
}

//GOOD
@(private)
deinit_entity_store :: proc(entity_store : $E/^$EntityStore){
    delete(entity_store.entities)
}
@(private)
internal_create_entity :: proc(entity_store : $E/^$EntityStore) -> u32{
    entity : u32 = entity_store.next_recycle

   if entity_store.available_to_recycle > 0{

    entity_store.next_recycle = entity_store.entities[entity] >> 16
    entity_store.entities[entity] = (entity + 1)  << 16
    entity_store.available_to_recycle -= 1

   }else{
    entity = u32(len(entity_store.entities)) //TODO:khal optimize

    append(&entity_store.entities, (entity + 1) << 16)
   }

   return entity
}

@(private)
internal_destroy_entity :: proc(entity_store : $E/^$EntityStore, entity : u32) #no_bounds_check{
    entity_store.entities[entity] += 1
    entity_store.available_to_recycle += 1

    entity_store.next_recycle = entity 
}

@(private)
internal_entity_is_valid :: #force_inline proc(entity_store : $E/^$EntityStore, entity : u32) -> int{
    return u32(len(entity_store.entities) -1) > entity ? 1 : 0
}

@(private)
internal_entity_is_alive :: #force_inline proc(entity_store : $E/^$EntityStore, entity : u32) -> bool #no_bounds_check{
    entity_detail := entity_store.entities[entity] 
    return(entity_detail & 0xFF) == (entity_detail >> 8) & 0xFF 
}

@(private)
internal_increment_version :: #force_inline proc(entity_store : $E/^$EntityStore, entity : u32){
    entity_store.entities[entity] += 257

    if entity_store.entities[entity] & 0xFF == 0xFF{
        entity_store.entities[entity] &= 0xFF_FF_00_00
    }
}
/////////////////////////////////////////////////////////////////

///////////////////////// ECS Group /////////////////////////////

Group :: struct{
    indices : []int,
    start : int,
    __extra__ : int, //TODO:khal this is a temp variable i will be using it later and will change the name
}

/////////////////////////////////////////////////////////////////

///////////////////// Component Store ///////////////////////////

ComponentInfo :: struct{
    group_indices : [2]int,
    sparse_index : int,
    __extra__ : int, //TODO:khal this is a temp variable i will be using it later and will change the name
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
        indices = []int{},
        start=0,
    }

    return component_store
}


@(private)
internal_register_sub_group :: proc(component_store : $C/^$ComponentStore, component_groups : []ComponentGroup){
    component_type_len := 0

    for group in component_groups{
        internal_register_group(component_store, group.component_ids)
        component_type_len += len(group.component_ids)
    }

    sub_group : Group

    sub_group_indices := make_slice([]int,component_type_len)

    sub_group_len := len(component_store.groups)
    component_group_len := len(component_groups)
    
    for group, group_index in component_groups{
        for id, index in group.component_ids{
            component_info := &component_store.component_info[id]
            component_info.group_indices[1] = sub_group_len

            sub_group_index := group_index * component_group_len + index 
            sub_group_indices[sub_group_index] = component_info.sparse_index
        }
    }

    sub_group.indices = sub_group_indices

    append(&component_store.groups, sub_group)
}

@(private)
internal_register_group :: proc(component_store : $C/^$ComponentStore, component_ids : []typeid)
{
    group : Group

    group_sparse_indices := make_slice([]int, len(component_ids))

    group_index := len(component_store.groups)

    for id, index in component_ids{
        component_info := &component_store.component_info[id]
        component_info.group_indices[0] = group_index

        group_sparse_indices[index] = component_info.sparse_index
    }

    group.indices = group_sparse_indices

    append(&component_store.groups, group)
}


@(private)
internal_register_component :: #force_inline proc(component_store : $C/^$ComponentStore, $component_type : typeid){

    if component_type not_in component_store.component_info{

        component_store.component_info[component_type] = ComponentInfo{
            sparse_index = len(component_store.component_sparse),
        }

        component_sparse := init_component_sparse(component_type)
        append(&component_store.component_sparse, component_sparse)

    }
}

@(private)
internal_add_component_group :: proc(component_store : $C/^$ComponentStore, #any_int entity : int, component : $T){
    component_info := component_store.component_info[T]
   
    group_index := component_info.group_indices[0]
    group := &component_store.groups[group_index]

    sub_group_index := component_info.group_indices[1]
    sub_group := &component_store.groups[sub_group_index]
   
    valid_group_add := normalize_value(len(group.indices) - 1)
    valid_sub_group_add := normalize_value(len(sub_group.indices) - 1)

    internal_sparse_push(&component_store.component_sparse[component_info.sparse_index], entity, component)

    //////////////////////////// Grouping //////////////////////////
     for group_sparse_id in group.indices{ //
        valid_group_add &= internal_sparse_has(&component_store.component_sparse[group_sparse_id], entity)
    }

    if valid_group_add == 1{
        for group_sparse_id in group.indices{
            group_sparse := component_store.component_sparse[group_sparse_id]

            if internal_sparse_get_index(&group_sparse, entity) > group.start{
                group_start_entity := internal_sparse_index_entity(&group_sparse, group.start)
                internal_sparse_swap(&group_sparse, group_start_entity, entity)
            }
        }
    }
    
    group.start += valid_group_add
    ////////////////////////////////////////////////////////////////

    //////////////////////// Sub Grouping //////////////////////////
    for group_id in sub_group.indices{
        for group_sparse_id in component_store.groups[group_id].indices{
            valid_sub_group_add &= internal_sparse_has(&component_store.component_sparse[group_sparse_id], entity)
        }
    }

    if valid_sub_group_add == 1{
        for group_id in sub_group.indices{
            group_sparse_indices := component_store.groups[group_id].indices
            
            for group_sparse_id in group_sparse_indices{
                group_sparse := component_store.component_sparse[group_sparse_id]
    
                if internal_sparse_get_index(&group_sparse, entity) > sub_group.start{
                    group_sub_start_entity := internal_sparse_index_entity(&group_sparse, sub_group.start)
        
                    internal_sparse_swap(&group_sparse, group_sub_start_entity, entity)
                }
            }
        }
    }

    sub_group.start += valid_sub_group_add

    /////////////////////////////////////////////////////////////
}

@(private)
internal_remove_component_group :: proc(component_store : $C/^$ComponentStore, #any_int entity : int, $component_type : typeid){
    component_info := component_store.component_info[component_type]

    group_index := component_info.group_indices[0]
    group := &component_store.groups[group_index]


    sub_group_index := component_info.group_indices[1]
    sub_group := &component_store.groups[sub_group_index]

    valid_group_remove := normalize_value(len(group.indices) - 1)
    valid_sub_group_remove := normalize_value(len(sub_group.indices) - 1)
    ////////////////////// Sub Grouping //////////////////////////

    for group_id in sub_group.indices{
        group_sparse_indices := component_store.groups[group_id].indices

        for group_sparse_id in group_sparse_indices{
            valid_sub_group_remove &= internal_sparse_has(&component_store.component_sparse[group_sparse_id], entity)
        }
    }

    if valid_sub_group_remove == 1{
         for group_id in sub_group.indices{
            group_sparse_indices := component_store.groups[group_id].indices
            for group_sparse_id in group_sparse_indices{
                group_sparse := component_store.component_sparse[group_sparse_id]
                swap_entity := internal_sparse_index_entity(&group_sparse, sub_group.start - 1)
            
                internal_sparse_swap(&group_sparse, swap_entity, entity)
            }
        }
    }

    sub_group.start -= valid_sub_group_remove
    //////////////////////////////////////////////////////////////

    ////////////////////////// Grouping /////////////////////////
    for group_sparse_id in group.indices{
        valid_group_remove &= internal_sparse_has(&component_store.component_sparse[group_sparse_id], entity)
    }
    
    if valid_group_remove == 1{
        for group_sparse_id in group.indices{
            group_sparse := component_store.component_sparse[group_sparse_id] 
    
            swap_entity := internal_sparse_index_entity(&group_sparse, group.start - 1)
            internal_sparse_swap(&group_sparse, swap_entity, entity)
        }
    }

    group.start -= valid_group_remove
    ////////////////////////////////////////////////////////////
    internal_sparse_remove(&component_store.component_sparse[component_info.sparse_index],entity,component_type)
}


@(private)
deinit_component_store :: proc(component_store : $C/^$ComponentStore){
    //TODO: optimize if possible it has a high cpi even on optimization


    for comp in component_store.component_sparse{
        deinit_component_sparse(comp)
    }

    delete(component_store.component_sparse)

    delete(component_store.component_info)

    for group in component_store.groups{ 
        delete(group.indices)
    }

    delete(component_store.groups)
}

//////////////////////// Sparse Set //////////////////////////
ComponentSparse :: struct { 
    component_blob : ^rawptr, 
    entity_blob : rawptr,
    sparse_blob : rawptr, 
    len : u32,  
    cap : u32,
    component_size : int, 
}

@(private)
init_component_sparse :: proc($type : typeid) -> ComponentSparse{

    @(static) component_soa_dense :#soa [dynamic]type
    component_soa_blob := (^rawptr)(&component_soa_dense)

    //component_blob,_ := mem.alloc(size_of(type) * DEFAULT_COMPONENT_SPARSE)
    entity_blob,_ := mem.alloc(DEFAULT_COMPONENT_SPARSE << 2)
    sparse_blob,_ := mem.alloc(524280)//TODO: khal high allocation maybe do pagination. Look at the pro, cons, and design....

    return ComponentSparse{
        sparse_blob = sparse_blob,
        entity_blob = entity_blob,
        component_blob = component_soa_blob,
        len = 0,
        cap = DEFAULT_COMPONENT_SPARSE,
        component_size = size_of(type),
    }
}

deinit_component_sparse :: proc(component_sparse :ComponentSparse ){
    raw_soa := component_sparse.component_blob

    free(raw_soa^)
    free(component_sparse.entity_blob)
    free(component_sparse.sparse_blob)

}

@(private)
internal_sparse_get_index :: #force_inline proc(component_sparse : $S/^$ComponentSparse, entity : int) -> int{
    return ([^]int)(component_sparse.sparse_blob)[entity] - 1
}

@(private)
internal_sparse_put_index :: #force_inline proc(component_sparse : $S/^$ComponentSparse, entity : int,  value : int) {
    sparse_ptr : ^int = ([^]int)(component_sparse.sparse_blob)[entity:]
    sparse_ptr^ = value
}

// @(private)
// internal_sparse_resize :: proc(component_sparse : $S/^$ComponentSparse){
//     capacity := int(component_sparse.cap) 

//     component_sparse.entity_blob,_ = mem.resize(component_sparse.entity_blob, capacity, capacity + capacity)
//     component_sparse.component_blob,_ = mem.resize(component_sparse.component_blob, capacity,  capacity + capacity)
//     component_sparse.cap += component_sparse.cap
// }

@(private)
internal_sparse_push :: proc(component_sparse : $S/^$ComponentSparse, entity : int, component : $T) #no_bounds_check{
    // if component_sparse.cap == component_sparse.len{
    //     internal_sparse_resize(component_sparse)
    // }

    soa_component := cast(^#soa[dynamic]T)(component_sparse.component_blob)

    sparse_len := int(component_sparse.len)
    component_sparse.len += 1

    append_soa_elem(soa_component, component)
    fmt.println(soa_component^)

    // comp_ptr :^T= ([^]T)(component_sparse.component_blob)[sparse_len:]
    // comp_ptr^ = component

    ent_ptr :^u32= ([^]u32)(component_sparse.entity_blob)[sparse_len:]
    ent_ptr^ = u32(entity)

    sparse_ptr : ^int = ([^]int)(component_sparse.sparse_blob)[entity:]
    sparse_ptr^ = int(component_sparse.len)
}

@(private)
internal_sparse_get :: proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int, $component_type : typeid) -> ^component_type {
    dense_index := internal_sparse_get_index(component_sparse, entity)
    return &([^]component_type)(component_sparse.component_blob)[dense_index]
}

@(private)
internal_sparse_index_component :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : int ,$component_type : typeid) -> component_type{
    return ([^]component_type)(component_sparse.component_blob)[index]
} 

@(private)
internal_sparse_index_entity :: #force_inline proc(component_sparse : $S/^$ComponentSparse, index : int) -> u32{
    return ([^]u32)(component_sparse.entity_blob)[index] 
}

@(private)
internal_sparse_put :: proc(component_sparse : $S/^$ComponentSparse,#any_int entity : int, component : $T) {
    dense_id := internal_sparse_get_index(component_sparse, entity)
    comp_ptr :^T= ([^]T)(component_sparse.component_blob)[dense_id:] 
    comp_ptr^ = component
}


@(private)
internal_sparse_remove :: proc(component_sparse : $S/^$ComponentSparse, entity : int, $component_type : typeid){
    component_sparse.len -= 1
    
    dense_id := internal_sparse_get_index(component_sparse, entity)

    last_comp_ptr := ([^]component_type)(component_sparse.component_blob)[component_sparse.len]
    removed_comp_ptr :^component_type= ([^]component_type)(component_sparse.component_blob)[dense_id:]
    removed_comp_ptr^ = last_comp_ptr

    last_entity := ([^]u32)(component_sparse.entity_blob)[component_sparse.len]
    ent_ptr :^u32 = ([^]u32)(component_sparse.entity_blob)[dense_id:]
    ent_ptr^ = last_entity

    sparse_ptr : ^int = ([^]int)(component_sparse.sparse_blob)[last_entity:]
    sparse_ptr^ = dense_id
    
    internal_sparse_put_index(component_sparse, entity, 0)
}

@(private)
internal_sparse_has :: #force_inline proc(component_sparse : $S/^$ComponentSparse, #any_int entity : int) -> int{
    sparse_val := internal_sparse_get_index(component_sparse, entity) 
    return 1 - (sparse_val >> 31) & 1
}

@(private)
internal_sparse_fetch_components :: #force_inline proc(component_sparse : $S/^$ComponentSparse, $component_type : typeid) -> []component_type{
    return ([^]component_type)(component_sparse.component_blob)[:component_sparse.len]
}

@(private)
internal_sparse_fetch_entities :: #force_inline proc(component_sparse : $S/^$ComponentSparse) -> []u32 {
    return ([^]u32)(component_sparse.entity_blob)[:component_sparse.len]
}

@(private)
internal_sparse_swap :: proc(component_sparse : $S/^$ComponentSparse, #any_int dst_entity, src_entity : int) #no_bounds_check{
    slice.ptr_swap_non_overlapping(([^]int)(component_sparse.sparse_blob)[dst_entity:], ([^]int)(component_sparse.sparse_blob)[src_entity:], size_of(int))

    dst_id := internal_sparse_get_index(component_sparse, dst_entity)
    src_id := internal_sparse_get_index(component_sparse, src_entity)
    
    dst_index := dst_id * component_sparse.component_size
    src_index := src_id * component_sparse.component_size

    slice.ptr_swap_non_overlapping(rawptr(uintptr(component_sparse.component_blob) + uintptr(dst_index)), rawptr(uintptr(component_sparse.component_blob) + uintptr(src_index)), component_sparse.component_size) 
    slice.ptr_swap_non_overlapping(([^]u32)(component_sparse.entity_blob)[dst_id:], ([^]u32)(component_sparse.entity_blob)[src_id:], size_of(u32))
}

///////////////////////////////////////////////////////////

///////////////////////// Systems /////////////////////////



//////////////////////////////////////////////////////////
