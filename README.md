# Journey_ECS
Fast Minimal Sparse set for Odin Language Which is being used in Odin-Journey.

Currently It is works, but still on heavy WIP
</br>

**Features**
- Fast single component iteration & entity iteration (Dense array iteration) 
- Unlimitied amount of component can be added
- Very minimal Only does what it needs and nothing more.

**Future Features**
- SOA component dense array
- Reduced memory consumption on sparse array
- Allow upto or more then 1 Million entity. Current max is 65,536 entity (requires re-implementing entity store)
- Extremely fast grouping and sub grouping iteration (eg. Entity with position and scale)

**Limitation**
- small to no checks so undefined behaviour on invalid use (so call register before adding component and make sure when removing a component entity actually has component or call has_component with a if statement)
- Slighly higher memory due to sparse array with holes 
- No shared component model out of the box (this is passed to the user and I don't see this as a limitation)


Example of code 
```odin
package main

import ecs "foldername that has this single script"

main :: proc(){

  
    world := init_world()

    

    DummyStruct :: struct{
        x : int,
        y : int, 
        z : int,
    }
    register(world, DummyStruct)


    //Not implemented yet for SOA
    //Used for fast group component query where entity contain both
    //group_registry(world, {f64, string})
    // Used for fast group component query where entity are group by each component group and entities with all the group component are first
    //subgroup_registry(world, ComponentGroup{component_ids = {f64, string}}, ComponentGroup{ component_ids = {bool} })

    entity := create_entity(world)
    entity1 := create_entity(world) 
    entity2 := create_entity(world) 
    entity3 := create_entity(world) 
    entity4 := create_entity(world)

    dummy1 := DummyStruct{
        1,2,3,
    }

    dummy2 := DummyStruct{
        4,5,6,
    }

    add_component(world, entity, dummy1)
    add_component(world, entity1, dummy2)

    remove_component(world, entity1,DummyStruct)


    dummy_soa_array, length := get_soa_components(world, SOAType(DummyStruct))


    for i in 0..< length{
        fmt.println("before",dummy_soa_array[i])
        dummy_soa_array[i].x += 1
        fmt.println("after",dummy_soa_array[i])
        fmt.println()

    }

    add_component(world,entity1,dummy2)
    deinit_world(world)
    free(world)

}

```

*Pull request, Issues, Contribution and Discussion on the design and implementation on the Journey_ECS is welcomed.*
