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
- Removing component or adding component on the of the same component type dense array that we are iterating will cause invalidation and problem.
- No hierarchy model out of the box (this is passed to the user and I don't see this as a limitiation) 

Example of code 
```odin
package main

import ecs "foldername that has this single script"

main :: proc(){

    world := init_world()
 
    register(world, string)
    register(world, f64)
    register(world, bool)

    
    //Used for fast group component query where entity contain both
    //group_registry(world, {f64, string})
    // Used for fast group component query where entity are group by each component group and entities with all the group component are first
    //subgroup_registry(world, ComponentGroup{component_ids = {f64, string}}, ComponentGroup{ component_ids = {bool} })

    entity := create_entity(world)
    entity1 := create_entity(world) 
    entity2 := create_entity(world) 
    entity3 := create_entity(world) 
    entity4 := create_entity(world) // 4

    add_component(world, entity1, 3.3)
    add_component(world, entity, 5.4)
    add_component(world, entity3, 2.4)

    add_component(world,entity4, "Who")
    add_component(world,entity1, "Hello")
    add_component(world,entity, "Bob")
    add_component(world, entity3, "NOOo")
    
    add_component(world, entity4, true)
    add_component(world, entity, true)
    add_component(world, entity1, true)

    //Get component allow you to modify the data since it a ptr to the data
    //Or you can call set component
    set_component(world, entity, 200.0)
    fmt.println(get_component(world, entity, f64)^)
    fmt.println(has_component(world, entity1, string))

    remove_component(world, entity, string )

    // Bob is removed so when we call these two function 0 is removed from the entity dense array
    // Bob is removed from the component dense array in the sparse set
    // and the sparse array at entity is reset. 
    fmt.println(get_entities_with_component(world, string))
    fmt.println(get_components_with_id(world, string))

   for float_component in get_components_with_id(world, f64){
    // logic
   }


   for float_entity in get_entities_with_component(world, f64){
    // logic
   }

   // Fast System iteration is a working progress currently for Group and Subgroup
   

	deinit_world(world)
    free(world)

}

```

*Pull request, Issues, Contribution and Discussion on the design and implementation on the Journey_ECS is welcomed.*
