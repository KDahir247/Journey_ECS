# Journey_ECS
Fast Minimal Sparse set for Odin Language Which is being used in Odin-Journey.

Currently It is works, but still on heavy WIP
</br>

**Features**
- Fast single component iteration & entity iteration (Dense array iteration) 
- Unlimitied amount of component can be added
- Very minimal Only does what it needs and nothing more.
- SOA component dense array

**Future Features**
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
    defer deinit_world(world)

    Position :: struct{
        val : #simd[4]f32,
    }

    Rotation :: struct{
        val : #simd[4]f32,
    }

    Scale :: struct{
        val : #simd[4]f32,
    }

    Velocity :: struct{
        val : #simd[4]f32,
    }

    register(world, Position)
    register(world, Rotation)
    register(world, Scale)
    register(world, Velocity)
    
    entity := create_entity(world)
    entity1 := create_entity(world) 
    entity2 := create_entity(world) 
    entity3 := create_entity(world) 
    entity4 := create_entity(world)

    velocityx := Velocity{
        val = {1.0, 0.0, 0.0, 0.0},
    }

    postion_x := Position{
        val = {2.0, 0.0, 0.0, 0.0},
    }

    postion_y := Position{
        val = {0.0,3.14,0.0,0.0},
    }

    position_xy := Position{
        val = {24.0,7.11,0.0,0.0},
    }
   

    Quaternion_IDENTITY := Rotation{
        val = {0.0,0.0,0.0,1.0},
    }

    add_soa_component(world, entity2, velocityx)
    add_soa_component(world, entity1, postion_x)
    add_soa_component(world, entity2, postion_y)
    add_soa_component(world, entity2, Quaternion_IDENTITY)

    add_soa_component(world, entity, position_xy)
    add_soa_component(world, entity, Quaternion_IDENTITY)

    postion_scale_query := query(world, Velocity, Scale) //register and sort using group
    position_rotation_query := query(world, Position, Rotation) //register and sort using group

    position_rotation_query1 := query(world, Position, Rotation) //doesn't register or sort using group uses the cache result
    postion_scale_query1 := query(world, Velocity, Scale) //doesn't register or sort using group uses the cache result
    
   
     for component_storage, index in run(&position_rotation_query){
        mut_component_storage := component_storage

        if component_storage.entities[index] == 2{
            fmt.println("Moving the player entity", component_storage.entities[index] , "Right by 100" )
            mut_component_storage.component_a[index].val += {100.0, 0.0, 0.0, 0.0}
        }

        fmt.print(component_storage.entities[index], " :\t")
        fmt.print(component_storage.component_a[index], "\t")
        fmt.print(component_storage.component_b[index])
        fmt.println()
     }
     fmt.println("\n")

}

```

*Pull request, Issues, Contribution and Discussion on the design and implementation on the Journey_ECS is welcomed.*
