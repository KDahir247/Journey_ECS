# Journey_ECS
Fast Minimal Sparse set for Odin Language Which is being used in Odin-Journey.

Currently It is works, but still on heavy WIP
</br>

**Features**
- Fast single component iteration & entity iteration (Dense array iteration) 
- Unlimitied amount of component can be added
- Very minimal Only does what it needs and nothing more.
- SOA component dense array
- Extremely fast grouping and sub grouping query (eg. Entity with position and scale)
- Reduced memory consumption on sparse array
- Allow upto or more then 1 Million entity


**Limitation**
- small to no checks so undefined behaviour on invalid use (so call register before adding component and make sure when removing a component entity actually has component or call has_component with a if statement)
- Slighly higher memory due to sparse array with holes (mitigated my using pagination)
- No shared component model out of the box (this is passed to the user and I don't see this as a limitation)
- Iterate from first to last, so adding or removing component while iterating may cause invalidation (user can easily add a run function that iterate last to first to resolve this), so adding and removing component should happen outside iterating loop of the same component/s.

**Working Progress**
- Optimization and profile code solution
- Implement Resource for unique component (1 per unique component)
- Implement Bulk function (eg. bulk register component, bulk add component)
- Writing Test
- Simd Iteration, since the data layout is already optimal for SIMD

Example of code 
```odin
package main

import ecs "foldername that has this single script"

main :: proc(){

    world := ecs.init_world()
    defer ecs.deinit_world(world)

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

    ecs.register(world, Position)
    ecs.register(world, Rotation)
    ecs.register(world, Scale)
    ecs.register(world, Velocity)
    
    entity := ecs.create_entity(world)
    entity1 := ecs.create_entity(world) 
    entity2 := ecs.create_entity(world) 
    entity3 := ecs.create_entity(world) 
    entity4 := ecs.create_entity(world)

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

    ecs.add_soa_component(world, entity2, velocityx)
    ecs.add_soa_component(world, entity1, postion_x)
    ecs.add_soa_component(world, entity2, postion_y)
    ecs.add_soa_component(world, entity2, Quaternion_IDENTITY)

    ecs.add_soa_component(world, entity, position_xy)
    ecs.add_soa_component(world, entity, Quaternion_IDENTITY)

    postion_scale_query := ecs.query(world, Velocity, Scale,4) //register and sort using group
    position_rotation_query := ecs.query(world, Position, Rotation,4) //register and sort using group

    position_rotation_query1 := ecs.query(world, Position, Rotation,4) //doesn't register or sort using group uses the cache result
    postion_scale_query1 := ecs.query(world, Velocity, Scale,4) //doesn't register or sort using group uses the cache result
    
   
     for component_storage, index in ecs.run(&position_rotation_query){
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
