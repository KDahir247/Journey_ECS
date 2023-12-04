# Journey_ECS
Fast Minimal Sparse set for Odin Language Which is being used in Odin-Journey.

Currently It is works, but still on heavy WIP
</br>

**Features**
- Fast single component iteration & entity iteration (dense array iteration) 
- Unlimitied amount of component can be added
- Support for duplicate component type in sparse storage 
- Very minimal Only does what it needs and nothing more.
- SOA component dense array
- Implement resource for unique component (1 per unique component)
- Reduced memory consumption on sparse array
- Allow upto or more then 1 million entity

**Limitation**
- small to no checks so undefined behaviour on invalid use (so call register before adding component and make sure when removing a component entity actually has component or call has_component with a if statement)
- Slighly higher memory due to sparse array with holes (mitigated my using pagination)
- No shared component model out of the box (this is passed to the user and I don't see this as a limitation)
- Iterate from first to last, so adding or removing component while iterating may cause invalidation (user can easily add a run function that iterate last to first to resolve this), so adding and removing component should happen outside iterating loop of the same component/s.

**Working Progress**
- Optimization and profile code solution
- Implement Bulk function 
- Writing Test
- Simd Iteration, since the data layout is already optimal for SIMD
- Extremely fast grouping and sub grouping query (eg. Entity with position and scale)
- Rework query system for new changes 

Example of code 
```odin
package main

import ecs "foldername that has this single script"

main :: proc() {

    Position :: struct {
        val : [2]f32,
    }

    Scale :: struct{
        x : f32,
        y : f32,
    }

    GameController :: struct{
        x : f32,
        y : f32,
        //data
    }

    GameStep :: struct{
        step : f32,
        //data
    }

    NPC_POSITION_STORAGE_INDEX :: 0
    ENEMY_POSITION_STORAGE_INDEX :: 1
    SCALE_STORAGE_INDEX :: 2

    world := init_world(7,7) // max of 7 sparse storage and 7 group storage

    /*
        Rather then combined all the component of the same type to a large container
        We will allow creating the same type component in different storage to allow 
        Distinction on different type regardless of it being the same struct
        This will allow faster query if your looking for a specific position and your layout is correct and iteration
        Eg rather then going over the full position (Player Position, Npc Position, Enemy Position, etc) 
        Which will take long just to find the player position
        We can seperate it like so (don't need another struct to make a distinction)

        storage_position_1 = [Player Position_0]
        storage_position_2 = [Npc Position_0, Npc_Position_1, Npc_Position_2]
        storage_position_3 = [Enemy_Position_0, Enemy_Position_1, Enemy_Position_2, Enemy_Position_3, Enemy_Position_4]
        storage_position_4 = [Decal Position_0 .......... Decal Position_1000]

        then we can fetch the storage we need a query using storage id rather then component type
        this also eliminate needing to use a key value pair for the sparse index ([Key = typeid, Value = sparse_id] aka map reducing our memory allocation and computation cost)
        we can use the storage_id for the sparse id directly

    */

    register_storage(world, Position) // storage 0
    register_storage(world, Position) // storage 1
    register_storage(world, Scale) // storage 2

    npc_0 := create_entity(world)
    npc_1 := create_entity(world)
    enemy_2 := create_entity(world)
    npc_3 := create_entity(world)
    enemy_4 := create_entity(world)
    enemy_5 := create_entity(world)
    remove_entity(world,npc_0)
    enemy_0 := create_entity(world)

    add_soa_component(world, npc_1, NPC_POSITION_STORAGE_INDEX, Position{val = {100, 200}})
    add_soa_component(world, npc_3, NPC_POSITION_STORAGE_INDEX, Position{val = {0, 10}})


    add_soa_component(world, enemy_0, ENEMY_POSITION_STORAGE_INDEX, Position{val = {1.0, 2.0}})
    add_soa_component(world, enemy_2, ENEMY_POSITION_STORAGE_INDEX, Position{val = {3, 4}})
    add_soa_component(world, enemy_4, ENEMY_POSITION_STORAGE_INDEX, Position{val = {5, 6}})
    add_soa_component(world, enemy_5, ENEMY_POSITION_STORAGE_INDEX, Position{val = {7, 8}})
    set_soa_component(world, enemy_4, ENEMY_POSITION_STORAGE_INDEX, Position{val = {247, 19}})
    
    fmt.println("Before removing component")
    fmt.println("Enemy Entity ID", enemy_2, "Has Component Position : ", has_soa_component(world, enemy_2, ENEMY_POSITION_STORAGE_INDEX, Position))
    remove_soa_component(world, enemy_2, ENEMY_POSITION_STORAGE_INDEX, Position)
    fmt.println("After removing component")
    fmt.println("Enemy Entity ID", enemy_2, "Has Component Position : ", has_soa_component(world, enemy_2, ENEMY_POSITION_STORAGE_INDEX, Position))

    fmt.println("\n")
    
    fmt.println("NPC EntityIDs :", get_id_soa_components(world, NPC_POSITION_STORAGE_INDEX))
    fmt.println("NPC Components :", get_soa_components(world, NPC_POSITION_STORAGE_INDEX, SOAType(Position)))

    fmt.println("Enemy EntityIDs : ", get_id_soa_components(world, ENEMY_POSITION_STORAGE_INDEX))
    fmt.println("Enemy Components : ", get_soa_components(world, ENEMY_POSITION_STORAGE_INDEX, SOAType(Position)))

    //Resource
    init_resource(world, [2]typeid{
        GameController,
        GameStep,
    })

    game_controller := (^GameController)(get_resource(world, 0)) // GameController
    game_step := (^GameStep)(get_resource(world, 1)) // GameStep


    game_controller.x += 2
    game_controller.y += 5

    game_step.step = 60.0 

    fmt.println(game_controller)
    fmt.println(game_step)

    deinit_resource(world)

    deinit_world(world)
}


```

*Pull request, Issues, Contribution and Discussion on the design and implementation on the Journey_ECS is welcomed.*
