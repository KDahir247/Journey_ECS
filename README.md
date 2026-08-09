# Journey_ECS

## Core principle





## ECS Query / Filter / Execution Pipeline

Component that fail in the query_mask (coarse), bit_zero_mask (granular), Indices Querying (eg. has Position + Rotation),Component Filtering (Position.x > 5 AND Rotation.yaw > 0.5) will exit early if possible.

There will be no structural changes that will happen to the data layout in the ColumnarBlock (base struct to hold components) if any component fails or succeeds in the final result. This mean there will be no swapping, removing, inserting or any other change in each step of the Execution pipeline. It will do this by doing a final blend
from the modified component (through the system) and the original component using a mask, which is very similar to a SIMD blendv operation.

```text
                              QUERY
                                │
                                ▼
                   ┌────────────────────────┐
                   │ ColumnarBlock          │
                   │      query_mask        │
                   └───────────┬────────────┘
                Disabled/Enabled 64 chunk components        
                               │ Coarse rejection
                               │ 1 bit = 64 components
                               ▼
                   ┌────────────────────────┐
                   │    ColumnarPage        │
                   │                        │
                   │   bit_zero_mask[8]     │
                   └───────────┬────────────┘
                  Disabled/Enabled component 
                               │ Exact rejection
                               │ 1 bit = 1 component
                               ▼
                   ┌────────────────────────┐
                   │      Query Cache       │
                   │                        │
                   │ Reuse discovered valid │
                   │ component locations    │
                   |                        | 
                   └───────────┬────────────┘
                              eg.
                         Group Pos + Scale    
                     Sub Group (Pos + Scale) + Vel 
                               │
                               ▼
                   ┌────────────────────────┐
                   │    Filter    Cache     │
                   │                        │
                   │ • Contradiction        │
                   │ • Redundancy           │
                   │ • Merging              │
                   │ • Ordering             │
                   │ • Compiled filter      │
                   └───────────┬────────────┘
                               eg.
                  WHERE Pos.x > 0 AND Pos.x > 5
                 Will get converted to Pos.x > 5
              It will get all Pos that satisfy condition
            by using SIMD refer to SYSTEM on how data is store
                              |
                   ┌────────────────────────┐
                   │     FINAL MASK         │
                   │                        │
                   │ coarse                 │
                   │      &                 │
                   │ granular               │
                   │      &                 |
                   │ group                  |
                   │      &                 |  
                   │ filter                 │
                   │      &                 │
                   │ operation              │
                   └───────────┬────────────┘
                               │
                   ┌────────────────────────┐
                   |                        |
                   │  Skip Pages or Block   │
                   │   Depending on mask    │
                   | eg if 4096 bits are 0  |
                   |     skip the Page      | 
                   └───────────┬────────────┘
                               │
                               ▼
                   ┌────────────────────────┐
                   │       Workspace        │
                   │                        │
                   │ • Save original data   │
                   │ • Temporary masks      │
                   │ • Intermediate data    │
                   │ • Per-thread storage   │
                   └───────────┬────────────┘
                               │
                               ▼
                   ┌────────────────────────┐
                   │         SYSTEM         │
                   │                        │
                   │ Compute on all lanes   │
                   |     SIMD by default    |
                   | SOA 64 chunk by default|
                   | eg. (x,x....)(y,y...)  |
                   │ without per-lane ifs   │
                   └───────────┬────────────┘
                               │
                               ▼
                       ┌───────┴───────┐
                       │               │
                    simd mask = 1   simd mask = 0
                       │               │
                       ▼               ▼
                  Keep new         Restore old
                    data             data
                       │               │
                       └───────┬───────┘
                               │
                               ▼
                        COMPONENT DATA

```

## Multi Threading



## Deferred Operations



## Reflection and Tracking


## Serialization


*Pull request, Issues, Contribution and Discussion on the design and implementation on the Journey_ECS is welcomed.*
