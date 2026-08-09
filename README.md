# Journey_ECS
## ECS Query / Filter / Execution Pipeline


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
*Pull request, Issues, Contribution and Discussion on the design and implementation on the Journey_ECS is welcomed.*
