# 📜 Systems Programming Checklist (Data-Oriented, Acton-Style)

---

# 0) 🧠 First Principle (Always)

- All programs transform data  
- Better data understanding ⇒ better solutions  
- If you don’t understand the data → **STOP**

---

# 1) 🧩 Problem (NO SOLUTIONS)

- What is needed? (no “how”)
- What data exists?
- What transformations are required?
- What are the system limits?
  - CPU
  - Memory
  - IO

---

# 2) 🔬 Understand the Data (MANDATORY)

> “Dump everything” — Mike Acton

- Log:
  - Inputs
  - Parameters
  - Intermediate values
  - Outputs

- Analyze:
  - Value ranges
  - Frequency distributions
  - Patterns / clustering

- Identify:
  - Common case
  - Rare case
  - Invariants

---

# 3) 🎯 Constraints (Global Truths)

- Memory limits
- Cache behavior
- Throughput / latency targets
- Determinism requirements
- Threading model

---

# 4) 🧱 Data Layout (CRITICAL)

- Prefer:
  - SoA > AoS (for processing)
  - Homogeneous memory
  - Contiguous layout

- Rules:
  - Sequential access > random
  - Keep hot data together
  - Align to cache lines (64B)
  - Largest fields first in struct
  - Large arrays last

- Avoid:
  - Pointer chasing
  - Heterogeneous blobs

---

# 5) 🔄 Data Flow

- Processing type:
  - Linear
  - Batched
  - Streaming

- Goals:
  - Single pass
  - Early rejection
  - Minimal passes over data

---

# 6) 🔀 Control Flow

- Reduce code paths

- Prefer:
  - Predictable branches
  - ≤ 2 conditions per branch
  - Short-circuit logic

- Eliminate:
  - Contradictions (`a > 5 && a < 5`)
  - Redundancy (`a > 5 && a > 4`)

---

# 7) ⚙️ Execution Model

- Batch operations
- Operate on arrays, not objects

- Prefer:
  - Loops over function calls
  - Inlining when beneficial

---

# 8) 🧮 CPU Efficiency

- Minimize instruction bytes
- Keep hot loops small

- Prefer:
  - Integer operations > floating point
  - 32-bit types (unless needed)
  - Simple instructions (≤ 2 µops)

- Avoid:
  - Microcoded instructions
  - Mixing SIMD domains (AVX + SSE)
  - Float/int conversions

---

# 9) 💾 Memory Behavior

- Access patterns:
  - Sequential
  - Cache-friendly
  - Full cache line (64B) when possible

- Rules:
  - Interleave load/store
  - Avoid memory-size mismatches
  - Use page locality (4KB)

- Allocation:
  - Avoid per-call allocation
  - Use arena allocators (per-thread)
  - Let user define memory size

---

# 10) 🧵 Concurrency

- Goal: **avoid synchronization**

- Rules:
  - Separate:
    - Read-only
    - Mutation
  - No shared mutable state
  - Per-thread allocators
  - Shared memory = read-only

- Warning:
  - More synchronization ⇒ less performance

---

# 11) 📦 Interface Design

For every function:

- Does it:
  - Return valid data every time?
  - Return predictable types?
  - Require validation?
  - Allocate memory?
  - Behave deterministically?

- Structure:
 >return = func(input, params, options, helpers)

---

# 12) 🔒 State & Mutability

- Prefer:
  - Stateless > stateful
  - Immutable > mutable

- If mutating:
  - Is it necessary?
  - What information is lost?

---

# 13) 🧪 Correctness & Stability

- Deterministic?
- Repeatable results?
- Edge cases meaningful?

- Strategy:
  - Solve common case first
  - Ignore rare unless critical

---

# 14) ✂️ Simplicity Rules

- Avoid:
  - Over-abstraction
  - Generic designs
  - Premature extensibility

- Prefer:
  - Specific solutions
  - Minimal code
  - Fewer code paths

---

# 15) 📉 Performance Strategy

Optimize in order:

1. Data layout  
2. Access pattern  
3. Branching  
4. Instructions  

- Avoid premature micro-optimization

---

# 16) 🧰 Types & Conventions

- Use:
  - Explicit sizes (`u8`, `u32`, `u64`)
  - Unsigned by default
  - UTF-8 strings

- Avoid:
  - `bool` → use `u32`
  - Implicit behavior

---

# 17) 🧹 Code Hygiene

- Remove unused code paths
- Refactor aggressively
- Keep functions small
- Keep scope tight

---

# 18) 🧠 Mental Model (Always Ask)

- Where is data read?
- Where is data written?
- What mutates?
- How often?
- What is hot?

---

# 19) 📊 Measurement (CRITICAL)

- Always measure:
  - Cache misses
  - Branch mispredicts
  - CPU cycles

- Tools:
  - perf
  - VTune
  - platform profilers

> If you didn’t measure it, you didn’t optimize it.

---

# 20) 📈 Data Metrics

- Cardinality → how many?
- Density → how packed?
- Locality → how close?
- Lifetime → how long alive?

---

# 21) 🧭 Locality

- Temporal → reuse soon
- Spatial → access nearby data

---

# 22) ⚠️ Failure Modes

- What happens on invalid data?
- Can system degrade gracefully?
- Is failure detectable?

---

# 23) 🧱 Invariants

- What must ALWAYS be true?
- Can invalid states be eliminated by design?

---

# 🔥 Ultra-Compact Version (Mental Loop)
