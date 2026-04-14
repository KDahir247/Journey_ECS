
# 📜 Programming Principles (Core Discipline)

When programming, follow this rule:

1) A brief informal statement of the problem  
2) The precise correctness conditions required of a solution  
3) The solution  
4) A proof (sanity check) that the solution satisfies the conditions  

---

# 🔍 Full Systems-Level Checklist

## 1) 🧩 Problem (Informal)

> What is needed? (**NO how**)

- What system-level issue exists?
- What must be controlled (memory, CPU, IO, Algorithm, Data Layout)?

---

## 2) 🎯 Goals

> What does success look like?

- Latency / throughput targets
- Memory footprint
- Determinism
- Predictability

---

## 3) ⚖️ Constraints

> What are the bounds of the solution?

- No allocation?
- Thread count / concurrency?
- Compile-time vs runtime?
- Leaf procedure?
- Reentry procedure?

---

## 4) 🧠 Model Thinking

> What is the clean logical model?

- What are we representing?
- What are the valid states?
- Can invalid states be eliminated?

---

## 5) 🧱 Representation

> How is the model encoded?

### Data (State)

- Layout: struct-of-arrays vs array-of-structs  
- Encoding: bitmask, dense, sparse  
- Memory: packed vs aligned  
- Size: fixed vs dynamic  

### Logic (Transformation)

- Form: loop, query (filtered iteration), pipeline (multi-stage)  
- State: read vs write  
- Access: linear vs random  
- Coupling: operates on SoA, AoS, bitmask, Struct, POD
- Granularity: element, chunk, batch  
- Order: deterministic vs unordered  

---

## 6) 🧬 Memory Layout ⭐ (CRITICAL)

> What does memory actually look like?

- Exact byte layout
- Alignment guarantees
- Padding (intentional or not)
- Cache line boundaries (typically 64 bytes)
- False sharing risks
- Page boundaries (4KB, huge pages)

---

## 7) 🔁 Access Pattern ⭐

> How is data touched?

- Sequential (cache-friendly)
- Random (cache-unfriendly)
- Strided access
- SIMD/vectorization potential
- Hot vs cold data separation

---

## 8) 💸 Cost Model ⭐

> What does this compile to?

- Loads / stores
- Branches (predictable or not?)
- Cache misses
- TLB pressure
- Pipeline stalls

Ask:
> “What instructions will this become?”

---

---

## 9) 🔒 Invariants (CRITICAL)

> What must ALWAYS be true?

- Bit ↔ index mapping
- Bounds relationships
- Alignment guarantees
- Ownership assumptions

---

## 10) ⚠️ Edge Cases / Failure Modes

- Boundary values (0, max)
- Overflow / underflow
- Empty data
- Alignment violations

---

## 11) 🧵 Concurrency / Memory Ordering ⭐

> Is this accessed concurrently?

- Atomic or not?
- Data races?
- Memory ordering (acquire/release)?
- False sharing?
- Lock-free vs locked?

---
## 12) 🔌 Interface (Inputs / Outputs)

### Inputs

- Types
- Value vs pointer vs reference
- Lifetime guarantees
- Alignment requirements
- Ownership (who owns?)
- Mutability (who can write?)
- Aliasing guarantees (can overlap?)

### Outputs

- Types
- Return by value vs write-to-pointer
- Allocation or not
- Lifetime (if allocated):
  - transient (stack / temporary)
  - frame-local
  - persistent
  - global

- Ownership transfer?
- Error handling (status codes, etc.)

## 🚫 13) Non-Goals / Forbidden

- What this system will NOT do
- What cases are intentionally unsupported

---

## 14) ⚙️ Implementation

- Write simple, correct version first
- Follow invariants strictly

---

## 15) 🧪 Proof / Sanity Check

> Does it satisfy correctness?

- Invariants preserved?
- No UB?
- Constraints met?
- Edge cases handled?

---

## 16) 🔍 Inspection (VERY IMPORTANT)

- Look at generated assembly
- Verify:
  - no unexpected branches
  - no redundant loads/stores
  - vectorization (if expected)

---

## 17) 🚀 Optimization Pass

- Improve cache locality
- Remove branches
- Align data
- Use SIMD where applicable
- Reduce memory traffic

---

## 18) 🔁 Re-validate Invariants

After optimization:

- Did anything break?
- Are assumptions still valid?

---

# ⚡ 5-Second Mental Loop

> **1. What is the model?**  
> (what am I actually representing?)

> **2. Where is it in memory?**  
> (exact layout, alignment, contiguity, rsp)

> **3. How is it accessed?**  
> (linear, random, strided, gather?)

> **4. How do I compute the address?**  
> (index → pointer → valid?)

> **5. What does this compile to?**  
> (loads, branches, stalls, SIMD?)

> **6. What must stay true?**  
> (invariants, no desync, no UB)
---
