## Programming Principles.

- When programming follow this rule;
1) A brief informal statement of the problem
2) The precise correctness conditions required of
a solution (input, output, constraint (bound on solution), goals, limit (bound on input))
3) The solution
4) A proof that the solution satisfies the
requisite conditions. 

-Take every procedure. Output every data change throughout a some reasonable run. Every change. Every parameter that's passed. Every value that's stored. Every variable in the procedure. Dump them all. Then look at the values. Then find a different way to look at the values. I guarantee there will be surprises that will change the way you understand the problem and change the way you think it should be solved. You simply cannot write good solutions without understanding the data, at least on some level. And the better you understand it, the better solutions you'll be able to provide. (Mike Acton)

- For any issue, always stop and figure out how you can understand the real problem better first, before you decide on any solutions.

- Organize data in a way that when processed the code will follow similar paths

- We want instruction bytes to be as little as possible so for front end fetch it can fetch more instructions for decoding. It also utilizes all the decoder, since >= 8 bytes can only be decoded by one decoder.

- Keep it in order, and you shall receive!

- Application should solve for the common case not all the case. This is important. Solve only the things that matter to the user and solve it really well. 

- Does everything need to run at 60, 240, 144 fps (eg. door, dialouge)?

- Don’t lean on customization as a crutch. Make sure your default design works well for most users, then add customization as a bonus, not a fix.

- Effective addresses (Virtual Address) are used to index into the correct cache set. It then check each set.

- Primitive type are passed by value (POD type eg u8, u16, u32, etc.... and Vector type __m126, __m256, __m512, etc.....)

- Is registry struct good? They are just struct with function ptr for a core execution. eg. log_registry contains only function ptr to print log in console and what not. 

- All programs are about transforming data.

- If you have no idea how a system works at all, you don’t know what the right questions are, nor how to ask them, and any answers you get will be opaque at best, if not outright garbage.

- Write code to operate on batch data.

- When using an array of objects, reserve the index 0 can be treated as a nil object or just header data.

- The fundamental truth is that data, though it can be generic by type, is not generic in how it is used

- When using memory keep the data in the memory homogeneous not heterogeneous. 

- Reduce codepath because every new codepath represents a new possibility of code execution, it also represents a new possibility of code failure

- When create a procedure and calling a procedure think of these following question;
1) Do i get a valid value back everytime?
2) What type of value do I get back?
3) Can i use the value without checking?
4) Can i run the fuction repeatedly and produce the same effect?
5) Does it allocate memory per call or does it reuse it?

- avoid global stateful code if possible keep it local.

- Prefer stateless code over stateful code. (passing input into a function and changing it will is stateful)

- Remeber the more code path you add the more harder debugging will be. 

- Write for readability. Code is read more often than it is written.

- Avoid Generic

- Don't make code that is pessimistic. Just write the simplest code for the specific problem.

- Design your system around data layouts and data flows first.

- When make changes, you should always leave the code in a better state than you found it.

- When writting code where memory is used. Let the user specify how much memory it will need and how much it will use. Rather than resizing.

- Make the specific solution to the specific problem as minimal as possible. Don't expect future needs.

- Immutable is simpler than mutable. When mutating variable is there a benefit (such as performance) or not? Remeber mutating data is lossie (you lose previous information).

- All struct field are public. There is no such thing as private, protect, friend, etc...

- Struct that include an enum should specify an integer type for the struct member so the type size is explicit.

- Function structure will be the following (I like OurMachinery layout)
  return_type, more_return_types = function_name(input_object, parameters, options, helpers)
  eg. return_type == primitive, object == in/out reference (only reading from), parameter == must set, options == optional mask, helpers == allocators, more_return_types == primitive return types.

- Explicit is better than implicit.

- Don't introduce unecessary abstraction layers. Only abstract when it brings clear value.

- Build more advanced things out of a few simple primitive type (virtual alloc, array, and hash only) or POD (u8, u16, u32, u128, u256, u512)

- Actively prune unused code paths.

- Refactor out uneeded complexity whener you can. Strive to make the code as small and simple as possible.

- Use explicit size type 

- Treat errors as just data

- When writing code keep focus on where reads occur, where write mutation occurs, and what is mutated.

- Remember the OS will reclaim any unclaimed memory at the end of the program, so persistent memory could be left to the OS

- When multithread keep in mind it not about how you synchronize it how you don't 

- When multithreading seperate the mutate from the read only. eg. two proc (lookup and insert)

- Arena allocator are mutable, so each thread should have it own allocator. There should not be a shared allocator.

- Thread can share a memory buffer if the buffer is intended only as read and never a write.

- Remember the more synchronization a multithreaded system requires, the more this benefit dissolves.

- Prefer unsigned integer over signed integer. Use signed integer only when casting to a float type or dealing with negative numbers.

- Be Explicit on when to use zero is initialization.

- Strings should be UTF-8

- Include file (.h) should only include one include file (.h)

- Remember where there is one there is many

- Try to avoid using library if possible. If not keep it very minimal 

- Store variable above in the beginning of the scope without assignment (variables should have the smallest scope possible)

- Use scopes rather than function call. Use function call when needed though.

- Aim to reduce the number of macro ops. Favor fastpath single (1 macro operation) more than fastpath double (two macro operation). Avoid microcoded instruction (more than 2 macro operation). 

- Use load execute instruction instead for discrete load and execute instruction when performing SIMD operations.

- For floating point constant it is stored in memory and not encoded in the instruction so use it sparingly and specify the suffix.

- Use array notation instead of pointer notation when working with arrays.

- No more than two logical expression in a branch. Make it predictable and short circuit it as well if possible.

- Floating point operation can't be reordered usually by the compiler, so try to optimize it by reordering it.

- Rule of thumb for struct layout. Biggest size type should be on top following a descending order to the bottom of the struct. Large array in struct should be the last. If possible keep likely used data together as well so it is access in sequence in the struct.

- Use 32 bit type unless higher precision is needed like a 64 bit type.

- Reduce the size of instructions when possible

- Try to use 8 bit displacement as much as possible. That is 127 to -128, since this significantly lower the instruction bytes

- For jump instruction try to make the jump displacement in the range of 127 to -128 since 8 bit jump instruction is significantly shorter than the 32 bit displacement jump instruction. This will require very near jumps and smaller instruction byte above to reduce the jump displacement.

- Avoid memory-size mismatches when different instructions operate on the same data.

- Interleave store and load.

- Everything need LIMITS and ASSUMPTION so make limits and assumption on the problem, but keep it reasonable. 

- Utilize branch fusion it can be fused into a single micro operation.

- Avoid conditional branch that depends on random data.

- Strive to make for loop counter no larger than 64.

- An array uses the same alignment as its elements, except that a local or global
array variable of length at least 16 bytes  (Linux ABI)

- Prefer function pointer rather than virtual function.

- If the function is a leaf function the compiler can use the 128 byte   readzone in the stack to avoid adjusting the stack pointer in the   prologue or epilogue remember it is 128 bytes no more. (Linux ABI)

- Avoid passing more than 256 bits as function parameter. Also make sure the field are aligned to avoid passing by memory. Just use simple POD (Plain old data) (Linux ABI)

- Avoid using boolean type.<D-`> If a boolean is absolutely required use a bool32 or bool64

- Keep application size below 2 GB to avoid code model switch (small to medium or large) (Linux ABI)

- Write 32 byte chunk if possible

- Array and hast tables are enough in most projects

- Read a full cache line (64 bytes) if possible.

- Careful with SIMD instruction with 3 operand it can steal a bus from one unit.

- Keep it two loads and one store per cycle (without SIMD 3 loads or 2 store).

- Use integer type unless floating point values are actually needed.

- Use a full page size (4kb usually) at a time and sequentially. Don't jump around through different pages.

- L3 is a victim cache (AMD). It is shared between a CCX (Zen3 = 6 to 8 cores) and follow three rules;
	1) Store from any core in the L3 cache will invalidate the line regardless size
	
	2) Read from the L3 cache by just one core in the CCX will invalidate the line.

	3) Reading the line from multiple core in the CCX will keep the line in the L3 cache.

- Favor non taken branches over taken branches. 

- OC mode will almost always happen in a loop due to a branch target.

- Try to avoid casting a SIMD integer to a SIMD floating point. There will be additional latency when crossing.

- Don't mix AVX with SSE.

- If a function is only called from a single place, consider inlining it.

- If a function is called from multiple places, see if it is possible to arrange for the work to be done in a single place, perhaps with flags, and inline that.

- If there are multiple versions of a function, consider making a single function with more, possibly defaulted, parameters.

- If the work is close to purely functional, with few references to global state, try to make it completely functional.

- Try to use const on both parameters and functions when the function really must be used in multiple places.
