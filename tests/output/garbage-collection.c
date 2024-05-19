#include <stdint.h>  // Included for standard integer types (e.g., uint8_t, int32_t)
#include <stdio.h>   // Included for standard input/output functions (e.g., fprintf, stderr)
#include <stdlib.h>  // Included for standard library functions (e.g., exit)
#include <string.h>  // Included for string manipulation functions (e.g., fputs)


/// exit the program, displaying an error message
void panic(const char *message) {
  fputs("PANIC:", stderr);
  fputs(message, stderr);
  exit(-1);
}

#ifdef DEBUG
#define DEBUG_PRINT(...)                                                       \
  do {                                                                         \
    fprintf(stderr, __VA_ARGS__);                                              \
  } while (0)
#else
#define DEBUG_PRINT(...)                                                       \
  do {                                                                         \
  } while (0)
#endif


/// A CodeLabel represents a function pointer within the runtime system.
///
/// This typedef defines a pointer to a function that takes no arguments 
/// and returns a `void*`. While ideally we'd like a more specific return 
/// type like `EntryFunction*` (referencing the function pointer type for 
/// closure entry points), C doesn't allow for recursive type definitions 
/// within the same declaration. In practice, a `CodeLabel` will almost 
/// always point to an `EntryFunction`.
typedef void *(*CodeLabel)(void);

/// An EvacFunction represents a function responsible for closure evacuation 
/// during garbage collection.
///
/// This typedef defines a pointer to a function that takes a pointer to the 
/// current location of a closure (`uint8_t*`) and returns a pointer to the 
/// new location (`uint8_t*`) after potentially moving the closure during 
/// garbage collection. This function is responsible for copying the closure 
/// and updating its internal references if necessary to ensure it remains 
/// reachable during memory management.
typedef uint8_t *(*EvacFunction)(uint8_t *);

/// An InfoTable stores information about a closure's functions.
///
/// This structure defines the `InfoTable` which holds information about 
/// the functions associated with a closure. It contains two key members:
///   - `entry`: This member is a `CodeLabel` pointer referencing the 
///     function that can be called to execute the closure's logic. This 
///     typically points to the closure's entry point function.
///   - `evac`: This member is an `EvacFunction` pointer referencing the 
///     function responsible for evacuating the closure during garbage 
///     collection. This function ensures the closure's continued existence 
///     if it's still reachable by the program. 
typedef struct InfoTable {
  CodeLabel entry; // Function pointer for closure entry
  EvacFunction evac; // Function pointer for closure evacuation
} InfoTable;

/// Evacuation function for static objects - simply returns the current location
///
/// This function, `static_evac`, is used for closures representing statically 
/// allocated objects. Since static objects never move in memory, this evacuation 
/// function simply returns the original location (`base`) passed as the argument. 
/// During garbage collection, static objects are guaranteed to remain in their 
/// original location, so there's no need to relocate them.
uint8_t *static_evac(uint8_t *base) {
  return base;
}

/// Special InfoTable for closures without an entry function (null table)
///
/// This variable, `table_for_null`, defines a special `InfoTable` entry used 
/// for closures that don't have a traditional entry point (null entry). This 
/// can be useful for situations where the closure represents data or a concept 
/// that doesn't require an explicit entry function. The `table_for_null` 
/// has the following members:
///   - `entry`: Set to `NULL` as there's no entry function to call.
///   - `evac`: Set to `&static_evac` because even without an entry function, 
///             the evacuation behavior is still the same (returning the current 
///             location) as with static objects.
///
/// A pointer to this table, `table_pointer_for_null`, is stored separately 
/// for convenient reference.
InfoTable table_for_null = {NULL, &static_evac};
static InfoTable *table_pointer_for_null = &table_for_null;

/// Evacuation function for already evacuated closures
///
/// This function, `already_evac`, is used for closures that have already been 
/// relocated during a previous garbage collection cycle. The evacuation 
/// process might involve moving closures to different memory locations. This 
/// function retrieves the new location of the closure after evacuation.
///
/// Here's how `already_evac` works:
///   1. `uint8_t *ret`: Declares a pointer `ret` to store the return value (new location).
///   2. `memcpy(&ret, base + sizeof(InfoTable *), sizeof(uint8_t *))`: 
///      - This line copies the new location pointer stored within the closure 
///        itself. The pointer is located at an offset of `sizeof(InfoTable *)` 
///        from the base pointer (`base`) because the first part of the closure 
///        typically holds the `InfoTable` structure.
///      - The `memcpy` function ensures a safe and efficient copy of the 
///        pointer data.
///   3. `return ret`: Returns the new location (`ret`) of the evacuated closure.
uint8_t *already_evac(uint8_t *base) {
  uint8_t *ret;
  memcpy(&ret, base + sizeof(InfoTable *), sizeof(uint8_t *));
  return ret;
}

/// Shared InfoTable for already evacuated closures
///
/// This variable, `table_for_already_evac`, defines a single `InfoTable` instance 
/// used for closures that have been relocated during garbage collection. This 
/// is a space-saving optimization. Closures share this table if they've already 
/// been evacuated. Sharing a single table is possible because:
///   - The `entry` member is always `NULL` for evacuated closures (no entry function to call).
///   - The `evac` member points to the `already_evac` function, which is responsible 
///     for finding the new location of any evacuated closure.
///
/// A pointer to this shared table, `table_pointer_for_already_evac`, is stored 
/// for convenient reference.
InfoTable table_for_already_evac = {NULL, &already_evac};
static InfoTable *table_pointer_for_already_evac = &table_for_already_evac;
/// Function pointer for string evacuation (declaration, implementation not shown)
uint8_t *string_evac(uint8_t *);

/// InfoTable for string closures (entry panics)
///
/// This variable, `table_for_string`, defines an `InfoTable` for closures 
/// representing strings. String closures  don't have a meaningful 
/// entry point function (e.g., you wouldn't typically call a function to 
/// access the string content). Therefore, the `entry` member is set to `NULL`. 
/// As a safeguard, the `evac` member points to `string_evac`, a function 
/// responsible for evacuating strings during garbage collection (implementation 
/// not shown here). Additionally, to indicate an invalid entry point, the 
/// `table_for_string` sets its `entry` member to a function that panics 
/// (`panic`). This helps catch accidental attempts to call a string closure 
/// directly.
///
/// A pointer to this table, `table_pointer_for_string`, is stored for 
/// convenient reference.
InfoTable table_for_string = {NULL, &string_evac};
static InfoTable *table_pointer_for_string = &table_for_string;

/// InfoTable for string literal closures (static evacuation)
///
/// This variable, `table_for_string_literal`, defines an `InfoTable` for closures 
/// representing string literals. String literals are essentially constant 
/// in-memory data and won't be moved during garbage collection.  Therefore, 
/// the `evac` member points to `static_evac`, which simply returns the 
/// current location (no need to evacuate). The `entry` member can be `NULL` 
/// or point to a valid entry function depending on how the string literal 
/// closure is intended to be used.
///
/// A pointer to this table, `table_pointer_for_string_literal`, is stored 
/// for convenient reference.
InfoTable table_for_string_literal = {NULL, &static_evac};

/// Structure representing a closure allocation frame (CAF) cell
///
/// This structure, `CAFCell`, defines a cell within the Closure Allocation 
/// Frame (CAF) list. The CAF list is a linked list used to track all 
/// allocated closures during program execution. Each `CAFCell` holds information 
/// about a single closure:
///   - `table`: This member is a pointer to an `InfoTable` structure 
///     associated with the closure. The `InfoTable` provides details about 
///     the closure's entry point and evacuation behavior.
///   - `closure`: This member is a pointer to the actual memory location 
///     where the closure data is stored.
///   - `next`: This member is a pointer to the next `CAFCell` in the linked list, 
///     allowing for efficient traversal of all allocated closures.
typedef struct CAFCell {
  InfoTable *table; // Pointer to closure's InfoTable
  uint8_t *closure; // Pointer to closure data
  struct CAFCell *next; // Pointer to next cell in CAF list
} CAFCell;

/// Head and tail pointers for the CAF list
///
/// These variables manage the CAF list, a singly linked list that tracks 
/// all closures:
///   - `g_CAFListHead`: This global variable is a pointer to the head node 
///     of the CAF list. Initially, it's set to `NULL` as the list is empty 
///     at program startup.
///   - `g_CAFListLast`: This global variable is a pointer to a pointer to 
///     the last node in the CAF list. This approach is used for efficient 
///     tail insertion during closure allocation. 
///         - It starts by pointing to the address of `g_CAFListHead`. 
///         - When a new closure is allocated, a new `CAFCell` is created 
///           and appended to the list.  
///         - `g_CAFListLast` is then updated to point to the newly created 
///           cell's `next` member, effectively making it the new tail.
CAFCell *g_CAFListHead = NULL;
CAFCell **g_CAFListLast = &g_CAFListHead;

/// Structure representing the argument stack (StackA)
///
/// This structure, `StackA`, defines the argument stack used by the runtime 
/// system. The argument stack holds information about closures passed as 
/// arguments to functions. 
///
/// Here's a breakdown of the `StackA` members:
///   - `top`: This member is a pointer to a pointer (`uint8_t **`). 
///     - It points to the top of the stack. The stack grows upwards in memory. 
///     - The value at `top` itself might not be a "live" value (it might point 
///       to unused memory on the stack).
///   - `base`: This member is another pointer to a pointer (`uint8_t **`). 
///     - It points to the base of the stack, which is the fixed memory 
///       allocation for the argument stack.
///   - `data`: This member is a pointer to a pointer (`uint8_t **`). 
///     - It points to the starting address of the entire argument stack memory 
///       allocation. This is used for freeing the entire stack when the program 
///       exits.
typedef struct StackA {
  uint8_t **top; // Pointer to top of argument stack
  uint8_t **base; // Pointer to base of argument stack
  uint8_t **data; // Pointer to entire argument stack memory
} StackA;

/// Global argument stack (StackA) instance
///
/// This variable, `g_SA`, is a global instance of the `StackA` structure. 
/// It represents the actual argument stack used by the runtime system. 
/// Initially, all its members (`top`, `base`, and `data`) are set to `NULL` 
/// as the stack is empty at program startup.
StackA g_SA = {NULL, NULL, NULL};

/// Union for representing different data types on the secondary stack (StackBItem)
///
/// This union, `StackBItem`, allows storing different data types on the 
/// secondary stack (`StackB`) depending on the context. It leverages the fact 
/// that these data types typically have the same memory size (usually 64 bits 
/// on most architectures):
///   - `as_int`: Stores a 64-bit integer value.
///   - `as_code`: Stores a pointer to a function (`CodeLabel`) for a continuation.
///     - A continuation represents the code to be executed after some 
///       operation is complete.
///   - `as_closure`: Stores a pointer to a closure data structure (`uint8_t *`).
///   - `as_sb_base`: Stores a pointer to the base of another `StackB` instance 
///     (used for nested continuations).
///   - `as_sa_base`: Stores a pointer to the base of the argument stack 
///     (`StackA *`). This is  used to track the argument stack state 
///     associated with a specific continuation on the secondary stack.
typedef union StackBItem {
  int64_t as_int;
  CodeLabel as_code; // Function pointer (continuation)
  uint8_t *as_closure;
  union StackBItem *as_sb_base; // Base of another StackB (nested continuation)
  uint8_t **as_sa_base; // Base of argument stack (associated with continuation)
} StackBItem;

/// Structure representing the secondary stack (StackB)
///
/// This structure, `StackB`, defines the secondary stack used by the runtime 
/// system. The secondary stack holds various data types like integers, 
/// continuations (code to be executed later), closure pointers, and information 
/// related to nested continuations and argument stacks.
///
/// Here's a breakdown of the `StackB` members:
///   - `top`: This member is a pointer to the top element on the stack.
///   - `base`: This member is a pointer to the base of the stack (fixed memory 
///     allocation).
///   - `data`: This member is a pointer to the starting address of the entire 
///     secondary stack memory allocation. 
typedef struct StackB {
  StackBItem *top;
  StackBItem *base;
  StackBItem *data; // Pointer to entire secondary stack memory
} StackB;

/// Global secondary stack (StackB) instance
///
/// This variable, `g_SB`, is a global instance of the `StackB` structure. 
/// It represents the actual secondary stack used by the runtime system. 
/// Initially, all its members (`top`, `base`, and `data`) are set to `NULL` 
/// as the stack is empty at program startup.
StackB g_SB = {NULL, NULL, NULL};

/// Global register for integer return values (g_IntRegister)
///
/// This global variable, `g_IntRegister`, is an integer register used by the 
/// runtime system. It  stores the return value from a function that 
/// returns a 64-bit integer. The initial value is set to 0xBAD, which might 
/// be a placeholder value indicating an uninitialized register.
int64_t g_IntRegister = 0xBAD;

/// Global register for string closures (g_StringRegister)
///
/// This global variable, `g_StringRegister`, is a pointer to a memory location 
/// (`uint8_t *`) that holds a string closure. It's important to note that 
/// `g_StringRegister` doesn't directly point to the character data of the string. 
/// Instead, it points to the closure structure itself, which  contains 
/// information about the string's data location and length. The initial value 
/// is set to `NULL`, indicating the register doesn't currently hold a string closure.
uint8_t *g_StringRegister = NULL;

/// Global register for constructor tags (g_TagRegister)
///
/// This global variable, `g_TagRegister`, is an integer register (`uint16_t`) 
/// used by the runtime system. It  stores a tag value associated with 
/// a constructor call. This tag might be used to identify the specific type 
/// of object being constructed. The initial value is set to 0xBAD, which might 
/// be a placeholder value indicating an uninitialized register.
uint16_t g_TagRegister = 0xBAD;

/// Global register for constructor argument count (g_ConstructorArgCountRegister)
///
/// This global variable, `g_ConstructorArgCountRegister`, is an integer register 
/// (`int64_t`) used by the runtime system. It  stores the number of arguments 
/// passed to a constructor during object construction. This information is crucial 
/// for the constructor to process its arguments correctly. The initial value is set 
/// to 0xBAD, which might be a placeholder value indicating an uninitialized register.
int64_t g_ConstructorArgCountRegister = 0xBAD;

/// Global register for current closure (g_NodeRegister)
///
/// This global variable, `g_NodeRegister`, is a pointer to a memory location 
/// (`uint8_t *`) that holds the currently executing closure. The runtime system 
/// might use this to keep track of the context during function calls and continuations. 
/// The initial value is set to `NULL`, indicating no closure is currently active.
uint8_t *g_NodeRegister = NULL;

/// Global register for constructor closure update (g_ConstrUpdateRegister)
///
/// This global variable, `g_ConstrUpdateRegister`, is a pointer to a memory location 
/// (`uint8_t *`) that holds a closure associated with constructor updates. Its 
/// purpose is not entirely clear from the provided code snippet, but it might be 
/// related to the internal mechanisms of constructor execution and finalization. 
/// The initial value is set to `NULL`, indicating no constructor closure update 
/// is in progress.
uint8_t *g_ConstrUpdateRegister = NULL;


/// Data structure representing the global heap (Heap)
///
/// This structure, `Heap`, defines the global memory pool used for allocating 
/// objects during program execution. It essentially acts as a large contiguous 
/// block of memory that the runtime system can manage. Here's a breakdown of 
/// the `Heap` members:
///   - `data`: This member is a pointer (`uint8_t *`) to the starting address 
///     of the allocated heap memory.
///   - `cursor`: This member is a pointer (`uint8_t *`) that represents the 
///     current write position within the heap. As new objects are allocated, 
///     the `cursor` is updated to point to the next available memory location.
///   - `capacity`: This member is a `size_t` value that represents the total 
///     capacity of the heap in bytes. It defines the maximum amount of memory 
///     that can be allocated from the heap before a garbage collection cycle 
///     might be necessary.
typedef struct Heap {
  uint8_t *data;  // Pointer to start of heap memory
  uint8_t *cursor; // Pointer to current write position
  size_t capacity; // Total capacity of the heap in bytes
} Heap;

/// Global heap instance (g_Heap)
///
/// This variable, `g_Heap`, is a static global variable of type `Heap`. It 
/// represents the actual heap memory pool used by the runtime system. The 
/// `static` keyword ensures that there's only one instance of this structure 
/// throughout the program. Initially, all members (`data`, `cursor`, and 
/// `capacity`) are  set to `NULL` or 0, indicating the heap is not yet 
/// initialized.
static Heap g_Heap = {NULL, NULL, 0};

/// Function to get the current heap cursor (heap_cursor)
///
/// This function, `heap_cursor`, retrieves the current write position within 
/// the global heap. It's  used for memory allocation purposes. When a new 
/// object needs to be allocated on the heap, this function is called to get the 
/// current free memory location. The returned value can then be used to write 
/// the object's data into the heap.
///
/// Here's how `heap_cursor` works:
///   - It simply returns the value of `g_Heap.cursor`. The `g_Heap.cursor` 
///     member always points to the next available memory location within the heap.
uint8_t *heap_cursor() {
  return g_Heap.cursor;
}


/// Function to write data to the heap (heap_write)
///
/// This function, `heap_write`, is used to write a block of data to the global heap. 
/// It takes two arguments:
///   - `data`: A pointer to the data that needs to be written. This data can be 
///     of any type.
///   - `bytes`: The size of the data to be written, in bytes.
///
/// Here's how `heap_write` works:
///   1. It uses `memcpy` to copy the data pointed to by `data` to the current 
///      write position (`g_Heap.cursor`) within the heap. The size of the data to 
///      copy is specified by the `bytes` argument.
///   2. After copying the data, `heap_write` updates the `g_Heap.cursor` by 
///      adding the `bytes` value. This effectively moves the cursor to the next 
///      available memory location after the written data.
///
/// **Important notes:**
///   - This function doesn't perform any boundary checks. It's the responsibility 
///     of the caller to ensure that there's enough space available in the heap 
///     to accommodate the data being written. Otherwise, it can lead to heap 
///     overflow and program crashes.
///   - The data is copied **by value**. If the data pointed to by `data` is 
///     itself a pointer, it's the address that gets copied, not the data it points to.
void heap_write(void *data, size_t bytes) {
  memcpy(g_Heap.cursor, data, bytes);
  g_Heap.cursor += bytes;
}

/// Function to write a pointer to the heap (heap_write_ptr)
///
/// This function, `heap_write_ptr`, is a convenience function that writes a 
/// pointer value to the heap. It takes a single argument:
///   - `ptr`: A pointer to the value that needs to be written to the heap.
///
/// Here's what `heap_write_ptr` does:
///   1. It casts the pointer argument (`ptr`) to a `void *` type. This is 
///      because `heap_write` expects a void pointer as its first argument.
///   2. It calls `heap_write` to write the pointer value to the heap. The size 
///      of the data to be written is automatically determined using `sizeof(uint8_t *)`. 
///      This ensures that the correct amount of bytes are copied to represent 
///      the pointer value on the target architecture.
void heap_write_ptr(uint8_t *ptr) {
  heap_write(&ptr, sizeof(uint8_t *));
}

/// Function to write an InfoTable pointer to the heap (heap_write_info_table)
///
/// This function, `heap_write_info_table`, is similar to `heap_write_ptr` but 
/// specifically designed to write pointers to `InfoTable` structures. It takes 
/// a single argument:
///   - `ptr`: A pointer to an `InfoTable` structure that needs to be written.
///
/// It follows the same logic as `heap_write_ptr` to write the pointer value to 
/// the heap, using `sizeof(InfoTable *)` to determine the size.
void heap_write_info_table(InfoTable *ptr) {
  heap_write(&ptr, sizeof(InfoTable *));
}

/// Function to write an integer to the heap (heap_write_int)
///
/// This function, `heap_write_int`, writes a 64-bit integer value to the heap. 
/// It takes a single argument:
///   - `x`: The integer value to be written.
///
/// It calls `heap_write` to write the value of `x` to the heap. The size of 
/// the data to be written is automatically determined using `sizeof(int64_t)`.
void heap_write_int(int64_t x) {
  heap_write(&x, sizeof(int64_t));
}

/// Function to write a short unsigned integer to the heap (heap_write_uint16)
///
/// (continued from previous comment)
///   - `x`: The 16-bit unsigned integer value to be written.
///
/// It follows the same logic as `heap_write_int` to write the value of `x` to 
/// the heap, using `sizeof(uint16_t)` to determine the size.
void heap_write_uint16(uint16_t x) {
  heap_write(&x, sizeof(uint16_t));
}

/// Function to read a pointer value from a chunk of data (read_ptr)
///
/// This function, `read_ptr`, is used to read a pointer value from a given chunk 
/// of data. It takes a single argument:
///   - `data`: A pointer to the memory location where the pointer value is stored.
///
/// Here's how `read_ptr` works:
///   1. It declares a local variable `ret` of type `uint8_t *`. This variable 
///      will be used to store the read pointer value.
///   2. It uses `memcpy` to copy the data from the memory location pointed to 
///      by `data` into the address of `ret`. The size of the data to be copied 
///      is `sizeof(uint8_t *)`, which ensures that enough bytes are copied to 
///      represent a pointer on the target architecture.
///   3. After copying, the function returns the value of `ret`, which now holds 
///      the pointer value that was read from the data.
uint8_t *read_ptr(uint8_t *data) {
  uint8_t *ret;
  memcpy(&ret, data, sizeof(uint8_t *));
  return ret;
}

/// Function to read a 64-bit integer from a chunk of data (read_int)
///
/// This function, `read_int`, is used to read a 64-bit integer value from a given 
/// chunk of data. It takes a single argument:
///   - `data`: A pointer to the memory location where the integer value is stored.
///
/// It follows the same logic as `read_ptr` but uses `sizeof(int64_t)` to determine 
/// the size of data to copy for an integer value.
int64_t read_int(uint8_t *data) {
  int64_t ret;
  memcpy(&ret, data, sizeof(int64_t));
  return ret;
}

/// Function to read a pointer to an InfoTable from a chunk of data (read_info_table)
///
/// This function, `read_info_table`, is used to read a pointer to an `InfoTable` 
/// structure from a given chunk of data. It takes a single argument:
///   - `data`: A pointer to the memory location where the InfoTable pointer is stored.
///
/// It follows the same logic as `read_ptr` but uses `sizeof(InfoTable *)` to determine 
/// the size of data to copy for an InfoTable pointer.
InfoTable *read_info_table(uint8_t *data) {
  InfoTable *ret;
  memcpy(&ret, data, sizeof(InfoTable *));
  return ret;
}


/// Heap growth factor (HEAP_GROWTH)
///
/// This constant, `HEAP_GROWTH`, is used during heap expansion. When the heap 
/// runs out of space and a new allocation needs to be made, the heap is resized 
/// by a factor of this constant. The initial value is set to `3.0`, which means 
/// the heap will triple in size whenever a growth is necessary. This value 
/// can be adjusted based on the expected memory usage patterns of the program. 
/// A higher growth factor can reduce the frequency of heap re-allocations but 
/// may also lead to more wasted memory. A lower growth factor can lead to 
/// more frequent re-allocations but may also improve memory utilization.
static double HEAP_GROWTH = 3;

/// Function to collect a single root during garbage collection (collect_root)
///
/// This function, `collect_root`, is  part of a garbage collection 
/// mechanism. It takes a single argument:
///   - `root`: A pointer to a pointer. This pointer points to the root object 
///     that needs to be processed during collection.
///
/// Here's how `collect_root` works:
///   1. It dereferences the `root` pointer once to get the actual pointer to 
///      the root object.
///   2. Then, it calls a function named `evac` on the dereferenced root object. 
///      The `evac` function is  responsible for performing some 
///      operations on the root object as part of the garbage collection process. 
///      The exact behavior of `evac` depends on the specific garbage collection 
///      implementation.
///   3. Finally, `collect_root` assigns the return value of the `evac` function 
///      back to the memory location pointed to by the original `root` pointer. 
///      This effectively updates the root pointer to point to the potentially 
///      modified version of the root object after garbage collection.
void collect_root(uint8_t **root) {
  *root = read_info_table(*root)->evac(*root);
}



/// Function to grow the heap and perform garbage collection (collect_garbage)
///
/// This function, `collect_garbage`, is triggered when the heap runs out of 
/// space and more memory is needed for allocation. It performs the following 
/// steps:
///
/// 1. **Heap Snapshot:**
///   - It creates a copy of the current global heap state by storing the values 
///     of all its members (`data`, `cursor`, and `capacity`) in a local variable 
///     of type `Heap` named `old`. This snapshot is used to track the old heap 
///     memory for later deallocation.
///
/// 2. **New Heap Capacity Calculation:**
///   - It calculates the new heap capacity based on two factors:
///     - `HEAP_GROWTH`: This constant defines a growth factor (initially 3.0). 
///       The new capacity will be the old capacity multiplied by this factor.
///     - `required_capacity`: This is the minimum required capacity to 
///       accommodate the current cursor position (`old.cursor`) relative to 
///       the start of the old heap (`old.data`) and any extra space (`extra_required`) 
///       needed for the new allocation that triggered the garbage collection.
///   - The function compares the calculated `new_capacity` with `required_capacity`. 
///     If `new_capacity` is less than `required_capacity`, it means the growth 
///     factor isn't enough to satisfy the immediate need. In that case, the 
///     `new_capacity` is simply set to `required_capacity` to ensure enough 
///     space for the current allocation.
///
/// 3. **New Heap Allocation:**
///   - It allocates a new contiguous block of memory for the grown heap using 
///     `malloc`. The size of the allocation is `new_capacity` multiplied by the 
///     size of a single `uint8_t` (usually 1 byte).
///   - It checks if the `malloc` call was successful. If it fails, the function 
///     panics by calling a function named `panic` ( indicating a critical 
///     error situation).
///   - If allocation is successful, it updates the global `g_Heap` structure 
///     to point to the newly allocated memory:
///     - `g_Heap.data`: Set to the starting address of the new heap memory.
///     - `g_Heap.cursor`: Set to the beginning of the new heap (no used space yet).
///     - `g_Heap.capacity`: Set to the newly calculated `new_capacity`.
///
/// 4. **Root Marking and Traversal (Partial Traversal):**
///   - The function then enters a marking and traversal phase to identify reachable 
///     objects (objects that can be still be accessed by the program) from the 
///     global roots. It performs this traversal partially, focusing on specific 
///     data structures that are  to hold references to heap objects.
///   - It checks and potentially updates the following global registers:
///     - `g_StringRegister`: If not `NULL`, it calls `collect_root` to process 
///       the string closure it points to.
///     - `g_NodeRegister`: Similar to `g_StringRegister`, it processes the closure 
///       pointed to by this register if not `NULL`.
///     - `g_ConstrUpdateRegister`: Similar processing for the constructor update 
///       closure if not `NULL`.
///   - It iterates over all elements in the `g_SA` data structure ( an 
///     argument stack) and calls `collect_root` for each element to process 
///     potential references to heap objects.
///   - It iterates over a linked list named `g_CAFListHead` ( containing 
///     closures) and calls `collect_root` for the `closure` member of each element 
///     in the list.
///   - It iterates over a nested stack structure represented by `g_SB` ( 
///     used for continuations). It traverses down the stack frames, calling 
///     `collect_root` on the `closure` member (at index 2) of each stack frame's 
///     base element.
///
/// 5. **Old Heap Deallocation:**
///   - Once the marking and traversal phase is complete, it's assumed that all 
///     reachable objects have been marked (identified) in the new heap. Any 
///     objects remaining in the old heap (`old.data`) are no longer reachable 
///     and considered garbage.
///   - The function calls `free` to deallocate the memory block pointed to by 
///     `old.data`, which was the starting address of the old heap before the 
///     growth. This effectively reclaims the memory used by the old heap that 
///     is no longer needed.
void collect_garbage(size_t extra_required) {
  Heap old = g_Heap;

  size_t new_capacity = HEAP_GROWTH * old.capacity;
  size_t required_capacity = old.cursor - old.data + extra_required;
  if (new_capacity < required_capacity) {
    new_capacity = required_capacity;
  }

  g_Heap.data = malloc(new_capacity * sizeof(uint8_t));
  if (g_Heap.data == NULL) {
    panic("Failed to allocate new heap during garbage collection");
  }
  g_Heap.cursor = g_Heap.data;
  g_Heap.capacity = new_capacity;

  if (g_StringRegister != NULL) {
    collect_root(&g_StringRegister);
  }
  if (g_NodeRegister != NULL) {
    collect_root(&g_NodeRegister);
  }
  if (g_ConstrUpdateRegister != NULL) {
    collect_root(&g_ConstrUpdateRegister);
  }

  for (uint8_t **p = g_SA.data; p < g_SA.top; ++p) {
    collect_root(p);
  }
  for (CAFCell *p = g_CAFListHead; p != NULL; p = p->next) {
    collect_root(&p->closure);
  }
  for (StackBItem *base = g_SB.base; base != g_SB.data;
       base = base[0].as_sb_base) {
    collect_root(&base[2].as_closure);
  }
  free(old.data);

  size_t necessary_size = g_Heap.cursor - g_Heap.data;
  size_t comfortable_size = HEAP_GROWTH * necessary_size;
  if (comfortable_size < g_Heap.capacity) {
    g_Heap.capacity = comfortable_size;
  }
  DEBUG_PRINT("GC Done. 0x%05X ↓ 0x%05X ↑ 0x%05X\n", old.capacity,
              necessary_size, g_Heap.capacity);
}
/// Function to reserve space in the heap (heap_reserve)
///
/// This function, `heap_reserve`, is used to reserve a certain amount of bytes 
/// in the global heap. It takes a single argument:
///   - `amount`: The number of bytes to be reserved in the heap.
///
/// Here's how `heap_reserve` works:
///   1. It checks if there's enough space available in the current heap to 
///      accommodate the requested `amount` of bytes. This check is done by 
///      comparing the sum of the current cursor position (`g_Heap.cursor`) and 
///      the requested `amount` with the total capacity (`g_Heap.capacity`) of 
///      the heap.
///   2. **Triggering Garbage Collection:**
///     - If the available space is not sufficient (i.e., `g_Heap.cursor + amount` 
///       is greater than `g_Heap.data + g_Heap.capacity`), it means there's not 
///       enough room for the requested allocation. In this case, the function 
///       calls `collect_garbage` to trigger garbage collection.
///     - The `collect_garbage` function will attempt to grow the heap by 
///       a factor of `HEAP_GROWTH` (usually 3), reclaim unused memory from 
///       unreachable objects, and then allocate a new heap with enough space 
///       to satisfy the original allocation request and any future allocations 
///       up to a certain limit.
///   3. **No Bounds Checking:**
///     - It's important to note that `heap_reserve` does **not** perform any 
///       bounds checking. This means it's the responsibility of the caller to 
///       ensure that the requested `amount` of bytes does not exceed the 
///       available space or the total capacity of the heap. Writing beyond 
///       the heap boundaries can lead to program crashes or security vulnerabilities.
///
void heap_reserve(size_t amount) {
  // We'd need to write beyond the capacity of our buffer
  if (g_Heap.cursor + amount > g_Heap.data + g_Heap.capacity) {
    collect_garbage(amount);
  }
}

/// Entry point for the black hole function (black_hole_entry)
///
/// This function, `black_hole_entry`,  serves as an entry point for 
/// a debugging or error handling mechanism. It doesn't take any arguments 
/// and returns a `void *` pointer (which is typically used for function pointers 
/// or generic pointers in C).
///
/// Here's what `black_hole_entry` does:
///   1. It prints a message "infinite loop detected" to the standard error 
///      stream (`stderr`). This message typically indicates that the program 
///      has encountered an infinite loop or some other unexpected condition.
///   2. It returns a `NULL` pointer.
///
void *black_hole_entry() {
  fputs("infinite loop detected\n", stderr);
  return NULL;
}

/// Evacuation function for the black hole (black_hole_evac)
///
/// This function, `black_hole_evac`, is  part of the garbage collection 
/// mechanism and specifically handles the evacuation process for objects 
/// associated with the "black hole". The black hole is  a debugging or 
/// error handling mechanism in this system.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the black hole object 
///     in the old heap.
///
/// Here's how `black_hole_evac` works:
///   1. **Allocate New Location:**
///     - It calls `heap_cursor` to get the current write position (`new_base`) 
///       in the heap. This essentially allocates a new memory location in 
///       the heap for the evacuated black hole object.
///   2. **Write Object Size:**
///     - It uses `heap_write` to write the total size of the black hole object 
///       data at the `base` location in the old heap. This size information 
///        includes the sizes of both the pointer to the entry function 
///       and the pointer to the evacuation function (explained later).
///   3. **Copy Evacuation Table Pointer:**
///     - It then uses `memcpy` to copy the address of a global variable named 
///       `table_pointer_for_already_evac` into the `base` location in the old heap. 
///       This pointer  points to a pre-defined table containing information 
///       about the already evacuated black hole object. The purpose of copying 
///       this pointer is to avoid redundant evacuation of the black hole during 
///       future garbage collection cycles.
///   4. **Copy New Base Pointer:**
///     - After copying the evacuation table pointer, it uses `memcpy` again 
///       to copy the value of the newly allocated `new_base` (the write position 
///       in the new heap) into the memory location `base + sizeof(InfoTable *)`. 
///       This effectively stores the new location of the evacuated black hole 
///       object within the old heap data structure.
///   5. **Return New Base:**
///     - Finally, the function returns the `new_base` pointer, which points 
///       to the newly allocated memory location in the heap where the black hole 
///       object has been evacuated.
///
uint8_t *black_hole_evac(uint8_t *base) {
  uint8_t *new_base = heap_cursor();
  heap_write(base, sizeof(InfoTable *) + sizeof(uint8_t *));
  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable*));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));
  return new_base;
}

/// Information table for the black hole (table_for_black_hole)
///
/// This variable, `table_for_black_hole`, is of type `InfoTable` and  
/// serves as a pre-defined table containing information about the black hole 
/// object. It's a global variable.
///
/// The table is initialized with two function pointers:
///   - `black_hole_entry`: This pointer points to the `black_hole_entry` 
///     function (explained earlier).
///   - `black_hole_evac`: This pointer points to the `black_hole_evac` function 
///     (explained just above).
///
/// By having a pre-defined table for the black hole, the garbage collector 
/// can efficiently identify and handle this special object during the 
/// evacuation process. The evacuation function (`black_hole_evac`) simply 
/// copies the pointer to this pre-defined table instead of performing any 
/// complex logic for the black hole object itself.
InfoTable table_for_black_hole = {&black_hole_entry, &black_hole_evac};


/// Function to concatenate two strings (string_concat)
///
/// This function, `string_concat`, takes two string pointers (`s1` and `s2`) 
/// as arguments and concatenates them into a new string. It returns a pointer 
/// to the newly created string in the heap.
///
/// Here's how `string_concat` works:
///   1. **Extract String Data Pointers:**
///     - It first extracts the actual string data pointers from the provided 
///       `s1` and `s2` pointers. This is done because the initial pointers 
///        point to `InfoTable` structures at the beginning of the string 
///       objects in the heap. The function uses pointer arithmetic to skip 
///       over the `sizeof(InfoTable *)` bytes to access the actual string data 
///       starting at `data1` and `data2`.
///   2. **Calculate String Lengths:**
///     - It then uses `strlen` (assuming it's a function from a standard library 
///       like `string.h`) to calculate the lengths (`len1` and `len2`) of the 
///       two input strings.
///   3. **Calculate Required Space:**
///     - It calculates the total space required (`required`) for the new 
///       concatenated string. This includes:
///       - `sizeof(InfoTable *)`: Space for the pointer to the `InfoTable` 
///         structure ( containing information about the string object).
///       - `len1`: Length of the first string.
///       - `len2`: Length of the second string.
///       - `+ 1`: Additional byte for the null terminator at the end of the 
///         concatenated string.
///   4. **Minimum Size Check (Optional):**
///     - The code includes an optional check for a minimum size (`min_size`). 
///       The purpose and exact value of `min_size` are unclear without more 
///       context. It might be a pre-defined minimum allocation size for strings 
///       in the system.
///     - If `required` is less than `min_size`, an extra space (`extra`) is 
///       calculated and added to `required` to ensure the string has enough 
///       space for potential relocation during garbage collection.
///   5. **Trigger Garbage Collection (if needed):**
///     - The function checks if there's enough space available in the current 
///       heap to accommodate the newly calculated `required` space for the 
///       concatenated string. It performs this check using the same logic as 
///       `heap_reserve`:
///       - If `g_Heap.cursor + required` is greater than `g_Heap.data + 
///         g_Heap.capacity`, it means there's not enough space.
///       - In this case, the function takes the following steps:
///         - **Push strings onto stack (roots for GC):** It pushes both 
///           original strings (`s1` and `s2`) onto the `g_SA` stack ( 
///           an argument stack). This ensures that the strings are treated 
///           as roots during garbage collection, preventing them from being 
///           marked as unreachable and potentially deallocated.
///         - **Call garbage collection:** It calls `collect_garbage` with 
///           `required` as the extra space argument to trigger garbage 
///           collection and potentially grow the heap.
///         - **Update string data pointers:** After garbage collection, 
///           the function assumes the strings might have been relocated. 
///           It retrieves the updated string data pointers (`data1` and `data2`) 
///           from the top of the `g_SA` stack and adjusts the pointers 
///           accordingly (skipping the `InfoTable *` at the beginning).
///           - It also pops both strings from the stack as they are no 
///             longer needed as explicit roots.
///   6. **Allocate New String in Heap:**
///     - Once there's enough space in the heap, the function assigns the 
///       current heap cursor (`g_Heap.cursor`) to a local variable `ret`. 
///       This `ret` pointer will hold the address of the newly allocated 
///       string in the heap.
///   7. **Write String Data:**
///     - The function then performs several `memcpy` operations to write 
///       data into the newly allocated string space in the heap:
///       - It copies the pointer to the `table_pointer_for_string` (which 
///          points to a pre-defined `InfoTable` for strings) at the 
///         beginning of the allocated space (`g_Heap.cursor`).
///       - It increments the cursor (`g_Heap.cursor`) by `sizeof(InfoTable *)` 
///         to move past the copied pointer.
///       - It copies the data from the first string (`data1`) into the heap 
///         using `memcpy`. The size of the copy is `len1` bytes, which is the 
///         length of the first string.
///       - It increments the cursor (`g_Heap.cursor`) by `len1` to move 
///         past the copied data from the first string.
///       - It copies the data from the second string (`data2`) into the heap 
///         using `memcpy`. The size of the copy is `len2 + 1` bytes, which 
///         includes the length of the second string (`len2`) and the null 
///         terminator (`\0`) at the end.
///       - It increments the cursor (`g_Heap.cursor`) by `len2 + 1` to move 
///         past the copied data from the second string.
///       - Finally, it increments the cursor (`g_Heap.cursor`) by `extra` 
///         (if applicable) to account for any additional space allocated 
///         for potential relocation during garbage collection.
///   8. **Return New String Pointer:**
///     - The function returns the `ret` pointer, which points to the 
///       beginning of the newly created and concatenated string in the heap.
///
/// Note:
///   - This function might trigger garbage collection if there's not enough 
///     space in the heap to accommodate the concatenated string. It achieves 
///     this by pushing the original strings onto the `g_SA` stack as roots 
///     before calling `collect_garbage`.
uint8_t *string_concat(uint8_t *s1, uint8_t *s2) {
  uint8_t *data1 = s1 + sizeof(InfoTable *);
  uint8_t *data2 = s2 + sizeof(InfoTable *);
  size_t len1 = strlen((char *)data1);
  size_t len2 = strlen((char *)data2);

  size_t required = sizeof(InfoTable *) + len1 + len2 + 1;
  size_t min_size = sizeof(InfoTable *) + sizeof(uint8_t *);
  size_t extra = 0;
  if (required < min_size) {
    extra = min_size - required;
    required += extra;
  }
  if (g_Heap.cursor + required > g_Heap.data + g_Heap.capacity) {
    g_SA.top[0] = s1;
    g_SA.top[1] = s2;
    g_SA.top += 2;

    collect_garbage(required);

    data2 = g_SA.top[-1] + sizeof(InfoTable *);
    data1 = g_SA.top[-2] + sizeof(InfoTable *);
    g_SA.top -= 2;
  }

  uint8_t *ret = g_Heap.cursor;

  memcpy(g_Heap.cursor, &table_pointer_for_string, sizeof(InfoTable *));
  g_Heap.cursor += sizeof(InfoTable *);
  memcpy(g_Heap.cursor, data1, len1);
  g_Heap.cursor += len1;
  memcpy(g_Heap.cursor, data2, len2 + 1);
  g_Heap.cursor += len2 + 1;
  g_Heap.cursor += extra;

  return ret;
}

/// Evacuation function for strings (string_evac)
///
/// This function, `string_evac`, is  part of the garbage collection 
/// mechanism and specifically handles the evacuation process for string objects.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the string object 
///     in the old heap.
///
/// Here's how `string_evac` works:
///   1. **Allocate New Location:**
///     - It calls `heap_cursor` to get the current write position (`new_base`) 
///       in the heap. This essentially allocates a new memory location in 
///       the heap for the evacuated string object.
///   2. **Calculate String Length (Excluding Null Terminator):**
///     - It calculates the length (`bytes`) of the string data pointed to by 
///       `base + sizeof(InfoTable *)`. This calculation uses `strlen` 
///       (assuming it's a function from a standard library like `string.h`), 
///       but it's important to note that it excludes the null terminator (`\0`) 
///       from the length calculation.
///   3. **Write String Size:**
///     - It uses `heap_write` to write the total size of the evacuated string 
///       data at the `base` location in the old heap. This size includes:
///       - `sizeof(InfoTable *)`: Space for the pointer to the `InfoTable` 
///         structure ( containing information about the string object).
///       - `bytes`: The calculated length of the string data (excluding null 
///         terminator).
///   4. **Ensure Space for Relocation Pointer (if needed):**
///     - The code performs a check to ensure there's enough space in the old 
///       heap location (`base`) to store the relocation pointer (size of 
///       `uint8_t *`). If the string data is very short (`bytes < sizeof(uint8_t *)`), 
///       there might not be enough space. In this case, it adjusts the heap 
///       cursor (`g_Heap.cursor`) by `sizeof(uint8_t *) - bytes` to effectively 
///       allocate the missing space for the relocation pointer.
///   5. **Copy Evacuation Table Pointer:**
///     - It then uses `memcpy` to copy the address of a global variable named 
///       `table_pointer_for_already_evac` into the `base` location in the old heap. 
///       This pointer  points to a pre-defined table containing information 
///       about already evacuated string objects. The purpose of copying this 
///       pointer is to avoid redundant evacuation of the string during future 
///       garbage collection cycles.
///   6. **Copy New Base Pointer:**
///     - After copying the evacuation table pointer, it uses `memcpy` again 
///       to copy the value of the newly allocated `new_base` (the write position 
///       in the new heap) into the memory location `base + sizeof(InfoTable *)`. 
///       This effectively stores the new location of the evacuated string object 
///       within the old heap data structure.
///   7. **Return New Base:**
///     - Finally, the function returns the `new_base` pointer, which points 
///       to the newly allocated memory location in the heap where the string 
///       object has been evacuated.
uint8_t *string_evac(uint8_t *base) {
  uint8_t *new_base = heap_cursor();
  size_t bytes = strlen((char *)(base + sizeof(InfoTable *))) + 1;
  heap_write(base, sizeof(InfoTable *) + bytes);
  if (bytes < sizeof(uint8_t *)) {
    g_Heap.cursor += sizeof(uint8_t *) - bytes;
  }
  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable *));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));
  return new_base;
}

/// Save the current contents of the B stack (save_SB)
///
/// This function, `save_SB`,  saves the current state of the B stack. 
/// The B stack is probably an internal data structure used by the system, 
/// and the specific details of its contents might be implementation-dependent.
///
/// Here's how `save_SB` works:
///   1. **Store Current B Stack Base:**
///     - It accesses the first element (`g_SB.top[0]`) of the SB stack (which 
///       might be an array) and assigns the current base address (`g_SB.base`) 
///       of the B stack to a member named `as_sb_base` within that element. 
///       This effectively captures the current "bottom" of the B stack.
///   2. **Update B Stack Base:**
///     - It then updates the `g_SB.base` pointer to point to the current 
///       position of the SB stack's `top`. This essentially moves the "bottom" 
///       of the B stack one element higher, effectively pushing a new frame 
///       onto the stack.
///   3. **Increment Top Pointer:**
///     - Finally, it increments the `g_SB.top` pointer by 1. This pointer 
///        keeps track of the next available slot at the "top" of the 
///       SB stack.
///
/// In summary, `save_SB` pushes a new frame onto the B stack, where the 
/// first element of the frame stores the previous base address of the B stack.
void save_SB() {
  g_SB.top[0].as_sb_base = g_SB.base;
  g_SB.base = g_SB.top;
  ++g_SB.top;
}


/// Save the current contents of the A stack (save_SA)
///
/// This function, `save_SA`,  works similarly to `save_SB` but for 
/// a different internal data structure called the A stack. The A stack is 
/// probably another internal stack used by the system.
///
/// Here's a breakdown of `save_SA` based on the similarities with `save_SB`:
///   1. **Store Current A Stack Base:**
///     - It  assigns the current base address of the A stack (`g_SA.base`) 
///       to a member named `as_sa_base` within the first element (`g_SB.top[0]`) 
///       of the SB stack. This captures the current "bottom" of the A stack.
///   2. **Update A Stack Base:**
///     - It  updates the `g_SA.base` pointer to point to the current 
///       position of the A stack's `top`. This moves the "bottom" of the A stack 
///       one element higher, effectively pushing a new frame onto the stack.
///   3. **Increment Top Pointer:**
///     - It  increments the `g_SB.top` pointer by 1 to keep track of the 
///       next available slot at the "top" of the A stack.
///
/// In summary, `save_SA`  pushes a new frame onto the A stack, where 
/// the first element of the frame stores the previous base address of the 
/// A stack.  However, due to the limited information and the use of `g_SB.top` 
/// for both stacks, there might be a more intricate relationship between 
/// the A and B stacks in this system.
void save_SA() {
  g_SB.top[0].as_sa_base = g_SA.base;
  g_SA.base = g_SA.top;
  ++g_SB.top;
}

/// Entry function for partial applications (partial_application_entry)
///
/// This function, `partial_application_entry`, serves as the entry point 
/// for handling partial function applications. It retrieves information about 
/// the partial application from memory and jumps to the actual function 
/// to be applied.
///
/// Here's a breakdown of what `partial_application_entry` does:
///   1. **Print Debug Message (optional):**
///     - The code includes a `DEBUG_PRINT` macro ( not shown here) 
///       that conditionally prints the function name for debugging purposes.
///   2. **Extract Information from Node Register:**
///     - A pointer `cursor` is initialized to point to the `g_NodeRegister` 
///       plus the size of an `InfoTable` structure. This suggests that 
///       the information about the partial application is stored in a specific 
///       memory layout following an `InfoTable`.
///     - The function then extracts the following information from memory 
///       using `memcpy`:
///       - `ret`: A `CodeLabel` structure,  containing the return 
///         address of the original function to be applied.
///       - `b_items`: A `uint16_t` value representing the number of arguments 
///         from the "B stack" ( another internal stack) that need to 
///         be restored for the partial application.
///       - `a_items`: A `uint16_t` value representing the number of arguments 
///         from the "A stack" (another internal stack) that need to be 
///         restored for the partial application.
///     - After each `memcpy`, the `cursor` is incremented to point to the 
///       next piece of information in memory.
///   3. **Restore Stack Arguments:**
///     - The function calculates the size (`b_size`) required to store the 
///       `b_items` arguments from the B stack. It then uses `memcpy` to copy 
///       `b_size` bytes of data from memory pointed to by `cursor` to the 
///       current "top" of the `g_SB` stack (B stack). It then increments 
///       `g_SB.top` by `b_items` to update the stack pointer.
///     - Similarly, it calculates the size (`a_size`) required to store the 
///       `a_items` arguments from the A stack and uses `memcpy` to copy them 
///       from memory to the current "top" of the `g_SA` stack (A stack), 
///       updating the `g_SA.top` pointer accordingly.
///   4. **Jump to Saved Function:**
///     - Finally, the function returns the value of the `ret` variable, 
///       which is  the return address of the original function to be 
///       applied with the restored arguments from the stacks. This effectively 
///       transfers control to the actual function for execution.
///
/// In summary, `partial_application_entry` acts as a dispatcher for 
/// partial function applications. It retrieves relevant information from 
/// memory, restores arguments from internal stacks, and jumps to the 
/// original function for execution. 
void *partial_application_entry() {
  DEBUG_PRINT("%s\n", __func__);
  uint8_t *cursor = g_NodeRegister + sizeof(InfoTable *);

  CodeLabel ret;
  memcpy(&ret, cursor, sizeof(CodeLabel));
  cursor += sizeof(CodeLabel);

  uint16_t b_items;
  memcpy(&b_items, cursor, sizeof(uint16_t));
  cursor += sizeof(uint16_t);
  uint16_t a_items;
  memcpy(&a_items, cursor, sizeof(uint16_t));
  cursor += sizeof(uint16_t);

  size_t b_size = b_items * sizeof(StackBItem);
  memcpy(g_SB.top, cursor, b_size);
  g_SB.top += b_items;
  cursor += b_size;
  size_t a_size = a_items * sizeof(uint8_t *);
  memcpy(g_SA.top, cursor, a_size);
  g_SA.top += a_items;

  return ret;
}

/// Evacuation function for a partial application (partial_application_evac)
///
/// This function, `partial_application_evac`, is  part of the garbage collection 
/// mechanism and specifically handles the evacuation process for partial application 
/// objects. A partial application is a technique where a function is applied with 
/// some arguments fixed beforehand, creating a new function object that takes 
/// fewer arguments.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the partial application 
///     object in the old heap.
///
/// Here's how `partial_application_evac` works:
///   1. **Extract Information and Calculate Sizes:**
///     - It first calculates a pointer `items_base` that points to the location 
///       where the number of arguments are stored within the partial application 
///       object. This skips the `InfoTable *` pointer and the `CodeLabel` structure 
///       at the beginning.
///     - It then uses `memcpy` to extract the number of arguments from both 
///       the B stack (`b_items`) and the A stack (`a_items`) into separate variables.
///     - Based on the retrieved argument counts, it calculates the sizes required 
///       for the B stack arguments (`b_size`) and the A stack arguments (`a_size`).
///   2. **Allocate New Location and Write Size:**
///     - The function calls `heap_cursor` to get the current write position 
///       (`new_base`) in the heap. This essentially allocates a new memory location 
///       in the heap for the evacuated partial application object.
///     - It calculates the total size (`total_size`) of the evacuated object, 
///       which includes:
///       - `sizeof(InfoTable *)`: Space for the pointer to the `InfoTable` 
///         structure ( containing information about the partial application).
///       - `sizeof(CodeLabel)`: Space for the `CodeLabel` structure,  
///         storing the return address of the original function.
///       - `2 * sizeof(uint16_t)`: Space for the two `uint16_t` variables 
///         storing the number of arguments from both stacks.
///       - `b_size`: Size required to store the B stack arguments.
///       - `a_size`: Size required to store the A stack arguments.
///     - It then uses `heap_write` to write the `total_size` to the base location 
///       in the old heap, indicating the total size of the evacuated data.
///   3. **Write Evacuation Indirection:**
///     - Similar to other evacuation functions, it replaces the original data 
///       at the base location in the old heap. Here, it copies the address of 
///       `table_pointer_for_already_evac` ( a pre-defined table 
///       containing information about already evacuated objects) into the base 
///       location. This avoids redundant evacuation during future garbage 
///       collection cycles.
///     - Then, it writes the address of the newly allocated memory (`new_base`) 
///       into the location following the pointer to the `InfoTable *` structure 
///       in the old heap. This effectively stores the new location of the 
///       evacuated partial application object.
///   4. **Recursive Root Collection (optional):**
///     - The code then performs a loop that iterates over the A stack arguments 
///       in the newly allocated memory (starting from the end). For each iteration:
///       - It uses `memcpy` to copy the value of a pointer to a potential root 
///         object stored in the current A stack argument location.
///       - It calls `collect_root` (assumed to be a separate function for 
///         recursive garbage collection) to handle this potential root object 
///         and its reachable objects. This ensures proper handling of any objects 
///         referenced by the partial application's arguments.
///       - After the recursive collection, it writes the potentially updated 
///         pointer value back into the A stack argument location in the new heap.
///   5. **Return New Base:**
///     - Finally, the function returns the `new_base` pointer, which points 
///       to the newly allocated memory location in the heap where the partial 
///       application object has been evacuated.
uint8_t *partial_application_evac(uint8_t *base) {
  uint8_t *items_base = base + sizeof(InfoTable *) + sizeof(CodeLabel);

  uint16_t b_items;
  memcpy(&b_items, items_base, sizeof(uint16_t));
  size_t b_size = b_items * sizeof(StackBItem);
  uint16_t a_items;
  memcpy(&a_items, items_base + sizeof(uint16_t), sizeof(uint16_t));
  size_t a_size = a_items * sizeof(uint8_t *);

  size_t total_size = sizeof(InfoTable *) + sizeof(CodeLabel) +
                      2 * sizeof(uint16_t) + b_size + a_size;
  uint8_t *new_base = heap_cursor();
  heap_write(base, total_size);
  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable *));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));

  for (uint8_t *cursor = new_base + total_size - a_size;
       cursor < new_base + total_size; cursor += sizeof(uint8_t *)) {
    uint8_t *root;
    memcpy(&root, cursor, sizeof(uint8_t *));
    collect_root(&root);
    memcpy(cursor, &root, sizeof(uint8_t *));
  }

  return new_base;
}

/// Table for partial application closures (table_for_partial_application)
///
/// This variable, `table_for_partial_application`, defines an `InfoTable` 
/// structure specifically used for partial application closures. An 
/// `InfoTable` structure  contains information about different types 
/// of objects in the system. In this case, the `table_for_partial_application` 
/// provides function pointers for the entry point (`partial_application_entry`) 
/// and the evacuation function (`partial_application_evac`) used for handling 
/// partial application closures during garbage collection.
///
/// The `InfoTable` structure  has the following members:
///   - `entry`: Function pointer to the entry point of the object type.
///   - `evac`: Function pointer to the evacuation function of the object type.
InfoTable table_for_partial_application = {&partial_application_entry,
                                           &partial_application_evac};

/// Entry function for an indirection (indirection_entry)
///
/// This function, `indirection_entry`, serves as the entry point for 
/// handling indirection objects. Indirection objects are  a type of 
/// object that simply forwards execution to another object being pointed to.
///
/// Here's a breakdown of what `indirection_entry` does:
///   1. **Print Debug Message (optional):**
///     - The code includes a `DEBUG_PRINT` macro ( not shown here) 
///       that conditionally prints the function name for debugging purposes.
///   2. **Read Pointed-to InfoTable:**
///     - It first reads the memory location pointed to by `g_NodeRegister` 
///       after skipping the size of an `InfoTable *` structure. This effectively 
///       retrieves the address of the object being indirectly referenced.
///     - It then calls `read_info_table` (assumed to be a separate function) 
///       to obtain the `InfoTable` structure associated with the pointed-to 
///       object.
///   3. **Jump to Pointed-to Entry Function:**
///     - Finally, it extracts the `entry` function pointer from the retrieved 
///       `InfoTable` structure and returns that pointer value. This effectively 
///       transfers control to the actual entry function of the pointed-to object.
///
/// In essence, `indirection_entry` acts as a trampoline, reading the 
/// information about the target object and jumping to its entry function 
/// for further processing.
void *indirection_entry() {
  DEBUG_PRINT("%s\n", __func__);
  g_NodeRegister = read_ptr(g_NodeRegister + sizeof(InfoTable *));
  return read_info_table(g_NodeRegister)->entry;
}

/// Evacuation function for an indirection (indirection_evac)
///
/// This function, `indirection_evac`, handles the evacuation process for 
/// indirection objects during garbage collection. Indirection objects 
/// are  a type of object that simply forwards execution to another 
/// object being pointed to.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the indirection object 
///     in the old heap.
///
/// Here's how `indirection_evac` works:
///   1. **Read Pointed-to Closure:**
///     - It first reads the memory location pointed to by `base` after 
///       skipping the size of an `InfoTable *` structure. This effectively 
///       retrieves the address of the closure object being indirectly referenced.
///     - It then calls `read_ptr` to dereference the pointer and obtain the 
///       actual closure object.
///   2. **Evacuate Pointed-to Closure:**
///     - It calls `read_info_table` (assumed to be a separate function) to 
///       obtain the `InfoTable` structure associated with the closure object.
///     - Then, it extracts the `evac` function pointer from the retrieved 
///       `InfoTable` structure and calls that function with the closure object 
///       itself (`closure`) as an argument. This effectively delegates the 
///       evacuation process to the specific evacuation function designed for 
///       the closure object being pointed to.
///     - The evacuation function pointed to by `evac` ( a function 
///       specific to the closure type) is responsible for evacuating the closure 
///       object to a new location in the heap and returning the new base pointer.
///   3. **Write Evacuation Indirection:**
///     - Similar to other evacuation functions, it replaces the original data 
///       at the base location in the old heap. Here, it copies the address of 
///       `table_pointer_for_already_evac` ( a pre-defined table containing 
///       information about already evacuated objects) into the base location. 
///       This avoids redundant evacuation during future garbage collection 
///       cycles.
///     - Then, it writes the address returned by the closure's evacuation 
///       function (`new_base`) into the location following the pointer to the 
///       `InfoTable *` structure in the old heap. This effectively replaces the 
///       original pointer to the closure object with the pointer to the evacuated 
///       closure in the new heap.
///   4. **Return New Base:**
///     - Finally, the function returns the `new_base` pointer, which was 
///       obtained from the evacuation function of the pointed-to closure object. 
///       This returned pointer points to the newly allocated memory location in 
///       the heap where the closure object has been evacuated.
///
/// In essence, `indirection_evac` avoids creating a new indirection object 
/// during evacuation. Instead, it relies on the evacuation function of the 
/// pointed-to closure to handle the evacuation process and returns the new 
/// location from the closure's evacuation.
///
uint8_t *indirection_evac(uint8_t *base) {
  uint8_t *closure = read_ptr(base + sizeof(InfoTable *));
  uint8_t *new_base = read_info_table(closure)->evac(closure);
  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable *));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));
  return new_base;
}

/// Table for indirection closures (table_for_indirection)
///
/// This variable, `table_for_indirection`, defines an `InfoTable` structure 
/// specifically used for indirection closures. An `InfoTable` structure  
/// contains information about different types of objects in the system. In this 
/// case, the `table_for_indirection` provides function pointers for the entry 
/// point (`indirection_entry`) and the evacuation function (`indirection_evac`) 
/// used for handling indirection closures during garbage collection.
///
/// The `InfoTable` structure  has the following members:
///   - `entry`: Function pointer to the entry point of the object type.
///   - `evac`: Function pointer to the evacuation function of the object type.
///
InfoTable table_for_indirection = {&indirection_entry, &indirection_evac};

/// Global pointer to the indirection table (table_pointer_for_indirection)
///
/// This variable, `table_pointer_for_indirection`, is a global pointer that 
/// always points to the `table_for_indirection` structure defined above. It 
/// provides a way to access the function pointers for indirection closures 
/// from anywhere in the code.
InfoTable *table_pointer_for_indirection = &table_for_indirection;

/// Table for CAF cells (table_for_caf_cell)
///
/// This variable, `table_for_caf_cell`, defines an `InfoTable` structure 
///  used for Constant Application Form (CAF) cells. CAF cells are 
/// possibly a specific type of object that represent constant function 
/// applications in some way.
///
/// Here, the `table_for_caf_cell` uses the same function pointers as 
/// `table_for_indirection` for both the entry point (`indirection_entry`) 
/// and the evacuation function (`static_evac`). This suggests that CAF cells 
/// might be treated similarly to indirection objects during garbage collection. 
/// However, it's also possible that `static_evac` is a placeholder and there's 
/// a separate evacuation function specifically designed for CAF cells.
InfoTable table_for_caf_cell = {&indirection_entry, &static_evac};

/// Handler for encountering an update frame instead of a case continuation (update_constructor)
///
/// This function, `update_constructor`, handles a scenario where the garbage collector 
/// encounters an update frame on the stack while expecting a case continuation 
/// during the evaluation process. This  indicates an error or unexpected 
/// situation.
///
/// Here's a breakdown of what `update_constructor` does:
///   1. **Adjust Stack Pointers:**
///     - It subtracts 4 from `g_SB.top`, effectively removing the topmost 
///       part of the update frame from the B stack. This is  because 
///       the topmost part contains information that is no longer needed.
///   2. **Extract Closure:**
///     - It retrieves the closure pointer from the third element (from the top) 
///       of the B stack and stores it in a local variable `closure`. The closure 
///        contains information about the continuation or the function 
///       being applied.
///   3. **Handle Existing Updating Thunk (optional):**
///     - It checks if a pointer to an existing "updating thunk" is stored in 
///       `g_ConstrUpdateRegister`. An updating thunk is a specific type of 
///       continuation that might be used during case expressions in tinyHaskell.
///     - If `g_ConstrUpdateRegister` is not NULL, it means an updating thunk 
///       already exists. In this case, the function sets up the closure to 
///       indirectly jump to the existing updating thunk using the 
///       `table_pointer_for_indirection` table. It copies the pointer to 
///       `table_pointer_for_indirection` and then the address stored in 
///       `g_ConstrUpdateRegister` into the closure memory. This effectively 
///       redirects the continuation to the existing updating thunk.
///     - If `g_ConstrUpdateRegister` is NULL, it means there's no existing 
///       updating thunk. In this case, the function simply stores the closure 
///       pointer itself in `g_ConstrUpdateRegister` for potential future use.
///   4. **Restore Stack Bases:**
///     - It updates the `g_SA.base` pointer to point to the saved A stack base 
///       stored in the second element (from the top) of the B stack. This 
///       restores the A stack to the state before the update frame was pushed.
///     - Similarly, it updates the `g_SB.base` pointer to point to the saved 
///       B stack base stored in the first element (from the top) of the B stack. 
///       This restores the B stack to the state before the update frame was 
///       pushed.
///   5. **Return Code Pointer:**
///     - Finally, the function returns the code pointer stored in the topmost 
///       element of the B stack. This code pointer  represents the 
///       continuation that should be executed after handling the unexpected 
///       update frame.
///
/// In essence, `update_constructor` acts as a recovery mechanism when an 
/// update frame is encountered instead of a case continuation. It adjusts 
/// stack pointers, handles existing updating thunks (if any), and returns 
/// the appropriate code pointer to resume execution. 
void *update_constructor() {
  g_SB.top -= 4;

  uint8_t *closure = g_SB.top[3].as_closure;
  if (g_ConstrUpdateRegister != NULL) {
    memcpy(closure, &table_pointer_for_indirection, sizeof(InfoTable *));
    memcpy(closure + sizeof(InfoTable *), &g_ConstrUpdateRegister,
           sizeof(uint8_t *));
  } else {
    g_ConstrUpdateRegister = closure;
  }
  g_SA.base = g_SB.top[2].as_sa_base;
  g_SB.base = g_SB.top[1].as_sb_base;
  return g_SB.top[0].as_code;
}

/// Entry function for the "with_int" constructor (with_int_entry)
///
/// This function, `with_int_entry`, serves as the entry point for handling 
/// the `with_int` constructor during evaluation. The `with_int` constructor 
/// is  a specific tinyHaskell construct that deals with integers.
///
/// Here's a breakdown of what `with_int_entry` does:
///   1. **Print Debug Message (optional):**
///     - The code includes a `DEBUG_PRINT` macro ( not shown here) 
///       that conditionally prints the function name for debugging purposes.
///   2. **Extract Integer Value:**
///     - It skips the `InfoTable *` pointer at the beginning and uses 
///       `read_int` (assumed to be a separate function) to read the integer 
///       value stored in the object pointed to by `g_NodeRegister`. This integer 
///       value is  an argument passed to the `with_int` constructor.
///     - The function stores the extracted integer value in the global 
///       `g_IntRegister` variable, which might be used later during evaluation.
///   3. **Discard Stack Frame and Return Code Pointer:**
///     - It decrements `g_SB.top` by 1, effectively discarding the stack frame 
///       associated with the `with_int` constructor on the B stack.
///     - Finally, it returns the code pointer stored in the topmost element 
///       of the B stack. This code pointer  represents the continuation 
///       that should be executed after processing the integer value from the 
///       `with_int` constructor.
void *with_int_entry() {
  DEBUG_PRINT("%s\n", __func__);
  g_IntRegister = read_int(g_NodeRegister + sizeof(InfoTable *));
  --g_SB.top;
  return g_SB.top[0].as_code;
}

/// Evacuation function for the "with_int" constructor (with_int_evac)
///
/// This function, `with_int_evac`, handles the evacuation process for the 
/// `with_int` constructor during garbage collection.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the `with_int` object 
///     in the old heap.
///
/// Here's how `with_int_evac` works:
///   1. **Allocate New Location:**
///     - It calls `heap_cursor` to get the current write position (`new_base`) 
///       in the heap. This essentially allocates a new memory location in 
///       the heap for the evacuated `with_int` object.
///     - It calculates the total size (`sizeof(InfoTable *) + sizeof(int64_t)`) 
///       required for the evacuated object, which includes space for the 
///       `InfoTable *` pointer and the integer value.
///     - It calls `heap_write` to write this total size to the base location 
///       in the old heap, indicating the size of the evacuated data.
///   2. **Write Evacuation Indirection:**
///     - Similar to other evacuation functions, it replaces the original data 
///       at the base location in the old heap. Here, it copies the address of 
///       `table_pointer_for_already_evac` ( a pre-defined table containing 
///       information about already evacuated objects) into the base location. 
///       This avoids redundant evacuation during future garbage collection 
///       cycles.
///     - Then, it writes the address of the newly allocated memory (`new_base`) 
///       into the location following the pointer to the `InfoTable *` structure 
///       in the old heap. This effectively stores the new location of the 
///       evacuated `with_int` object.
///   3. **Return New Base:**
///     - Finally, the function returns the `new_base` pointer, which points 
///       to the newly allocated memory location in the heap where the 
///       `with_int` object has been evacuated.
uint8_t *with_int_evac(uint8_t *base) {
  uint8_t *new_base = heap_cursor();
  heap_write(base, sizeof(InfoTable *) + sizeof(int64_t));

  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable *));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));
  return new_base;
}

/// Table for the "with_int" constructor (table_for_with_int)
///
/// This variable, `table_for_with_int`, defines an `InfoTable` structure 
/// specifically used for the `with_int` constructor. An `InfoTable` structure 
///  contains information about different types of objects in the system. 
/// In this case, the `table_for_with_int` provides function pointers for the 
/// entry point (`with_int_entry`) and the evacuation function (`with_int_evac`) 
/// used for handling the `with_int` constructor during evaluation and garbage 
/// collection.
///
/// The `InfoTable` structure  has the following members:
///   - `entry`: Function pointer to the entry point of the object type.
///   - `evac`: Function pointer to the evacuation function of the object type.
///
InfoTable table_for_with_int = {&with_int_entry, &with_int_evac};


/// Function to update the state for the "with_int" constructor (update_with_int)
///
/// This function, `update_with_int`,  performs an update operation 
/// specifically related to the `with_int` constructor. The exact functionality 
/// might depend on the context of how the `with_int` constructor is used in 
/// tinyHaskell.
///
/// Here's a breakdown of what `update_with_int` does:
///   1. **Get Table Pointer:**
///     - It creates a local variable `table` and assigns the address of 
///       `table_for_with_int` to it. This effectively retrieves the 
///       `InfoTable` structure associated with the `with_int` constructor.
///   2. **Store Table Pointer in Register:**
///     - It copies the address of the `table` (which points to the 
///       `InfoTable` structure) into the `g_ConstrUpdateRegister` location. 
///       The `g_ConstrUpdateRegister`  stores information about the 
///       current update state, and this might be used to remember how to 
///       handle the `with_int` constructor later during evaluation.
///   3. **Store Integer Value in Register:**
///     - It copies the value of the global `g_IntRegister` into the memory 
///       location following the `InfoTable *` pointer stored in 
///       `g_ConstrUpdateRegister`. This effectively stores the integer value 
///       ( extracted from the `with_int` constructor earlier) along 
///       with the `InfoTable` pointer in the update register.
///
/// In essence, `update_with_int` updates the `g_ConstrUpdateRegister` with 
/// information about the `with_int` constructor, including the relevant 
/// `InfoTable` pointer and the integer value. This information is  
/// used later during evaluation to continue processing the `with_int` 
/// constructor.
///
void update_with_int() {
  InfoTable *table = &table_for_with_int;
  memcpy(g_ConstrUpdateRegister, &table, sizeof(InfoTable *));
  memcpy(g_ConstrUpdateRegister + sizeof(InfoTable *), &g_IntRegister,
         sizeof(int64_t));
}

/// Entry function for the "with_string" constructor (with_string_entry)
///
/// This function, `with_string_entry`, serves as the entry point for handling 
/// the `with_string` constructor during evaluation. The `with_string` 
/// constructor is  a specific tinyHaskell construct that deals with strings.
///
/// Here's a breakdown of what `with_string_entry` does:
///   1. **Print Debug Message (optional):**
///     - The code includes a `DEBUG_PRINT` macro ( not shown here) 
///       that conditionally prints the function name for debugging purposes.
///   2. **Extract String Pointer:**
///     - It skips the `InfoTable *` pointer at the beginning and uses 
///       `read_ptr` (assumed to be a separate function) to read a pointer 
///       to the string data stored in the object pointed to by `g_NodeRegister`. 
///       This pointer  points to the actual string characters in memory.
///     - The function stores the extracted string pointer in the global 
///       `g_StringRegister` variable, which might be used later during 
///       evaluation.
///   3. **Discard Stack Frame and Return Code Pointer:**
///     - It decrements `g_SB.top` by 1, effectively discarding the stack frame 
///       associated with the `with_string` constructor on the B stack.
///     - Finally, it returns the code pointer stored in the topmost element 
///       of the B stack. This code pointer  represents the continuation 
///       that should be executed after processing the string value from the 
///       `with_string` constructor.
void *with_string_entry() {
  DEBUG_PRINT("%s\n", __func__);
  g_StringRegister = read_ptr(g_NodeRegister + sizeof(InfoTable *));
  --g_SB.top;
  return g_SB.top[0].as_code;
}

/// Evacuation function for the "with_string" constructor (with_string_evac)
///
/// This function, `with_string_evac`, handles the evacuation process for the 
/// `with_string` constructor during garbage collection.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the `with_string` object 
///     in the old heap.
///
/// Here's how `with_string_evac` works:
///   1. **Allocate New Location:**
///     - It calls `heap_cursor` to get the current write position (`new_base`) 
///       in the heap. This essentially allocates a new memory location in 
///       the heap for the evacuated `with_string` object.
///     - It calculates the total size (`sizeof(InfoTable *) + sizeof(uint8_t *)`) 
///       required for the evacuated object, which includes space for the 
///       `InfoTable *` pointer and the pointer to the string data.
///     - It calls `heap_write` to write this total size to the base location 
///       in the old heap, indicating the size of the evacuated data.
///   2. **Write Evacuation Indirection:**
///     - Similar to other evacuation functions, it replaces the original data 
///       at the base location in the old heap. Here, it copies the address of 
///       `table_pointer_for_already_evac` ( a pre-defined table containing 
///       information about already evacuated objects) into the base location. 
///       This avoids redundant evacuation during future garbage collection 
///       cycles.
///     - Then, it writes the address of the newly allocated memory (`new_base`) 
///       into the location following the pointer to the `InfoTable *` structure 
///       in the old heap. This effectively stores the new location of the 
///       evacuated `with_string` object.
///   3. **Evacuate String (optional):**
///     - It retrieves the pointer to the string data from the new location 
///       (`new_base + sizeof(InfoTable *)`).
///     - It calls `collect_root` (assumed to be a separate function) to 
///       potentially mark the string data as a root object that should not 
///       be collected during garbage collection.
uint8_t *with_string_evac(uint8_t *base) {
  uint8_t *new_base = heap_cursor();
  heap_write(base, sizeof(InfoTable *) + sizeof(uint8_t *));

  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable *));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));

  uint8_t *cursor = new_base + sizeof(InfoTable *);
  uint8_t *root;
  memcpy(&root, cursor, sizeof(uint8_t *));
  collect_root(&root);
  memcpy(cursor, &root, sizeof(uint8_t *));

  return new_base;
}

/// Table for the "with_string" constructor (table_for_with_string)
///
/// This variable, `table_for_with_string`, defines an `InfoTable` structure 
/// specifically used for the `with_string` constructor. An `InfoTable` structure 
///  contains information about different types of objects in the system. 
/// In this case, the `table_for_with_string` provides function pointers for the 
/// entry point (`with_string_entry`) and the evacuation function (`with_string_evac`) 
/// used for handling the `with_string` constructor during evaluation and garbage 
/// collection.
///
/// The `InfoTable` structure  has the following members:
///   - `entry`: Function pointer to the entry point of the object type.
///   - `evac`: Function pointer to the evacuation function of the object type.
///
InfoTable table_for_with_string = {&with_string_entry, &with_string_evac};


/// Function to update the state for the "with_string" constructor (update_with_string)
///
/// This function, `update_with_string`,  performs an update operation 
/// specifically related to the `with_string` constructor. The exact functionality 
/// might depend on the context of how the `with_string` constructor is used in 
/// tinyHaskell.
///
/// Here's a breakdown of what `update_with_string` does:
///   1. **Get Table Pointer:**
///     - It creates a local variable `table` and assigns the address of 
///       `table_for_with_string` to it. This effectively retrieves the 
///       `InfoTable` structure associated with the `with_string` constructor.
///   2. **Store Table Pointer in Register:**
///     - It copies the address of the `table` (which points to the 
///       `InfoTable` structure) into the `g_ConstrUpdateRegister` location. 
///       The `g_ConstrUpdateRegister`  stores information about the 
///       current update state, and this might be used to remember how to 
///       handle the `with_string` constructor later during evaluation.
///   3. **Store String Pointer in Register:**
///     - It copies the value of the global `g_StringRegister` into the memory 
///       location following the `InfoTable *` pointer stored in 
///       `g_ConstrUpdateRegister`. This effectively stores the string pointer 
///       ( extracted from the `with_string` constructor earlier) along 
///       with the `InfoTable` pointer in the update register.
///
/// In essence, `update_with_string` updates the `g_ConstrUpdateRegister` with 
/// information about the `with_string` constructor, including the relevant 
/// `InfoTable` pointer and the string pointer. This information is  
/// used later during evaluation to continue processing the `with_string` 
/// constructor.
void update_with_string() {
  InfoTable *table = &table_for_with_string;
  memcpy(g_ConstrUpdateRegister, &table, sizeof(InfoTable *));
  memcpy(g_ConstrUpdateRegister + sizeof(InfoTable *), &g_StringRegister,
         sizeof(uint8_t *));
}


/// Entry function for constructor applications (with_constructor_entry)
///
/// This function, `with_constructor_entry`, serves as the entry point for 
/// handling constructor applications (including `with_int` and `with_string` 
/// as special cases) during evaluation.
///
/// Here's a breakdown of what `with_constructor_entry` does:
///   1. **Print Debug Message (optional):**
///     - The code includes a `DEBUG_PRINT` macro ( not shown here) 
///       that conditionally prints the function name for debugging purposes.
///   2. **Extract Tag and Number of Arguments:**
///     - It skips the `InfoTable *` pointer at the beginning and uses a cursor 
///       to iterate through the remaining data in the object pointed to by 
///       `g_NodeRegister`.
///     - It reads the constructor tag (`uint16_t`) from the memory location 
///       pointed to by the cursor and stores it in `g_TagRegister`. This tag 
///       identifies the specific constructor being applied.
///     - The cursor is then advanced by the size of a `uint16_t` to point to 
///       the next piece of data.
///     - It reads the number of arguments (`uint16_t`) for the constructor 
///       application from the memory location pointed to by the updated cursor 
///       and stores it in `items`. This value indicates how many subexpressions 
///       were used to create the constructor application.
///     - It stores the number of arguments in the global 
///       `g_ConstructorArgCountRegister` for potential later use.
///     - The cursor is again advanced by the size of a `uint16_t` to point 
///       to the start of the arguments.
///   3. **Copy Arguments to A Stack:**
///     - It copies `items` (number of arguments) elements of type `uint8_t *` 
///       from the memory location pointed to by the cursor to the top of the 
///       A stack (`g_SA.top`). These elements  point to the subexpressions 
///       used in the constructor application.
///     - The `g_SA.top` pointer is incremented by `items` to reflect the 
///       newly pushed arguments on the A stack.
///   4. **Discard Stack Frame and Return Code Pointer:**
///     - It decrements `g_SB.top` by 1, effectively discarding the stack frame 
///       associated with the constructor application on the B stack.
///     - Finally, it returns the code pointer stored in the topmost element 
///       of the B stack. This code pointer  represents the continuation 
///       that should be executed after processing the constructor application.
void *with_constructor_entry() {
  DEBUG_PRINT("%s\n", __func__);
  uint8_t *cursor = g_NodeRegister + sizeof(InfoTable *);

  memcpy(&g_TagRegister, cursor, sizeof(uint16_t));
  cursor += sizeof(uint16_t);

  uint16_t items;
  memcpy(&items, cursor, sizeof(uint16_t));
  cursor += sizeof(uint16_t);
  g_ConstructorArgCountRegister = items;

  memcpy(g_SA.top, cursor, items * sizeof(uint8_t *));
  g_SA.top += items;

  --g_SB.top;
  return g_SB.top[0].as_code;
}

/// Evacuation function for constructor applications (with_constructor_evac)
///
/// This function, `with_constructor_evac`, handles the evacuation process for 
/// constructor applications (including `with_int` and `with_string` 
/// as special cases) during garbage collection.
///
/// It takes a single argument:
///   - `base`: A pointer to the base memory location of the constructor object 
///     in the old heap.
///
/// Here's how `with_constructor_evac` works:
///   1. **Find Size of Arguments:**
///     - It calculates the base location where the number of arguments (`items`) 
///       is stored in the old heap. This is `sizeof(InfoTable *) + sizeof(uint16_t)`.
///     - It copies the number of arguments (`items`) from the old heap location 
///       into a local variable.
///   2. **Calculate Total Size:**
///     - It calculates the total size required for the evacuated constructor 
///       object in the new heap. This size includes:
///         - Space for the `InfoTable *` pointer.
///         - Space for the constructor tag (`uint16_t`).
///         - Space for the number of arguments (`uint16_t`).
///         - Space for pointers to each of the constructor arguments (`items * sizeof(uint8_t *)`).
///   3. **Allocate New Location and Update Old Location:**
///     - It calls `heap_cursor` to get the current write position (`new_base`) 
///       in the heap. This essentially allocates a new memory location in 
///       the heap for the evacuated constructor object.
///     - It writes the total size calculated earlier to the base location 
///       in the old heap. This informs the garbage collector about the size 
///       of the evacuated data.
///     - It replaces the original data at the base location in the old heap 
///       with an evacuation indirection. This involves:
///         - Copying the address of `table_pointer_for_already_evac` ( a 
///           pre-defined table containing information about already evacuated 
///           objects) into the base location. This avoids redundant evacuation 
///           during future garbage collection cycles.
///         - Copying the address of the newly allocated memory (`new_base`) 
///           into the location following the pointer to the `InfoTable *` structure 
///           in the old heap. This effectively stores the new location of the 
///           evacuated constructor object.
///   4. **Evacuate Arguments (recursively):**
///     - It creates a cursor (`cursor`) pointing to the start of the argument 
///       locations in the new heap. This location is `new_base + sizeof(InfoTable *) 
///       + sizeof(uint16_t) + sizeof(uint16_t)`.
///     - It calculates the end of the argument locations in the new heap 
///       based on the total size and the size of each argument pointer.
///     - It iterates through each argument location in the new heap:
///       - It reads the pointer to the argument (`root`) from the current 
///         argument location.
///       - It calls `collect_root` (assumed to be a separate function) to 
///         potentially mark the argument (`root`) as a root object that should 
///         not be collected during garbage collection. This is  important 
///         to avoid accidentally collecting subexpressions used in the 
///         constructor application.
///       - After potentially marking the argument, it writes the updated 
///         pointer (`root`) back to the argument location in the new heap.
///   5. **Return New Base Pointer:**
///     - Finally, the function returns the `new_base` pointer, which points 
///       to the newly allocated and evacuated memory location for the constructor 
///       object in the new heap.
uint8_t *with_constructor_evac(uint8_t *base) {
  uint8_t *items_base = base + sizeof(InfoTable *) + sizeof(uint16_t);

  uint16_t items;
  memcpy(&items, items_base, sizeof(uint16_t));
  size_t items_size = items * sizeof(uint8_t *);

  size_t total_size = sizeof(InfoTable *) + 2 * sizeof(uint16_t) + items_size;

  uint8_t *new_base = heap_cursor();
  heap_write(base, total_size);
  memcpy(base, &table_pointer_for_already_evac, sizeof(InfoTable *));
  memcpy(base + sizeof(InfoTable *), &new_base, sizeof(uint8_t *));

  uint8_t *cursor = new_base + sizeof(InfoTable *) + sizeof(uint16_t);

  uint8_t *end = new_base + total_size;
  for (uint8_t *cursor = end - items_size; cursor < end;
       cursor += sizeof(uint8_t *)) {
    uint8_t *root = read_ptr(cursor);
    collect_root(&root);
    memcpy(cursor, &root, sizeof(uint8_t *));
  }

  return new_base;
}


/// Table for constructor applications (table_for_with_constructor)
///
/// This variable, `table_for_with_constructor`, defines an `InfoTable` structure 
/// specifically used for handling constructor applications (including 
/// `with_int` and `with_string` as special cases) during evaluation and 
/// garbage collection.
///
/// The `InfoTable` structure  has the following members:
///   - `entry`: Function pointer to the entry point of the object type.
///   - `evac`: Function pointer to the evacuation function of the object type.
///
/// In this case, the `table_for_with_constructor` provides function pointers 
/// for:
///   - `with_constructor_entry`: The entry point for handling constructor 
///     applications during evaluation (see `with_constructor_entry` function 
///     for details).
///   - `with_constructor_evac`: The evacuation function for handling constructor 
///     applications during garbage collection (see `with_constructor_evac` 
///     function for details).
InfoTable table_for_with_constructor = {&with_constructor_entry,
                                        &with_constructor_evac};
/// Pointer to the table for constructor applications (table_pointer_for_with_constructor)
///
/// This variable, `table_pointer_for_with_constructor`, is a pointer to the 
/// `table_for_with_constructor` variable. It provides a convenient way to access 
/// the `InfoTable` structure associated with constructor applications.
InfoTable *table_pointer_for_with_constructor = &table_for_with_constructor;


/// Function to update the state for constructor applications (update_with_constructor)
///
/// This function, `update_with_constructor`,  performs an update operation 
/// specifically related to constructor applications during evaluation. The exact 
/// functionality might depend on how constructor applications are used in tinyHaskell.
///
/// Here's a breakdown of what `update_with_constructor` does:
///   1. **Calculate Required Space:**
///     - It retrieves the number of arguments (`items`) from the global 
///       `g_ConstructorArgCountRegister`, which was previously set in 
///       `with_constructor_entry`. This indicates the number of subexpressions 
///       used in the constructor application.
///     - It calculates the total size (`required`) required in the heap to store 
///       the constructor application data. This size includes:
///         - Space for the `InfoTable *` pointer.
///         - Space for the constructor tag (`uint16_t`).
///         - Space for the number of arguments (`uint16_t`).
///         - Space for pointers to each of the constructor arguments (`items * sizeof(uint8_t *)`).
///     - It calls `heap_reserve` (assumed to be a separate function) to reserve 
///       the required amount of space in the heap. This ensures sufficient 
///       memory is available before allocating the data.
///   2. **Allocate Memory and Write Info:**
///     - It calls `heap_cursor` to get the current write position (`indirection`) 
///       in the heap. This effectively allocates the memory for the constructor 
///       application data.
///     - It writes the `InfoTable` structure (`table_for_with_constructor`) 
///       using `heap_write_info_table`. This stores the pointer to the relevant 
///       `InfoTable` structure in the new heap location.
///     - It writes the constructor tag (`g_TagRegister`) and the number of arguments 
///       (`items`) to the new heap location using `heap_write_uint16`.
///     - It copies the argument pointers from `g_SA.top - items` (offset to access 
///       the beginning of the arguments on the A stack) to the new heap location 
///       using `heap_write`. This effectively copies the pointers to the 
///       subexpressions used in the constructor application.
///   3. **Update Construction Update Register:**
///     - It creates a pointer (`table_pointer_for_indirection`) to the 
///       `table_for_with_constructor` table. This pointer identifies the 
///       `InfoTable` structure used for constructor applications.
///     - It copies the pointer to the `table_pointer_for_indirection` into the 
///       `g_ConstrUpdateRegister`. This register  stores information 
///       about the current construction process. The pointer allows the 
///       garbage collector to later identify the relevant evacuation function 
///       (stored in the `InfoTable`) when needed.
///     - It copies the address of the allocated memory (`indirection`) into 
///       `g_ConstrUpdateRegister` following the pointer to the `InfoTable`. 
///       This effectively stores the location of the newly allocated data 
///       in the update register. This location will  be used later 
///       during evaluation to continue processing the constructor application.
void update_with_constructor() {
  uint16_t items = g_ConstructorArgCountRegister;
  size_t items_size = items * sizeof(uint8_t *);
  size_t required = sizeof(InfoTable *) + 2 * sizeof(uint16_t) + items_size;
  heap_reserve(required);

  uint8_t *indirection = heap_cursor();
  heap_write_info_table(&table_for_with_constructor);
  heap_write_uint16(g_TagRegister);
  heap_write_uint16(items);
  heap_write(g_SA.top - items, items_size);

  memcpy(g_ConstrUpdateRegister, &table_pointer_for_indirection,
         sizeof(InfoTable *));
  memcpy(g_ConstrUpdateRegister + sizeof(InfoTable *), &indirection,
         sizeof(uint8_t *));
}

/// Check if we need to create an application update (check_application_update)
///
/// This function, `check_application_update`, determines whether an update 
/// for a function application is necessary during evaluation. It's  
/// called when the number of arguments provided on the stack (`arg_count`) 
/// is insufficient for the current function application.
///
/// Here's a breakdown of what `check_application_update` does:
///   1. **Check Argument Count:**
///     - It calculates the actual number of arguments (`arguments`) available on 
///       the A stack by subtracting the base pointer (`g_SA.base`) from the 
///       top pointer (`g_SA.top`) of the A stack.
///     - It compares the available arguments (`arguments`) with the expected 
///       argument count (`arg_count`).
///     - If the available arguments (`arguments`) are greater than or equal to 
///       the expected argument count (`arg_count`), it means enough arguments 
///       are present, and the function can continue without an update. In this 
///       case, the function returns `NULL` to indicate no update is needed.
///   2. **Calculate Required Space (if update needed):**
///     - If there are insufficient arguments (`arguments < arg_count`), the function 
///       proceeds to create an update record on the heap.
///     - It calculates the number of items (`b_items`) on the B stack by 
///       subtracting the base pointer (`g_SB.base + 4`) from the top pointer 
///       (`g_SB.top`) of the B stack. The `+ 4`  accounts for the size of 
///       additional data stored in the update frame on the B stack (explained 
///       later).
///     - It calculates the number of items (`a_items`) on the A stack using 
///       the same method as for `b_items`.
///     - It calculates the total size (`required`) required on the heap to 
///       store the update record. This size includes:
///         - Space for the `InfoTable *` pointer.
///         - Space for a pointer to the continuation (`CodeLabel`).
///         - Space for the number of items (`b_items`) on the B stack.
///         - Space for the number of items (`a_items`) on the A stack.
///         - Space for the actual data from the B stack (`b_size`).
///         - Space for the actual data from the A stack (`a_size`).
///     - It calls `heap_reserve` (assumed to be a separate function) to 
///       reserve the required amount of space in the heap. This ensures 
///       sufficient memory is available before allocating the update record.
///   3. **Extract Update Frame Data (if update needed):**
///     - It retrieves the closure (`closure`) pointer from the second element 
///       (`g_SB.base[2]`) of the update frame on the B stack. The closure 
///        points to an object that holds information about the function 
///       application.
///     - It retrieves the saved base pointer (`saved_SB_base`) for the B stack 
///       from the first element (`g_SB.base[0]`) of the update frame. This 
///       pointer  represents the state of the B stack before the current 
///       function application began.
///     - It retrieves the saved base pointer (`saved_SA_base`) for the A stack 
///       from the third element (`g_SB.base[1]`) of the update frame. This 
///       pointer  represents the state of the A stack before the current 
///       function application began.
///   4. **Remove Update Frame from B Stack (if update needed):**
///     - It iterates over the elements (`b_items`) on the B stack.
///     - For each element, it shifts the elements on the B stack 
///       (`g_SB.base[i]`) four positions down (`g_SB.base[i + 4]`) to effectively 
///       remove the update frame from the stack.
///     - It updates the `g_SB.top` pointer to reflect the new top of the B 
///       stack after removing the update frame.
/// 5. **Construct New Closure and Update Record (if update needed):**
///   - It calls `heap_cursor` to get the current write position (`indirection`) 
///     in the heap. This effectively allocates the memory for the update record.
///   - It writes the `InfoTable` structure (`table_for_partial_application`) 
///     using `heap_write_info_table`. This stores the pointer to the relevant 
///     `InfoTable` structure in the new heap location, indicating a partial 
///     application update.
///   - It writes the continuation code label (`current`) to the heap using 
///     `heap_write`. This stores the label of the code to be executed 
///     when the application is resumed after gathering more arguments.
///   - It writes the number of items (`b_items`) on the B stack to the heap 
///     using `heap_write_uint16`.
///   - It writes the number of items (`a_items`) on the A stack to the heap 
///     using `heap_write_uint16`.
///   - It copies the data from the B stack (`g_SB.base`) of size `b_size` 
///     (previously calculated) to the allocated memory in the heap using 
///     `heap_write`. This effectively saves the B stack contents relevant to 
///     the partial application.
///   - It copies the data from the A stack (`g_SA.base`) of size `a_size` 
///     (previously calculated) to the allocated memory in the heap using 
///     `heap_write`. This effectively saves the A stack contents relevant to 
///     the partial application.
///   - It creates a pointer (`table_pointer_for_indirection`) to the 
///     `table_for_partial_application` table. This pointer identifies the 
///     `InfoTable` structure used for partial application updates.
///   - It copies the pointer to the `table_pointer_for_indirection` into the 
///     `closure` object at the beginning. This stores the relevant 
///     `InfoTable` pointer in the closure, allowing the garbage collector to 
///     later identify the appropriate evacuation function.
///   - It copies the address of the allocated memory (`indirection`) into the 
///     `closure` object following the `InfoTable *` pointer. This effectively 
///     stores the location of the update record in the heap within the closure. 
///     This location will  be used later to resume the application 
///     when more arguments become available.
/// 
/// 6. **Restore Stack Bases (if update needed):**
///   - It restores the saved base pointer (`saved_SA_base`) for the A stack 
///     back into `g_SA.base`. This effectively reverts the A stack to its state 
///     before the current function application began.
///   - It restores the saved base pointer (`saved_SB_base`) for the B stack 
///     back into `g_SB.base`. This effectively reverts the B stack to its state 
///     before the current function application began.
///
/// 7. **Return Continuation Code Label (if update needed):**
///   - It returns the continuation code label (`current`) to indicate that an 
///     update has been created and the code execution should return to the 
///     specified label to wait for more arguments.
CodeLabel check_application_update(int64_t arg_count, CodeLabel current) {
  int64_t arguments = g_SA.top - g_SA.base;
  if (arguments >= arg_count) {
    return NULL;
  }

  uint16_t b_items = g_SB.top - (g_SB.base + 4);
  uint16_t a_items = g_SA.top - g_SA.base;
  size_t b_size = b_items * sizeof(StackBItem);
  size_t a_size = a_items * sizeof(uint8_t *);
  size_t required = sizeof(InfoTable *) + sizeof(uint8_t *) + a_size + b_size;
  heap_reserve(required);

  uint8_t *closure = g_SB.base[2].as_closure;
  StackBItem *saved_SB_base = g_SB.base[0].as_sb_base;
  uint8_t **saved_SA_base = g_SB.base[1].as_sa_base;

  for (size_t i = 0; i < b_items; ++i) {
    g_SB.base[i] = g_SB.base[i + 4];
  }
  g_SB.top -= 4;

  uint8_t *indirection = heap_cursor();
  heap_write_info_table(&table_for_partial_application);
  heap_write(&current, sizeof(CodeLabel));
  heap_write_uint16(b_items);
  heap_write_uint16(a_items);
  heap_write(g_SB.base, b_size);
  heap_write(g_SA.base, a_size);

  memcpy(closure, &table_pointer_for_indirection, sizeof(InfoTable *));
  memcpy(closure + sizeof(InfoTable *), &indirection, sizeof(uint8_t *));

  g_SA.base = saved_SA_base;
  g_SB.base = saved_SB_base;

  return current;
}

/// The starting size for the Heap (BASE_HEAP_SIZE)
///
/// This constant, `BASE_HEAP_SIZE`, defines the initial size allocated for 
/// the heap in bytes. The value is currently set to 0xfffffff*7. This initial size can be adjusted based on the expected memory 
/// requirements of the program being compiled.
static const size_t BASE_HEAP_SIZE = 0xfffffff*7;

/// The starting size for each Stack (STACK_SIZE)
///
/// This constant, `STACK_SIZE`, defines the initial size allocated for 
/// each stack (A stack and B stack) in bytes. The value is currently set to 
/// 0xfffffff. This initial size can be adjusted 
/// based on the expected stack depth of the program being compiled.
static const size_t STACK_SIZE = 0xfffffff;

/// Setup all the memory areas that we need (setup)
///
/// This function, `setup`, initializes the memory areas required for the 
/// garbage collector and evaluation process. 
///
/// Here's a breakdown of what `setup` does:
///   1. **Allocate Heap Memory:**
///     - It allocates memory for the heap using `malloc`. The size of the 
///       allocated memory is determined by `BASE_HEAP_SIZE` multiplied by the 
///       size of a `uint8_t *` pointer. This is because the heap  stores 
///       pointers to objects allocated during evaluation.
///     - It checks if the memory allocation for the heap (`g_Heap.data`) 
///       was successful. If not, it calls `panic` (assumed to be a separate 
///       function for handling critical errors) with a message indicating 
///       failure to initialize the heap.
///     - It initializes the `g_Heap.cursor` to point to the beginning of 
///       the allocated heap memory (`g_Heap.data`). This cursor keeps track 
///       of the next available location for writing data to the heap.
///     - It sets the `g_Heap.capacity` to the initial capacity of the heap 
///       based on `BASE_HEAP_SIZE`. This value represents the maximum amount 
///       of data that can be stored in the heap before a resize is required.
///   2. **Allocate Argument Stack (A Stack) Memory:**
///     - It allocates memory for the A stack using `malloc`. The size of the 
///       allocated memory is determined by `STACK_SIZE` multiplied by the 
///       size of an `InfoTable *` pointer. The A stack  stores pointers 
///       to subexpressions during evaluation.
///     - It checks if the memory allocation for the A stack (`g_SA.data`) 
///       was successful. If not, it calls `panic` with a message indicating 
///       failure to initialize the argument stack.
///     - It initializes both the `g_SA.base` and `g_SA.top` pointers to point 
///       to the beginning of the allocated A stack memory (`g_SA.data`). This 
///       represents an empty stack initially.
///   3. **Allocate Secondary Stack (B Stack) Memory:**
///     - It allocates memory for the B stack using `malloc`. The size of the 
///       allocated memory is determined by `STACK_SIZE` multiplied by the 
///       size of a `StackBItem` structure. The B stack  stores 
///       additional information related to function calls and evaluation 
///       context.
///     - It checks if the memory allocation for the B stack (`g_SB.data`) 
///       was successful. If not, it calls `panic` with a message indicating 
///       failure to initialize the secondary stack.
///     - It initializes both the `g_SB.top` and `g_SB.base` pointers to point 
///       to the beginning of the allocated B stack memory (`g_SB.data`). This 
///       represen
void setup() {
  g_Heap.data = malloc(BASE_HEAP_SIZE * sizeof(uint8_t *));
  if (g_Heap.data == NULL) {
    panic("Failed to initialize Heap");
  }
  g_Heap.cursor = g_Heap.data;
  g_Heap.capacity = BASE_HEAP_SIZE;

  g_SA.data = malloc(STACK_SIZE * sizeof(InfoTable *));
  if (g_SA.data == NULL) {
    panic("Failed to initialize Argument Stack");
  }
  g_SA.base = g_SA.data;
  g_SA.top = g_SA.data;

  g_SB.data = malloc(STACK_SIZE * sizeof(StackBItem));
  if (g_SB.data == NULL) {
    panic("Failed to initialize Secondary Stack");
  }
  g_SB.top = g_SB.data;
  g_SB.base = g_SB.data;
}

/// Cleanup all the memory areas that we've created (cleanup)
///
/// This function, `cleanup`, deallocates the memory areas used by the 
/// garbage collector and evaluation process. It's  called at the end 
/// of the program execution or when no further processing is needed.
///
/// Here's a breakdown of what `cleanup` does:
///   1. **Free Heap Memory:**
///     - It calls `free` to release the memory allocated for the heap (`g_Heap.data`).
///     - This memory is no longer needed after the program execution is 
///       complete or garbage collection is finished.
///   2. **Free Argument Stack (A Stack) Memory:**
///     - It calls `free` to release the memory allocated for the A stack 
///       (`g_SA.data`).
///     - This memory is no longer needed after the program execution is 
///       complete or evaluation is finished.
///   3. **Free Secondary Stack (B Stack) Memory:**
///     - It calls `free` to release the memory allocated for the B stack 
///       (`g_SB.data`).
///     - This memory is no longer needed after the program execution is 
///       complete or evaluation is finished.
void cleanup() {
  free(g_Heap.data);
  free(g_SA.data);
  free(g_SB.data);
}

