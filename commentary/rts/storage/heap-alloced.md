# HEAP_ALLOCED


This page is about the `HEAP_ALLOCED()` macro/function in the runtime system.  See #8199 which is about getting rid of `HEAP_ALLOCED`.

```c
StgBool HEAP_ALLOCED(void *p);
```


It is defined in `rts/sm/MBlock.h`.  The purpose of `HEAP_ALLOCED()` is to return true if the given address is part of the dynamically-allocated heap, and false otherwise.  Its primary use is in the Garbage Collector: when examining a pointer, we need to get to the block descriptor for that object.  Static objects don't have block descriptors, because they live in static code space, so we need to establish whether the pointer is into the dynamic heap first, hence `HEAP_ALLOCED()`.


On a 32-bit machine, `HEAP_ALLOCED()` is implemented with a 4096-entry byte-map, one byte per megabyte of the address space (the dynamic heap is allocated in units of aligned megabytes).  


On a 64-bit machine, it's a bit more difficult.  The current method (GHC 6.10.1 and earlier) uses a cache, with a 4096-entry map and a 32-bit tag.  If the upper 32 bits of the pointer match the tag, we look up in the map, otherwise we back off to a slow method that searches a list of mappings (bug #2934 is about the lack of thread-safety in the slow path here).  This arrangement works fine for small heaps, but is pessimal for large (multi-GB) heaps, or heaps that are scattered around the address space.

## Speeding up `HEAP_ALLOCED()`


We should consider how to speed up `HEAP_ALLOCED()` for large heaps on 64-bit machines.  This involves some kind of cache arrangement - the memory map is like a page table, and we want a cache that gives us quick access to commonly accessed parts of that map.

faster-heap-alloced.patch.gz implements one such scheme.  Measurements show that it slows down GC by about 20% for small heaps (hence it wasn't committed), though it would probably speed up GC on large heaps.

## Eliminating `HEAP_ALLOCED` completely


Can we eliminate `HEAP_ALLOCED` altogether?  We must arrange that all closure pointers have a valid block descriptor.

### Method 1: put static closures in an aligned section


ELF sections can be arbitrarily aligned.  So we could put all our static closures in a special section, align the section to 1MB, and arrange that there is space at the beginning of the section for the block descriptors.


This almost works (see eliminate-heap-alloced.patch.gz), but sadly fails for shared libraries: the system dynamic linker doesn't honour section-alignment requests larger than a page, it seems. Here is a simple test program which shows the problem on Linux:

```wiki
// test.S
    .section foo,"aw"
    .p2align 20
    .global foo_start
foo_start:
    .ascii "A"
```

```c
// main.c
#include <stdio.h>
extern char foo_start;
int main(void) {
    printf("%c\n", foo_start); // force libtest.so to be loaded
    printf("%p\n", &foo_start);
}
```


Compare static linking and dynamic linking:

```wiki
ezyang@javelin:~/Dev/labs/reloc$ gcc test.S main.c -g && ./a.out
A
0x700000
ezyang@javelin:~/Dev/labs/reloc$ gcc test.S  -shared -o libtest.so -fPIC
ezyang@javelin:~/Dev/labs/reloc$ gcc -Wl,-R`pwd` -L. main.c -ltest -g -O0
-fPIC && ./a.out
/usr/bin/ld: warning: type and size of dynamic symbol `foo_start' are not
defined
A
0x7f012f9e7000
```

### Method 2: copy static closures into a special area at startup


We could arrange that we access all static closures via indirections, and then at startup time we copy all the static closures into a special area with block descriptors.


Disadvantages:
  

- references to static objects go through another indirection. (This includes all of the RTS code!)

  - when doing dynamic linking, references to static objects in another package
    already go through an indirection and we could arrange that only one indirection is required.
  - References to static closures from the fields of a static constructor would not incur the extra indirection,
    only direct references to static closures from code.
  - we currently reference the static closure of a function from the heap-check-fail code, but in fact
    we only really need to pass the info pointer.


Advantages

- we get to fix up all the tag bits in static closure pointers
- we get to eliminate HEAP_ALLOCED, speeding up GC and removing complexity
- CAFs might get a bit simpler, since they are already indirections into the heap
