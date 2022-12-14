# WIP: Constraints imposed by GHC target platforms


## Common to all targets

  - The native call stack is used only for calls to foreign functions.  Our calls use the "Haskell stack."  Haskell stacks are lightweight so we can enjoy thousands of them.  In GHC, the pointer to the youngest from on the Haskell stack is called SP.



## POSIX address space running native machine instructions

  - Machine registers are available, and GHC uses a handful in stylized ways.
    (What lives in global registers?)

  - Stack pointer SP and heap pointer HP live in machine registers (if possible).

  - Some function parameters can be passed in registers?  Or is it just global registers?

  - Foreign calls expect some parameters in registers and the rest on the C stack.

  - Local variables can be kept in registers or spilled to the Haskell stack

  - GHC may generate code for a native function, but GHC does not use any local variables that are native to that function.  What GHC considers a local variable is stored either in a machine register or in a slot in a frame on the Haskell stack.

  - Control flow within a function may be irreducible.

  - In principle, GHC's run-time system could walk a native call stack.

  - Native instructions expect operands in registers and place results in registers.  Sometimes also memory.

  - Pointers may (and usually do) have 64 bits.  Function pointers and data pointers share the same address space.

  - Memory is byte-addressed and may have either endianness.  Alignment may be required.

  - Indirect branches are supplied by the hardware and may not require a call.
    Branch-prediction hardware may not work well on indirect calls.

## WebAssembly module running WASM instructions (Asterius)

  - There are no machine registers

  - Stack pointer SP and heap pointer HP live in fields of `StgRegTable`

  - Foreign calls expect all parameters on the evaluation stack.

  - Local variables are allocated per function; GHC does not do any explicit spilling.

  - **Does Asterius use function-local variables for anything?**

  - Control flow must be structured.  (Structured control flow can be recovered from a CFG; see [Asterius #22](https://github.com/tweag/asterius/issues/22).)  (Now transitioned back to Binaryen relooper.)

  - The contents of a WebAssembly call stack are inaccessible at run time, even in principle.

  - Native instructions expect operands on the WebAssembly *operand stack* and return results on that same stack.  At every point in the code, the layout of the operand stack is known statically. 

  - Numbers may have 64 bits, but pointers have only 32 bits.  Function pointers and data pointers occupy distinct address spaces (function tables vs linear memory).

  - Memory is byte-addressed and is little-endian.  Multibyte accesses need not be aligned, but for performance reasons, alignment may be desirable.

  - Indirect branches are available for function calls only, and in principle, an indirect branch requires a dynamic type check (to make sure the call meets expectations for the operand stack).

  - Cross-compilation is fundamental: We expect the development model to be "develop on the desktop, deploy to WASM."  When running GHC, can't the baselibs from the native platform; need a mechanism to get the WASM base libs.  (Problem is solved using Nix.)  Nix builds the right thing and puts them in the right places.
    (Run Hadrian from within the Nix build.)  Generates some header files and .hs files. 

  - External tool chain can coalesce local variables, so Asterius/GHC doesn't have to.


## POSIX address space running LLVM code

https://llvm.org/docs/GetElementPtr.html

And see https://gitlab.haskell.org/ghc/ghc/-/issues/20370#note_378096


## Browser running JavaScript


# Wish lists for different target platforms

## Asterius wish list

  - Better support for foreign imports from JavaScript (centered
    around type checking and desugaring of foreign code)

  - 32-bit pointers from GHC (pointer tagging presumed OK in least significant _two_ bits)

  - Cross-platform development model

  - Principled way to add a back end to GHC (currently being done with the patched hooks) 

  - Sane story about baselibs, ideally solve
  
  - (Structured control flow, if it wouldn't compromise native performance.)

  - Would prefer to go through LLVM.
  
  - Abstract over calling convention, use registers more effectively (also would help LLVM) 

  - Relieve this pain point: a global register like `R1` might be used
    as a pointer in some places and as a non-pointer in others.  Life
    would be easier if the kind of `R1` did not change.
    
  - Support within the GHC run-time system for `JSVal`

  - A way of dealing with 8-bit and 16-bit computation that can be
    shared with other platforms that, like WebAssembly, provide
    computations only down to 32 bits
    
  

## LLVM wish list

  - Sufficient support for the LLVM `getElementPtr` instruction

  - Prototypes for every foreign call (that is, every call that LLVM recognizes as a call)
