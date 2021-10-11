## POSIX address space running native machine instructions

  - Machine registers are available, and GHC uses a handful in stylized ways.
    (What lives in global registers?)

  - Some function parameters can be passed in registers?  Or is it just global registers?

  - Foreign calls expect some parameters in registers and the rest on the C stack.

  - Local variables can be kept in registers or spilled to the Haskell stack

## WebAssembly module running WASM instructions

  - There are no machine registers

  - Foreign calls expect all parameters on the evaluation stack.

  - Local variables are allocated per function; GHC does not do any explicit spilling

## POSIX address space running LLVM code

## Browser running JavaScript