# Primitive Operations (PrimOps)


PrimOps are functions that cannot be implemented in Haskell, and are provided natively by GHC.  For example, adding two `Int#` values is provided as the PrimOp `+#`, and allocating a new mutable array is the PrimOp `newArray#`.


PrimOps are made available to Haskell code through the virtual module `GHC.Prim`.  This module has no implementation, and its interface never resides on disk: if `GHC.Prim` is imported, we use a built-in `ModIface` value - see `ghcPrimIface` in [compiler/GHC/Iface/Load.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Iface/Load.hs).


It would also be useful to look at the [Wired-in and known-key things](commentary/compiler/wired-in) wiki page to understand this topic.

## The primops.txt.pp file


The file [compiler/GHC/Builtin/primops.txt.pp](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/primops.txt.pp) includes all the information the compiler needs to know about a PrimOp, bar its actual implementation.  For each PrimOp, `primops.txt.pp` lists:

- Its name, as it appears in Haskell code (eg. int2Integer\#)
- Its type
- The name of its constructor in GHC's `PrimOp` data type.
- Various properties, such as whether the operation is commutable, or has side effects.


For example, here's the integer multiplication PrimOp:

```wiki
primop   IntegerMulOp   "timesInteger#" GenPrimOp   
   Int# -> ByteArr# -> Int# -> ByteArr# -> (# Int#, ByteArr# #)
   with commutable = True
        out_of_line = True
```


The `primops.txt.pp` file is processed first by CPP, and then by the `genprimopcode` program (see [utils/genprimopcode](https://gitlab.haskell.org/ghc/ghc/blob/master/utils/genprimopcode)).  `genprimopcode` generates the following bits from `primops.txt.pp`:

- Various files that are `#include`d into [compiler/GHC/Builtin/PrimOps.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/PrimOps.hs),
  containing declarations of data types and functions describing the PrimOps.  See
  [compiler/Makefile](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/Makefile).

- `libraries/base/GHC/PrimopWrappers.hs`, a file that contains (curried) wrapper
  functions for each of the PrimOps, so that they are accessible from byte-code, and
  so that the [byte-code interpreter](commentary/rts/interpreter) doesn't need to implement any PrimOps at all: it
  just invokes the compiled ones from `GHC.PrimopWrappers`.

- `libraries/ghc-prim/dist-install/build/autogen/GHC/Prim.hs`, a generated source file containing dummy declarations for
  all the PrimOps, solely so that Haddock can include documentation for `GHC.Prim`
  in its documentation for the `base` package.  The file `GHC/Prim.hs` is never
  actually compiled, only processed by Haddock.


Note that if you want to create a polymorphic primop, you need to return `(# a #)`, not `a`.

## Implementation of PrimOps


PrimOps are divided into two categories for the purposes of implementation: inline and out-of-line.

### Inline PrimOps


Inline PrimOps are operations that can be compiled into a short sequence of code that never needs to allocate, block, or return to the scheduler for any reason.  An inline PrimOp is compiled directly into [Cmm](commentary/rts/cmm) by the [code generator](commentary/compiler/code-gen).  The code for doing this is in [compiler/GHC/StgToCmm/Prim.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs).

### Out-of-line PrimOps


All other PrimOps are classified as out-of-line, and are implemented by hand-written C-- code in the file [rts/PrimOps.cmm](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/PrimOps.cmm).  An out-of-line PrimOp is like a Haskell function, except that

- PrimOps cannot be partially applied.  Calls to all PrimOps are made at the correct arity; this is ensured by 
  the [CorePrep](commentary/compiler/hsc-main) pass.

- Out-of-line PrimOps have a special, fixed, [calling convention](commentary/rts/haskell-execution#calling-convention):
  all arguments
  are in the [registers](commentary/rts/haskell-execution#registers) R1-R8.  This is to make it easy to write the
  C-- code for these PrimOps: we don't have to write code for multiple calling conventions.


It's possible to provide inline versions of out-of-line PrimOps. This is useful when we have enough static information to generated a short, more efficient inline version. For example, a call to `newArray# 8# init` can be implemented more efficiently as an inline PrimOp as the heap check for the array allocation can be combined with the heap check for the surrounding code. See `shouldInlinePrimOp` in [compiler/GHC/StgToCmm/Prim.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs).

### Foreign out-of-line PrimOps and `foreign import prim`


A new and somewhat more flexible form of out-of-line PrimOp is the foreign out-of-line PrimOp. These are essentially the same but instead of their Cmm code being included in the RTS, they can be defined in Cmm code in any package and instead of knowledge of the PrimOp being baked into the compiler, they can be imported using special FFI syntax:

```wiki
foreign import prim "int2Integerzh"
  int2Integer# :: Int# -> (# Int#, ByteArray# #)
```

The string (e.g. "int2Integerzh") is the linker name of the Cmm function. Using this syntax requires the extensions `ForeignFunctionInterface`, `GHCForeignImportPrim`, `MagicHash`, `UnboxedTuples` and `UnliftedFFITypes`. The current type restriction is that all arguments and results must be unlifted types, with two additional possibilities: An argument may (since GHC 7.5) be of type `Any` (in which case the called function will receive a pointer to the heap), and the result type is allowed to be an unboxed tuple. The calling convention is exactly the same as for ordinary out-of-line primops. Currently it is not possible to specify any of the PrimOp attributes.


The `integer-gmp` package now uses this method for all the primops that deal with GMP big integer values. The advantage of using this technique is that it is a bit more modular. The RTS does not need to include all the primops. For example in the integer case the RTS no longer needs to link against the GMP C library.

### Future directions

The first step is to extend this syntax to allow PrimOp attributes to be specified. Then external-only (some PrimOps have external fallbacks and internal special-cases!) primops can be converted to use `foreign import prim`, bypassing the Compiler for interactions strictly between `base` and the `rts`.

In https://gitlab.haskell.org/ghc/ghc/blob/581cbc28e143a4ed8e7f794ed1618161222a5646/compiler/GHC/HsToCore/Foreign/Decl.hs#L289-302 there is a tech debt note that `foreign import prim` is currently a hacked up foreign call. But maybe this is actually good. If they support both support all the same strictness and other metadata, maybe primops, even internal ones, are "just" another type of foreign function. This seems good for decoupling core from the backend, and insuring there are no regressions between "true" primop and `foreign import prim` functionality.

Another direction is the calling convention for primops and ordinary compiled Haskell functions may be unified in future and at that time it the restriction on using only unlifted types may be lifted. [This sounds weird to @Ericson2314. Aren't primops definitionally inlined? Both they and foreign calls have wrappers that deal with lifted types. I'd want be sure when writing a primop whether I was getting an implicit wrapper, otherwise it's hard to reason about the cost and ABI.]

It has been suggested that we extend this PrimOp definition and import method to cover all PrimOps, even inline ones. This would replace the current `primops.txt.pp` system of builtin PrimOps. The inline PrimOps would still be defined in the compiler but they would be imported in any module via `foreign import prim` rather than appearing magically to be exported from the `GHC.Prim` module. Hugs has used a similar system for years (with the syntax `primitive seq :: a -> b -> b`).

## Adding a new PrimOp


To add a new primop, you currently need to update the following files:

- [compiler/GHC/Builtin/primops.txt.pp](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/primops.txt.pp), which includes the
  type of the primop, and various other properties.  Syntax and
  examples are in the file.

- if the primop is inline, then:
  [compiler/GHC/StgToCmm/Prim.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Prim.hs) defines the translation of
  the primop into `Cmm`.


                


- for an out-of-line primop:

  - [includes/stg/MiscClosures.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/stg/MiscClosures.h) (just add the declaration),
  - [rts/PrimOps.cmm](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/PrimOps.cmm) (implement it here)
  - [rts/Linker.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Linker.c) (declare the symbol for GHCi. Note that many are actually declared in `rts/RtsSymbols.c` and `#include`d into `rts/Linker.c`)

- for a foreign out-of-line primop You do not need to modify the rts or compiler at all.

  - `yourpackage/cbits/primops.cmm`: implement your primops here. You have to arrange for the .cmm file to be compiled and linked into the package. The GHC build system has support for this. Cabal does not yet.
  - `yourpackage/TheCode.hs`: use `foreign import prim` to import the primops.

**Note:** In case of compile errors like

```wiki
Not in scope: ???GHC.Prim.<newPrimOp>???
```


run `make clean` so the build system will pick up changes to the generated `GHC.Prim` module.



In addition, if new primtypes are being added, the following files need to be updated:


- [utils/genprimopcode/Main.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/utils/genprimopcode/Main.hs) -- extend ppType :: Type -\> String function


  


- [compiler/GHC/Builtin/Names.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Names.hs) -- add a new unique id using mkPreludeTyConUnique

- [compiler/GHC/Builtin/Types/Prim.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Types/Prim.hs) -- there are a raft of changes here; you need to create `*PrimTy`, `*PrimTyCon` and  `*PrimTyConName` variables. The most important thing to make sure you get right is when you make a PrimTyCon, you pick the correct `PrimRep` for your type.  For example, if you???ve introduced a new GC'able object, you should use `PtrRep`; however, if it's a pointer that shouldn't be GC'd, you should use `AddrRep` instead.  The full list is in [compiler/GHC/Core/TyCon.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/TyCon.hs)


See also [AddingNewPrimitiveOperations](adding-new-primitive-operations), a blow-by-blow description of the process for adding a new out-of-line primop from someone who went through the process.

## Explanation of attributes

`TBV` (To be verified)

### has_side_effects


default = False

### out_of_line


default = False


Set to True if there is a function in PrimOps.cmm. This also changes to code generator to push the continuation
of any follow on code onto the stack.

### commutable


default = False

### needs_wrapper


default = False

### strictness


default = \[lazyDmd, ... \] TopRes


This is the strictness of the PrimOp. Unboxed things should be marked as lazyDmd. (Someone explain why?).

### usage


default = nomangle other
