## Summary of changes

Examples use `Int`, but equally holds for `Word`.

### GHC before 9.2

#### Data types
```haskell
data Int8 = I8 Int#
data Int16 = I16 Int#
data Int32 = I32 Int#

#if WORD_SIZE_IN_BITS == 64
data Int64 = I64 Int#
#else
data Int64 = I64 Int64#
#endif
```

#### Numeric ops
- `Int8#` some, not used by boxed
- `Int16#` some, not used by boxed
- `Int32#` none, barely existed
- `Int64#` complete when `WORD_SIZE_IN_BITS /= 64`, none otherwise

#### Array ops
```haskell
indexInt<N>OffAddr# :: Addr# -> Int# -> Int#
readInt<N>OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
writeInt<N>OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
-- ...
```

#### Conversion ops
`N` is restricted to be 8 and 16:
```haskell
extendInt<N> :: Int<N># -> Int#
NarrowInt<N> :: Int# -> Int<N>#
```

### GHC HEAD as of 2021-02-17

#### Data types
```haskell
data Int8 = I8 Int8#
data Int16 = I16 Int16#
data Int32 = I32 Int32#

#if WORD_SIZE_IN_BITS == 64
data Int64 = I64 Int#
#else
data Int64 = I64 Int64#
#endif
```

#### Numeric ops
- `Int8#` complete, not used by boxed
- `Int16#` complete, not used by boxed
- `Int32#` complete, not used by boxed
- `Int64#` complete when `WORD_SIZE_IN_BITS /= 64`, none otherwise

#### Array ops
Same as before

#### Conversion ops
Same as before, but `N` can also be 32.

### GHC 9.2 goal

#### Data types
```haskell
data Int8 = I8 Int8#
data Int16 = I16 Int16#
data Int32 = I32 Int32#
data Int64 = I64 Int64#
```

#### Numeric ops
- `Int8#` complete, used by boxed
- `Int16#` complete, used by boxed
- `Int32#` complete, used by boxed
- `Int64#` complete, used by boxed

#### Array ops
```haskell
indexInt<N>OffAddr# :: Addr# -> Int# -> Int<N>#
readInt<N>OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int<N># #)
writeInt<N>OffAddr# :: Addr# -> Int# -> Int<N># -> State# s -> State# s
-- ...
```

#### Conversion ops
```haskell
int<N>ToInt :: Int<N># -> Int#
intToInt<N> :: Int# -> Int<N>#
```
(`N` is not restricted, all of 8, 16, 32, and 64 are supported.)

## Issues being addressed

- The old system is overly complex and asymmetric.
  The new systems isn't, and is thus easier to learn.
  It also matches expectations for users coming from C since C99, and Rust.
  Overall, the wider programming community seems to agree retrofitting fixed-sized numerics on top of native ones was a historical convenience that lived far too long.

- Sized standard (boxed) Haskell types, especially via their `Foreign.C.Types` newtypes, not corresponding to sized C-- types broke FFI on Aarch64.

- `Int64#` requires copious CPP to use correctly portably across 64-bit and 32-bit platforms. The CPP is pure cruft as there is no difference in *intent* just the *mechanism* by which that attempt is accomplished.

- The `Int8#` and `Int16#` primops better express the intent to the backend, allowing perhaps faster numeric code (e.g. between autovectorization in LLVM and OOM magic on the CPU itself).

- The `Int8#` and `Int16#` primops by *not* being used by their boxed type counterparts are somewhat under-tested.

- The implementation of the dissapearing `Int64#` primops is the final remaining single-target assumption baked into the GHC binary.
  Otherwise GHC is is entirely multi-target as controlled by the configuration files and CLI flags.

- The old `{extend,narrow}Int<N>` naming scheme didn't scale to 32 and 64 bits:

   - With 32-bit prim types on 32-bit arch, we aren't "extending" or "narrowing" at all, everything is a no op.

   - With 64-bit prim types on 32-bit arch, we are in fact narrowing with the "extending" op, and  extending with the "narrowing" op.
     In other words, the names would be completely backwards!

   - Then new `<type>To<type>` naming scheme avoids extra connotations that would be invalidated.

## How we got here

1. Old discussion about `Int64#`:

   - #11953: Export Word32#, Word64# on all architectures

     Maybe this is the oldest issue here; some conversation.

   - #16964: Documentation of RuntimeRep is wrong about 64-bit integer reps

     This are the issue with the most conversation.
     The main point of contention was not whether we should more faithfully represent the fixed-sized options, which everyone seemed to agree with, but whether the native options should be nominally distinct or aliases:

       - `Int` should be distinct from `Int<N>` for back compat.
       - `Int#` could be an alias of `Int<N>#` (for some `N`).
       - `IntRep` could be an alias of `Int<N>Rep` (for some `N`).

     The most conservative option for now is to keep everything nominally distinct.

   - From #16964, two follow-up issues made to track specific issues with no conversation yet:

     - #17377: Int64#/Word64# are defined on all platforms, but some of the operations are only available on 32-bit

     - #17375: Int64# is exported on 64-bit systems (probably unintentionally), but you can't do anything with it.

2. Aarch64 NGC needed the boxed types changed.

   1. problem stated: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019317.html

   2. (later in thread) solution agreed upon: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019332.html. In short, `Int<N>` *must* wrap `Int<N>#` for various FFI to work properly.

   3. (side note) @Ericson2314 points out this dovetails nicely with earlier plans: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019333.html

   4. https://gitlab.haskell.org/ghc/ghc/-/commit/be5d74caab64abf9d986fc7290f62731db7e73e7 so changes sized types. Submodules are bumped, but changes to submodules were not merged to their master branches yet.

3. https://gitlab.haskell.org/ghc/ghc/-/issues/19026 opened to track doing the rest.

   - Tracks remaining PRs, including !4492, !4717, and !3658

   - TODO find comment where @RyanGlScott pointed out that array primops no longer agreeing with boxed wrapper type constructors was causing pain.

## Unresolved questions

- Should we create reexports for the old conversion primop names? So far, @Ericson2314 has no found any breakage caused by the rename.
  Most (all?) of the uses in boot libraries are only since the Aarch64 NCG, and thus can just be changed without CPP.
  This is because all that unboxed code was optimizing boxed interfaces, and until 9.2 none of the boxed interfaces use `Int8#` or `Int16#`.
  Also it's because `Int8#` or `Int16#` are so much newer.

  @Ericson2314 therefore thinks deprecated shims won't provide much benefit, but is happy to add them if others want them.
