## Summary

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
- `Int32#` none
- `Int64#` complete when `WORD_SIZE_IN_BITS /= 64`, none otherwise

#### Array ops
```haskell
indexInt<N>OffAddr# :: Addr# -> Int# -> Int#
readInt<N>OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int# #)
writeInt<N>OffAddr# :: Addr# -> Int# -> Int# -> State# s -> State# s
-- ...
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

### GHC 9.2 goal

#### Data types
```haskell
data Int8 = I8 Int#
data Int16 = I16 Int#
data Int32 = I32 Int#
data Int64 = I64 Int64#
```

#### Array ops
```haskell
indexInt<N>OffAddr# :: Addr# -> Int# -> Int<N>#
readInt<N>OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, Int<N># #)
writeInt<N>OffAddr# :: Addr# -> Int# -> Int<N># -> State# s -> State# s
-- ...
```

#### Numeric ops
- `Int8#` complete, used by boxed
- `Int16#` complete, used by boxed
- `Int32#` complete, used by boxed
- `Int64#` complete, used by boxed

## How we got here

1. Old discussion about `Int64#` TODO fill in.

   - #11953: Export Word32#, Word64# on all architectures

     Maybe this is the oldest issue here; some conversation.

   - #16964: Documentation of RuntimeRep is wrong about 64-bit integer reps

     These are the issues with the most conversation.
     The main point of contention was not whether we should more faithfully represent the fixed-sized options, but whether the native options should be nominally distinct or aliases:

       - `Int` should be distinct from `Int<N>` for back compat.
       - `Int#` could be an alias of `Int<N>#` (for some `N`).
       - `IntRep` could be an alias of `Int<N>Rep` (for some `N`).

     The most conservative option for now is to keep everything nominally distinct.

   - From #16964, two follow-up issues made to track specific issues with no conversation yet:

     - #17377: Int64#/Word64# are defined on all platforms, but some of the operations are only available on 32-bit

     - #17375: Int64# is exported on 64-bit systems (probably unintentionally), but you can't do anything with it.

2. Aarch64 NGC needed the boxed types changed.

   1. problem stated: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019317.html

   2. (later in thread) solution agreed upon: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019332.html

   3. (side note) @Ericson2314 points out this dovetails nicely with earlier plans: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019333.html

3. https://gitlab.haskell.org/ghc/ghc/-/issues/19026 opened to track doing the rest.

   - TODO find comment where @RyanGlScott pointed out that array primops no longer agreeing with boxed wrapper type constructors was causing pain.
