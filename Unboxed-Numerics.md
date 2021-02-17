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
- `Int64#` complete when `WORD_SIZE_IN_BITS == 64`, none otherwise

#### Array ops
```haskell
index{Int,Word}<N>OffAddr# :: Addr# -> Int# -> {Int,Word}#
read{Int,Word}<N>OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, {Int,Word}# #)
write{Int,Word}<N>OffAddr# :: Addr# -> Int# -> {Int,Word}# -> State# s -> State# s
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
- `Int64#` complete when `WORD_SIZE_IN_BITS == 64`, none otherwise

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
index{Int,Word}<N>OffAddr# :: Addr# -> Int# -> {Int,Word}<N>#
read{Int,Word}<N>OffAddr# :: Addr# -> Int# -> State# s -> (# State# s, {Int,Word}<N># #)
write{Int,Word}<N>OffAddr# :: Addr# -> Int# -> {Int,Word}<N># -> State# s -> State# s
-- ...
```

#### Numeric ops
- `Int8#` complete, used by boxed
- `Int16#` complete, used by boxed
- `Int32#` complete, used by boxed
- `Int64#` complete, used by boxed

## How we got here

1. Old discussion about `Int64#` TODO fill in.

   - https://gitlab.haskell.org/ghc/ghc/-/issues/11953

   - https://gitlab.haskell.org/ghc/ghc/-/issues/17377

   - https://gitlab.haskell.org/ghc/ghc/-/issues/17375

2. Aarch64 NGC needed the boxed types changed.

   1. problem stated: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019317.html

   2. (later in thread) solution agreed upon: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019332.html

   3. (side note) x@Ericson2314 points out this dovetails nicely with earlier plans: https://mail.haskell.org/pipermail/ghc-devs/2020-October/019333.html

3. https://gitlab.haskell.org/ghc/ghc/-/issues/19026 opened to track doing the rest.

   - TODO find comment where @RyanGlScott pointed out that array primops no longer agreeing with boxed wrapper type constructors was causing pain.
