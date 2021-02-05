The [Dependent Haskell](dependent-haskell) page outlines a limited form of DH which we will call DH-Base. It establishes some key design principles, such as the **Predictable Erasure Principle**, **Lexical Scoping Principle**, and **Syntactic Unification Principle**.

We argue that while this is a useful step in the right direction, it alone would not fulfil the expectations of users coming from other dependently-typed languages. For example, the following code would not compile:

```haskell
max :: Nat -> Nat -> Nat
max Z n = n
max (S m) Z = S m
max (S m) (S n) = S (max m n)

vReplicateMax :: foreach (n :: Nat) (m :: Nat) -> a -> Vec (max n m) a
vReplicateMax n m a = vReplicate (max n m) a

v10x :: Vec 10 Char
v10x = vReplicateMax 5 10 'x'
```

(Pretend that instead of `5` and `10` it uses `S (S (S .. Z))`)

The problem is that `max 5 10 ~ 10` does not hold in DH-Base, as `max` will not reduce at the type level. In this article we set out to specify additional principles that build upon DH-Base and enable proper dependently-typed programming.

[[_TOC_]]

### 1. Singletons

First and foremost, we set out to make singleton types entirely redundant. Take inductive natural numbers for instance:

```haskell
data Nat = Z | S Nat
```

There shall be no circumstance whatsoever under which the user would be compelled to define its singletonization:

```haskell
data SNat (n :: Nat) where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)
```

Let us call this **No Singletons Principle (NSP)**. It is a desirable property for two reasons:

1. The structure of `SNat` duplicates that of `Nat`, and duplication of code is undesirable.
2. There is runtime overhead associated with conversion between `SNat` and `Nat` (in both directions).

At first glance, DH-Base seems to satisfy NSP, because we can write `foreach (n :: Nat) -> ty` instead of `SNat n -> ty`.

However, remember the `v10x` example above. It cannot be written with DH-Base, but it can be written using singleton types:

```haskell
type Max :: Nat -> Nat -> Nat
type family Max m n where
  Max Z n = n
  Max (S m) Z = S m
  Max (S m) (S n) = S (Max m n)

sMax :: SNat m -> SNat n -> SNat (Max m n)
sMax SZ n = n
sMax (SS m) SZ = SS m
sMax (SS m) (SS n) = SS (sMax m n)

vReplicate :: SNat n -> a -> Vec n a
vReplicate SZ     _ = Nil
vReplicate (SS n) x = x :> replicate n x

vReplicateMax :: SNat n -> SNat m -> a -> Vec (Max n m) a
vReplicateMax n m a = vReplicate (sMax n m) a

v10x :: Vec 10 Char
v10x = vReplicateMax 5 10 'x'
```

This is accepted because the `Max` type family can, in fact, reduce. Now, imagine we could use this `Max` type family as a term-level function, too. Then we would not need singleton types:

```haskell
type Max :: Nat -> Nat -> Nat
type family Max m n where
  Max Z n = n
  Max (S m) Z = S m
  Max (S m) (S n) = S (Max m n)

vReplicate :: foreach (n :: Nat) -> a -> Vec n a
vReplicate Z     _ = Nil
vReplicate (S n) x = x :> replicate n x

vReplicateMax :: foreach (n :: Nat) (m :: Nat) -> a -> Vec (Max n m) a
vReplicateMax n m a = vReplicate (Max n m) a
```

It follows, then, that in order to achieve NSP, we must take one of the two routes:

* Make type families (e.g. `Max`) available at the term level
* Make functions (e.g. `max`) reducible at the type level

We find the latter option more attractive because term-level functions are more familiar. Note that we do not need to support arbitrary functions: maybe we disallow FFI calls, `unsafePerformIO`, etc; but basic pattern matching is required.