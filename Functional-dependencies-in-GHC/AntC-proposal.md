Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

[WIP]

**Contents**

[[_TOC_]]

## Proposal: Relaxed Instance Consistency Conditions (RICC)

This is a proposal to relax the Strict Instance Consistency Conditions, but not so much as the Liberal ICC. It supports all the desired use cases I'm aware of (including `TypeEq, AddNat`), but rejects cases we know to be problematic -- such as ticket #10675. It is not aimed to support 'Dysfunctional dependencies' nor 'Wiggly Arrows' nor the 'circular constraints trick'. Implementing RICC will accept existing well-structured 'advanced' use of FunDeps but likely will break some existing code: I'd argue that code is an accident waiting to happen.

A smaller but significant benefit is to greatly reduce the need for `UndecidableInstances`; and somewhat reduce the need for Liberal Coverage; thereby making instances easier to read with less clutter in constraints.

* The base position is to try the SICC first -- that is, compare instance heads pairwise, applying the SICC for each FunDep of the class. Only if that fails, move to the RICC just for the FunDeps and pairs of instances that fail.

For a pair of instances to be valid under RICC wrt a set of FunDeps:

* All FunDeps must be **Full** (term from Schrijvers et al § 6.1, definition below);
* Instance heads must be either apart or overlap in strict substitution order;
* Going by an amended definition of apartness/overlap for FunDeps (note two given instance heads might be apart wrt one FunDep but overlap wrt a different FunDep);
* If the instance heads overlap wrt two or more FunDeps, it must be the same instance that is more general for each FunDep involved.

# Full FunDeps

A FunDep is **Full** just in case it mentions all class params -- either in determining or dependent positions; otherwise non-Full. These are Full:

```haskell
class TypeEq a b (r :: Bool)  | a b -> r

class AddNat (x :: Nat) (y :: Nat) (z :: Nat)  | x y -> z
                                               , x z -> y
                                               , y x -> z
```

These are non-Full:

```haskell
class CX x a b | a -> b where ...        -- #10675, doesn't mention x

class SetField x s t a b  | x s b -> t   -- doesn't mention a
                          , x t a -> s   -- doesn't mention b
                          , x s   -> a   -- doesn't mention t, b
                          , x t   -> b   -- doesn't mention s, a

      -- Note this is *not* the currently proposed decl for SetField,
      -- IIRC this is the form used by Lenses some time ago
```

Contrast this set of FunDeps for `SetField`, which are all Full:

```haskell
class SetField x s t a b  | x s b -> t a -- added a
                          , x t a -> s b -- added b
                          , x s t -> a b -- combines the two below
                      --  , x s   -> a   -- now redundant (but see below)
                      --  , x t   -> b   -- now redundant

```

Is the Full set for `SetField` equivalent to the non-Full set? No: the Full set is implied by the non-Full set, but the reverse implication doesn't hold. Does that matter? In the case of `SetField` there happens to be an 'escape hatch', but in general it might matter. Consider:

```haskell
instance (...) => SetField "foo" (T p1 a) (T p2 b) a b  where ...

    -- T has both a phantom type param p1/p2
    -- and a param that gives the type of field "foo" a/b

[W] SetField "foo" (T "Phantom1" Int) tau alpha Bool
```

So this is a type-changing update to field "foo" from `Int` to `Bool`. (It might also be changing the phantom type -- I'm not considering that here.)

Can we use the instance head + FunDep `x s t -> a b` to improve `alpha` in the wanted? We could do that with the non-Full Fundep `x s -> a`, but here no: we need to match the wanted's determining positions against the instance head, but for class param `t` we have only `tau`, not even `(T pi2 beta)`.

The escape hatch is we can use FunDep `x s b -> t a`, because the wanted specifies all of those determining positions. As a by-product we also improve `tau ~ (T pi2 Bool)`.

Could a class have a mix of Full and non-Full FunDeps? Yes: then for the non-Full FunDeps, the instance heads must meet SICC.

# Overlap/Apartness amended definition

Two instance heads **overlap** wrt a FunDep just in case the FunDep's determining positions from the `OVERLAPPABLE` instance is strictly more general than from the `OVERLAPPING` instance. The instance heads are **apart** wrt a FunDep just in case the determining positions don't unify. (To borrow database terminology -- also borrowed by Schrijvers et al, 'project' the instance heads on to the FunDep's determining positions.) Examples:

```haskell
class TypeEq a b (r :: Bool)  | a b -> r

instance {-# OVERLAPPING  #-} TypeEq a a True

instance {-# OVERLAPPABLE #-} TypeEq a b False    -- a b strictly more general than a a

    -- Note this avoids the need for UndecidableInstances or even Liberal Coverage

class AddNat (x :: Nat) (y :: Nat) (z :: Nat)  | x y -> z
                                               , x z -> y
                                               , y x -> z

instance                                          AddNat Z      Z      Z
instance                                          AddNat Z     (S y') (S y')
instance {-# OVERLAPPABLE #-} (AddNat x' y z') => AddNat (S x') y     (S z')

    -- Observe these instances are all apart going by GHC's current rules.
    -- They fail GHC's consistency checking.
    -- Under the amended definition of overlap/apart:
    -- The first two instances are apart, going by all of the FunDeps.
    -- The third instance is apart from the first, going by all of the FunDeps.
    -- The third is apart from the second, going by x y -> z, x z -> y.
    -- The third is strictly more general than the second by z x -> z.

```

Observe again `AddNat` avoids `UndecidableInstances`. It does need Liberal Coverage for the recursive constraint on the third instance.

To work the pairwise comparison of the second and third instances:

```haskell
-- by FunDep x y -> z, that is, 'project' x y positions
Z (S y') apart from (S x') y
-- by FunDep x z -> y
Z (S y') apart from (S x') (S z')
-- by FunDep y z -> x
(S y') (S y') unifies with y (S z') using y := (S y'), z' := y'
    -- the OVERLAPPABLE instance is therefore more general
```

This version of `AddNat` fails the RICC (sadly, also is rejected by GHC)

```haskell
instance                                          AddNat  Z     y      y
instance {-# OVERLAPPABLE #-} (AddNat x' y z') => AddNat (S x') y     (S z')
```

(A brave attempt to avoid `UndecidableInstances`.)

Projecting on the determining positions of `y z -> x`:\
`y y unifies with y (S z') using y := (S z')`\
Those are in no substitution ordering.

Here's a two-instance version for `AddNat`, complies with RICC (exercise for the reader), rejected by GHC (those heads don't overlap by current rules), but needs `UndecidableInstances`:

```haskell
instance                                          AddNat  Z     y      y
instance {-# OVERLAPPABLE #-} (z ~ (S z'), AddNat x' y z')
                                               => AddNat (S x') y      z
```

# Gnarly and seldom-needed extra condition

* If the instance heads overlap wrt two or more FunDeps, it must be the same instance that is more general for each FunDep involved.

[To do: this one makes my head hurt.]