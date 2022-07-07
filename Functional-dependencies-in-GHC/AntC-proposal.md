Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

[WIP]

## Proposal: Relaxed Instance Consistency Conditions (RICC)

This is a proposal to relax the Strict Instance Consistency Conditions, but not so much as the Liberal ICC. It supports all the desired use cases I'm aware of (including `TypeEq, AddNat`), but rejects cases we know to be problematic -- such as ticket #10675. It is not aimed to support 'Dysfunctional dependencies' nor 'Wiggly Arrows' nor the 'circular constraints trick'. Implementing RICC will accept existing well-structured 'advanced' use of FunDeps but likely will break some existing code: I'd argue that code is an accident waiting to happen.

* The base position is to try the SICC first -- that is, compare instance heads pairwise, applying the SICC for each FunDep of the class. Only if that fails, move to the RICC just for the FunDeps that fail.

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

Contrast this set of FunDeps for `SetField`:

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

Can we use the instance head + FunDep `x s t -> a b` to improve `alpha` in the wanted? We could do that with the non-Full Fundep `x s -> a`, but here no: we need to match the wanted's determining positions against the instance head, but for class param `t` we have only `tau`, not `(T p2 b)`.

The escape hatch is we can use FunDep `x s b -> t a`, because the wanted specifies all of those determining positions. As a by-product we also improve `tau ~ (T p2 Bool)`.
