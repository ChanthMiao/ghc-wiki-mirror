Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

[WIP]

**Contents**

[[_TOC_]]

## Proposal: Relaxed Instance Consistency Conditions (RICC)

This is a proposal to relax the Strict Instance Consistency Conditions, but not so much as the Liberal ICC. It supports all the desired use cases I'm aware of (including `TypeEq, AddNat`), but rejects cases we know to be problematic -- such as ticket #10675. It is not aimed to support 'Dysfunctional dependencies' nor 'Wiggly Arrows' nor the 'circular constraints trick'. Implementing RICC will accept existing well-structured 'advanced' use of FunDeps but likely will break some existing code: I'd argue that code is an accident waiting to happen.

A smaller but significant benefit is to greatly reduce the need for `UndecidableInstances`; and somewhat reduce the need for Liberal Coverage; thereby making instances easier to read with less clutter in constraints.

## Principles

* Instance selection must select exactly one instance, then behave at the usage site as if that is the only instance for the class. (Then #10675 is in error because it tangles together two different instances.)
* Instance selection must never be driven by matching a dependent position. (Then `[W] TypeEq alpha beta False` alone must not select an instance until `alpha, beta` are further instantiated.)
  * in case a position is both dependent and determining, wrt different FunDeps (as with `AddNat`), a determining position can drive instance selection.
* Any type improvement prior to selecting a single instance can apply only if all the candidate instances meet the SICC for the applicable FunDep -- possibly relaxed to the RICC defined here. (Then #10675 is further in error, because its instances don't meet SICC -- indeed they don't even meet LICC.)

## Approach

* The base position is to try the SICC first -- that is, compare instance heads pairwise, applying the SICC for each FunDep of the class. Only if that fails, move to the RICC just for the FunDeps and pairs of instances that fail.

For a pair of instances to be valid under RICC wrt a set of FunDeps (given they're not consistent by SICC):

* All FunDeps must be **Full** (term from JFP-paper § 6.1, definition below);
* Instance heads must be either apart or overlap in strict substitution order;
* Going by an amended definition of apartness/overlap for FunDeps (note two given instance heads might be apart wrt one FunDep but overlap wrt a different FunDep);
* If the instance heads overlap wrt two or more FunDeps, it must be the same instance that is more general for each FunDep involved. [Best explained by example](#Bi-overlap-avoidance-condition)

# Full FunDeps

A FunDep is **Full** just in case it mentions all class params -- either in determining or dependent positions; otherwise non-Full. Note in Mark Jones' ESOP paper, all the examples are Full FunDeps. I'm not sure that paper's rules for validity and type improvement through instances are intended to cater for non-Full. These are Full:

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
      -- IIRC this is the form used by Lenses some years ago
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


- **Caveat:** The definition of 'Full' here is not as used in the JFP-paper [§ 6.1 Definition 13]. (Indeed theirs is rather strange and restrictive.) That requires a single dependent/RHS class parameter, and all other class parameters determining/LHS. [The definition of 'Full' in this wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Background-and-terminology#15-full-functional-dependencies) omits an important condition from the JFP-paper (see note there).


# Overlap/Apartness amended definition

Two instance heads **overlap** wrt a FunDep just in case the FunDep's determining positions from the `OVERLAPPABLE` instance is strictly more general than from the `OVERLAPPING` instance. The instance heads are **apart** wrt a FunDep just in case the determining positions don't unify. (To borrow database terminology -- also borrowed by JFP-paper, 'project' the instance heads on to the FunDep's determining positions.) Examples:

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

This version of `AddNat` fails the RICC (sadly. Also is rejected by GHC):

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

# Bi-overlap avoidance condition

* If the instance heads overlap wrt two or more FunDeps, it must be the same instance that is more general for each FunDep involved.

Consider:

```haskell
class Janus a b c  | a b -> c, b c -> a

instance {-# OVERLAPPABLE #-} (c1 ~ Bool) => Janus a1 a1 c1

instance {-# OVERLAPPABLE #-} (a2 ~ Char) => Janus a2 b2 Int
```

The SICC doesn't hold for those instance heads; GHC accepts them; OTOH both need `UndecidableInstances`. Both FunDeps are full, they comply with RICC by the rules laid out so far. But both instances are `OVERLAPPABLE`. What's going on?

```
-- by FunDep a b -> c, projecting on a b
a1 a1 unifies with a2 b2 using a2 := a1, b2 := a1
    -- the second instance is therefore strictly more general
-- by FunDep b c -> a, projecting on b c
a1 c1 unifies with b2 Int using a1 := b2, c1 := Int
    -- the first instance is therefore strictly more general
```

Challenge: write a method using this class that demonstrates (any sort of) type improvement. All I can get is `Overlapping instances` rejections or `Couldn't match type` -- that is, improvement is inconsistent with constraints. These instances appear to be unusable.

Up 'til now, when needing to consider overlapping, there's only been one FunDep that needed to invoke the overlapping rule (by other FunDeps, the instance heads were apart). `Janus` is a case of invoking overlap for two different FunDeps:

* When invoking overlap for two or more FunDeps under RICC, it must be the same instance that is more `OVERLAPPABLE` across all projections on determining positions.

# Selecting instances

(No change to selecting instances for classes without FunDeps.)

Sets of instances (heads) that are pairwise valid according to RICC are globally coherent in the sense:

* If a Wanted is specific enough to match the determining positions of some instance, either
  * the Wanted determining positions match only one instance (all other instances are apart, projected on those positions); or
  * if multiple matching instances, they are in strict substitution order, projected on those positions; then
  * do not reject a more specific instance/do not select a more general instance until seeing sufficient improvement that the more specific instance can't match.
  * (I believe this is GHC's current behaviour for selecting overlapping instances, but GHC looks at the whole instance head.)

* `INCOHERENT` messes this up -- as to be expected with overlaps in general.

Example:

```haskell
class TypeEq a b (r :: Bool)  | a b -> r

instance {-# OVERLAPPING  #-} TypeEq a a True

instance {-# OVERLAPPABLE #-} TypeEq a b False

[W] TypeEq alpha beta rho    -- both instances match, don't choose yet
[W] TypeEq alpha beta False  -- still both instances match on a b, don't choose yet
[W] TypeEq Int   beta False  -- still don't choose
[W] TypeEq Int   Int  False  -- choose the True instance
                             -- report: Couldn't match type 'True with 'False
                             -- reject the program
[W] TypeEq Int (Maybe beta2) rho
                             -- can't unify the first two params, so reject True instance
                             -- choose False instance, improve rho := False
                             -- don't need to solve beta2, as usual

--  AddNat with three-way FunDep, see class decl above
instance                                          AddNat  Z     y      y
instance {-# OVERLAPPABLE #-} (z ~ (S z'), AddNat x' y z')
                                               => AddNat (S x') y      z

[W] AddNat (S chi) (S (S Z)) (S (S (S zeta)))
                              -- either choose (S x') instance by FunDep x y -> z
                                 -- because (S chi) apart from Z
                              -- or choose (S x') instance by FunDep x z -> y
                                 -- for same reason
                              -- or choose (S x') instance by FunDep z y -> x
                                 -- because (S (S Z)) apart from (S (S (S zeta)))
```

Can we elaborate a Wanted into a disjunction (inclusive-or) of Wanteds?

```haskell

[W] AddNat (S (S x')) (S y')  (S (S z'))
===>                                      -- for fresh zeta0, ypsilon0, chi0
[W] AddNat (S (S x')) (S y')   zeta0      ; [W] zeta0    ~ (S (S z'))
[W] AddNat (S (S x')) ypsilon0 (S (S z')) ; [W] ypsilon0 ~ (S y')
[W] AddNat chi0       (S y')   (S (S z')) ; [W] chi0     ~ (S (S x'))
```

Any of those `[W] AddNat ...`s will select the `AddNat (S x') y z` head (but not the `AddNat Z y y`).

# Selecting instances -- comparison to GHC User Manual's procedure

Ref steps at [§ 6.8.8.4 'Overlapping instances'](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#overlapping-instances) -- amended for a definition of Overlapping to be FunDep-aware. We wish to preserve these criteria:

<blockquote>
Notice that these rules are not influenced by flag settings in the client module, where the instances are used. These rules make it possible for a library author to design a library that relies on overlapping instances without the client having to know.
</blockquote>

... including the library relying on overlap in the revised sense with FunDeps.

I'll reword for a simple example (`TypeEq`) with overlapping instances (under the amended definition) and a single FunDep, then a more complex (`AddNat`) with multiple FunDeps.

| steps from User Manual | revise for `TypeEq` (one FunDep) | revise for `AddNat` (three FunDeps) |
| ---------------------- | ------------------- | -------------------- |
| ● Find all instances _I_ that match the target constraint; that is, the target constraint is a substitution instance of _I_. These instance declarations are the candidates. | Find all instances _I_ that match the target constraint _ignoring the dependent positions (third class param)_. _That is, match whether or not the target constraint's third param is a substitution instance of the instance's third param._\ These instance declarations are the candidates. | Find all instances _I_ that match the target constraint _trying the match once per FunDep, ignoring the dependent position (for each FunDep respectively)_. _That is, match whether or not the target constraint's respective dependent param is a substitution instance of the instance's dependent param._\ These instance declarations are the candidates. |
| ● If no candidates remain, the search fails | _stet_ | _stet_ |
| ● Eliminate any candidate _IX_ for which there is another candidate _IY_ such that both of the following hold:\ ○ _IY_ is strictly more specific than _IX_. That is, _IY_ is a substitution instance of _IX_ but not vice versa.\ ○ Either _IX_ is overlappable, or _IY_ is overlapping. (This “either/or” design, rather than a “both/and” design, allow a client to deliberately override an instance from a library, without requiring a change to the library.) | _stet_ except ... That is, _IY_ is strictly more specific than _IX_, _ignoring the third class param_.  | _stet_ except ... That is, _IY_ is strictly more specific than _IX_, _by comparing for each FunDep, ignoring the dependent position (for each FunDep respectively)_.   [Note ‡] |
| ● If all the remaining candidates are incoherent, the search succeeds, returning an arbitrary surviving candidate. | _stet_ plus upon returning: _Add a wanted equality constraint (`~`) to unify the target constraint's dependent position (third class param) with the instance's third param._ [Note †] | _stet_ plus upon returning: _Add wanted equality constraints (`~`) to unify the target constraint's dependent positions (from each FunDep) with the instance's corresponding positions._ [Note †] |
| ● If more than one non-incoherent candidate remains, the search fails.  | _stet_ | _stet_ |
| ● Otherwise there is exactly one non-incoherent candidate; call it the “prime candidate”. | _stet_ | _stet_ |
| ● Now find all instances, or in-scope given constraints, that unify with the target constraint, but do not match it. Such non-candidate instances might match when the target constraint is further instantiated. If all of them are incoherent top-level instances, the search succeeds, returning the prime candidate. Otherwise the search fails. | _stet_ except _unify with the target constraint ignoring the dependent position (third class param)_; plus upon returning: _Add a wanted equality constraint (`~`) to unify the target constraint's dependent position (third class param) with the instance's third param._ [Note †] | _stet_ except _unify with the target constraint ignoring in turn each dependent position in attempting to match by each set of determining positions_; plus upon returning: _Add wanted equality constraints (`~`) to unify the target constraint's dependent positions (from each FunDep) with the instance's corresponding positions._ [Note †] |
| **?** Is there a step missing here? "might match when the target constraint is further instantiated" but the outcome is to fail, not wait to see if they can match?

**[Note ‡]** Revised definition of instance overlap. As described above, the instance heads have been pre-validated to be in strict substitution ordering ignoring their dependent positions. I believe GHC currently doesn't pre-validate, so the filtering for most specific instance at bullet three is the point at which non-strict overlapping will come to light: reject the instance rather than reject the usage site.

**[Note †]** Improvement upon returning the selected instance. This generated equality constraint is the improvement currently applied to a Wanted from a (randomly-chosen) instance before trying to select an instance. Under this proposal, no improvement is to be applied until a unique instance is selected. The effect is as if:

| These instances under proposed rules | were written as these instance, under current GHC |
| ------------------------------ | -------------------------------- |
| `instance TypeEq a a True `    | `instance (r ~ True ) => a a r`  |
| `instance {-# OVERLAPPABLE #-} TypeEq a b False`    | `instance {-# OVERLAPPABLE #-}(r ~ False) => a b r`   |

(But that style of rewrite is not possible in general -- as with `AddNat`, for example.)
