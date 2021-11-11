# Motivation

`Coercion`s (as defined in GHC) always contain enough information for us to be able to retrieve the left and right hand sides that they relate. Examples:

  - the reflexive coercion stores its type `Refl ty :: ty ~# ty`,
  - a type constructor application stores the `TyCon`:  
    `TyConAppCo r tc [co_1 :: s_1 ~# t_1, ..., co_n :: s_n ~# t_n]`  
    `  :: tc s_1 ... s_n ~# tc t_1 ... t_n`
  - type family reductions store the argument coercions.

As a result, if we use `Coercion`s when rewriting type family applications, we can end up storing a large amount of extra types and coercions, many of which are redundant. This can cause severe compile-time performance issues; see e.g. #8095.

In many situations, we don't actually need this. The idea is to introduce a different kind of equality evidence which doesn't store all this information, but instead only allows us to **compute the RHS type given the LHS type (and the role)**: this is a  **directed coercion**. This was originally suggested in https://gitlab.haskell.org/ghc/ghc/-/issues/8095#note_374102.  

Side note: there are other sources of large Core terms besides coercions, e.g. types alone can easily grow non-linearly with program size (see #5642, #20264).  For now we are not attempting to tackle those problems.

## Why do we need coercions at all?

Coercions are primarily present to allow Core Lint to check the type-correctness of Core terms. Since this check is not carried out when `-dcore-lint` is not used, it may seem like they could be omitted in that case. However, retaining at least the sets of free coercion variables is crucial even when `-dcore-lint` is disabled.

For example, a GADT pattern match can bind a coercion variable such that the type-correctness of a term on the match RHS depends on it. If we did not store the free variables of the coercion, the term might be floated out past the match during optimization, which would be unsound.

Previous work (!611, !3792) has explored "zapping" coercions, removing the coercion body and storing only the coercion kind (i.e. the LHS and RHS types of the equality being proved) and the set of free variables. However it is difficult to make this a uniform performance win, because the types themselves may in general be large.

For now we are primarily interested in representing coercions more compactly while still being able to reconstruct a complete proof. In the future we may explore yet more compact "zapped" representations, but for now, unrestricted support for `-dcore-lint` is preferable.

## What is a directed coercion?

`DCoercion` is a datatype similar to `Coercion`, but designed to have a more compact representation.  
A directed coercion should be thought of as going left-to-right, just like GHC's rewriter. Given an input type and a role, we should be able to compute the output type, storing as little information as possible.

So:
  - no `SymDCo`,
  - `ReflDCo` takes no arguments,
  - no `SubDCo` (as the role, as well as the LHS type, is given from context),
  - no `NthDCo` or `LRDCo`: if we had `NthCo :: Int -> DCoercion -> DCoercion`, then we would have no way of computing the overall `TyConApp` which we are decomposing from the LHS type of the `NthCo`.

## Relation to bidirectional type systems

[Bidirectional Typing](https://arxiv.org/abs/1908.05839) by Jana Dunfield and Neel Krishnaswami is a useful reference to the design of bidirectional systems.  The key idea is this: introduction forms (e.g. constructors) are *checked*; elimination forms (e.g. projections) are *synthesized*.

Introduction forms: `ReflCo`, congruence rules such as `TyConAppCo`, `AppCo` ...

Elimination forms: `SymCo`, `NthCo`, `LRCo`, `InstCo`, `KindCo` ...

**AMG**: I need to write some more here.

# Where to use directed coercions?

We are chiefly interested in using directed coercions to speed up compile time of type family-heavy programs, so we want the rewriter and canonicaliser to use directed coercions as much as possible. This motivates changing the `Reduction` datatype to store a directed coercion.

Not every constructor of `Coercion` has a corresponding `DCoercion` constructor. This can be a problem, e.g. when we are rewriting a Wanted using a `CoercionHole` that we need to first decompose. We thus need to add a way to embed a `Coercion` into a `DCoercion`.

(**AMG**: I'm not very familiar with `CoercionHole`, but I have been wondering if it should be included in `DCoercion`?)

In the other direction, once the rewriter produces a directed coercion, we need to turn it into a `Coercion`, e.g. so that we can cast (but see [below](#using-directed-coercions-in-more-places) for using directed coercions in casts).

# Design choices

## Embedding directed coercions in coercions

We need a way to upgrade a directed coercion to a coercion, given an input type and role, filling in all the other information.

**Fluffing up**: `fluffUp :: Role -> Type -> DCoercion -> Coercion` computes the coercion associated with a directed coercion (adding missing decorations, or "fluffing it up"), given its role and LHS type.  (For efficiency we might want to separately implement a function `follow :: Role -> Type -> DCoercion -> Type` such that `follow r t dco == coercionRKind (fluffUp r ty dco)`.)

**Injecting**: We can instead use a constructor `Inject :: Role -> Type -> DCoercion -> Coercion`. If we choose to, we can then compute the actual fluffed-up coercion by using `fluffUp`.

**TransCo uses DCoercion**: `TransCo :: Coercion -> DCoercion -> Coercion`  
More radical: change `TransCo`'s second argument to be a directed coercion. After all, we can read off the type we are composing through from the first coercion, so there is no need to store it in the second coercion too.  
We would still need to enforce right-biasedness: never use ```(co `TransCo` dco_1) `TransCo` dco_2```, always ```co `TransCo` (dco_1 `TransDCo` dco_2)```.  
It might also make sense to cache the RHS type, as it might now be costly to compute. For example, in ```co `TransCo` (dco_1 `TransDCo` ... `TransDCo` dco_n)```, we would need to go through each of the directed coercions from left to right `dco_1`, ..., `dco_n` to compute the RHS type given the LHS type.  
Ups & downs of this approach: 
  - **Upside**: This gives us a stronger guarantee that we are not storing redundant information. In particular ```covar `TransCo` dco``` is more compact than either ```covar `TransCo` Inject r ty dco``` or ```Inject r ty' (covar `TransDCo` dco)```.
  - **Downside:** It's a bit of a roundabout way of doing the embedding: if we want to turn a lone `DCoercion` into a `Coercion`, we have to write ```Refl ty `TransCo` dco```, which seems a bit silly.

## Embedding coercions in directed coercions

**Constructor**: Use a forgetful constructor `CoDCo :: Coercion -> DCoercion` to embed coercions into directed coercions.

We can include a smart constructor for cases where it is clearly profitable to represent the coercion as a `DCoercion`: `ReflCo ty |-> ReflDCo`, `CoVarCo cv |-> CoVarDCo cv`. In other cases, it falls back to using the constructor.

## Kind coercions

Should we use directed kind coercions instead of kind coercions, e.g. in the `ForAllDCo` constructor of `DCoercion`?

**AMG**: in my first attempts at the patch I tried this, and rapidly got lost trying to implement functions over `ForAllDCo`. But perhaps it is worth another shot.

## Using directed coercions in more places

The current idea is to only use directed coercions in the rewriter/canonicaliser. It would also be possible to use them more broadly:

  - in casts,
  - in the constraint solver.

For casts, note that the type of the expression being cast is available (with a bit of work); however, this would require nontrivial changes to later stages of the compilation pipeline.  
In the constraint solver, when we are handling Givens we often have to take apart evidence using `Nth`-co, so we would often need to go through `Coercion` (embedding both ways).

For now we prefer to modify the type-checker and leave the rest of the pipeline relatively unchanged, hence using `Coercion`.

# Possible other interactions

`AxiomInstCo` takes a list of coercions as an argument, instead of what one might more naively expect, a list of types. It seems that this design was motivated by coercion optimisation concerns that may no longer be relevant with directed coercions. This might allow us to simplify the implementation in GHC, e.g. removing `GHC.Core.Unify.ty_co_match`.