This wiki page summarises the difficulties encountered during the implementation of directed coercions, which are a compact form of coercions used when rewriting type families to speed up compilation of type-family heavy programs. The main wiki page explaining directed coercions can be found [here](https://gitlab.haskell.org/ghc/ghc/-/wikis/Directed-coercions).

## Summary

Adam and Sam implemented directed coercions in [MR !6476](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6476). 
Some subtle aspects of the implementation are outlined in [§ The Hydration invariant](#the-hydration-invariant).

The performance results of this MR in type-family heavy programs are very good, but there are rare but extreme regressions in programs which rely on coercion optimisation (such as test cases `T15703`, `CoOpt_Singletons`, or the `singletons` library more generally).

Lacking a robust strategy for coercion optimisation, we also investigated zapping coercions in the coercion optimiser in [MR !7787](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7787). We survey some of the difficulties in [§ Zapping](#zapping).

For the moment, we are putting these patches aside, hoping to revisit them once we can find a better strategy to handle coercion optimisation. Several avenues are explored in [§ Strategies for coercion optimisation](#strategies).

---

[toc]

---

### MRs

**Directed coercions**
  - [!6476](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6476).
    This implements directed coercions and uses them in the rewriter. Runs into trouble with coercion optimisation (tests `T15703` and `CoOpt_Singletons`).

**Directed coercions + zapping in coercion optimisation**
  - [!7787](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7787). This approach has the most promising performance results so far, but has the highest implementation complexity.

**Coercion zapping**
  - [!611](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/611). Zaps coercions in the middle of the rewriter.
  - [!3792](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3792). Zaps coercions when zonking (still handling large coercions in the rewriter).
  - [!7844](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7844). The rewriter starts with a zapped coercion and `Reduction` combinators propagate the zappedness. (Significantly improves over !611 in tests such as `T12227`, `T5030`.)

# Rewriting

The job of the rewriter is to traverse through a type, rewriting individual components within this type, and then put it all back together.

For example, if one has `co :: a ~# b`, then we can rewrite `Maybe a` to `Maybe b` using `TyConAppCo Nominal Maybe [co]`.

Given an input type `ty` and an ambient role `r`, the rewriter produces a **reduction** `Reduction co xi`, where

```haskell
data Reduction = Reduction Coercion !Type
```

and `co :: ty' ~r xi`, where `ty'` is a type that is equal to `ty` up to zonking.

The rewriter then combines reductions together using the combinators from the `GHC.Core.Reduction` module, which all correspond to coercion introduction rules.

## Bidirectional type inference

In bidirectional type systems, one distinguishes two fragments: introduction rules are **checked**, while elimination rules are **inferred**.

A coercion `co :: a ~r b` must contain enough information on its own to recover its role `r`, as well as its LHS and RHS types `a` and `b`. For example, `Refl` stores a type, and `TyConAppCo` stores a role.

However, if we limit ourselves to the checkable fragment consisting only of introduction rules, then we can instead consider directed typing judgements of the form

```
dco :: a r~> b
```

in which the type `a` and the role `r` are external inputs, and `b` can be computed.

For example, the directed form of `Refl` does not need to store anything, as given the input type `a` we simply return the type `a` as the output.

This motivates the introduction of **directed coercions** as the checkable fragment of `Coercion`, which afford a more efficient representation which is appropriate for use in the rewriter. The typing rules are laid out in more details on the [Wiki page](Directed-coercions#typing-rules)

## Substitution lifting

Substitution lifting takes a type and applies a substitution in which type variables are mapped to coercions.

This relies on the fact that constructors of `Type` have associated constructors in `Coercion`, e.g. `AppTy`/`AppCo`, `TyConApp`/`TyConAppCo`, `FunTy`/`FunCo`, `ForAllTy`/`ForAllCo`. These constructors correspond to introduction rules for coercions, so it would make sense to implement a substitution lifting operation which works with directed coercions.

In practice, there are a few situations which call for a form of substitution lifting for directed coercions. These are:

  - `mkTyConAppCo`: if the `TyCon` is a type synonym, we should expand the type synonym. This produces a substitution, and we use substitution lifting to produce a coercion. The directed coercions equivalent is used where one would expect in the rewriter, when rewriting `TyConApp`s (calls to `mkTyConAppRedn`, as well as in `rewrite_app_ty_args`).
  - `simplifyArgsWorker`: in order to rewrite arguments to an application while preserving kinds, this function accumulates a substitution which it applies using substitution lifting. This function is crucially called in the rewriter when rewriting type family applications.

The existing attempts at implementing directed coercions did not implement substitution lifting for directed coercions. Instead, to perform substitution lifting, we would use an embedding `DCoercion -> ...extra_context -> Coercion`, and perform substitution lifting on the level of coercions.

## Hydration

There are two main ways to turn a directed coercion into a coercion, which involve supplying the extra context that's needed. Recall that instead of storing the role and LHS type, directed coercions take in that information as an input and produce a RHS type. So we expect to have:

```haskell
fullyHydrateDCo :: Role -> Type -> DCoercion -> Coercion
```

which does the work of reconstituting the full proof (the result `Coercion`) from the stripped down directed coercion. For example, if the directed coercions is `StepsDCo n` (indicating "take `n` steps unwrapping newtypes and reducing type families"), we need to produce the relevant axioms, a costly operation. Moreover, by hydrating a coercion in this way, we lose its compact representation, as we end up storing many more times.

It thus also makes sense to embed directed coercions as a constructor to `Coercion`:

```haskell
data Coercion
  = ...
  | HydrateDCo Role Type DCoercion
```

Note that this makes `Coercion` and `DCoercion` mutually recursive.

### The Hydration invariant

Whenever one wants to hydrate a directed coercion, one must be careful that the supplied LHS type is of the correct form to allow the computation of the RHS type. For example, consider:

```haskell
[G] cv :: a ~# b
alpha := F a
dco = HydrateDCo Nominal alpha (TyConAppDCo [CoVarDCo cv])
```

We can't purely compute the RHS type of `dco` from the LHS `alpha`. Instead, we would need to zonk `alpha` to `F a`, and this would allow us to reconstitute the `TyCon` `F` which is not stored in the directed coercion, to be able to produce:

```haskell
co = TyConAppCo F Nominal [CoVarCo cv]
```

This means that whenever we call `fullyHydrateDCo` or use the `HydrateDCo` constructor within the typechecker (before the final zonk), we must carefully preserve the invariant that we be able to purely reconstitute the full coercion.

### The Hydration invariant in practice

In practice, the places where we hydrate during typechecking are related to uses of substitution lifting during rewriting, as detailed previously.

To maintain the hydration invariant in the rewriter, we additional store an LHS type in `Reduction`:

```haskell
data Reduction = Reduction Type DCoercion Type
```

with the property that when we produce a reduction `ty ~r~> Reduction ty' dco xi`, we should be able to call `fullyHydrateDCo r ty' dco` without it crashing.

Then, in the rewriter, we keep track of when we have followed filled metavariables, and make sure to zonk the LHS before storing it in `Reduction`.

Example:

```haskell
type family F a where
  F a = G a
type family G b where
  G Bool = Float
```

Suppose we are rewriting `F alpha`, where `alpha := Bool` is a filled metavariable. First we rewrite `F alpha` without rewriting arguments, to get `G alpha`. Then we recursively rewrite `G alpha`. We can't immediately rewrite, so we rewrite its argument, which is a filled metavariable. Then we can rewrite `G Bool` to `Float`.

The reductions we get are as follows:

```haskell
Reduction (F alpha) (StepsDCo 1) (G alpha)
Reduction (G Bool) (StepsDCo 1) Float
```

The problem now is that if we naively composed these reductions we would get:

```haskell
Reduction (F alpha) (StepsDCo 2) Float
```

which violates the hydration invariant because we can't take two type family reduction steps from `F alpha`, as we would get to `G alpha` and then not be able to make further progress without the information that `alpha := Bool` (to reduce `G alpha`).

So, instead, we keep track of when we have recursively rewritten arguments when rewriting an application, and when we have we additional zonk the LHS, and up storing `Reduction (F Bool) (StepsDCo 2) Float`, and we can compute `fullyHydrateDCo Nominal (F Bool) (StepsDCo 2)` without problems.

Keeping track of the necessary zonks (by adding an `IORef` tracking followed metavariables in the rewriter) did not seem to cause any significant performance regressions in the rewriter.
Note however that storing an extra LHS does increase allocations in the rewriter, regressing by around `10~15%` in tests such as `T9872{a,b,c}`.


# Coercion optimisation

GHC's simplifier can produce very large coercions. The role of coercion optimisation
is to reduce the size of these coercions by exposing cancellation opportunities,
e.g. cancelling an `InstCo` applied to a `ForAllCo`, or an `NthCo` applied to a `TyConAppCo`. This is explained in the paper [*Evidence normalisation in System FC* by Dimitrios Vytiniotis and Simon Peyton Jones](https://www.microsoft.com/en-us/research/publication/evidence-normalization-system-fc-2/).

Up until now (working in the rewriter), we managed to restrict ourselves to the checkable fragment of coercions, using directed coercions. However, now we must think about how to bring in the elimination forms too.

### Important optimisations

The coercion optimiser works by pushing down the `SubCo`, `SymCo` and `TransCo` constructors inwards, down into the leaves of the coercion, to expose cancellation opportunities.

For example, in a composition

```haskell
TyConAppCo r tc [co_1a, co_1b] ;; co2
```

we replace `co2` with `TyConAppCo r tc [NthCo 0 co2, NthCo 1 co2]` and push down the transitivity to get

```haskell
TyConAppCo r tc [co_1a ;; NthCo 0 co2, co_1b ;; NThCo 1 co2]
```

This operation exposes additional cancellation opportunities; in this situation, we could have ```coercionLKind co_1a `eqType` coercionRKind (NthCo 0 co2)```, which would allow us to cancel the first argument in the `TyConAppCo` (which could be a big win if `co_1a` is large).


### Sources of large coercions

These optimisations can be absolutely crucial in practice; without these optimisations, the simplifier can cause coercion sizes to blow up. In particular, pushing coercions into arguments (`GHC.Core.Opt.Arity.pushCoValArg`) and into lambdas (`GHC.Core.Opt.Arity.pushCoercionIntoLambda`) can cause coercions to be duplicated many times over due to the calls to `decomposeFunCo`, which when applied to a big coercion `big_co` will return `(mkNthCo 0 big_co, mkNthCo 3 big_co, mkNthCo 4 big_co)` which can duplicate `big_co` if the `mkNthCo` smart constructor doesn't manage to simplify away (and we end up with e.g. `(NthCo 0 big_co, NthCo 3 big_co, NthCo 4 big_co)`. We then rely on coercion optimisation to expose the cancellations, in order to avoid successive passes of the simplier generating exponentially large coercion terms.

A few test cases in the test-suite are particularly sensitive to changes in coercion optimisation, such as `T15703` and `CoOpt_Singletons`. More generally, compiling the `singletons` library is a good benchmark. See also [this code from the `eliminators` package](https://github.com/RyanGlScott/eliminators/blob/2e374acc4f451d3efcea240c37e7b165e413639b/tests/EqualitySpec.hs#L194-L262) which had to be commented out due to large coercion sizes bringing compilation to a halt.

### Strategies for coercion optimisation

The optimisations involving elimination terms are no longer directly available when handling directed coercions. For example, if we have ``TyConAppDCo dcos `TransDCo` dco'``, we don't have a mechanism for pushing the transitivities inwards like we did above with coercions.

#### Approach 1: consider directed coercions to be atomic

The rewriter produces a (potentially large) proof term, and we just treat it as a single entity and try not to touch it (although the coercion optimiser still needs to apply a substitution, so we still need to traverse directed coercions to apply the substitution at the very least).

#### Approach 2: implement optimisations for directed coercions

Alternatively, we could try to perform the same steps with directed coercions that we do with undirected coercions. For example, if we encounter `TyConAppDCo dcos `TransDCo` dco'`, we could decide to hydrate `dco'` (either fully or just at the top-level) and apply the usual optimisation. Perhaps we could even revert back to using the more compact `dco'` as opposed to its hydrated form if the optimisation did not bear fruit (but it is not clear how would we would make this decision).

#### Approach 3: avoid creating large coercions in the first place

Instead of trying to improve coercion optimisation, we could try to nab the problem at the source, by identifying where these large coercions get created (e.g. `pushCoValArg` as noted above) and finding ways to avoid duplicating large proofs.  
One way to do this might be to improve the smart constructors for elimination forms, such as `mkNthCo`. For example, `(mkNthCo 0 big_co, mkNthCo 3 big_co, mkNthCo 4 big_co)` but we know that `big_co = FunCo w_co big_arg_co res_co`, then instead of getting duplication we simply extract out the relevant parts to get `(w_co, arg_co, res_co)`. If we spot more opportunities for this to happen (e.g. `mkNthCo` applied to a transitivity?), then we might avoid the duplication entirely.  
Alternatively, we could embrace the fact that we have a common coercion and introduce sharing, by introducing let-bindings in coercions.


# Zapping

[Approach 1](#approach-1) suggests another idea entirely: if we consider the proof terms produced by the rewriter to be atomic, we might as well elide them completely, only keeping track of the LHS and RHS types they relate. This is **zapping**.

This seems especially attractive as it was discovered during the implementation of directed coercions that storing the RHS type in the `HydrateDCo` constructor, caching the result of following the directed coercion and computing its RHS type from an input role and LHS type, lead to performance benefits. After we call the rewriter, we package up the directed coercion using `HydrateDCo`, thus storing both the LHS and RHS types, before e.g. using this coercion in a cast (e.g. in `rewriteEvidence`).

The following explores zapping at the source (in the rewriter). It's also possible to zap later on (e.g. during zonking, or during coercion optimisation), but this means we still need to do something in the rewriter to avoid large coercions. [MR !7787](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7787) implements directed coercions in the rewriter and coercion zapping in the coercion optimiser, which leads to good results overall, although allocations are doubled when compiling the `CoOpt_Singletons` test case.

## Zapped coercions

One way to zap coercions is to add a specific `UnivCoProvenance` for zapped coercions. This constructor must store the coercion variables used within the coercion to avoid it being floated out (e.g. a coercion making use of a local coercion variable introduced by a GADT pattern match floating outside the scope of the coercion variable).

This approach was explored in [MR !7844](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7844). This approach has several downsides, including the fact that it commits to using zapped coercions in all circumstances, even though a non-zapped representation might be more advantageous (e.g. `ReflCo a` instead of `UnivCo (ZappedProv []) Nominal a a`).

## Zapped nullicoercions

However, as pointed out in [§ Rewriting](#rewriting), in the rewriter it makes more sense to use a directed form of coercions, and only wrap the final result into a coercion.

So we can imagine storing different levels of information in the rewriter:

```haskell
ZappedCo covars role lhs rhs
ZappedDCo covars rhs
ZappedNulliCo covars
```

It would thus make sense to have a rewriter that does not produce any proofs, but only keeps track of coercion variables, producing a "zapped nullicoercion", which we then wrap at the end into a zapped coercion as above.

No implementation of this approach has been attempted.

## Disadvantages of zapping

Zapped coercions present two main disadvantages.

### Core Lint

If we elide proofs, we obviously can no longer check their validity, and must simply trust them. This means that:

  1. We still need to retain an alternative code path (e.g. an alternative implementation of the rewriter) when we want to run Core Lint,
  2. the code path used when zapping coercions will not be checked by Core Lint,
  3. Core Lint will potentially be very slow to check programs in which the rewriter handles large coercions.

### Zapping is not always advantageous

Zapped coercions are not always more compact. For example, `ReflCo ty` is more compact than `UnivCo (ZappedProv []) Nominal ty ty`, especially if `ty` is large. Similarly, we might prefer

```haskell
TyConAppCo tc r [ReflCo ty1, ReflCo ty2, UnivCo (ZappedProv []) l_ty3 r_ty2]
```

to

```haskell
UnivCo (ZappedProv []) (TyConApp tc r [ty1, ty2, l_ty3]) (TyConApp tc r [ty1, ty2, r_ty3])
```

Depending on where zapping occurs in the compiler, it is more or less straightforward to accomodate these situations. In the approach of [§ Zapped nullicoercions](#zapped-nullicoercions), we would have a special representation for zapped nullicoercions in the rewriter. This might be

```haskell
newtype ZappedNulliCoercion = ZappedNulliCoercion CoVarSet
```

but it could also be:

```haskell
data ZappedNulliCoercion
  = ReflNulliCoercion
  | ...
  | ZappedNullicoercion CoVarSet
```

If zapping occurs after typechecking, then we get more lee-way: we can simply inspect the proof term and decide whether to zap it or keep it as is (e.g. keep `Refl` as `Refl` and recurse into `TyConAppCo`s). This was the approach tried in [MR !7787](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7787), where we use directed coercions in the rewriter and then zap in coercion optimisation.

# Next steps

The most promising approach seems to be to generate directed coercions in the rewriter, then make sure that when the simplifier pushes coercions around (e.g. in `pushCoValArg`) they are sufficiently optimised to avoid exponential growth in coercion size.

Adam has a half-baked idea for how we might be able to make this easier: instead of the current
```hs
HydrateDCo :: Role -> Type -> DCoercion -> Type -> Coercion
```
we would instead have something like this:
```hs
PeakCo :: Role -> DCoercion -> Coercion -> DCoercion -> Coercion
```
with the typing rule:
```
γ : σ_1 ~r σ_2
δ_1 : σ_1 ->r τ_1
δ_2 : σ_2 ->r τ_2
------------------------------
PeakCo r δ_1 γ δ_2 : τ_1 ~ τ_2
```

Now:
 * `PeakCo r δ_1 γ δ_2` is rather like a transitive composition `sym δ_1 ; γ ; δ_2`.
 * By taking γ to be `ReflCo τ` and δ_1 to be `ReflDCo` we get back the old `HydrateDCo` embedding of `DCoercion`s in `Coercion`s.
 * This rule is symmetric; we have `mkSymCo (PeakCo r δ_1 γ δ_2) = PeakCo r δ_2 (mkSymCo γ) δ_1`. We could also have rules like `mkTransCo (PeakCo r δ_1 γ δ_2) (PeakCo r ReflDCo γ' δ_3) = PeakCo r δ_1 γ (δ_2 ; mkDehydrateCo γ' ; δ_3)`.
 * The idea is that `γ` will typically be `Refl` or `TyConAppCo`. We may want invariants that restrict the forms of the coercions/dcoercions, but they are not yet precisely specified.
 * This representation should make it easier to spot patterns like `PeakCo r (AxiomInstDCo ax ; ...) (F γs) (AxiomInstDCo ax ; ...)` which are crucial to optimise.
 * The hope is that more optimisation can happen on-the-fly in smart constructors, if we get the representation right. In particular, eliminators such as `mkNThCo` should try harder to expose a canonical form such as `TyConAppCo`, normalising away symmetry/transitivity and cancelling out axioms. Better still, `decomposeFunCo` etc. should do this on-the-fly optimisation once, then share it between all the calls to `mkNThCo` they make.
 * We may also want to factor out associativity of `TransCo`, either by always storing `TransCo` nested one way, or changing the data structure for `TransCo` to use some sort of sequence instead of unbalanced binary trees.
 * We probably want the `PeakCo` constructor to cache the ultimate LHS and RHS types. Adam conjectures that these caches should never be stored in interface files or traversed (e.g. substitution should insert thunked calls to `followDCo`/`coercionKind`). This will maximise sharing, though it could cost runtime if we have to repeatedly follow `DCoercion`s.

## Other future work

We have not yet implemented substitution lifting for directed coercions. This seems like it should be unproblematic in principle, and might allow the hydration invariant to be removed (along with the need for the rewriter to keep track of the LHS type). However it would lead to yet more code duplication.

It is an open question whether this code duplication could be reduced, perhaps by making more things polymorphic in the choice of coercion representation, or perhaps by removing constructors of `Coercion` that we always want to represent as `DCoercion`s instead.