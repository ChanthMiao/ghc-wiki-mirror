
Tasks discussed by Richard and Simon. This page is mostly for our own notes, but others are welcome to read it.

[Our google doc, with easy to edit notes](https://docs.google.com/document/d/1MAXhZmrCDq_zRF_-Rt4ZMLeeSLPF4VmFlBLMZUrc2P8/edit#).

# Active agenda

- #12088: SCC for kind inference: we know what to do, it's just a question of doing it. See also #7503, #14451.  [Here is the wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/Type-&-Class-Dependency-Analysis)
- #19076: coercion holes
- #17567: free floating kind vars and zonking.  This is stalling !2313
- Casts might get in the way of instantiation: #18062
- #11602, 11371: exponential `promoteCoercion`
- #11084, #6234: instance loading for type families
- #17580: remove roles from the flattener
- #17581: swap sense of coercion in the flattener
- #17582: re-engineer rebindable syntax
- [Proposal 99: explicit specificity](https://github.com/ghc-proposals/ghc-proposals/pull/99).  Lets us write `T :: forall {k} (a :: k).blah`. #16393, #17569
- Move levity poly checks from zonker to desugarer
- PredTree stuff (RAE).   https://gitlab.haskell.org/rae/ghc/tree/pred, #17536
- `TypeLike` and `KnownRuntimeRep` constraints: #17201, #15979, #17536, also fixes #8388.  And #17201, #17113, #13105. #17131 is another example. And #19573.  Blocked on PredTree stuff
- #17295 (confluence in solver) badly needs execution.  We have a plan.  RAE.
- #17202, #16502: bug in the superclass expansion of quantified constraints; also we need to eagerly expand superclasses for quantified constraints to fix [this comment](https://gitlab.haskell.org/ghc/ghc/issues/16502#note_189978) in #16502.  Richard has !2283 in progress
- #17323 and !2042: the PKTI is not good enough. Also #17223 (Refl zapping)
- #17644 about loop in the solver and vars in kinds
- #16775: don't zap naughty quantification candidates.  RAE.
- #17024: `anyRewritableTyVar` and related gubbins
- #16726: document conclusions
- #16512: type checker loop with injective type families.  Anything to do with #17139?  Or #15772.   Seond bug in #17186.  Counter in `normaliseType` (#17306)
- Function result signatures.  [Accepted proposal #228](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0228-function-result-sigs.rst), and [proposed amendment #254](https://github.com/ghc-proposals/ghc-proposals/pull/254).  Something is not quite right.  [Typing rules here](https://gitlab.haskell.org/rae/haskell)
- Singletonised Outputable (RAE making a ticket); and eliminate `(a ~ GhcPass p)` constraints #17304
- [Data/data family return kinds](https://gitlab.haskell.org/ghc/ghc/issues/17021#note_227290) eg #17021




# Tiresome bugs

- #17812: levity polymorphism in typed holes
- #17021: type families in return kinds
- #16995, !1496: Flattening and `coreFlattenTyFamApp`
- #17064: uniques for built in tyvars
- #16442: bogus short cut for `:~:`
- !1236: Typeable evidence for casts
- #16234: type family reduction
- #16980: meta tyvar leakage in TH
- #17225: infelicities in the pure unifier
- #16835: Typeable evidence for casts
- #16980: metavarialbe leakage in TH
- #16436: incompleteness with injectivity: we might need `GD` constraints similar to `WD` ones.

# Roadmap of new stuff we want to get done


These things are all either new features, or significant refactorings.  All aimed at "filling out" what Haskell does to be simple and consistent.



We should be clear about the dependencies between items on this list.

- #17526, !2249: implement [accepted GHC proposal 203](https://github.com/ghc-proposals/ghc-proposals/blob/8a2f26408decd4be7799179213b3d3416509eb18/proposals/0203-pointer-rep.rst), on levity polymorphism and `BoxedRep`.   See #17126, #17817

- #17443: way of injecting levity polymorpihc bindings
- #17441: ensure that tcView and coreView can inline
- #17440: seqId magic

- Visible type abstraction: [Original proposal](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0050-type-lambda.rst), modified [here](https://github.com/ghc-proposals/ghc-proposals/pull/238). Also Richard & Simon Slack 27 June 19.

- #17026: `IntRep` and `WordRep`
- #17033: kind checking recursive types

- Stuff around #14198: inconsistent treatment of kind variables, and #16635 (making `checkExpectedKind` dumber)

- #16110 and #16762 refactoring of `HsImplicitBndrs`.
- #16763: get tyvars in the right order in partial type sigs
- Finish !1132: Refactor kindGeneralize and friends
- #15809, #8995: use level numbers for generalisation; see Richard/Simon Slack call 7 June 19.
- #16982, to move `TcTyVar` into its own type.

- Fix #14963, #16887: `tcSyntaxOp` refactoring

- Foralls and pretty printing
  * Point 1 of [Proposal 179: tweak printing of foralls](https://github.com/ghc-proposals/ghc-proposals/pull/179)
  * #16320
  * Check pretty-printing for foralls with mixed req/inf/specified
  * Print invisible arguments properly (#16456)
    ```
    T :: forall {k} k2. k2 -> Type

    f :: T {Type} @Type Int
    ```

- [Proposal 36: top-level kind signatures](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0036-kind-signatures.rst), ticket #16794. (depends on Proposal 81). 
  * See [this tweak](https://github.com/ghc-proposals/ghc-proposals/pull/227) for associated types.  The ticket is #16794.

  * Also #16734: we agreed that foralls from the signature _do not scope_ over the binding.

  * Also (see #16726).  We agreed that kind variables in kind annotations should stand for arbitrary kinds, just like pattern type signatures.
    ```
    data T (a::k) = MkT a
    ```
    Here `k` gets bound to `Type`.


- [Proposal 126: Type applications in patterns](https://github.com/ghc-proposals/ghc-proposals/pull/126).   #11350, and dup #15530, MR !2464

- [Make equality homogeneous](https://gitlab.haskell.org/ghc/ghc/wikis/dependent-haskell/phase2)

# Refactoring of existing stuff that we'd like to get done

- #17717: Refactor `mapType` to be like `foldType`. Should improve performance.

- #17718: Consider adding breadcrumbs to `CtLoc` to record how the constraint came to be (its proof path) and, for unsolved constraints, why unsolved.  Example: matches more than one Given quantified constraint. #17718

- #17698: `coVarsOfType` is almost certainly wrong

- #16947: refactor `Ct` and friends.

- #16982: break out `TcTyVar` into its own type.

- #15579: `topNormaliseType`; also #14729, #15549.  This is high priority: it's an outright bug causing Lint failures; it's not hard; and fixing it will close three tickets.

- #8095: coercion zapping.  Nearly done!  But not quite.  And will be valuable for everyone.

- #15977: restructure typechecking modules. New module `KcTyClsDecls` that pulls from `TcTyClsDecls` and `TcHsType`.
- #14873: make `typeKind` monadic in the type checker

- #14164: comments, invariant, asserts (Richard)
- #15577: surprising coercions: see comment:5
- #15621, #14185: using implication constraints to improve error messages
- `zonkPromote`: the remaining ones are there for a reason; but Simon still unhappy; see RAE/SLPJ Slack channel 31 Aug; and #15588, #15141, and #15589.  Stuff about "fully-known type variables".
- #15474: (small) `Any` etc.
- #15192: `GRefl`: still looking into perf changes
- Better floatEqualites based on level numbers?
- #16967: more of `Note [Let-bound skolems]`

- #15479: refactoring `tcHsType` (Simon is not 100% convinced)

- #17291: coercions are not expressions, and coercion variables are not Ids.  The horrible `eq_sel` and `coercible_sel`.

# I'm unsure of the status of these things

- (May 18) #14040, which I think is not fixed.  But it???s somehow linked to #15076.  And that in turn is caused by #14880.  Which Richard has a patch for that doesn???t quite work yet.  And the fix might cure #14887

- Homogeneous flattener (#12919, #13643).  [Phab:D3848](https://phabricator.haskell.org/D3848).   [ Phab:D4451](https://phabricator.haskell.org/D4451) is a patch to D3848 that fixes performance

- #11715: constraint vs \*

- How to fix #11719. We can't ever infer a type variable to have a higher-rank kind (as would seem necessary in this example). But perhaps we should type-check type patterns in a different manner than ordinary types, just like `tcPat` is distinct from `tcExpr`. Then, we could use bidirectional type-checking to get things to work out. This is a pretty significant refactor, though. Is it worth it? Or do we just wait until we have dependent types?

# Fuller list


- #12564: type family calls on the LHS of type instance equations.  Vladislav cares about this, and something looks do-able.

- Deliver on #13959 (`substTyVar` etc)
- Change flattener to be homogeneous (#12919, #13643)
- Sort out `mkCastTy`

  - Implement KPush in `splitTyConApp`. (#13650)
  - Some invariants to make sure of: No nested `CastTy`s. No `AppTy (TyConApp ... |> co) ty`. No reflexive coercions.
  - Change the premises to `LRCo` so that there may be an outer coercion. That is:

    ```wiki
    g : (t1 t2 |> co1) ~ (s1 s2 |> co2)
    -----------------------------------
    Left g : t1 ~ s1
    ```

>
> >
> >
> > There is more work to do to make this homogeneous.
> >
> >
>

- Implement homogeneous as per Stephanie's paper

  - An-Refl2 makes me think that the output of `coercionKind` would be hetero. Indeed it would. But we could still have `(~#) :: forall k. k -> k -> Type` because we don't have to abstract over hetero equalities. Note that Wanteds are CoercionHoles, and that we can always homogenize givens. This would also require storing `PredTree`s in `CtEvidence` instead of `PredType`s (because we can't write the type of a hetero coercion.
- Fix #11715 according to Richard's plan
- Generalized injectivity #10832, vis-a-vis Constrained Type Families paper
- Taking better advantage of levity polymorphism:

  - Could `[]` be a data family?
  - Unlifted newtypes
  - Unlifted datatypes
  - generalized classes in base
  - ...
- #11739 (simplify axioms)

  - Also: consider making a closed-type-family axiom into a bunch of top-level axioms using some proof of apartness. It might simplify the step in coercion optimization where we optimize a c.t.f. coercion only to abandon the changes because they break apartness constraints. It would also allow us to delete gobs of code dealing with "branched axioms" vs regular ones.
- Fix all the `TypeInType` bugs
- Clean up pure unifier to make the fact that kind coercions *only* affect type variables by using, e.g., `getCastedTyVar_maybe`.
- It seems that `quantifyTyVars` is duplicating some logic from `simplifyInfer`, in that it removes covars. This should really be done in `kindGeneralize`, because `simplifyInfer` *uses* `quantifyTyVars`. This should be just about possible, but with some twists and turns:

  - H98 constructors are strange in that they have tyvars that aren't mentioned in the type. So be careful here and make sure the type is closed (w.r.t. user-written tyvars) before calling `kindGeneralize`.
  - `tcFamTyPats` needs a hard look
  - So does `tcRule`.
  - Could also separate out `kindGeneralizeKind` and `kindGeneralizeType`. The latter works only over closed types.
  - If we remove the "remove covars" call from `quantifyTyVars`, we should really put it in `decideQuantifiedTyVars`. Perhaps we *don't* need to remove covars in `kindGeneralize` because `solveEqualities` will fail if any covars are around. It is **OK** to remove a covar without removing its kind, because the covar will be solved in the residual implication constraint from `simplifyInfer`.
  - Example of why we need to exclude coercions during generalization:

```wiki
data X where
  MkX :: Proxy a -> Proxy b -> (Refl :: a :~: b) -> X
```

- Remove `quantifyTyVars` call from `simplifyInfer`. Instead call `skolemiseUnboundMetaTyVars` from `simplifyInfer` directly.
- Stable topological sort may not be well specified. But we can always write a deterministic algorithm. Perhaps that should be in the manual.
- Can remove `closeOverKinds` in most places. Otherwise, just gather the kinds of user-written tyvars (e.g. fundep RHS)
- Re-do the fix for #13233. There are two separate problems:

  1. How to ascertain whether or not a primop is saturated during desugaring (or, possibly, earlier). On a call, we thought that we could do this in the desugarer by decomposing nested `HsApp`s, using a little stack data type to denote all the different ways a function could be applied (`HsApp`, `HsWrap` with the right wrapper, sections, tuple-sections, `HsTypeApp`, maybe more) uncovering what the function was underneath, and then checking how many parameters are supplied. But now, I think it's much better to do this in the type-checker, especially because the type-checker already decomposes nested `HsApp`s. (See `TcExpr.tcApp`.) When it discovers what the function is, it can check whether the function is a `hasNoBinding` primop. If so, it can eta-expand as necessary (but only if necessary) and use a new piece of `HsSyn` to denote a saturated primop. (It will be a new invariant that no unsaturated primop passes the type-checker.) This seems better than redoing the stack type in the desugarer. The original problem in #13233 was around levity polymorphism. If we make this change in the type checker, then the existing levity polymorphism checks should just work. We'll have to be careful to make the `HsSyn` structure printable in the way a user expects, so that the levity-polymorphism error message doesn't talk about an argument the user didn't write.
  1. How to make sure that saturated primops stay that way in Core. This would be a new check in Lint as well as new checks on any code that does eta-contraction. It has been suggested that levity-polymorphic primops desugar to a family of levity-monomorphic primops. This surely would work, but there doesn't seem to be benefit over a plan simply to keep primops eta-expanded always. Then, there's no worry about eta-contracting levity-polymorphic arguments.
- Make better use of the `uo_thing` field, including refactoring `noThing` away and improving term-level error messages.

  - Simon also asks that the contents of `uo_thing` should only be `HsSyn`. This would obviate the current zonking/tidying stuff. A quick pass suggests that this will be easy to do.
  - Also see #13819, where the current treatment of `uo_thing` leads to an outright bug.
- Take full advantage of `TcTyCon`, getting rid of the dreaded type-checking knot. (#13737)
- Document why we're not worried about casts in class wanteds. (Short story: any cast should be available for rewriting, and so it will rewrite the kinds.)
- Sort out `matchTypeable` (see email) #13333
- Fix equality printing: 

  - Remove IfaceEqualityTyCon in favor of a new IfaceEquality constructor of IfaceType, which would be the conversion of a TyConApp
  - Make explicit-kinds print the kinds (duh) and equality-rels control the equality relation (duh)
  - Print \~ for homo in practice; print `~~` for hetero in practice (unless equality-rels)
- Merge fsk and fmv treatment, by returning the list of created fsks from `solveSimpleGivens` (which would now be fmvs) and fill them in after solving the wanteds. This eliminates problems around the fact that zonking in the flattener might zonk fsks back to type family applications and that fsks might lurk in residual constraints.
- Remove `wc_insols`. The distinction isn't paying its way.
- It's terrible if we ever inspect a meta-tyvar in pure code. Something is surely awry. Add an `ASSERT` to `coreView` to stop this from happening and fix any consequences.

## Completed tasks

- DONE (in !361) [Proposal 103: treat kind and type variables identically in forall](https://github.com/ghc-proposals/ghc-proposals/pull/103) (depends on Proposal 83). Includes applying the "forall-or-nothing rule" to kind variables. The proposal says "wait until two releases after Proposal 83 is done (which was in 8.6)".  So we can do this in HEAD as soon as 8.8 forks.  Subsumes #14548.  See also #16110 (comments around 8 May).

- DONE [Proposal 81: Visible dependent quantification](https://github.com/ghc-proposals/ghc-proposals/pull/81).  Just syntax!  Lets you say `forall a -> ty` in types.  See [GhcKinds/KindInference](ghc-kinds/kind-inference) and [GhcKinds/KindInference/Examples](ghc-kinds/kind-inference/examples).

- Remove pushing from `mkCastTy`. But see bullet above about remaining tasks.
- Remove `solveSomeEqualities`
- Take a look at `tidyToIfaceType`: I don't think it needs to tidy the env.
