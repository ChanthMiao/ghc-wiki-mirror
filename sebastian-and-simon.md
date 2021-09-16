# Agenda

I move stuff here from the different sections that is on our radar at the moment.

- #20111, !6168: Bug in `exprMightThrowPreciseException`
  - Morally, we need a new `primOpHasExternallyVisibleSideEffects`. But that's a huge time sink with large potential for regression (#17653)
  - So just add `CatchOp` to "the list" for now?
- #19970, #20269, #20273: Floating case expressions, related to `Note [Case MFEs]`
  - In general it seems like non-cheap case scrutinees get in the way of eta-expanding PAPs
- #19871, !5790 boxity analysis.  See also https://gitlab.haskell.org/ghc/ghc/-/issues/19824#note_353112
  - Nice idea for a paper: DmdAnal, (then termination analysis,) then Boxity analysis, which infers boxity for parameters and results.
- !5667: Nested CPR light, part 2
- #5075, !4229: CPR for sum types
  - Nearly no regressions after !5667 and simpler analysis code
- #14816, !5349: Drop `reuseEnv` in DmdAnal
  - I got hung up on improving efficiency of fixed-point iteration which got more costly as a result. Should we worry? Maybe add GHC.Ix as a regression test and be done with it
- #19584: Demand Analysis scales quadratically
  - Many ideas there, but it is much more complicated than it seems. Monotonicity issues etc.
  - !5583 might be a faintly related first step at improvement here
  - I'm thinking that we need to properly track data dependencies, e.g., the unstable set idea
- Arity analysis vs. Termination analysis: What are the differences between `exprOkForSpec` and `exprIsCheap`?
  - Divergence is cheap, but not speculatable
  - Cheap means "Are we OK with evaluating it multiple times or not at all when normally we'd evaluate it exactly once?", e.g., may we defer and unshare the work?
  - Speculatable means "Are we OK with evaluating it (once) when normally we might not evaluate it at all?"
  - Why can't they share more code? It feels pretty similar; E.g., HNF things are both speculatable (ignoring the annoyance with eval'd unfoldings of case binders) and cheap

# Simplifier and occurrence analysis

- #19529: glomming and occurrence analysis, Dependency analysis of IMP-RULEs

# Exceptions

- #20121, #20111 side effects

# Closure representation

- #14461: Closure sharing
  - Zack is on it, but results aren't particularly enlightening.

# CPR

- #13331, #19276, #19326: CPR, free variables, and loss of sharing
- #19309: conditional CPR and free variables
- #19240: Don't pass constants/free variables in unboxed tuple
- #19309: Do CPR even if not all code paths construct products by returning an unboxed sum
- #18174, !1866: Nested CPR Light
  - On hold again, no clear wins and even regressions. Prbobably loses too much thunk sharing.
  - Probably related to not doing CPR for recursive data cons. !5667 does much better because of it.

# Demand Analysis

- #20325: cost model for w/w (not hard)

- #19917: better w/w for bottoming functions

- #18907 Product demands

- #18349 Trimming of DmdAnal results.
  * !3466 (merged) fixes #18304, but Andreas pointed out a shortcoming; !3558 (merged) fixes that; but we need a regression test for the latter; then close #18304
  * !3516 is a failed attempt to break the loop.
  * Sebastian thinks he has a better way to detect potentially-recursive type constructors

- #14620 join points [this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/14620#note_315900)

- #14816, [this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/14816#note_315980)
  - Drop `reuseEnv` in DmdAnal, check `lazy_fvs` for equality.

- #18885: Make product strictness demands relative
  - In adding hack after hack, I felt less and less confident that it works.
  - I think we only want the product demand to apply relatively, when the outer
    cardinality is lazy (e.g. 0). See
    https://gitlab.haskell.org/ghc/ghc/-/issues/18885#note_315189
    for a summary.

# Return-pattern specialisation

- #16335 return pattern specialisation
  - Exploring this since CPR turned out to suffer too much from loss of sharing
  - As part of SpecConstr or as a separate pass?

# SAT

- #18962, !4553, #9374: Only SAT unfoldings
  - Works as follows:
    1. Do SA transform in Simplifier by attaching the SAT'd defn as an unfolding
    2. Then do callSiteInline that unfolding
    3. SA analysis just before OccurAnal. Pretty simple stuff (150loc)
  - But INLINABLE will override the unfolding with a stable one (I think?). Should we SAT that unfolding? See for example `GHC.Utils.FV.mapUnionFV`. It seems that INLINABLE is quite useless, it doesn't even specialise or anything. But zapping SA info for now.
  - We can try to "solve" stream fusion this way. See [the stream-fusion paper, section 7.2 "Static Argument Transformation"](http://fun.cs.tufts.edu/stream-fusion.pdf). The key missing features:
    - Managed to optimise that example, simply by SA analysing each binding of the mutually recursive group in isolation and then taking care not make specialisable functions loop-breakers
    - But running into tick-exhaustions on `>>=` on `CmdM`, so I opened some unwanted back door. How to debug?

- Ultimately pick up the SAT work again #18962, but I feel like we need a better story for derived unfoldings here
  - Maybe new unfolding source? Or attach unfolding deriving strategy to
    InlineRHS. On the other hand, it would also be useful for stable unfoldings..

# Join points



# Eta expansion

- !4700: Refactor arity analysis
- !1492: free variable traversals
- #19302: eta-expand SimplM
- #19251 INLINE makes things worse: again eta-expansion.
- #18993: regression in 9.1 due to eta-expansion

- #18231: eta expansion. Mysteries remain.
  - In particular, we wondered whether (or when, rather) `etaExpand` has to expose lambdas manifestly. Makes a difference for PAPs (special case: trivial exprs?)
  - We investigated call sites of `etaExpand` and concluded that the only call site that really needs lambdas manifestly is CoreToStg.Prep
  - On inlining PAPs: Makes sense operationally (so do it before STG), but keeping PAPs makes bindings much more likely to inline
  - (Apparently, CoreToStg.Prep has its own eta expander)
  - SPJ: "in mkLam I think it'd be fine not to eta-expand a trivial exprssion" (despite Note [Eta expanding lambdas])
- #18202, #18238: state hack
  - We don't care to preserve one-shot-ness in the compiler. But it's also only use site info, so that should be fine
  - e.g. `exprEtaExpandArity` only returns `Arity`, not `ArityType`, and so on
  - Also eta reduction (for e.g. trivial expressions) loses one-shot-ness
  - Idea: Preserve one-shot-ness in the pipeline (for eta contraction in particular!), only do full eta contraction in CorePrep
  - Thought: one-shot is like call-by-name, cf. "Kinds are Calling conventions", then `multiShot (a ~> b) -> (a -> b)` and `oneShot` other way around
  - We floated around the idea of having an explicit `MultiShot` annotation for lambdas, but that isn't effective
    - Think about `multiShot expensive ==> let f = expensive in \x{ms}. f x`. What if `expensive = \y{os}. e`? Then we will inline into `e`! Bad
  - Fundamental: `let f y{ms} = e in \x{os}. f x`
    - eta reduce, then inline ==> `\y{ms}.e`
    - inline, then beta reduce ==> `\x{os}.e[x/y]`
    - In some situations, we want one, in some we want the other!
    - Idea: No eta reduction whenever there's os or ms, only if there is no annotation

- #17881, #17896: eta reduction (based on usage Demand)

# Pattern-match checking

- GHC proposal 400, #14422: Return type signatures for COMPLETE pragmas

## Ideas

- Use e-graph representation

# Misc

- #19001: float-out and case-expressions. Quick win here?
- #17900, !3013: primop effects
- #15532: Levity polymorphism and ANF
  - We talked about it with Richard and came to the understanding that it would probably work, but entail refactorings of Core to Core passes which assume they can just let-bind everything.
  - Also we shouldn't worry about it until we need it. But it's a logical next step after we have unlifted datatypes, otherwise there is no chance of code re-use.

- #18927: Use `SmallArray#`
  - I have a handy small library now, just have to use it
  - But I got distracted by trying to solve list fusion, again...
  - Anyway: I think we want an explicit fusion framework exposed as maybe Data.Stream.FB
  - Stalled because I need a PrimOp introduced in 8.10

# On hold

- https://gitlab.haskell.org/ghc/ghc/tree/wip/ext-arity: Rebased Zach's implementation of the extensionality paper
  - Wait for levity polymorphism and matchability polymorphism to work out

- #915: Specialisation through type classes/defunctionalisation
  - #17592: Specialisation for call patterns is quite unreliable:
    ```hs
    f :: Maybe Bool -> Int -> Int
    f (Just True) 0 = 1
    f (Just True) n = f (Just True) (n-1)
    f _ 0 = 2
    f x n = f x (n-1)

    g x n = f (Just x) n
    h n = g True n
    ```
    There are situations in which `g` has not been inlined into `h` by the time SpecConstr runs. SpecConstr will then create two specialisations: One for `(Just True, _)` (`f1`) and one for `(Just _, _)` (`f2`), the former of which is a strict specialisation of the latter. The simplifier will then rewrite the call site in `g` to `f2`. Now, at some point `g` will be inlined and we see the call site `f2 True n`, which we *could* rewrite to `f1`. But all specialisation rules only apply to `f`, so we can't do the rewrite. The solution is simply to attach a derived specialisation rule to `f2`.
  - (Obsolete) Why not do specialisation of recursive functions instead of inlining them, as part of the simplifier? Consider separate pragma `{-# SPECIALISABLE #-}` or something
  - Pros:
    - No complicated and brittle reliance on rewrite rules
    - Like `INLINE`, the pragma is persisted throughout transformations
    - It seems like the logical way to do inlining for recursive functions
  - Cons:
    - Probably quirky for complicated recursion schemes
    - How does this work for rewriting recursive call sites? Seems impossible without RULEs and thus SpecConstr. OK, that won't work

- https://github.com/ghc-proposals/ghc-proposals/pull/43 Or patterns: Potential bachelor's thesis?
  - osa1 ultimately after a long and tedious discussion gave up.
  - Why? What's needed? A formal Specification? Which part? Static or dynamic semantics?
  - Also how much? Whole pattern language or just enough of a fragment to explain or patterns?
  - I see there is https://gitlab.haskell.org/rae/haskell as a starting point, but it seems to focus entirely on static semantics. But probably the document to complete?
  - We talked about it; it's a matter of pushing the proposal forward rather than investing actual elbow grease into an impl.