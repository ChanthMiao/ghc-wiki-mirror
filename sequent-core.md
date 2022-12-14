# Join points

- [Compiling without continuations](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations) describes the design.  It grew out of an earlier paper: [ Sequent calculus as an intermediate language](https://www.microsoft.com/en-us/research/publication/sequent-calculus-as-a-compiler-intermediate-language/)

- Repo:  `git://github.com/lukemaurer/ghc`, branch `wip/join-points`

- Ticket to track progress: #12988

- Phab patch: [https://phabricator.haskell.org/D2853](https://phabricator.haskell.org/D2853)

- New variant of Core.

  - IdDetails has a constructor for JoinPointId, with its arity.
  - Join points can be recursive
  - Lint checks many invariants about join points
  - All transformations are "join-point aware"; that is, they maintain join-point-hood.
  - Nullary join points do not take a void argument (as they did before).
  - Join-point Ids survive in Iface unfoldings

## Tickets

See the ~"join points" label.


## Tour of the patch


- Core changes

  - `compiler/coreSyn/CoreSyn.hs`:
    Mostly of interest for the new Notes giving an overview and the invariants on join points.
  - `compiler/basicTypes/IdInfo.hs`:
    The all-important change to `IdDetails`. Also changes to `OccInfo` for use by the occurrence analyser.
  - `compiler/basicTypes/Id.hs`:
    Basic functionality for checking and assigning the join-point-hood of an identifier.
  - `compiler/coreSyn/CoreLint.hs`:
    Checks for all the new invariants.
- Occurrence analysis

  - `compiler/simplCore/OccurAnal.hs`:
    Retrofitted occurrence analyser now looks for join points, marking each local binder that can become one.
  - `compiler/simplCore/CoreSubst.hs`:
    After the occurrence analyser marks binders to become join points, either `simpleOptPgm` or the simplifier
    actually changes the `IdDetails` (and makes other adjustments like eta-expansion).
  - `compiler/simplCore/CoreArity.hs`:
    Has a hand in converting bindings to join points; also used by Specialise to produce rules that comply with the invariants. Eta-expansion is a nice example of something that had to change to meet the new invariants???when we push arguments and casts inward, they have to be pushed into join points and disappear from jumps.
- The simplifier

  - `compiler/simplCore/SimplEnv.hs`
    Join points float separately from values, since they're severely restricted in how far they can float.
  - `compiler/simplCore/SimplUtils.hs`
    Just a few technicalities.
  - `compiler/simplCore/Simplify.hs`
    The "case-of-join-point" functionality goes through `simplJoinRhs` (which copies the continuation into the join point) and `simplIdF` (which now throws away the continuation from a jump). Most of the rest of the changes are more mundane. `matchOrConvertToJoinPoint` is where we convert what the occurrence analyser says we can.
  - `compiler/coreSyn/CoreUnfold.hs`
    Changes to `sizeExpr` have made a pretty big impact by letting more things get inlined, particularly things with tail-recursive loops, since those loops are often turned into join points and hence get counted as cheaper (since they're not allocated).


The rest of the changes are rather more isolated???places where we needed to change something to preserve the invariants on join points. There are also new tests in `testsuite/tests/perf/join_points` and a few updated tests.

## Benchmarks

- [imaginary.nofib](/trac/ghc/attachment/wiki/SequentCore/imaginary.nofib)
- [spectral.nofib](/trac/ghc/attachment/wiki/SequentCore/spectral.nofib)
- [real.nofib](/trac/ghc/attachment/wiki/SequentCore/real.nofib)
- [shootout.nofib](/trac/ghc/attachment/wiki/SequentCore/shootout.nofib)


The `shootout` tests are non-standard, but it's hard to ignore "-100.0%" in allocations (for both `fannkuch-redux` and `n-body`). Join points (and the "case-of-join-point transform" in particular) are apparently quite good at wringing out the last few allocations from an already heavily optimized loop by making sure the inner loops actually compile as mere loops rather than closures.


Among the standard tests, the biggest winner is `spectral/puzzle` at -21.1% allocations. Partly this is due to the updated inlining heuristics that give join points a discount compared to functions. Sadly, the new heuristics are also responsible for the biggest loser by far, which is `spectral/boyer2` at +7.5% allocs. In `RewriteFns`, the new heuristics cause `$wonewayrewrite1` to be inlined into `$wonewayrewrite1lst`, causing an explosion of code but few gains. (Rewriting `$wonewayrewrite1lst Nil _ u` as `(# True, u #)` would help, but tragically `$wonewayrewrite1lst` is now far too big to inline or to specialize.)

# Overview

## Join Point Analysis (JPA)


Join Point Analysis (JPA), implemented as part of the occurrence analyser, is a new analysis that identifies potential join points, and marks them to be converted. The simplifier (or `simpleOptPgm`) will then perform the conversion and propagate the change to the occurrence sites.

## Transformations


These places need to be made join-point aware

- Worker/wrapper for strictness: we do want w/w for arguments, but not for the return side (CPR).

>
>
> We can't do CPR because (in the recursive case) the worker calls the wrapper, so it needs to be a join point, but a CPR wrapper always invokes the worker from a `case` expression, so it can't be a join point. Fortunately, CPR is rarely necessary for join points because they benefit from the CPR on their context:
>
>
> ```wiki
>   f z = let join j x y = (x+1, y+1)
>         in case z of A -> j 1 2
>                      B -> j 2 3
> ```
>
>
> Performing CPR on `f` gives us
>
>
> ```wiki
>     f z = case $wf z of (# a, b #) -> (a, b)
>   $wf z = case (let join j x y = (x+1, y+1)
>                 in case z of A -> j 1 2
>                              B -> j 2 3) of (a, b) -> (# a, b #)
> ```
>
>
> and now the simplifier will push the outer `case` into the join point:
>
>
> ```wiki
>   f z = case $wf z of (# a, b #) -> (a, b)
>   $wf z = let join j x y = (# x+1, y+1 #)
>           in case z of A -> j 1 2
>                        B -> j 2 3
> ```
>
>
> (But what if the join point has the CPR property but the outer function doesn't? Seems like we're still ahead because original GHC would've ruined the join point.)
>
>

- Float In is crucial for finding join points, especially recursive ones. Consider:

  ```wiki
  f1 x = let j y = ... j z ... in
         case j x of A -> ...
                     B -> ...
  ```

  If neither branch mentions `j`, then `j` *could* be a join point if we moved it inward a bit:

  ```wiki
  f2 x = case (let join j y = ... j z ... in
               j x) of A -> ...
                       B -> ...
  ```

  Now the call to `j z` is in tail position with respect to `j`'s definition, so `j` can be a join point. However, the existing Float In pass goes a bit too far:

  ```wiki
  f3 x = case (let j y = ... j z ... in
               j) x of A -> ...
                       B -> ...
  ```

  This is *equivalent* to the second version, but it doesn't follow the join point invariant.


  


>
>
> This is a funny habit of the Float In implementation: it often floats a `let`-bound function inward so far that the body of the `let` becomes just the identifier itself. Normally the simplifier fixes this right up, so it hasn't ever mattered, but the simplifier will just move the `let` all the way out again, turning `f3` back into `f1`. We need Float In to get it exactly right, since handling `case`-of-recursive-join is exactly what lets us do fusion with recursion.
>
>

- Float-out.

  - First approximation: don't float join points at all.
  - Nullary join points, if floated, cease to be join points but instead become shared thunks.  On balance this is a win.
  - Floating to top level.  Doesn't make much difference either way.  BUT we lose the ability to move case context into the join point. eg

    ```wiki
    f x = let j y = blah in
          case x of
            True  -> j 1
            False -> j 2
    ```

    Now if we inline `f` into a case scrutinee, the case will move into `blah`.  BUT if we float `j` to top level.  So you might think that floating to top level was harmful. But consider (non-recursive case):

    - If `blah` is big, `f` will not inline, so we will never wrap its RHS in a case.
    - If `blah` is small enough for `f` to inline, then a fortiori `j` will inline too.
      Moreover, floating to top level makes f more likely to inline.  Example:

      ```wiki
      f x y = let j v = blah(strict in v) in
              case x of
                A -> j y
                B -> j y
                C -> x
      ```

      Here `f` is strict in `x` but not `y`.  If we float the joint point to top level, `f` can inline, which exposes the strictness in `y`.

>
> >
> >
> > If `j` is recursive, the above argument doesn't apply; not floating a small join point would be good, so that f can inline with it intact.
> >
> >
>

- Simplifier, obviously.  Moving the context into the RHS of join points.  Never float a join point at all.

- Rule matcher does some let-floating of its own.

  ```wiki
  RULE   f (g x) = x+1

       ...(f (let v = e in g (v-2)))....
  ==> (rule fires)
       ...(let v = e in
           let x = v-2 in
           x+1)...
  ```

  Be careful not to do this for join points, since you can't float a join point out of an argument.

- NB: Float-in is a transformation that often creates join points:

  ```wiki
       let f x = ...f x'... in
       case f t of alts
  ==>
       case (let f x = ...f x'... in f t) of alts
            -- Now f is a join point!
  ```

  NB: the very next run of the simplifier will float that `let`-binding for `f` out of the `case` scrutinee.  So it's important to look for join points before running the simplifier again.  Thus: (float-in; then find-join-points; then simplify)

- Added Late Lambda Lift.  But still work to do here.

## Cases where we win


Add `testsuite/test/perf/join-points/`

- For each place where you had to work to retain join points, make an example in which GHC currently destroys one, and behaves badly as a result.  Plus some examples like Section 4.3 in the paper.

- The new simplifier opens new possibilities for fusion, especially in the `unfold`/`destroy` tradition (which we can think of as stream fusion without the `Skip` constructor).

  ```wiki
  data Stream a = forall s. Stream (s -> Step a s) s
  data Step a s = Done | Yield a s

  {-# INLINE stream #-}
  stream :: [a] -> Stream a
  stream xs = Stream snext xs where
    snext []     = Done
    snext (x:xs) = Yield x xs

  {-# INLINE unstream #-}
  unstream :: Stream a -> [a]
  unstream (Stream next s) = go s where
    go s = case next s of
             Done       -> []
             Yield x s' -> x : go s'

  {-# INLINE sfilter #-}
  sfilter :: (a -> Bool) -> Stream a -> Stream a
  sfilter p (Stream next s) = Stream fnext s
    where
      fnext s = 
        let fgo s = case next s of
              Done -> Done
              Yield x s' | p x       -> Yield x s'
                         | otherwise -> fgo s'
         in fgo s

  filter :: (a -> Bool) -> [a] -> [a]
  filter = unstream . sfilter . stream
  ```

  Getting good performance out of fusion depends on getting rid of the `Done` and `Yield` constructors, which are never intended to create long-lived data, only to direct control flow. Hence any allocation is a waste.
  So how does `filter` get compiled? Inlining the three constituents `unstream`, `sfilter`, and `stream` gets us here (after a bit of floating):

  ```wiki
  filter xs =
    let snext []     = Done
        snext (x:xs) = Yield x xs

        fnext s = 
          let <join> fgo s = case snext s of
                Done -> Done
                Yield x s' | p x       -> Yield x s'
                           | otherwise -> fgo s'
           in fgo s

        go s = case fnext s of
                 Done       -> []
                 Yield x s' -> x : go s'
    in go xs
  ```

  Note that `fgo` is flagged as a join point; this will be crucial! Now, `snext` is non-recursive, so it inlines happily enough into fnext:

  ```wiki
    ...
    let fnext s = 
          let <join> fgo s = case (case s of []     -> Done
                                             x : xs -> Yield x xs) of
                Done -> Done
                Yield x s' | p x       -> Yield x s'
                           | otherwise -> fgo s'
           in fgo s
    ...
  ```

  And the usual case-of-case transform does its magic:

  ```wiki
    ...
    let fnext s = 
          let <join> fgo s = case s of
                [] -> Done
                x : s' | p x       -> Yield x s'
                       | otherwise -> fgo s'
           in fgo s
    ...
  ```

  But can we do the same when we inline `fnext` into `go`?

  ```wiki
  filter p xs =
    let go s = case (let <join> fgo s = case s of
                           [] -> Done
                           x : s' | p x       -> Yield x s'
                                  | otherwise -> fgo s'
                     in fgo s) of
                 Done       -> []
                 Yield x s' -> x : go s'
    in go xs
  ```

  The original simplifier gets stuck here; `fgo` will get floated out, but the `Done` and `Yield` constructors will remain. Since `fgo` is a join point, however, the new simplifier will instead pull its context *in*:

  ```wiki
  filter p xs =
    let go s = let <join> fgo s =
                     case (case s of
                             [] -> Done
                             x : s' | p x       -> Yield x s'
                                    | otherwise -> fgo s') of -- (*)
                       Done       -> []
                       Yield x s' -> x : go s'
               in fgo s
    in go xs
  ```

  And now case-of-case does the rest:

  ```wiki
  filter p xs =
    let go s = let <join> fgo s =
                     case s of
                       [] -> []
                       x : s' | p x       -> x : go s'
                              | otherwise -> fgo s'
               in fgo s
    in go xs
  ```

  We are left with the `filter` function as it would be hand-written.

>
>
> The alert reader may have an objection: On the starred line above, there is a jump (a call to a join point) that is not in tail position (with respect to the join point). Indeed, that example would not pass Core Lint! However, that snippet is a bit of a fiction; the Core AST never takes that form. Rather, the Core code shown here represents the state of the simplifier as it carries the `case` continuation inward. Whenever the simplifier comes to a jump, it throws away the continuation, thus maintaining the invariant that the jump is a tail call.
>
>

>
>
> (For the reader familiar with continuation-passing style or the sequent calculus: The case-of-case transform is simply an administrative reduction, substituting a continuation for free occurrences of a continuation variable. A join point (itself actually just a continuation) usually contains free occurrences of a continuation variable, whereas a jump does not. Hence the above behavior, pushing the context into a join point but leaving it off of a jump.)
>
>

- NB: that sometimes `go` functions do not start life as join points; we could also write `sfilter` above simply as

  ```wiki
  sfilter p (Stream next s)
    = Stream fnext s
    where
      fnext s = case next s of Done                   -> Done
                               Yield x s' | p x       -> Yield x s'
                                          | otherwise -> fnext s'
  ```

>
>
> Here `fnext` is not a join point, because it is not called in a saturated way.   But when we inline `sfilter`, it becomes one.  We must spot this pronto before we destroy it.  (A reason for doing JPA in the occurrence analyser.)
>
>

- The original Float Out is quite hazardous to join points. Since a join point is never allocated as a closure, floating it out doesn't improve sharing, and in most cases it can't be a join point anymore, so floating only *increases* allocations. (As always, there may be second-order effects, however; for instance, floating outward may leave behind a function that's small enough to inline.)

  ```wiki
  f x =
    let g y =
      let <join> j z = ... x ...
      in case p y of A -> j 1
                     B -> j 2
    
    in ...
    
    =>

  f x =
    let j z = ... x ... -- ruined!
        g y = case p y of A -> j 1
                          B -> j 2
    in ...
  ```

>
>
> We do still want to float out join points, however, just not too far. A good example occurs during unfold/destroy fusion, where a chain of filters becomes a series of trivially nested "loops":
>
>
> ```wiki
> filter odd . filter (> 4)
>
>   =>
>
> \xs ->
>   let next xs0 =
>     let <join> go1 xs1 =
>       let <join> go2 xs2 =
>         case xs2 of []    -> []
>                     x:xs' -> case x > 4 of False -> go2 xs'
>                                            True  -> case odd x of False -> go1 xs'
>                                                                   True  -> x : next xs'
>       in go2 xs1
>     in go1 xs0
>   in next xs
> ```
>
>
> Here we consider two filters, but this works to arbitrary depth. Since `go1` does nothing but invoke `go2`, it is just a needless bit of indirection. Float Out de-nests the loops:
>
>
> ```wiki
> \xs ->
>   let next xs0 =
>     let <join>
>       go1 xs1 = go2 xs2
>       go2 xs2 =
>         case xs2 of []    -> []
>                     x:xs' -> case x > 4 of False -> go2 xs'
>                                            True  -> case odd x of False -> go1 xs'
>                                                                   True  -> x : next xs'
>     in go1 xs0
>   in next xs
> ```
>
>
> And now `go1` can be inlined, completing the flattening process.
>
>

## Alternatives

- Rather than enforce a new invariant, we could give a semantics to non-tail calls to join points by seeing them as *abortive continuations*, as seen in Scheme. This would wreak havoc on Core, however! Calling a continuation would constitute a side effect; in fact, one could write `call/cc` quite easily:

  ```wiki
  callcc f = let<join> k x = x in f (\y -> k y)
  ```

  Keeping the join point invariant effectively restricts us to those programs for which jumps and function calls act in precisely the same way, thus making most of the Core-to-Core (and Core-to-STG) machinery blissfully unaware of the new construct.

- However! As described in [the paper](https://www.microsoft.com/en-us/research/publication/compiling-without-continuations), the abortive semantics *does* work if we only allow a jump inside an evaluation context:

  ```wiki
  let<join> j x = x + 1 in (j 1) True
  ```

  We can, of course, throw away the evaluation context at compile time, rewriting the body as simply `j 1`, since the jump aborts the function call. In fact, the simplifier already effectively does this, since it's often necessary after doing the case-of-join-point transform. Relaxing the type system in this way might allow other passes to be more loose so that the simplifier could clean them up. (Running the simplifier with case-of-case enabled would then be absolutely necessary before Core Prep, unless we want to mess with code gen as well.)

>
>
> It's not immediately clear what concrete advantage there would be, though, and a tighter type system finds more mistakes.
>
>

- The paper gives join points types like `Int -> Int -> forall a. a`, for a join point taking two `Int`s. Core Lint would still check that the body of the join point has the same type as its context, but this way we wouldn't have to fiddle with the types of join points after case-of-join-point. It would also work well with abortive semantics, as the extra type argument serves to give the jump a type that couldn't be inferred otherwise:

  ```wiki
  let<join> j x = x + 1 in (j 1 @(Bool -> Int)) True
  ```

  (In fact, this typing scheme is required for the abortive semantics so that we can have `exprType` still give a unique type to any expression.)

- Conceivably, we could allow passing join points as arguments. This is especially pressing because pattern synonyms desugar essentially as CPS functions that take the desugarer's "join points" as arguments (and without primitive existential types, it's hard to see how else to do it), so as things stand, the desugarer can't always create join points. I (Luke) see two problems with allowing labels as parameters:

1. If any labels-as-parameters are around after Core Prep, we need codegen and RTS support. Off the top of my head, one could use a pointer into the stack to represent the label; the pointer would point at the return address and so jumping would mean setting the stack pointer and then jumping where it now points. But I'm sure the back-end guys would tell me where this is crazy.


    


1. It becomes necessary to know a function's exact arity (its "join arity," counting type arguments) so that we know what constitutes a tail call, because passing a join point to a function would only be allowed when tail-calling the function. This turns arity from a squishy property to a hard invariant. At this point, we might as well be implementing Strict Core, or at least the "Types Are Calling Conventions" paper with its polyadic functions of fixed arity. (We could allow join points to take join points as arguments, but that's not nearly as useful.)

## Still to do

- ~~Try not propagating join points to occurrences in `findJoinsInPgm`; instead rely on simplifier.~~ (done)

- Desugarer should not add Void args to nullary join points.

  - Still needs to do this for unboxed types. The desugarer can't make genuine join points because the "join points" it creates sometimes get passed around as arguments rather than tail-called directly (happens particularly with pattern synonyms).
  - Also needs to do this for types with levity-polymorphic kinds, since a let binding with a levity-polymorphic kind isn't allowed, but `Void# -> r` has kind `*` even if `r` is levity-polymorphic.
  - Also needs to do this in more cases yet to be determined precisely. Rewriting `mkFailurePair` in the obvious way to add the `Void#` only for unlifted or levity-polymorphic types breaks test T12698.

- Dump the CoreToStg join point analysis in favour of the known join points.

  - ~~Check: does the CoreToStg analysis miss any JoinPointIds~~ (warning now in place)
  - Known cases where CoreToStg finds a join point where OccurAnal doesn't:

    - Function is polymorphic in its return type
    - Function is mentioned in RULES, which Core Prep strips

      - Could solve this by running OccurAnal one last time *after* Core Prep (would need to turn off binder swap); this might also eliminate dead code kept alive by RULES only

        - Harder than it sounds: after Tidy, top-level vars are marked as global, so need to modify OccurAnal so it works either before or after Tidy
        - Also need to change occurrences of newly discovered join points; normally we lean on the simplifier to do this, but *that* would be *much* harder after Tidy

- Question: since STG is untyped, could it find more join points than JPA does?)

  - Yes, at least in one case: when the putative join point is polymorphic in its return type.

- ~~Currently CorePrep adds a void arg for a nullary join point.  Check: why?  What goes wrong if we don't do this?~~ (done; `CoreToStg` relied on the old let/app invariant)

- Idea: heap check for join point done at call site, not in join point itself. (Does not work for recursive join points.)

- ~~`CoreUnfold.sizeExpr`: SPJ claims: we should charge nothing for a join point binding or for its lambdas, or for its call.  (Reason: a join-point binding generates no allocation.)  Luke thinks that this was catastrophic in at least one case.  Investigate.~~ (done; *fully* implementing the suggested change is a big win)

  - If we charge nothing for a join point binding and its lambdas, and 10\*n for a jump with n args (where a function call is 10\*(n+1)), nothing changes except that `boyer2` gets +7.5% allocations (due to a cascade from an unfortunate inlining) and `parser` gets -1.2%.
  - If instead we charge nothing *at all* for a jump, `boyer2` still gets +7.5% but `puzzle` gets -21.1% (!). (Also `cryptarithm` gets -1.6%.)
  - Charging nothing for a jump, nothing for a join binding, *and* nothing for the lambdas makes `boyer2` break even again. Now it's an improvement nearly across the board; implemented.
  - **BUT:** Charging *nothing* reopens bug #6048 by allowing certain join points to keep getting inlined, leading to exponential behavior. Currently solved by charging for jumps, but only 20% as much as for function calls. This number was arrived at because it is small enough that `puzzle` still gets its big improvement, but big enough to prevent #6048. TODO Worry about overfitting. Possibly tune this some more. Maybe it should be a command-line option?

- Do Late Lambda Lifting (followed by simplify) *after* `CoreTidy`.

  - Then post LLL unfoldings won't affect downstream modules
  - But newly-small functions can still be inlined
  - Absolutely requires Arity/CAF info to be fed back from `CoreToStg`

- ~~Join points are always fully eta-expanded, even when they would be trivial otherwise. This greatly simplifies many traversals, since typically the first step in processing a join point of arity N is to grab exactly N lambdas. The problem is that `exprIsTrivial` then returns `False`. This is particularly bad in `preInlineUnconditionally`, so there we check if a join point is eta-reducible to a trivial expression. But it's an ugly workaround, and there are other issues as well (sometimes trivial join points become loop breakers, for instance). Better would be to relax the invariant to allow trivial join points to elide lambdas, then provide a convenience function to eta-expand when needed (not hard to do for a trivial expression!).~~ (done)
