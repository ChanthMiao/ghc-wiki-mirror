Go up to [Key examples # 3](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Key-examples#example-3-lcc-and-licc-threaten-confluence)

## Q's for SPJ

[AntC] Following an email exchange with SPJ:

<blockquote>
@simonpj Don't forget the (a) improvement vs (b) instance selection separation.  It's important because consider

```haskell
class C a b c |  a -> b             -- [AntC] note non-Full FunDep
instance C Int Bool Char
```

and `[W] C Int alpha v`, where `v` is (say) a skolem variable.   Then improvement spits out `alpha ~ Bool`, even though the instance doesn't match.

-- previously (see Key Example 4, although the discussion to be moved from there)

GHC does two *entirely separate* steps:

1. Improvement of a Wanted. The only effect is to emit new equality constraints
2. Discharging a Wanted using an instance.

</blockquote>

More starkly, these classes/instances are accepted (we don't need an extraneous parameter to the class):

```haskell
class Chaos a b  | a -> b                         -- Full FunDep
    
instance {-# OVERLAPPING #-}                               Chaos Int (Maybe Bool)
instance {-# OVERLAPPABLE, LIBERAL #-}      (b' ~ Char) => Chaos Int (Maybe b')
    
instance {-# OVERLAPPABLE, LIBERAL #-} Chaos2 (Maybe b) => Chaos Int b
    
class Chaos2 b  | -> b                            -- null LHS of FunDep
instance                                                   Chaos2 (Maybe String)

instance {-# OVERLAPPABLE, LIBERAL #-}    (b' ~ Float) =>  Chaos2 (Maybe b')

```

And it's easy to get `[W] Chaos Int Float` accepted -- even though it directly contradicts the `OVERLAPPING` first instance.

Now with `(... ~ t)` in a constraint (rather than `t` in the head), GHC never [but see next para] 'spits out' an improvement `alpha ~ t` -- which is the trick to getting a type-level `TypeEq` to work. And yet `~` constraints are supposed to be morally equivalent to writing the equated type directly(?) To be clear: I don't have use cases for these examples/I'd prefer they get rejected -- but I do want `TypeEq` to work.

Back with SPJ's 3-parameter `C`, with `instance {#- OVERLAPPABLE -#} (b ~ String) => C Int b ()` it's also easy to get `[W] C Int alpha ()` accepted, and thereby get `alpha` improved to `String` -- again contradicting the FunDep, and making it seem the FunDep should be `a c -> b`. Is this the same sort of "spit out"? It happens as a result of matching to the instance, so raising new Wanteds.

It's fairly easy to get improvement going different ways in different equations, then getting the program rejected `Bool /~ String`. I haven't managed to get a contradicting program to compile and seg fault. Perhaps if I put the contradictions in separate modules?

This feels like getting different answers to the same question, depending on how you ask it: 'What are six sevens?' ===> 42; 'Are six sevens == 39?' ===> Yes, ok. There's nothing principled going on.

The SICC is aimed to prevent these contradictions, by not allowing conflicting instance decls. In the face of LCC (using `~` in constraints or classes themselves with FunDeps), Consistency Checking would have to chase through constraints and their instances, possibly through very long chains, possibly `Undecidable`. Too hard. Hugs AFAICT takes a very simple approach: if there's a tyvar in a dependent position in the head which isn't covered by the same tyvar in a determining position -- that is, the instance is using Liberal Coverage -- reject the instance as inconsistent/don't even try to unify tyvars with the other instance you're comparing to for consistency. So Hugs rejects this instance is inconsistent with the first -- despite it transparently (to me) making the same improvement:

```haskell
instance {-# OVERLAPPABLE #-} (MakeEq Bool b) => SomeC Int b Char
   
class MakeEq b b1 | b -> b1
instance MakeEq b b
```

## Conclusions

* In the face of LCC, it's impossible to enforce SICC -- or we'd have to enforce something far more restrictive than SICC.
* Being unable to enforce SICC, improving a Wanted from some instance or other is premature/hazardous: it could easily make the wrong improvement. (That is, 'wrong' considered against the whole structure of instances for the class.)
* And is this improvement the root of the lack of evidence problem? We've improved the Wanted from somewhere -- we're not quite sure where or how; so we can't provide an 'audit trail'.
* Then don't do that. That is, don't improve a Wanted from an instance until we're sure that's the only instance that could match. (Hmm ? should this read ... until we've definitively matched the instance, including on positions that aren't involved in the FunDep.)
* Compare Associated Types, where there's some concern that type `F t` appears to be valid, even though there's no instance that `t` matches; or Type Families in general, where `F t` behaves as if a type, even though there's no equation `t` matches. (This feels like a type-level counterpart of partial functions.)


* Without improvement, though, GHC has a problem: `[W] C Int alpha Char` (in which something else has improved `(v ~ Char)`) won't match any instance, using usual (for non-FunDeps) instance matching. It's liable to match some more-general instance or get 'stuck', so failing to undergo exactly the improvement the FunDep is supposed to provide.
* Then instance matching for a class with a FunDep should match the Wanted to an instance head ignoring dependent/RHS positions. (To the most specific instance ignoring those positions, for overlaps.)

## So ... ?

Given how hard it would be to enforce the SICC, it seems to me there's a cleaner approach:

(A) Don't make any improvement under a FunDep until you've committed to an instance.

(B) To 'commit to' an instance, match on the determining/LHS positions of a FunDep, ignore the Wanted types in the dependent/RHS positions.

That is, with `[W] C Int String Char`; observe `C`'s 2nd position is a dependent/RHS of a FunDep, so ignore the `String`/read that as `[W] C Int alpha Char`; match the `Int` to the first instance above, read as `instance C Int beta0 Char`; from the instance improve `beta0 ~ Bool`; then observe you want `Bool ~ String` and reject the program.

**Sadly:** This won't reveal a set of instances is inconsistent until a usage site wants improvement at the crux of inconsistencies -- which might never happen. As the User Manual already says re validating instances: "These [instances] potentially overlap, but GHC will not complain about the instance declarations themselves, regardless of flag settings." [§ 6.8.8.4, link below]

**Complication:** suppose multiple FunDeps, then some position(s) in the instance head are both determining/LHS and dependent/RHS. Can we elaborate a Wanted into a disjunction (inclusive-or) of Wanteds? 

```haskell
class AddNat x y z  | x y -> z, x z -> y, y z -> x

[W] AddNat (S (S x')) (S y')  (S (S z'))
===>                                      -- for fresh zeta0, ypsilon0, chi0
[W] AddNat (S (S x')) (S y')   zeta0      ; [W] zeta0    ~ (S (S z'))
[W] AddNat (S (S x')) ypsilon0 (S (S z')) ; [W] ypsilon0 ~ (S y')
[W] AddNat chi0       (S y')   (S (S z')) ; [W] chi0     ~ (S (S x'))
```

There would have to be some sort of (non-strict) consistency condition on the instances, such that if any of the disjuncts matched, it would be only to the same instance (and thereby their improvement would make the result matchable for the other disjuncts).

**Extension:** We might try to relax (A) -- but this sounds hard:

(A') If unable to commit to a single instance, but all the remaining candidate instances make the same improvement, apply that, to avoid getting stuck. ["Candidate instances" in the sense of [§ 6.8.8.4 of the User Manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html)]

(B)'s considering only the determining positions is to give instance selection the best hope. Suppose

```haskell
instance C Int Bool Char              -- SPJ's 'kosher' instance from above

[W] C Int alpha v                     -- SPJ's wanted
```

We want to ignore the `alpha`, because that's the position awaiting improvement via the FunDep. That gives a benefit, in context of the current discussion. Suppose

```haskell
instance {-# AMBIGUOUS #-} C Char (t -> t) c

[W] C Char alpha v
```

We don't care what `t` is/we don't even inspect that parameter. We just match the instance and improve `alpha ~ (t0 -> t0)` for fresh `t0`.

* Then `AMBIGUOUS` there is in the sense of `AllowAmbiguousTypes`: don't require `t` to appear elsewhere in the instance head or constraints, even though it appears only in a dependent/RHS type.

Should we ignore the `v`? There might be other instances:

```haskell
instance (b ~ Bool ) => SomeC Int b Float              -- consistent
instance (b ~ String => SomeC Int b Integer            -- not
```

I don't have an answer. This is why I (and the JFP-paper) are deeply suspicious of non-Full FunDeps.

## SPJ thoughts

I think that your instance selection rules are equivalent to this.

1. Transform the instance declarations as below
2. Drop constraint-to-instance fundep generation entirely
3. Use GHC's existing rules for instance selection

The transformation.  Consider the class
```
    class C a b c | a -> b
    instance blah => C t1 t2 t3
```
Transform the instance to
```
    instance (blah, b~t2) => C t1 b t3
```
where I've put a wildcard in the determined position. Now we get something
that effectively combines instance selection and fundep generation in one,
just as described in 6.1

But I'm not sure what to do if there are multiple fundeps
```
    class C a b c | a -> b, b -> a
```

### AntC response

So the penny is dropping slowly: 

> 3.  GHC's existing rules for instance selection

are to select an instance only when the Wanted is a substitution instance of the instance head. A Wanted with a wildcard/skolem in a dependent position (which is what we usually expect) would never select an instance. So GHC uses FunDep improvement to force the instance's type into the Wanted; then the Wanted will match the instance.

That's workable (I guess -- although it seems like going round the houses) provided all instances obey the SICC. But GHC has never (AFAIK) enforced the SICC.

> instance selection rules are equivalent to ...

Yes, and @AdamG covers [similar ground here](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Wiggly-arrows#whats-the-difference-between-fundeps-and-type-equalities). As he points out, that's not enough without considering consistency conditions. For example:

* If I make that transformation to the instances in #10675, at least I don't get the baffling entanglement between instances.
* _But_ I do get `[x]` improved differently from the different instances, thus breaking the consistency expectations.
* Contrast that transformation with Hugs: it still rejects the instances as inconsistent; because it refuses to unify two wildcards arising from different instance heads.
* Or are you saying the Liberal Instance Consistency should accept differing improvement from selecting a different instance? (In case of #10675, really improvement is being driven from the first param, which isn't mentioned in the FunDep.)

> what to do if there are multiple fundeps

Indeed, and I notice @AdamG wants multiple FunDeps for `setField` -- as do all proposals around record-alikes, including the original Lenses. Also Adam wants non-Full FunDeps with apparently liberaller-than-Liberal Consistency.

Multiple FunDeps don't always muck up the wildcard-with-`~`-constraint idea. Take this version of `AddNat`:

```haskell
class {- AddNat y x z =>  -- ?? flip the args, would give an extra FunDep -}
         AddNat x y z  | x y -> z, x z -> y  -- only 2 FunDeps, both have x on LHS

instance                 (z ~ y     ) => AddNat Z      y z
instance (AddNat x' y z', z ~ (S z')) => AddNat (S x') y z
```

Since both FunDeps have `x` as determining; and the instances' `x` positions are apart, this is the usual way to write `AddNat`.

My `??` superclass constraint is rightly rejected: no smaller than class head. You could put it as instance constraints, but that doesn't genuinely induce FunDep-like improvement until the instance is selected -- by which time it's redundant.

We could perhaps figure out some rules for when you can 'get away with' multiple FunDeps. I fear it'll need looking across all instances for the class; and I guess would lead to some horrendous error messages, leaving users perplexed as to whether/how they could patch up their instances.

So my (more tractable) approach is to say instance heads must overlap in strict substitution order -- for an adapted definition of overlap. More tractable because it needs only comparing instances pairwise.