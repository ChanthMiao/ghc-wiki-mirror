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