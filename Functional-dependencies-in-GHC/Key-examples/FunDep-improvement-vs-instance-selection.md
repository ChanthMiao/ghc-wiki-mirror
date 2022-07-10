Go up to [Key examples # 3](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Key-examples#example-3-lcc-and-licc-threaten-confluence)

## Q's for SPJ

[AntC] Following an email exchange with SPJ:

<blockquote>
@simonpj Don't forget the (a) improvement vs (b) instance selection separation.  It's important because consider

```haskell
class SomeC a b c |  a -> b             -- [AntC] note non-Full FunDep
instance SomeC Int Bool Char
```

and `[W] C Int alpha v`, where `v` is (say) a skolem variable.   Then improvement spits out `alpha ~ Bool`, even though the instance doesn't match.

-- previously (see Key Example 4, although the discussion to be moved from there)

GHC does two *entirely separate* steps:

1. Improvement of a Wanted. The only effect is to emit new equality constraints
2. Discharging a Wanted using an instance.

</blockquote>

These instances are accepted along with the one above:


```haskell
instance {-# LIBERAL #-} (b ~ String) => SomeC Int b ()

instance {-# OVERLAPPABLE, LIBERAL #-} (b ~ String) => SomeC Int b Char
```

And it's easy to get `[W] SomeC Int String Char` accepted -- even though it directly contradicts the `OVERLAPPING` first instance.

Now with `~ String` in a constraint (rather than `String` in the head), GHC never [but see next para] 'spits out' an improvement `alpha ~ String` -- which is the trick to getting a type-level `TypeEq` to work. And yet `~` constraints are supposed to be morally equivalent to writing the equated type directly(?) To be clear: I don't have use cases for these examples/I'd prefer they get rejected -- but I do want `TypeEq` to work.

It's also easy to get `[W] SomeC Int alpha ()` accepted, and thereby get `alpha` improved to `String` -- again contradicting the FunDep. Is this the same sort of "spit out"? It happens as a result of matching to the instance, so raising new Wanteds.

It's fairly easy to get improvement going different ways in different equations, then getting the program rejected `Bool /~ String`. I haven't managed to get a contradicting program to compile and seg fault. Perhaps if I put the contradictions in separate modules?

The SICC is aimed to prevent these contradictions, by not allowing conflicting instance decls. In the face of LCC (using `~` in constraints or classes themselves with FunDeps), Consistency Checking would have to chase through constraints and their instances, possibly through very long chains, possibly `Undecidable`. Too hard. Hugs AFAICT takes a very simple approach: if there's a tyvar in a dependent position in the head which isn't covered by the same tyvar in a determining position -- that is, the instance is using Liberal Coverage -- reject the instance as inconsistent/don't even try to unify tyvars with the other instance you're comparing to for consistency. So Hugs rejects this instance is inconsistent with the first -- despite it transparently (to me) making the same improvement:

```haskell
instance {-# OVERLAPPABLE #-} (MakeEq Bool b) => SomeC Int b Char
   
class MakeEq b b1 | b -> b1
instance MakeEq b b
```

## Conclusions

* In the face of LCC, it's impossible to enforce SICC -- or we'd have to enforce something far more restrictive than SICC.
* Being unable to enforce SICC, improving a Wanted from some instance or other is premature/hazardous: it could easily make the wrong improvement. (That is, 'wrong' considered against the whole structure of instances for the class.)
* Then don't improve a Wanted from an instance until we're sure that's the only instance that could match. (Hmm ? should this read ... until we've definitively matched the instance, including on positions that aren't involved in the FunDep.)

## So ... ?

Given how hard it would be to enforce the SICC, it seems to me there's a cleaner approach:

(A) Don't make any improvement under a FunDep until you've committed to an instance.

(B) To 'commit to' an instance, match on the determining/LHS positions of a FunDep, ignore the Wanted types in the dependent/RHS positions.

That is, with `[W] SomeC Int String Char`; observe `SomeC`'s 2nd position is a dependent/RHS of a FunDep, so ignore the `String`/read that as `[W] SomeC Int alpha Char`; match the `Int` to the first instance above; from the instance improve `alpha ~ Bool`; then observe you want `Bool ~ String` and reject the program.

**Sadly:** This won't reveal a set of instances is inconsistent until a usage site wants improvement at the crux of inconsistencies -- which might never happen. As the User Manual already says re validating instances: "These [instances] potentially overlap, but GHC will not complain about the instance declarations themselves, regardless of flag settings." [§ 6.8.8.4, link below]

**Complication:** suppose multiple FunDeps, then some position(s) in the instance head are both determining/LHS and dependent/RHS. Can we elaborate a Wanted into a disjunction (inclusive-or) of Wanteds? 

```haskell
class AddNat x y z  | x y -> z, x z -> y, y z -> x

[W] AddNat (S (S x')) (S y')  (S (S z'))
===>                                      -- for fresh zeta, ypsilon, chi
[W] AddNat (S (S x')) (S y')  zeta       ; [W] zeta    ~ (S (S z'))
[W] AddNat (S (S x')) ypsilon (S (S z')) ; [W] ypsilon ~ (S y')
[W] AddNat chi        (S y')  (S (S z')) ; [W] chi     ~ (S (S x'))
```

There would have to be some sort of (non-strict) consistency condition on the instances, such that if any of the disjuncts matched, it would be only to the same instance (and thereby their improvement would make the result matchable for the other disjuncts).

**Extension:** We might try to relax (A) -- but this sounds hard:

(A') If unable to commit to a single instance, but all the remaining candidate instances make the same improvement, apply that, to avoid getting stuck. ["Candidate instances" in the sense of [§ 6.8.8.4 of the User Manual](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html)]

(B)'s considering only the determining positions is to give instance selection the best hope. Suppose

```haskell
instance SomeC Int Bool Char              -- SPJ's 'kosher' instance from above

[W] SomeC Int alpha v                     -- SPJ's wanted
```

We want to ignore the `alpha`, because that's the position awaiting improvement via the FunDep. That gives a benefit, in context of the current discussion. Suppose

```haskell
instance {-# AMBIGUOUS #-} SomeC Char (t -> t) c

[W] SomeC Char alpha v
```

We don't care what `t` is/we don't even inspect that parameter. We just match the instance and improve `alpha ~ (t0 -> t0)` for fresh `t0`.

* Then `AMBIGUOUS` there is in the sense of `AllowAmbiguousTypes`: don't require `t` to appear elsewhere in the instance head or constraints, even though it appears only in a dependent/RHS type.

Should we ignore the `v`? There might be other instances:

```haskell
instance (b ~ Bool ) => SomeC Int b Float              -- consistent
instance (b ~ String => SomeC Int b Integer            -- not
```

I don't have an answer. This is why I (and the JFP-paper) are deeply suspicious of non-Full FunDeps.