# Injective type families


This page summarizes the design of injective type families (#6018).


Injective type families have been merged into HEAD in September 2015 and became available in GHC 8.0 with the `TypeFamilyDependencies` language extension.


There are plans to further extend implementation of injective type families to
match the expressive power of functional dependencies.  The work was started in
September 2015 but later stalled and was left unfinished.  Partial
implementation is available as  [Phab:D1287](https://phabricator.haskell.org/D1287) and on the
wip/T10832-generalised-injectivity branch. The relevant Trac ticket is
#10832. See
[this section below](https://ghc.haskell.org/trac/ghc/wiki/InjectiveTypeFamilies#TypeCinjectivityaka.generalizedinjectivity) for full details.


Other tickets related to further development of injective type families: #10833, #11511.


Person responsible for this page is Jan Stolarek (just so you now who is meant
by "I").  I am also responsible for most of the implementation.  Simon Peyton
Jones and Richard Eisenberg participated in the development of theory behind
injective type families, so whenever I say "we" I mean the three of us.  For
full discussion of injective type families see Haskell Symposium 2015 [paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/injective-type-families-acm.pdf)
"Injective type families for Haskell" (henceforth referred to as the
"injectivity paper").

## Tickets

See the ~"injective type families" label.



## Forms of injectivity


The idea behind #6018 is to allow users to declare that a type family is
injective.  We have identified three forms of injectivity:

1. Injectivity in all the arguments, where knowing the result (right-hand
   side) of a type family determines all the arguments on the left-hand
   side. Examples:

   ```haskell
   type family Id a where
        Id a = a
   ```

   ```haskell
   type family F a b c
   type instance F Int  Char Bool = Bool
   type instance F Char Bool Int  = Int
   type instance F Bool Int  Char = Char
   ```

1. Injectivity in some of the arguments, where knowing the RHS of a type
   family determines only some of the arguments on the LHS. Example:

   ```haskell
   type family G a b c
   type instance G Int  Char Bool = Bool
   type instance G Int  Char Int  = Bool
   type instance G Bool Int  Int  = Int
   ```

   
   
   Here knowing the RHS allows us to determine first two arguments, but not the
   third one.
   
   

1. Injectivity in some of the arguments, where knowing the RHS of a type
   family and some of the LHS arguments determines other (possibly not all)
   LHS arguments. Examples:

   ```haskell
   type family Plus a b where
        Plus Z     n = n
        Plus (S m) n = S (Plus m n)
   ```

   
   
   Here knowing the RHS and the first argument uniquely determines the remaining
   argument.
   
   

   ```haskell
   type family H a b c
   type instance H Int  Char   Double = Int
   type instance H Bool Double Double = Int
   ```
   Knowing the RHS and either `a` or `b` allows to uniquely determine the
   remaining two arguments, but knowing the RHS and `c` gives us no information
   about `a` or `b`.
   
   


In the following text I will refer to these three forms of injectivity as A, B
and C.


Currently GHC implements injectivity of type B (and therefore of type A as well,
since A is a subset of B).

## Proposed syntax


When deciding on a concrete syntax our two main design goals were:

- **Backwards compatibility**: injective type families are automatically
  enabled with the `TypeFamilies` language extension. This means that the
  proposed syntax had to be backward compatible, ie. not break any existing
  code that uses type families.

- **Future extensibility**: currently we we only implemented injectivity A
  and B but we still want to be able to implement injectivity C in the future
  without breaking A and B.


We decided to use syntax borrowed from functional dependencies.  First of all
the user must be able to introduce a variable that names the result of a type
family.  To achieve that we extend syntax of type family head by allowing to
write `= tyvar` or `= (tyvar :: kind)` annotations in addition to already
allowed `:: kind` annotation. In other words all these declaration are
well-formed:


```haskell
type family Plus (a :: Nat) (b :: Nat) where ...
type family Plus (a :: Nat) (b :: Nat) :: Nat where ...
type family Plus (a :: Nat) (b :: Nat) = c where ...
type family Plus (a :: Nat) (b :: Nat) = (c :: Nat) where ...
```


but the third or fourth form is required if the user wants to introduce
injectivity annotation. The injectivity annotation itself begins with `|`
following the result type variable.  `|` is followed by an injectivity
condition. Injectivity condition has the form:


```haskell
A -> B
```


where `A` is the result type variable and `B` is a non-empty lists of type
variables declared in type family head. Things on the left and right of `->` are
called LHS and RHS of an injectivity condition, respectively. If a type family
is injective in a given argument respective type variable must be mentioned in
the RHS of an injectivity condition. Variables may be skipped if a type family
is not injective in a given argument.



Here are examples of injectivity declarations using proposed syntax:


```haskell
type family Id a = result | result -> a where { ... }
type family F a b c = d | d -> c a b
type family G a b c = foo | foo -> a b where { ... }
```


This syntax can be easily extended in the future if we want to implement
injectivity of type C. First required change is that there may be many
comma-separated injectivity conditions.  Second change is that LHS of
injectivity condition can mention type variables that name the arguments, not
just the result.  With this extended syntax we could write:


```haskell
type family Plus a b = (sum :: Nat) | sum a -> b, sum b -> a where
type family H a b c = xyz | xyz a -> b c, xyz b -> a c
```


Note that for open and closed type families it is correct to declare a type
variable that names the result but skip the injectivity annotation.  That is
not the case for associated types.  If you name the result but ignore the
injectivity annotation GHC will interpret this as an associated type default.

## Implementation outline

### Verifying correctness of injectivity annotation


Before checking that a type family is indeed injective, as declared by the user,
GHC needs to check the correctness of injectivity annotation itself.  This
includes checking that:

- only in-scope type variables are used. For example
  `type family F a = r | r -> b` should result with "not in scope: b" error.


See test T6018rnfail in the testsuite for more examples of invalid declarations.

### Verifying that type family equations agree with injectivity annotation


Once the user declares type family to be injective GHC must verify that this
declaration is correct, ie. type family really is injective.  Below are the
rules we follow when checking for injectivity of a type family. For full details
and rationale behind them see the injectivity paper, Section 4.2.

**Important**: these rules are only for the currently implemented injectivity
of types A and B.  RHS refers to the right-hand side of the type family equation
being checked for injectivity.  LHS refers to the arguments of that type family
equation.  Term "type family equation" can refer to equations of both open and
closed type families, unless stated otherwise.

**Rules for checking injectivity**


A general idea is that if we find at least one equation (bullets (1), (2) and
(3)) or a pair of equations (bullets (4) and (5)) that violate injectivity
annotation then we conclude that a type family is not injective in a way user
claims and we report an error.

1. If a RHS of a type family equation is a type family application we conclude
   that the type family is not injective.

1. If a RHS of a type family equation is a bare type variable we require that
   all LHS variables (including implicit kind variables) are also bare.  In
   other words, this has to be a sole equation of that type family and it has to
   cover all possible patterns.  If the patterns are not covering we conclude
   that the type family is not injective.

1. If a LHS type variable that is declared as injective is not mentioned on
   injective position in the RHS we conclude that the type family is not
   injective.  By "injective position" we mean argument to a type constructor or
   argument to a type family on injective position.

Open type families (including associated types)


Open type families are typechecked incrementally.  This means that when a module
is imported type family instances contained in that module are checked against
instances present in already imported modules.  In practice this is done by
checking equations pair-wise (a new equation against all already checked
equations -- modulo optimisations).

1. When checking a pair of an open type family equations we attempt to unify
   their RHSs. If they don't unify this pair does not violate injectivity
   annotation.  If unification succeeds with a substitution (possibly empty)
   then LHSs of unified equations must be identical under that substitution. If
   they are not identical then we conclude that a type family is not injective.


Note that we use a special variant of the unification algorithm that treats type
family applications as possibly unifying with anything.

Closed type families


In a closed type family all equations are ordered and in one place. Equations
are also checked pair-wise but this time an equation has to be paired with all
the preceeding equations.  Of course a single-equation closed type family is
trivially injective (unless (1), (2) or (3) above holds).

1. When checking a pair of closed type family equations we try to unify their
   RHSs.  If they don't unify this pair does not violate injectivity annotation.
   If the RHSs can be unified under some substitution (possibly empty) then
   either the LHSs unify under the same substitution or the LHS of the latter
   equation is subsumed by earlier equations.  If neither condition is met we
   conclude that a type family is not injective.


Again, we use a special variant of the unification algorithm.

### Source code tour


Below is a list of primary source code locations that implement injectivity:

- [compiler/GHC/Rename/Module.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Rename/Module.hs).`rnInjectivityAnn`: checks
  correctness of injectivity annotation (mostly variable scoping).

- [compiler/GHC/Tc/Instance/Family.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Tc/Instance/Family.hs).`checkForInjectivityConflicts` is
  an entry point for injectivity check of open type families.

- [compiler/GHC/Tc/TyCl.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Tc/TyCl.hs).`checkValidClosedCoAxiom` is
  an entry point for injectivity check of closed type families.

- [compiler/GHC/Core/FamInstEnv.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/FamInstEnv.hs).`injectiveBranches` checks that a
  pair of type family axioms does not violate injectivity annotation.

- [compiler/GHC/Core/FamInstEnv.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/FamInstEnv.hs).`lookupFamInstEnvInjectivityConflicts`
  implements condition (4) of injectivity check.

- [compiler/GHC/Tc/TyCl.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Tc/TyCl.hs).`checkValidClosedCoAxiom.check_injectivity.gather_conflicts`
  implements condition (5) of injectivity check.

- [compiler/GHC/Core/Unify.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/Unify.hs).`tcUnifyTyWithTFs` is our special
  variant of a unification algorithm.

- [compiler/GHC/Tc/Instance/Family.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Tc/Instance/Family.hs).`makeInjectivityErrors` checks
  conditions (1), (2) and (3) of the injectivity check.  It also takes as an
  argument results of check (4) or (5) and constructs error messages, if
  necessary.

- [compiler/typecheck/TcInteract](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/typecheck/TcInteract), functions
  `improveLocalFunEqs.do_one_injective` and `improve_top_fun_eqs` implement
  typechecking improvements based on injectivity information.


Relevant source code notes are:

- `Note [FamilyResultSig]` in [compiler/GHC/Hs/Decls.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Hs/Decls.hs)
- `Note [Injectivity annotation]` in [compiler/GHC/Hs/Decls.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Hs/Decls.hs)
- `Note [Injective type families]` in [compiler/GHC/Core/TyCon.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/TyCon.hs)
- `Note [Renaming injectivity annotation]` in [compiler/GHC/Rename/Module.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Rename/Module.hs)
- `Note [Verifying injectivity annotation]` in [compiler/GHC/Core/FamInstEnv.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/FamInstEnv.hs)
- `Note [Type inference for type families with injectivity]` in [compiler/GHC/Tc/Solver/Interact.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Tc/Solver/Interact.hs)


Implementation discussion and progress was recorded in
[Phab D202](https://phabricator.haskell.org/D202). That patch is now fully merged and given here only for historical purposes.

## Injectivity for poly-kinded type families


With *PolyKinds* extension enabled it is possible to declare kind variables as
injective.  Moreover, if a type variable is declared as injective its associated
kind variable is also considered injective.  See section 6 of the injectivity
paper for full details.

## Connection with functional dependencies


In the course of our work it turned out that there is a close correspondence
between injective type families and functional dependencies.  Section 7 of
the injectivity paper discusses this connection.

## Future plans and ideas

### Type C injectivity (aka. generalized injectivity)


We would like to implement generalized injectivity in GHC.  This would give
injective type families expressive power identical to this of functional
dependencies. Here are several properties of generalized injectivity annotations:

- `type family F a b = r | r -> a, r -> b`. This is technically correct but we
  could just say `result -> a b`.

- there are no identical conditions (this wouldn't hurt, but the user deserves
  a warning about this)

- type variables are not repeated on either LHS or RHS of the injectivity
  condition. For example `r a a -> ...` or `... -> a b a` should generate
  a warning. Note that it probably is OK to have the same variable both on the
  LHS and RHS of an injectivity condition: in the above examples it is true
  that `type family G a b c | result c -> a b c`. The question is whether this
  has any practical relevance.

- injectivity conditions don't overlap (eg. `result -> a b` overlaps
  `result -> a`). This probably deserves a warning.


I started implementation work on generalized injectivity in September 2015 but
didn't finish it. The relevant Trac ticket is #10832. Partial implementation
is available as [Phab:D1287](https://phabricator.haskell.org/D1287) and on the wip/T10832-generalised-injectivity
branch. In October/November 2016 I tried to rebase this branch on top of HEAD
and resume work but the typechecker code has changed significantly and I was
unable to make it work. It seems that the easiest way here is to salvage parser
changes and the data type definitions in `HsDecls` and rewrite the rest of the
patch from scratch.


We also had some interesting email discussion. I am recording the most
relevant emails here so they don't get lost and are available to anyone who
decides to take up the work.


Richard Eisenber, 16 July 2015:

> Here is an interesting case:
> 
> ```haskell
> type family F a b = r | r a -> b where
>   F Int ??a = Maybe (F a Int)
>   F Bool a = [a]
>   F Char a = [a]
> ```
> 
> This should be rejected, because of the following facts:
> ```haskell
>   F Int Bool ~ Maybe [Int]
>   F Int Char ~ Maybe [Int]
> ```
> Thus, the first parameter and the result do not, in concert, uniquely determine the second parameter.
> 
> Yet Simon's "just tuple them up" approach would label F as injective. The only interesting pairwise check is the first equation against itself. We run this:
> 
> ```
>   U( ( (Int, Maybe (F a Int)), (Int, Maybe (F b Int)) )
> ```
> 
> If we are to recur into F via equation (7), then we get a substitution `[a |-> b]`, and the pairwise check (erroneously) succeeds. Simon said that the arguments to the left of -> need to be syntactically identical, which this case fails. But now consider this:
> 
> ```haskell
> type family G a b = r | r a -> b where
>   G Int ??(a,b) = (Maybe (G a b), a)
>   G Bool a ?? ?? = [a]
>   G Char a ?? ?? = [a]
> 
>   U( (Int, (Maybe (G a b), a)) , (Int, (Maybe (G c d), c)) )
> ```
> 
> Here, it is OK to recur via equation (7), because the presence of a and c outside of the recursive call gets them to unify. Indeed we must recur to accept G, because we need to look under G to get the mapping [b |-> d]. (The fundep version of F is rejected and the fundep version of G is accepted.)
> 
> I believe this G exemplifies the order-dependency Jan was worried about. We somehow need to unify the second component of that tuple before looking under G.
> 
> So the question is: when can equation (7) apply?
> 
> I naturally tried to answer this myself, via comparison with functional dependencies. My guess was that our check is a little more liberal than the fundep check, in that we accept benign overlap between equations. When I pushed this on the fundep side, I discovered that fundeps as implemented in 7.10.1 are broken! Here is the test case:
> 
> ```haskell
> {-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
>             FlexibleInstances, UndecidableInstances #-}
> 
> module FD where
> 
> class H a b r | a b -> r, r a -> b
> instance H Bool Double Bool
> 
> class G a b c r | a b c -> r, r a -> b
> instance H a b r => G Int (a,b) () ?? (Maybe r, a)
> instance ?? ?? ?? ?? ?? ??G Int (a,a) Char (Maybe a, a)
> 
> foo :: G a b c r => (a,b,c,r)
> foo = undefined
> 
> -- (1): instance G Int (Bool, Bool) ?? Char (Maybe Bool, Bool)
> x = foo :: (Int, (Bool, Bool), Char, (Maybe Bool, Bool))
> 
> -- (2): instance G Int (Bool, Double) () ?? (Maybe Bool, Bool)
> y = foo :: (Int, (Bool, Double), (), (Maybe Bool, Bool))
> ```
> 
> This compiles in 7.10.1. Yet we can see that GHC finds instances (1) and (2) marked above, even though these violate the second functional dependency on G: r and a do *not* uniquely determine b. (The c parameter is there simply to avoid overlapping instances.)
> 
> So perhaps the story *is* rather more complicated than we thought, and fundeps' simplicity in this matter is bogus.
> 
> And, quite separately, I'm also worried about the bare-type-variable-on-the-right problem. But let's solve this before tackling that.

Simon Peyton Jones, 23rd July 2015:

> I'm being slow here, but I have learned some things.
> 
> 1. As you'll see from "Understanding FDs using CHRs", we have to check 
>   TWO things for FDs:
> 
>   ```
>         class C a b | a -> b
>   ```
> 
> * Coverage: for each instance, individually, check coverage
>         instance Q => C s t
>   The na??ve version just checks that fvs(s) is a superset of fvs(t).
>   The "liberal" coverage condition takes account of Q.
> 
> * Consistency: compare all instances pairwise to check that they are consistent
>   ```haskell
>         instance Q1 => C s1 t1
>         instance Q2 => C s2 t2
>   ```
>   if S = unify(s1,s2), then check that S(t1) = S(t2)
>   Notice that this does not take account of Q1, S2 at all.
> 
> 
> 2. In our paper we have ONE check. ??But I believe that 
>     - consistency = comparing equations i,j, where i is *distinct* from j
>     - coverage ?? ??= comparing equation I with itself
>   So that makes our approach rather nicely uniform.
> 
> 
> 3. GHC has an outright bug in its handling of consistency,
>   as you both point out. ??But it's a bug that seems to be exploited!
>   See Trac #10675.
> 
> 4. For *consistency*, our approach is a little more liberal than FDs,
>   because the FD stuff takes no account of Q1, Q2. ??Example
>   (in type-family form)
>   ```haskell
>       F (a,Int) ??= ([G a], ?? ??Maybe Int)
>       F (Bool,b) = ([G Bool], Maybe b)
>   ```
>   where G is injective, and F is claimed to be.
>   We find that F is injective because, using the injectivity
>   of G, we unify a:=Bool.
> 
>   But in FD form
>   ```haskell
>       class F a r | r -> a
>       instance G a r ?? ??=> F (a,Int) ([r], Maybe Int)
>       instance G Bool r => F (Bool,b) ([r], Maybe b)
>   ```
> 
>   These two will be rejected by the FD world because of
>   ignoring Q1, Q2.
> 
>   That said, I don't think this additional expressiveness
>   is very useful. The BIG thing about our ability to exploit
>   injectivity comes when comparing an equation with itself i=j.
> 
> 5. For *coverage*, the "liberal" coverage condition behaves
>   very like our "exploiting injectivity in algorithm U" idea.
> 
>   However if we extend to injectivity constraints like
>         a r -> b
>   then, as you both point out, we'll need some kind of
>   iterative scheme. ??Suppose we have
>         type family F a b = r | r a -> b
>   Then Rule (7) would become something like
>         U(F s1 s2, F t1 t2) T = U(s2,t2) T, ?? if T(s1)=T(t1)
>   and the side condition (syntactic equality) might change 
>   as unification progresses.
> 
>   What happens for coverage for fudeps? ??We don't use unification;
>   we use free variables, as in Defn 7 of the paper. ??But crucially
>   we use an iterative algorithm that iterates to a fixpoint;
>   see FunDeps.oclose.
> 
>   We would have to do the same, in some form.
> 
> Questions:
>   - FDs uses different technology for the coverage (i/i) and
>     consistency (i/j) tests. ??We could do so too; after all,
>     in the i/I case we know that the type structure matches!
> 
>   - Do we really care about the extra expressiveness of rule (7)
>     for the (i/j) case?

Simon Peyton Jones, 24rd July 2015:

> Jan says (and I can see) that we ALREADY use different 
> mechanisms for i/j and i/i.
> 
> So I propose that:
> 
> * We elaborate the i/i (coverage) test to do the iterative 
>   'oclose' thing. ??This already does not use algorithm U at all.
> 
> * We simplify algorithm U by dropping Rule (7) altogether.
>   - U is only used for i/j tests
>   - it's hard to craft an example where Rule (7) matters
>   - even such examples that exist are terribly fragile
>     (e.g. in my point (4) if I replace (G Bool) by its
>     values, say Char, then the test will fail)

Richard Eisenber, 26 July 2015:

> Simon has come to the same conclusions I have while pondering this all: we differ from FDs in that we cleverly use the same pairwise check between two different equations as we do when comparing an equation against itself. But this is perhaps too clever, and perhaps explains why the proof has foundered around this very issue (that is, equation 7).
> 
> So, I agree with Simon's proposal: explain all of this in terms of two separate checks. One check does a coverage check (very like the FD coverage check) for each equation, and another pairwise check is used only when i /= j. The Algorithm U stuff stays, but *without equation 7*. The proof would have to be tweaked, but I think this will give us a full proof of soundness. (Yay!) And it will all be easier to extend to generalized injectivity annotations.


### Inferring injectivity

[Here](https://gitlab.haskell.org/ghc/ghc/issues/6018) it was suggested by Simon that we could infer
injectivity for closed type families. I initially argued that, while inferring
injectivity of type A should be simple, inferring injectivity of type B would be
exponential in the numer of arguments to a type family. I take back that claim
as I now believe the algorithm can be made linear in the number of
arguments. Say we have a closed type family:


```
type family F a b c ... n = r where
```


Since there are n arguments there are 2<sup>n</sup> possible injectivity annotations.
That's why I believed that we have to check every possible annotationton.  I now
believe that instead checking every possible annotation we should only check
whether knowing the RHS allows to infer each argument independently.  In other
words when inferring injectivity we would check whether annotations `r -> a`, \`r
-\> b`, `r -\> c` ... `r -\> n` hold true. If we learn for example that knowing `r\`
allows to infer `a`, `c` and `n` but not the other argumenst we infer the
annotation `r -> a c n`.


There are several concerns to consider before implementing this:

- Before GHC 8.0 comes out there will already be some code in the wild that uses
  closed type families introduced in GHC 7.8. None of these type families
  require injectivity to work because GHC 7.8 does not support injectivity. If
  we attempt to infer injectivity for all these already existing closed type
  families we will only increase compilation time of existing code with
  absolutely no gain in functionality of the code. There were some complaints
  about GHC's performance decreasing with each release and I don't want to add
  to that.

- I believe that requiring explicit injectivity annotations is a valuable
  source code documentation for the programmer.  This is very subjective, but
  the truth is injectivity is very subtle and can easily be broken by slight
  modifications of type family definition.  It is much better to be explicit
  about relying on injectivity.

- Annotations also make it explicit which code compiles with GHC 8.0 and
  which does not. If we infer injectivity the code that works with 8.0 might
  result in typing errors for earlier GHC versions. Of course requiring
  annotations will also prevent that code from compiling but I believe that it
  will be easier for the programmer to track the source of the problem when
  she gets syntax errors rather than typing errors.

- I don't like the idea of mismatch between open type families and closed type
  families, meaning that injectivity of open type families would be openly
  documented whereas for closed type families it would be hidden.
  Counterargument: there is already a mismatch between open and closed type
  families, since the latter have kind inference.

- If we ever implement injectivity of type C it might not be possible to infer
  injectivity annotation of type C.  I think that this time the algorithm will
  really be exponential.

## Example use cases


In the injectivity paper we presented two practical use cases for injectivity.
If you have more uses cases to demonstrate please add them here.

[GLambda issue tracker (???Make Val injective???)](https://github.com/goldfirere/glambda/issues/6).
