## Two approaches to fundeps

This page describes a new design idea for in the functional-dependency space.

Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

## Overview

I think there are two alternative ways to look at fundeps.  Both have validity, but they take us in two different directions.  Here they are.

* **TrueFundeps**. Under the TrueFundeps view, functional dependencies are truly functional dependencies. That is, if we have
  ```
  class C2 a b | a -> b
  ```
  and the constraint `(C2 t1 t2)`, then knowing `t1` complete forces what `t2` must be. That is, `t2` is a function of `t1`.

* **SingleChoiceInference**.  Under the SingleChoiceInference view, fundeps are *only* a means to guide type inference, by doing some extra unifications, thus removing ambiguity.

GHC's current story on fundeps is an uneasy compromise between these two.  Much of the debate is driven by the competing priorities of the two, which pull in opposite directions.


## TrueFundeps: true functional dependencies

TrueFundeps is partly a philosophical position: a "fundep" should truly be a functional dependency.  But why does it *matter*?

Well, if we had true fundeps, then this program should typecheck.
```
class C2 a b | a -> b where ...

f :: (C2 a b, C2 a c) => b -> c
f x = x
```
We have two givens, `[G] C2 a b, [G] C2 a c`.  Under TrueFundeps, we must have that `b ~ c`, and we should be able to use it in the body.

[AntC] This might be a quibble, but we need to see an `instance C2 ta tbc` for the applicable `ta` -- which means delaying 'til a usage site for `f`. (So this is a difference compared to type families, in which `F2 ta` is treated as a type even though there's no equation matching it.)

This is worked out in [Elaboration on functional dependencies](https://people.cs.kuleuven.be/~tom.schrijvers/portfolio/haskell2017a.html), who translate fundeps into GHC's type families.  For each fundep we create an associated type family of the class, and make an equality superclass, thus:
```
class (b ~ F2 a) => C a b where
  type F2 a
  ...methods...
```
So the (true!) functional dependencies are witnessed by type functions.

Now, if we have `[G] C2 a b, [G] C2 a c`, we also have (by superclases)
```
  [G] b ~ F2 a
  [G] c ~ F2 a
```
and hence by transitivity we have `b ~ c` as desired.  Easy!

This approach is compatible with the liberal coverage condition. For example, this instance declaration doesn't satisfy the SCC, but *does* satisfy the LCC:
```
instance C2 a b => C2 [a] [b] where ...
```
And indeed it translates fine:
```
instance C2 a b => C2 [a] [b] where
  type F2 [a] = [F2 a]
```
For this instance to be legal, we must be able to come up the superclas of `C2 [a] [b]`, that is
`[b] ~ F2 [a]`. But that is easy:
```
     [W] [b] ~ F2 [a]
==>  {type intance for F2 [a]
     [W] [b] ~ [F2 a]
==>  {decompose}
     [W] b ~ F2 a
which is available as the superclass of `C2 a b`
```
So the logical destination of the TrueFundeps view is to treat fundeps
as true type functions, fully expressed in System FC. The fundep
syntax is simply convenient syntactic sugar.

[AntC] The 'Elaboration' paper doesn't consider overlapping instances (such as needed for `TypeEq` or three-FunDep `AddNat`). I think that's a severe weakness.

**ToDo** I'd love to have compelling examples of when TrueFundeps is
what we need. Deriving equalities from Givens might be one such --
but since GHC can't do that today, what are the *other* compelling
applications of TrueFundeps?

[AntC] A compelling example ought to be compound `SetField`s (this would avoid the classic `show . read` ambiguity): `setField @foo x . setField @bar y` -- where the `setField`s are potentially changing the type of the record, so the compiler needs to infer the type of the record after setting `bar`/that's passed to setting `foo`. There'd be a similar example with compounded `HasField`s: `getField @baz . getField @quux`, where the type of container field `quux` is parameterised by the type of nested field `baz`.

The big problem is, of course, that many uses of fundeps today are simply incompatible with this approach: see [key examples](./key-examples).


## SingleChoiceInference: guiding type inference

The SingleChoiceInference approach completely abandons the goal that a fundep is a true functional dependency. Rather, it's a mechanism that gives rise to some extra equality constraints, that in turn guide inference.

A classic example is [Example 5](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/key-examples#example-5-even-lcc-is-too-restrictive).
```
class HasField (name :: Symbol) s a | name s -> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance {-# DYSFUNCTIONAL #-} HasField "fld" T ([p] -> [p])
  getField (MkT f) = f
```
Here the instance wants to define the *shape* of the field type (in this case `[p] -> [p]` for some
unknown type `p`), but specifically does not want to fully define the field type.

This is a bit similar to [the notion of "head dependency" discussed here](https://gitlab.haskell.org/ghc/ghc/-/issues/6018#note_87924).

To make the point very explicitly, I'll describe an extreme design that focuses entirely on SingleChoiceInference.

* When trying to solve a constraint `(HasField t1 t2 t3)`, if any instances *match* the constraint, just use the existing matching rules for instances.

* If none match, find all instance declarations that unify with it.

* If there is just one such unifying instance, say `forall a b. ... => HasField s1 s2 s3`, instantiate a,b with fresh unification variables, and emit Derived equalities `t1 ~ s1', t2 ~ s2', t3 ~ s3'`.

* If there is more than one such instance, do nothing.

In this design I have not even annotated the class declaration!  It amounts to saying: if there is only one way to solve the constraint, solve it that way.

* [AntC] How does this play with separate compilation and the dratted orphan instances problem? There might be "only one way to solve the constraint" within this module, but suppose the only instance is marked `OVERLAPPABLE`? There might be a more specific instance that matches when this module is imported into another.

There are many details to work out. For example:

* What about local Givens?   We could treat them just like local instances, with no quantified variables.   But eg. if we have a Given `Eq a` and a Wanted `Eq alpha`, do we really want to emit `[D] alpha ~ a`?   Maybe so.

* Do we really want to use SingleChoiceInference for every of every class?  Maybe not. Consdier
  ```
  module M where
     data T = MkT { fld :: Int }

     f x = x.fld
  ```
  What type would you expect to be inferred for `f`?  There is only one `HasField` instance in
  scope, so the constraint `[W] HasField "fld" alpha beta` matches it, and we get `alpha := T, beta := Int`,
  and `f :: T -> Int`.  But if there were *no* `HasField` instances for `"fld"`, or if there were *more than one*,
  we'd get `f :: HasField "fld" r a => r -> a`.

  Perhaps we want to say that we only want to use SingleChoice on the third field of `HasField`, like this
  ```
  class HasField (n :: Symbol) s a | singlechoice(a) where ...
  ```
  Now the rule becomes: find all instances that *match* on the non-singlechoice fields, and *unify* on the single-choice fields. If there is just one such, instantiat with fresh variables and emit Derived equalities for the single-choice fields. Otherwise do nothing.

  With no `singlechoice` (syntax TBD) annotations, the behaviour is as today.

* The very same idea would work for type families -- see #19568.  For example
  ```
    type family Zip as bs = r | r -> as bs, singlechoice(as,bs) where
      Zip '[]       '[]       = '[]
      Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs
  ```
  Now if we have a stuck call `Zip b (c : ds)`, it unifies with exactly one equation, so we pick it and emit `[D] b ~ (a:as)`

* Suppose two
  intances match but one is more general than the other.  Example:
  `instance forall a. C [a]` and `instance C [Int]`. Then, if there is a unique most-general instance (in this case the `C [a]` instance), use it for generating the derived constraints.  The idea is that it summarise what is common among all the unifying instances, one of which may ultimately solve the constraint.

* [AntC] BTW the (database) theory of Functional Dependencies doesn't entail there be an algorithm or formula to get from LHS to RHS. A database table is merely a long list of correspondences, in which the LHS position(s) act as an index to look up the RHS. Mark Jones' original ESOP paper § 6.1 'Ensuring that Dependencies are Valid' introduces inference/improvement "to ensure pairwise compatibility
between instance declarations for" a class.\
  Then in the case of classes like `HasField/GetField`, there will be only one instance for each combination of field label and record type; no risk of non-confluent type improvement between instances, because no other instances. This does suggest that _contra_ the current 'Wiggly arrows' proposal for `SetField`, the LHS of each FunDep include as many parameters as possible, to remove the risk of collisions. That is, not `x s -> t`, but `x s b -> t`; to cover the case where field `x`'s type is a parameter to `s`'s type, so changing the field's type to `b` should produce a type `t` also parametric on `b`. (If `s` happens to be not parametric on `b`, that does no harm.)

--------------------------

## Examples of SingleChoiceInference

SingleChoiceInference deals rather straighforwardly with the [Key examples](./key-examples):

In these examples I don't get the class declaration, because the rules apply to all classes (rather simple).

### Example 1

```
class Mul a b c
instance Mul a b c => Mul a (Vec b) (Vec c)
```
Try solving `[W] Mul alpha (Vec beta) beta`. This unifies wtih the instance
with `b := beta, beta := Vec delta, a := alpha` where `delta` is fresh.
So we get a loop, just as before.  SingleChoiceInference does not have guaranteed termination.

### Example 2

```
instance                 CX Bool [x] [x]
instance  CX Char x y => CX Char [x] [Maybe y]
```
and try solving `[W] CX Bool [alpha] beta`.
Ths unifies only with the first instance, so we get `beta := [alpha]` and solve.
No problem.

###  Example 3
```
instance (q ~ Int)  => D Int  p (Int,q)
instance (s ~ Bool) => D Bool r (s,Bool)
```
try solving `[W] C alpha beta (gamma, delta)`.
This unifies with both instances, so we do nothing. SingleChoiceInference is confluent, I believe.

### Example 4
```
instance {-# OVERLAPPING #-}  TypeEq a a True
instance {-# OVERLAPPABLE #-} TypeEq a b False
```
Try solving `[W] TypeEq Int Bool alpha`.  This unifies only with the second instance, so we get `alpha := False`.

Try solving `[W] TypeEq Int Int alpha`.  This unifies with both, but first is most specific, so we pick it, and get `alpha := True`.


### Example 5

```
class HasField (name :: Symbol) s a | name s -> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance {-# DYSFUNCTIONAL #-} HasField "fld" T ([p] -> [p])
  getField (MkT f) = f
```
If we have `[W] HasField "fld" T alpha`, that will unify with only
one instance, so we will emit `[D] alpha ~ ([beta] -> [beta])`
