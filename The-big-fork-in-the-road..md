This page describes some design ideas for functional dependencies.

Go up to [Functional dependencies in GHC](..)

## Overview

I think there are two alternative ways to look at fundeps.  Both have validity, but they take us in two different directions.  Here they are.

* **TrueFundeps**. Under the TrueFundeps view, functional dependencies are truly functional dependencies. That is, if we have
  ```
  class C2 a b | a -> b
  ```
  and the constraint `(C2 t1 t2)`, then knowing `t1` complete forces what `t2` must be. That is, `t2` is a function of `t1`.

* **GuidedInference**.  Under the GuidedInference view, fundeps are *only* a means to guide type inference, by doing some extra unifications, thus removing ambiguity.

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

**ToDo** I'd love to have compelling examples of when TrueFundeps is
what we need. Deriving equalites from Givens might be one such --
but since GHC can't do that today, what are the other compelling
applications?

The big problem is, of course, that many uses of fundeps today are simply incompatible with this approach: see [key examples](key-examples).


## GuidedInference: fundeps guide type inference

The GuidedInference approach completely abandons the goal that a fundep is a true functional dependency. Rather, it's a mechanism that gives rise to some extra equality constraints, that in turn guide inference.

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

To make the point very explicitly, I'll describe an extreme design that focuses entirely on GuidedInference.

* When trying to solve a constraint `(HasField t1 t2 t3)`, if any instances *match* the constraint, just use the existing matching rules for instances.

* If none match, find all instance declarations that unify with it.

* If there is just one such unifying instance, say `forall a b. ... => HasField s1 s2 s3`, instantiate a,b with fresh unification variables, and emit Derived equalities `t1 ~ s1', t2 ~ s2', t3 ~ s3'`.

* If there is more than one such instance, do nothing.

In this design I have not even annotated the class declaration!  It amounts to saying: if there is only one way to solve the constraint, solve it that way.

There are many details to work out. For example: ```

* Suppose two
  intances match but one is more general than the other.  Example:
  `instance forall a. C [a]` and `instance C [Int]`. Then, if there is a unique most-general instance (in this case the `C [a]` instance), use it for generating the derived constraints.  The idea is that it summarise what is common among all the unifying instances, one of which may ultimately solve the constraint.

* What about local Givens?   We could treat them just like local instances, with no quantified variables.   But eg. if we have a Given `Eq a` and a Wanted `Eq alpha`, do we really want to emit `[D] alpha ~ a`?   Maybe so.

* Or maybe we want to declare classes as "improvable" and only do this stuff for improvable classes?






