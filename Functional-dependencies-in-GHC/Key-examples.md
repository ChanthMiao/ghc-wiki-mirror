This page has key examples related to functional dependencies.

Go up to [Functional dependencies in GHC](..)

**Contents**

[[_TOC_]]

## Example 0: liberal coverage condition is highly desirable

Many real-life examples need the LCC. Here are some [everyone: add more]:

* Example 19 from JFP-paper (and Control.Monad) library
  ```
  class (Monad m) => MonadReader r m | m -> r
  instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m)
  ```
* Example 6 from JFP-paper (also demonstrates the risk of non-termination)
  ```
  class Mul a b c | a b -> c where
    (*)::a->b->c
  instance Mul a b c => Mul a (Vec b) (Vec c) where ...
  ```

## Example 1: liberal coverage breaks termination

The liberal coverage condition means that type inference can diverge.
Example from 5.2 of the JFP-paper:
```
class Mul a b c | a b -> c
instance {-# LIBERAL #-} Mul a b c => Mul a (Vec b) (Vec c)
```
This satisfies Paterson and LIBERAL.
Now suppose we are solving the constraint `[W] Mul alpha (Vec beta) beta`
* Fundeps give us `beta := Vec delta`
* Substituting we have `[W] Mul alpha (Vec (Vec delta)) (Vec delta)`
* That matches the instance decl, giving `[W] Mul alpha (Vec delta) delta`
* And now we are back where we began.

## Example 2: LCC and LICC do weird improvement (#10675 OP)

Consider
```
class CX x a b | a -> b where
  op :: x -> a -> b
instance                                CX Bool [x] [x]
instance {-# LIBERAL #-} CX Char x y => CX Char [x] [Maybe y]

f x = op True [x]
```
The instance decls require LICC.  But notice that they do not overlap, because of the first parameter.

From `f` we get `[W] CX Bool [alpha] beta`.
* Now GHC takes fundeps from *both* instances, giving `beta ~ [alpha]` and `beta ~ [Maybe gamma]`
* That leaves us with `CX Bool [Maybe gamma] [Maybe gamma]`
* We can solve that from the first instance decl.
* So we infer `f :: Maybe g -> [Maybe g]`.
  Bizarre.  Where did that `Maybe` come from?  It's nothing to do with it.

## Example 3: LCC and LICC threaten confluence

Consider:
```
class D a b c | b -> c
instance {-# LIBERAL #-} (q ~ Int)  => D Int  p (Int,q)
instance {-# LIBERAL #-} (s ~ Bool) => D Bool r (s,Bool)
```
These instances satisfy the Liberal Coverage and Liberal Instance Consistency conditions.

Now suppose we are trying to solve a Wanted constraint `[W] C alpha beta (gamma, delta)`.
* We'll get fundeps from both instances, yielding `gamma ~ Int` and `delta ~ Bool`.
* But if `alpha` later turns out to be `Int`, we'll select the first instance decl, getting `delta ~ Int`, resulting in a contradiction.
* If, on the other hand, we learned `alpha := Int` and `gamma := Int` earlier, we'd have picked the first instance immediately, and succeeded.

This reflects a loss of confluence.

## Example 4: Even LICC is too restrictive

Consider (assuming overlapping instances):
```
class TypeEq a b (res :: Bool)  | a b -> res
instance TypeEq a a True
instance TypeEq a b False
```
These instances satisfy SCC, but not SICC.
Nor does it satisfy the more liberal LICC, because True and False are not unifiable!  But imagine we rewrote it like this:
```
instance r ~ True  => TypeEq a a r
instance r ~ False => TypeEq a b r
```
Now the fundep is effectively vacuous, but if it remains we'd need LCC and LICC.  But the program works fine: the overlapping-instance technology will pick an instance only when it is the unique one, and that will fix `r`.

But it's a bit un-satisfying to have to encode our desired behaviour like this.
(Question: with bidirectional fundeps is this encoding even always possible?)

## Example 5: Even LCC is too restrictive

We can use fundeps to support record selection in records with polymorphic fields (#18759, #20188).  Consider
```
class HasField (name :: Symbol) s a | name s -> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance {-# DYSFUNCTIONAL #-} HasField "fld" T ([p] -> [p]) where
  getField (MkT f) = f

f x = (getField @"fld" x, True)
```
Here the instance doesn't even satisfy the LCC, so I've marked it DYSFUNCTIONAL.  And yet it is very useful!
* From `f` we get `[W] HasField "fld" T alpha`.
* Using the fundep we can get `alpha ~ ([beta] -> [beta])`, which is just what we want.

In effect, the fundep gives the *shape* of `alpha` but not its complete type.  This is a pretty compelling example.

Here is [a real-world example of someone wanting DYSFUNCTIONAL](https://stackoverflow.com/questions/65514023/how-to-require-functional-dependencies-in-kind-signature).

## Example 6: LIBERAL can get you DYSFUNCTIONAL

It turns out that with LIBERAL and UNDECIDABLE you can trick GHC into lifting the coverage condition algotether, effectively achieving DYSFUNCTIONAL.  Consider, this variant of Example 5:
```
instance {-# LIBERAL, UNDECIDABLE #-}
         HasField "fld" T ([p] -> [p])
         => HasField "fld" T ([p] -> [p]) where
  getField (MkT f) = f
```
We have added a strange context to the instance declaration, equal to itself!  Now the LCC is satisfied.  You might think that the instance is now non-terminating, because solving `HasField "fld" T ([p]->[p])` via the intance gives us a new sub-goal `HasField "fld" T ([p]->[p])`, and so on.

But GHC's type-class constraint solver has a long-standing trick whereby it solves goals co-inductively. I think it was first documented in [Scrap your boilerplate with class](https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-with-class/), where it is *essential* to allow SYB-with-class to work at all.  You might enjoy the paper; the coinductive part is discussed in Section 5.   Coinduction is switched on all the time, but it only has an effect when you have `UndecidableInstances`, which allows instance declarations that don't provably terminate.

So in principle, LIBERAL+UNDECIDABLE lets you express DYSFUNCTIONAL (no coverage condition at all).  But it's a weird coding trick, and so we leave DYSFUNCTIONAL in our vocabulary, for now anyway, to mean "lift coverage condition".

## Example 7: Overlapping instances

Here's an example from Csongor:
```
class HasField field s a | field s -> a where ...

-- Generic instance
instance {-# OVERLAPPABLE #-} 
         GHasField field s a => HasField field s a  where ...

-- Specific instance
instance HasField "foo" Foo Int  where ...
```
His intent is that the "generic instance" provides a generic but perhaps inefficient way to extract a field.  But in many cases the programmer will provide a more efficient override, here the "specific instance".

These definitions do not satisfy the liberal instance consistency condition (SICC), because the two instance heads unify on `field` and `s`, but that unifier does not force the `a` part to be the same.   So Csongor wants a weaker instance consistency condition: in this case the LICC is enough.

The `ether` library does something similar. Here's an edited highlight
```
class Monad m => MonadReader tag r m | m tag -> r where ...

-- Generic instance
instance {-# OVERLAPPABLE #-}
         ( Lift.LiftLocal t
         , Monad (t m)
         , MonadReader tag r m
         ) => MonadReader tag r (t m) where ...

-- Specific instance
instance (Monad m, r ~ r') 
      => MonadReader tag r (R.ReaderT tag r' m) where ...
```