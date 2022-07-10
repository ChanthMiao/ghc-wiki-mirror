This page has key examples related to functional dependencies.

Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

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
* [`AddNat` Example 8 below](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Key-examples#example-8-multiway-fundeps); `HElem` [from the `HList` paper 2003] searching for an element in a type-level list; and in general any instance using recursive descent down a structure.

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

## Example 2:  LICC does weird improvement (#10675 OP, #9210)

Consider #10675
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

Here's another (#9210):
```
class Foo s t a b | a b s -> t 
instance Foo (x, a) (y, a) x y 
instance Foo (a, x) (a, y) x y 
```
Note that the fundep is full.  The instances do not satisfy SICC, because `Foo (p,p) r p y` would imply that `r` is both `(y,p)` (by I1) and `(p,y)` (by I2).  But they do satisfy LICC. 

So if I have `[W] Foo (Int,Int) alpha Int Bool` I will emit improvement equalities `alpha ~ (Int,Bool)` and `alpha ~ (Bool,Int)`. Boo!

[AntC] The crux here is semi-overlap: the instances unify on `Foo (x, x) (...) x y`. (Hint: try putting `OVERLAPPABLE/OVERLAPPING` pragmas on those instances.) I think GHC's attitude to overlap is too liberal (that causes trouble other places, too): it should insist instances overlap in a strict substitution ordering.

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

[AntC] A simpler example of the same non-confluence

```haskell
class SomeC a b c  | a -> b

instance SomeC Int Bool Char                    -- covered

instance {-# LIBERAL #-} (b ~ String) => SomeC Int b ()

someMeth :: SomeC a b c => a -> b -> c -> b
someMeth x y z = y
```

If the SICC held, the compiler could safely improve using `[W] SomeC Int alpha v` (where `v` is say a skolem variable) and spit out `alpha ~ Bool` without needing to check which instance matches `v`.

But you can call `someMeth` and yield either a `Bool` or a `String` -- depending on what you supply as the _third_ argument, which is no part of the FunDep.

More starkly, this instance is valid with the two above:

```haskell
instance {-# LIBERAL, OVERLAPPABLE #-} (b ~ String) => SomeC Int b Char
```

Again you can call `someMeth` and yield a `String`: merely by supplying a `String` as second argument -- despite the FunDep+first instance giving that should be `Bool`.[improvement vs instance selection q's](Functional-dependencies-in-GHC/Key-examples/FunDep-improvement-vs-instance-selection)

This illustrates the weakness of 'non-Full' FunDeps (defined in the JFP-paper § 6.1 "The real benefit of full FDs is that together with the Weak Coverage Condition they guarantee confluence." Also "For full FDs we can shorten the translation to CHRs by combining the instance improvement and instance rules into one rule." -- 'instance rules' means instance selection). 


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
instance {-# OVERLAPPING #-}  r ~ True  => TypeEq a a r
instance {-# OVERLAPPABLE #-} r ~ False => TypeEq a b r
```
Now the fundep is effectively vacuous, but if it remains we'd need LCC and LICC.  But the program works fine: the overlapping-instance technology will pick an instance only when it is the unique one, and that will fix `r`.

You might think that it's a bit un-satisfying to have to encode our desired behaviour like this. (Question: with bidirectional fundeps is this encoding even always possible? Answer [AntC following teleconf with SPJ 6-Jul-2022]: not always, or at least not easily: [take the example of `AddNat` with three-way FunDeps](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Key-examples#example-8-multiway-fundeps) -- then each of the three positions is a target. If you reduce all three to bare distinct tyvars, because each is a target, you can only write one instance -- see ## Example 8.)

Moreover, you have to add `-XUndecidableInstances` (currently module-wide) to allow these instances, which is a bit of a heavy hammer.
But there is a good reason. Suppose we allowed the instances
```
class TypeEq a b (res :: Bool)  | a b -> res
instance TypeEq a a True
instance TypeEq a b False
```
and we want to solve `[W] TypeEq Int Int False`.  Well that does not match the first instance, but it does match the second, so we'll succeed, instantiating `a` and `b` to `Int`!  But that was definitely not what we intended.  If instead we had the rewritten form
```
instance {-# OVERLAPPING #-}  r ~ True  => TypeEq a a r
instance {-# OVERLAPPABLE #-} r ~ False => TypeEq a b r
```
and try to solve `[W] TypeEq Int Int False`.  We'd pick the first, and end up trying to solve `False ~ True` which is exactly what should happen.

TL;DR: weakening beyond LICC is dangerous.

**Discussion between AntC and SPJ; ultimately to be moved**

[AntC Note: puzzling behaviour -- almost doing the right thing]

With these instances:

```haskell
instance                                   TypeEq a a True
instance {-# OVERLAPPABLE #-} r ~ False => TypeEq a b r
```

`[w] TypeEq Int Bool False` yields `False` (expected)<br>
`[W] TypeEq Int Int  False` yields `False` (expected, but not desired)

With only the `instance TypEq a a True`:

`[w] TypeEq Int Bool False` yields `No instance for (TypeEq Int Bool 'False)`
(rejection expected)\
`[W] TypeEq Int Int  False` yields (rejection expected, it's the reason that's interesting)
```
                * Couldn't match type 'True with 'False
                  arising from a functional dependency between:
                  constraint `TypeEq Int Int 'False' arising from ...
                  instance `TypeEq a a 'True'
```

So(?) GHC recognises the instance ought to apply; and does select it; then rejects the program because the improvement gives type inequality. Then why can't it recognise the instance applies in the two-instance case?

SPJ says.  GHC does two *entirely separate* steps:
1. Improvement of a Wanted.  The only effect is to emit new equality constraints
2. Discharging a Wanted using an instance.

For (1), GHC sees `[W] TypeEq Int Int False`, and does improvement against the first instance, emitting a new equality `[W] False ~ True`.  This is what yields the error.  But this is totally unrelated to step 2.

For (2), GHC sees `[W] TypeEq Int Int False`, and sees that it matches no instance, so does nothing.


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

#8634 is another example of where even the liberal coverage condition was too restrictive.

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

## Example 8: Multiway FunDeps

(Ref Example # 4 Question: with bidirectional fundeps is this encoding even always possible? -- that is, an encoding with bare tyvar in the target position.)

If we write base case:

```haskell
class AddNat (x :: Nat) (y :: Nat) (z :: Nat)  | x y -> z, x z -> y, y z -> x

instance                                          AddNat Z      y      y
```

All three positions are targets; that is all of `x`, `y`, `z` appear in the RHS of a fundep arrow. 

Can we follow Example # 4's "imagine we rewrote" to put a fresh bare tyvar in the target positions of the instance?; No, we'd get a 'catch-all' `instance AddNat x y z`, so no way to write a distinct/more general instance for the Successor case(s). Note even with the base case as above, we have to write the Successor case like the catch-all second below:

```haskell
instance {-# OVERLAPPABLE #-} (z ~ (S z'), AddNat x' y z')
                                               => AddNat (S x') y      z

instance {-# OVERLAPPABLE #-} (x ~ (S x'), z ~ (S z'), AddNat x' y z')
                                               => AddNat x      y      z
```

Despite the `OVERLAPPABLE` allegation, the `instance AddNat (S x') y z` is not more general than `instance Z y y`; it is apart because `(S x') /~ Z`. Then for the FunDep consistency check on `y z -> x`, GHC finds it can unify the determining positions (with `z := y`); under that unification the dependent position namely `(S x')`, doesn't unify with `Z`.

With more complex Multiway FunDeps, the catch-all instance might need to call an auxiliary class to discriminate more cases.