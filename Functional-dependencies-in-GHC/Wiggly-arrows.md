"Wiggly arrows" are another idea for guiding type inference, comparable to [Single choice inference](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Single-choice-inference).

A **true fundep** `lhs -> rhs` means that if the `lhs` parameters are fully
determined, the `rhs` parameters will necessarily be fully determined by the
instances.  That is, instances are supposed to imply the existence of a
function.  True fundeps should always be translatable to use type families
instead.

A **wiggly arrow** `lhs ~> rhs` means that we can use (partial) knowledge about the
`lhs` parameters to guide type inference for the `rhs` parameters, based on the
class instances, even though there may not be a function determining them fully.

Our hypothesis is that all current uses of functional dependencies can either
use true fundeps or wiggly arrows.


## The idea

In the following, suppose we have:
```hs
class C a b | a ~> b
instance C Int Bool
instance C Char [b]
```

When **comparing a wanted constraint with instances**, we use the normal
improvement rule for functional dependencies.  For each wiggly arrow `lhs ~>
rhs`, if the instance matches all the `lhs` parameters, refine the shape of the
`rhs` parameters to match the instance as well.  This may or may not be enough
to allow the instance to match; we make the refinement anyway.

For example, if we have `[W] C Int beta` we can refine `beta := Bool` based on
the `C Int Bool` instance.  If we have `[W] C Char gamma`, we can refine `gamma
:= [delta]` where `delta` is fresh.

When **interacting constraints** (given or wanted), unlike functional
dependencies, we do not learn anything from wiggly arrows. For example, if we
have wanteds `C Char alpha` and `C Char beta` we do not learn that `alpha ~
beta` (as we would with true fundeps), merely that they both have the same outer
structure.  This would be overly restrictive, because both `C Char [Int]` and `C
Char [Bool]` are solvable, and we certainly do not have `[Int] ~ [Bool]`.

For the **ambiguity check**, wiggly arrows are treated just like functional
dependencies.  The ambiguity check is in any case an approximation (because even
if a type is "ambiguous", there may be particular instantiations in which the
constraints can be solved anyway).  Thus it is fine to trust the programmer
here; the worst that can happen if they use too many wiggly arrows is that type
inference will fail with ambiguity errors.  For example, the type `(C a b, D b)
=> a` will not be considered ambiguous, even though the constraint `D b` does
not mention any variables present in the type, because `b` is determined by the
wiggly arrow from `a`.

There is no **coverage condition** for wiggly arrows.  In particular, `instance
C Char [b]` is permitted even though `b` is not covered, and would be rejected
by a true fundep (even under the LCC).

We use the **liberal instance consistency check** (LICC) for wiggly arrows. For example, the
following is rejected:
```hs
class D a b c | a ~> b
instance D Int Bool Int
instance D Int Char Bool
```

It wouldn't be useful to accept these instances, because if we had a wanted `[W]
D Int alpha beta` we would be able to refine `alpha := Bool` from the first
instance and `alpha := Char` from the second, at which point we would fail with
an unsolvable constraint.  The point of the instance consistency check is merely
to reject such nonsense earlier.

TODO: Sam observes that the following should perhaps be rejected too:
```hs
class D a b c | a ~> b
instance D Int Bool Int
instance x ~ Char => D Int x Bool
```
but the LICC as stated will accept it, because the instance heads are unifiable.
Adam thinks we might be able to slightly strengthen the LICC to rule this out,
by requiring that under the unifying substitution the instance contexts should
not be obviously inconsistent.

TODO: which consistency condition do we need for true fundeps?

TODO: the rest of this page needs to be reviewed to make sure it is consistent
with the (revised) idea described above.


## `SetField` examples

Suppose we define

```hs
class SetField x s t b | x t -> b  -- normal fundep
                       , x s ~> t    -- wiggly
                       , x t ~> s    -- wiggly
                       where
  setField :: b -> s -> t
```

Here `x t -> b` is a normal fundep, because if we know the record type and field
name, we know the field's type.  (Actually this might still be a bit
restrictive, because it prevents instances for polymorphic fields, but that's a
design choice we can make.)

Now for a datatype
```hs
data T a = MkT { f :: a }
```
we expect the constraint solver to generate
```hs
instance SetField "f" (T a) (T b) b
```
(without any extra special magic to deal with the constraints, beyond the
fundeps/wiggly arrows).


We can use knowledge of either the LHS or RHS types or both to solve:
```hs
  fun1 :: T () -> T Int
  fun1 t = setField @"f" 0 t
  -- constraints arising:  SetField "f" (T ()) (T Int) alpha  (Num alpha)
  -- from x t -> b refine alpha := Int and match the instance

  fun2 t = setField @"f" 0 (t :: T ())
  -- interim inferred type:  T () -> beta
  -- constraints arising:  SetField "f" (T ()) beta alpha  (Num alpha)
  -- from x s ~> t refine beta := T beta' for fresh beta'
  -- from x t -> b refine alpha := beta' and match the instance
  -- generalise over beta'
  -- final inferred type:  Num a => T () -> T a

  fun3 t = (setField @"f" 0 t) :: T Int
  -- interim inferred type:  gamma -> T Int
  -- constraints arising:  SetField "f" gamma (T Int) alpha  (Num alpha)
  -- from x t ~> s refine gamma := T gamma' for fresh gamma'
  -- from x t -> b refine alpha := Int and match the instance
  -- generalise over gamma'
  -- final inferred type:  T a -> T Int

  fun4 t = setField @"f" 0 t
  -- interim inferred type:  delta -> epsilon
  -- constraint arising:  SetField "f" delta epsilon alpha
  -- final inferred type:  (Num b, SetField "f" s t b) => s -> t
  -- not ambiguous because b is determined from s (by x t -> b)

  fun5 t = setField @"g" True . setField @"f" () $ t
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetField "f" beta gamma ()
  --                       SetField "g" gamma delta Bool
  -- final inferred type:  (SetField "f" s t (), SetField "g" t u Bool) => s -> u
  -- not ambiguous because t is determined (from either s or u)

  fun6 t = setField @"k" 0 . setField @"h" [] $ t
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetField "h" beta gamma [alpha]
  --                       SetField "k" gamma delta epsilon  (Num epsilon)
  -- final inferred type:  (Num b, SetField "h" s t [a], SetField "k" t u b) => s -> u

  fun7 () = setField @"l" () undefined
  -- interim inferred type: () -> gamma
  -- constraints arising: SetField "l" beta gamma ()
  -- final inferred type:  SetField "l" s t () => () -> t
```

The following example demonstrates why we do not want two wanted constraints
that match in the `x` and `s` parameters to unify the `t` parameters:

```hs
  hmm v r = (setField @"foo" v r, setField @"foo" v r)
  -- interim inferred type: alpha -> beta -> (gamma, delta)
  -- constraints arising:  SetField "foo" beta gamma alpha
  --                       SetField "foo" beta delta alpha
  -- most general type:    (SetField "foo" s t b, SetField "foo" s t' b) => b -> s -> (t, t')

  -- if we assumed gamma ~ delta we would incorrectly specialise to:
  --                       SetField "foo" s t b => b -> s -> (t, t)

```

**Question**: this feels rather like a convenient syntax for the
[SameModulo approach](https://github.com/effectfully-ou/sketches/tree/master/has-lens-done-right#the-samemodulo-approach-full-code). Is there a semantic difference?


## Key examples

Here are the
[key examples](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/key-examples)
with the fundeps replaced with wiggly arrows.

### Example 1

```hs
class Mul a b c | a b ~> c
instance Mul a b c => Mul a (Vec b) (Vec c)
```
Try solving `[W] Mul alpha (Vec beta) beta`. (This unifies with the instance
with `b := beta, beta := Vec delta, a := alpha` where `delta` is fresh.)
Here the LHS of the wiggly arrow matches so we refine `beta := Vec gamma`,
match the instance and simplify to `[W] Mul alpha (Vec gamma) gamma`.
So we get a loop, just as before.  Wiggly arrows do not have guaranteed termination.

### Example 2

```hs
class CX x a b | a ~> b where
  op :: x -> a -> b

instance                 CX Bool [x] [x]
instance  CX Char x y => CX Char [x] [Maybe y]
```
and try solving `[W] CX Bool [alpha] beta`.
Ths unifies only with the first instance, so we get `beta := [alpha]` and solve.
No problem.

###  Example 3

```hs
class D a b c | b ~> c

instance (q ~ Int)  => D Int  p (Int,q)
instance (s ~ Bool) => D Bool r (s,Bool)
```
try solving `[W] C alpha beta (gamma, delta)`.
This unifies with both instances, so we do nothing.

### Example 4

```hs
class TypeEq a b (res :: Bool)  | a b ~> res

instance {-# OVERLAPPING #-}  TypeEq a a True
instance {-# OVERLAPPABLE #-} TypeEq a b False
```

Try solving `[W] TypeEq Int Bool alpha`.  This unifies only with the second
instance, so we get `alpha := False`.

Try solving `[W] TypeEq Int Int alpha`.  This unifies with both, but first is
most specific, so we pick it, and get `alpha := True`.

Try solving `[W] TypeEq Int Int False`.  This unifies only with the second
instance, so we select it.

TODO: these instances violate LICC so will be rejected.  Moreover we want to
reject them, because they allow `[W] TypeEq Int Int False` to be solved, which
makes very little sense.

### Example 5

```hs
class HasField (name :: Symbol) s a | name s ~> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance HasField "fld" T ([p] -> [p])
  getField (MkT f) = f
```
If we have `[W] HasField "fld" T alpha`, that will unify with only
one instance, so we will emit `[W] alpha ~ ([beta] -> [beta])`.


## Wiggly arrows versus Single Choice Inference

The [Single Choice Inference](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Single-choice-inference)
approach suggests that if there is exactly one instance that could match a
wanted constraint, refine the constraint based on the instance.

When **comparing a wanted constraint with instances**, we could say:

 * If any instances *match* the constraint, use the existing matching rules for
   instances.

 * If none match, find all instance declarations that *unify* with it. If there is
   more than one such instance, do nothing. (TODO: maybe we could do something more
   refined if there are overlapping instances.)

 * If there is exactly one unifying instance, then for each wiggly arrow `lhs ~>
   rhs` where the instance matches all the `lhs` parameters, refine the shape of
   the `rhs` parameters to match the instance as well.  This may or may not be
   enough to allow the instance to match; we make the refinement anyway.

This definition would mean that `singlechoice(a)` can be specified as `~> a`
(i.e. a wiggly arrow with an empty LHS). This means that whenever there is
exactly one unifying instance, the `a` parameter will always be refined to
match.

Alongside this it would make sense to say there is no **instance consistency
check** for wiggly arrows. For example, the user is at liberty to define
instances for `C Int Bool` and `C Int Char`; this means that a constraint `[W] C
Int alpha` will no longer refine `alpha`.

We could do this, but it takes wiggly arrows further from current fundeps, and
moreover it violates the open world assumption: adding an instance can make code
cease to type-check because some constraint no longer has a single choice.
Hence we prefer the simpler design described above.


## More examples

### Zip

From #19568 consider the `Zip` type family

```hs
type family Zip as bs = r | r -> as bs where
  Zip '[]       '[]       = '[]
  Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs
```

which can be reformulated as a class with wiggly arrows:

```hs
class Zip as bs cs | as bs -> cs, as ~> bs, bs ~> cs, cs ~> as

instance Zip '[] '[] '[]
instance (Zip as bs cs, ab ~ '(a, b)) => Zip (a : as) (b : bs) (ab : cs)
```

Now if we have `[W] Zip a '[] c` we get refinements `a := '[]` and `c := '[]`.

Similarly if we have `[W] Zip as bs (c : cs)` we get refinements `as := (a : as')`,
`bs := (b : bs')`, `c := '(a, b)`. Note that it is crucial we have `ab ~ '(a, b)`
in the instance context rather than the head, because otherwise the instance would
not match the constraint until we know `c` was a pair.

**Question**: how does this compare to approaches based on
```hs
type family SameShapeAs xs ys :: Constraint where
  SameShapeAs '[]      ys = (ys ~ '[])
  SameShapeAs (x : xs) ys = (ys ~ Head ys : Tail ys)
```

That seems enough to give `Zip` with equivalent inference, but requires the
definition of lots of constraint families and auxiliary definitions like `Head`
and `Tail`.  Generalising it to work for arbitrary head type constructors seems
hard.

### Plus

Consider:

```hs
data Nat = Z | S Nat

type Plus :: Nat -> Nat -> Nat -> Constraint
class Plus x y z | x y -> z, y z ~> x, x z ~> y

instance Plus Z y y
instance Plus x y z => Plus (S x) y (S z)
```

Morally `Plus` could have three fundeps `x y -> z, y z -> x, x z -> y`, because any two parameters determine the other. However the instance consistency condition rejects these instances if fundeps are used, perhaps because we might have some `y ~ S y` which would match both?

...