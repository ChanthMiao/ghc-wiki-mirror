See #21779 for discussion.

"Wiggly arrows" are an idea for an alternative to functional dependencies, used merely to guide type inference. Hopefully this could provide a replacement for some of the current "abuses" of fundeps, and allow fundeps to be subsequently made stricter (e.g. by translation to type families).

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

Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC), which has pointers to terminology etc.

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


## Instance consistency

The following instances should be rejected:
```hs
class D a b c | a ~> b
instance D Int Bool Int
instance D Int Char Bool
```

It wouldn't be useful to accept these instances, because if we had a wanted `[W]
D Int alpha beta` we would be able to refine `alpha ~ Bool` from the first
instance and `alpha ~ Char` from the second, at which point we would fail with
an unsolvable constraint.  The point of the instance consistency check is to
reject such nonsense earlier, and also to ensure confluence.

The **liberal instance consistency check** (LICC) is too liberal.  In
particular, key example 2 shows it does "weird improvement" and key example 3
shows it is non-confluent.  Moreover it accepts examples like this:
```hs
class D a b c | a ~> b
instance D Int Bool Int
instance x ~ Char => D Int x Bool
```

The **strict instance consistency check** (SICC), at least as interpreted on the
[background page](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/background-and-terminology#31-strict-instance-consistency-condition-sicc), is too strict.  The following should be allowed:
```hs
class C a b c | a ~> b
instance b1 ~ Bool => C Int [b1] Bool
instance b2 ~ Char => C Int [b2] Char
```
because we can α-rename the second instance to
```hs
instance b1 ~ Char => C Int [b1] Char
```
and now the instances clearly satisfy the SICC.  Thus we propose to use the following,
which amounts to the SICC modulo α-equivalence:

**Refined instance consistency condition**.  Consider a declaration for class
`TC`, and any pair of instance declarations for that class:
```
class blah => TC a1 ... an | fd1; ...; fdm
instance D1 => TC t1...tn
instance D2 => TC s1...sn
```
Then, for each wiggly arrow `di`, of form `ai1; ...; aik ~> ai0`, the following
condition must hold: for any substitution `S` such that `S(ti1; ..., tik) = S(si1; ..., sik)`
there must be a permutation of variables `π` such that `π(S(ti0)) = S(si0)`.

Under the SICC, once an instance has established how much information can be
obtained from a particular wiggly arrow `lhs ~> rhs` and a particular choice of
`lhs`, other instances are required to be completely consistent with that
choice.  LICC allows other instances to provide more or less information, and
thereby risks making it possible to observe the order in which instances are
used.  In many cases, instances using the LICC can be rewritten to use a type
equality constraint in the context instead, and thereby satisfy the SICC.

For both fundeps and wiggly arrows the programmer should be able to explicitly
relax the SICC to the LICC for particular instances, or to impose no check at
all.  For example, this could be indicated by using a modifier on one of the
instances.  This would allow the programmer to accept potential non-confluence
in exchange for being able to express examples that do not satisfy the SICC.


## Interacting wanteds without LCC leads to non-confluence

This example shows that since wiggly arrows drop the LCC, they must also drop
wanted-wanted interactions.  The following instance (from
[this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/8634#note_86125)
and see also
[this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/8634#note_86143))
is allowed for wiggly arrows but not fundeps as it violates LCC:

```hs
class F a b | a ~> b where
   f :: a -> b -> a

instance F b a => F [a] [b]
```

Suppose we had the constraints `[W] F [alpha] [beta], F [alpha] gamma`.  Using
the instance we simplify to `[W] gamma ~ [beta'], F beta alpha, F beta' alpha`.
But if the original wanteds were allowed to interact to derive `gamma ~ [beta]`
we could also simplify to `[W] gamma ~ [beta], F beta alpha`, i.e. constraint
solving would not be confluent.


## What's the difference between fundeps and type equalities?

Consider:

```hs
class C a b c | a ~> b

instance C Int Char Bool -- (1)

instance x ~ Char => C Int x Bool -- (2)
```

A longstanding point of confusion for fundeps has been what precisely the
difference is between (1) and (2).  With wiggly arrows and SICC, the position is
this:

 * (1) says that a constraint `[W] C Int alpha beta` allows us to derive
   `[W] alpha ~ Char` irrespective of whether we commit to the instance.  All
   other instances must be consistent with this choice (at least under SICC).
   As the example demonstrates, this may not be enough to make the instance apply.
   And even when the instance matches, overlapping instances may mean that we cannot
   commit to it immediately.

 * (2) says that we can refine `x ~ Char` only after committing to the instance
   (i.e. once it matches and any overlaps have been resolved).  For example,
   from `[W] C Int alpha beta` we will not learn anything, but from
   `[W] C Int alpha Bool` we will match the instance and require `[W] alpha ~ Char`
   from the instance context.

If a collection of instances pass LICC but not SICC, it should always be
possible to reformulate them to pass SICC by taking the anti-unifier, putting
variables in the instance heads and introducing equality constraints in the
contexts instead.  This has the effect of delaying the additional constraints
until it is known which instance is applicable.


## `SetField` examples

Suppose we define

```hs
class SetField x s t b | x t -> b    -- true fundep
                       , x s ~> t    -- wiggly
                       , x t ~> s    -- wiggly
                       where
  setField :: b -> s -> t
```

Here `x t -> b` is a true fundep, because if we know the record type and field
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
  fun1 r = setField @"f" 0 r
  -- constraints arising:  SetField "f" (T ()) (T Int) alpha  (Num alpha)
  -- from x t -> b refine alpha := Int and match the instance

  fun2 r = setField @"f" 0 (r :: T ())
  -- interim inferred type:  T () -> beta
  -- constraints arising:  SetField "f" (T ()) beta alpha  (Num alpha)
  -- from x s ~> t refine beta := T beta' for fresh beta'
  -- from x t -> b refine alpha := beta' and match the instance
  -- generalise over beta'
  -- final inferred type:  Num a => T () -> T a

  fun3 r = (setField @"f" 0 r) :: T Int
  -- interim inferred type:  gamma -> T Int
  -- constraints arising:  SetField "f" gamma (T Int) alpha  (Num alpha)
  -- from x t ~> s refine gamma := T gamma' for fresh gamma'
  -- from x t -> b refine alpha := Int and match the instance
  -- generalise over gamma'
  -- final inferred type:  T a -> T Int

  fun4 r = setField @"f" 0 r
  -- interim inferred type:  delta -> epsilon
  -- constraint arising:  SetField "f" delta epsilon alpha
  -- final inferred type:  (Num b, SetField "f" s t b) => s -> t
  -- not ambiguous because b is determined from t (by x t -> b)

  fun5 r = setField @"g" True . setField @"f" () $ r
  -- interim inferred type: beta -> delta
  -- constraints arising:  SetField "f" beta gamma ()
  --                       SetField "g" gamma delta Bool
  -- final inferred type:  (SetField "f" s t (), SetField "g" t u Bool) => s -> u
  -- not ambiguous because t is determined from either s or u

  fun6 r = setField @"k" 0 . setField @"h" [] $ r
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

### Record updates with phantom parameters

Consider the following datatype, which has a phantom type parameter (i.e. the
type parameter does not occur in any of the field types):

```hs
data T a = MkT { foo :: Char, bar :: Bool }
```

A consequence of this is that (in traditional Haskell) a record update may
change the type of the phantom parameter arbitrarily, just as explicitly writing
out the case analysis can change the type:

```hs
upd1 :: T a -> T b
upd1 r = r { foo = 'c' }

upd1' :: T a -> T b
upd1' r = case r of MkT{bar=b} -> MkT{foo = 'c', bar=b}
```

Hence we want the following to be accepted:
```
upd2 :: T a -> T b
upd2 r = setField @"foo" 'c' r
```

This means we need to be prepared to solve the constraint `SetField "foo" (T a)
(T b) Char`, with `a` and `b` varying freely. Consequently in the class
`SetField x s t b`, we cannot have a functional dependency `x s b -> t`, because
`t` is not functionally dependent on the other parameters of the class.


## Key examples

Here are the
[key examples](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/key-examples)
with the fundeps replaced with wiggly arrows.

### Example 1

```hs
class Mul a b c | a b ~> c
instance Mul a b c => Mul a (Vec b) (Vec c)
```
Try solving `[W] Mul alpha (Vec beta) beta`.
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
and try solving `[W] CX Bool [alpha] beta`. Since we are allowed to use wiggly
arrows from both instances regardless of whether they match, we get
`beta := [alpha], beta := [Maybe gamma]` and hence `[W] CX Bool [Maybe gamma] [Maybe gamma]`,
i.e. weird improvement.

But note that these declarations require LICC. If we instead demand SICC this
doesn't arise. Instead the second instance will need to be rewritten like this:

```hs
instance                                CX Bool [x] [x]
instance  (CX Char x y, x ~ Maybe y) => CX Char [x] [x]
```

Now everything is fine: `[W] CX Bool [alpha] beta` gives `beta := [alpha]` and
so becomes `[W] CX Bool [alpha] [alpha]` which matches the first instance and
leaves us with the fully general type we expect.


###  Example 3

```hs
class D a b c | b ~> c

instance (q ~ Int)  => D Int  p (Int,q)
instance (s ~ Bool) => D Bool r (s,Bool)
```

Under LICC this violates confluence, including with wiggly arrows.  But under
SICC these instances are rejected.  Instead the user must write:

```hs
instance (q ~ Int)  => D Int  p (q,q)
instance (s ~ Bool) => D Bool r (s,s)
```

Now `[W] C alpha beta (gamma, delta)` does not make any progress.  If we later
learn `alpha ~ Int` then the first instance applies and gives
`gamma ~ Int, delta ~ Int`.


### Example 4

```hs
class TypeEq a b (res :: Bool)  | a b ~> res

instance {-# OVERLAPPING #-}  TypeEq a a True
instance {-# OVERLAPPABLE #-} TypeEq a b False
```

These instances violate LICC, let alone SICC.  And we want to reject them,
because otherwise `[W] TypeEq Int Int False` is solvable.  Instead the user
should write:

```hs
instance {-# OVERLAPPING #-}  r ~ True  => TypeEq a a r
instance {-# OVERLAPPABLE #-} r ~ False => TypeEq a b r
```


### Example 5

```hs
class HasField (name :: Symbol) s a | name s ~> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance HasField "fld" T ([p] -> [p])
  getField (MkT f) = f
```
If we have `[W] HasField "fld" T alpha`, we get `[W] alpha ~ ([beta] -> [beta])`,
just as we want.


### Example 6

We do not need the circular instance trick to bypass the coverage condition,
because wiggly arrows do not enforce the coverage condition anyway.


### Example 7

The example as stated violates SICC, but we can rewrite it thus:

```hs
class HasField field s a | field s ~> a where ...

-- Generic instance
instance {-# OVERLAPPABLE #-}
         GHasField field s a => HasField field s a  where ...

-- Specific instance
instance a ~ Int => HasField "foo" Foo a  where ...
```

The combination of a wiggly arrow `field s ~> a` and a fully polymorphic
"generic instance" `HasField field s a` is fine.  The wiggly arrow relaxes the
ambiguity check.  The SICC means that the specific instances must be written
using equality constraints in the context.



## Wiggly arrows versus Single Choice Inference

The [Single Choice Inference](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Single-choice-inference)
approach suggests that if there is exactly one instance that could match a
wanted constraint, refine the constraint based on the instance.

When **comparing a wanted constraint with instances**, we could say:

 * If any instances *match* the constraint, use the existing matching rules for
   instances.

 * If none match, find all instance declarations that *unify* with it. If there is
   more than one such instance, do nothing. (Maybe we could do something more
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

type Add :: Nat -> Nat -> Nat -> Constraint
class Add x y z | x y -> z, y z ~> x, x z ~> y

instance Add Z y y
instance Add x y z => Add (S x) y (S z)
```

Morally `Add` could have three fundeps `x y -> z, y z -> x, x z -> y`, because
any two parameters determine the other. However the LICC rejects these
instances, because if `y = S a, z = S a` then the first instance gives `x := Z`
while the second gives `x := S b`.

See also [further discussion on the Evidenced Functional Dependencies page](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies/Evidenced-Functional-Dependencies#adding-natural-numbers-with-3-way-fun-dep).
That page defines:

```hs
instance Add Z b b
instance {-# OVERLAPPABLE #-} (a ~ S a', c ~ S c', Add a' b c') => Add a b c
```

This violates SICC but satisfies LICC.  It has the nice property that, for
example, `[W] Add beta p p` automatically refines `beta := Z`.  It seems to be
difficult to retain this behaviour while satisfying SICC.


## Open questions

Wiggly arrows should be guarded by a language extension, but what should it be
called? Should we give the arrows themselves a better name?

Do we really want to use the syntax `a ~> b`?  This is close to the syntax of
the original fundeps paper (Jones 2000).

Given that (in the absence of the coverage condition) wiggly arrows may give
rise to non-termination of instance search, should they require
`UndecidableInstances`?

Should we try to tighten up the specification of functional dependencies at the
same time as introducing wiggly arrows, or should we defer it?
