"Wiggly arrows" are another idea for guiding type inference, comparable to [Single choice inference](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Single-choice-inference).

A normal fundep `lhs -> rhs` means that if the `lhs` parameters are fully
determined, the `rhs` parameters will necessarily be fully determined by the
instances.  That is, instances are supposed to imply the existence of a
function.

A wiggly arrow `lhs ~> rhs` means that we can use (partial) knowledge about the
`lhs` parameters to guide type inference for the `rhs` parameters, based on the
class instances, even though there may not be a function determining them fully.

## The idea

In the following, suppose we have:
```hs
class C a b | a ~> b
instance C Int Bool

class D a b c | a ~> b
instance D Int Bool Int
instance D Int Char Bool
```

For the ambiguity check, wiggly arrows are treated just like functional
dependencies.  The ambiguity check is in any case an approximation (because even
if a type is "ambiguous", there may be particular instantiations in which the
constraints can be solved anyway).  Thus it is fine to trust the programmer
here; the worst that can happen if they use too many wiggly arrows is that type
inference will fail with ambiguity errors.  For example, the type `C a b => a`
will not be considered ambiguous, even though `b` appears only in the
constraint, because it is determined by the wiggly arrow from `a`.

When interacting (given or wanted) constraints, unlike functional dependencies,
we do not learn anything from wiggly arrows. For example, if we have wanteds `C
Int alpha` and `C Int beta` we do not learn that `alpha ~ beta`.  As the `D`
example demonstrates this might be overly restrictive: if we have `D Int gamma1 delta`
and `D Int gamma2 delta` we do not want `gamma1 ~ gamma2` as we could instead
later have `gamma1 ~ Bool, delta1 ~ Int` and `gamma2 ~ Char, delta1 ~ Bool`.

When comparing a wanted constraint with instances, we use a variant of the rule
for functional dependencies.  If there is a wiggly arrow `lhs ~> rhs`, and out
of the instances that unify with the constraint, exactly one instance head
matches in the `lhs` parameters, we refine the shape of the `rhs` parameters to
match the instance head as well.  This may or may not be enough to allow the
instance to match; we make the refinement anyway. Note that we do not require
the `rhs` parameters to be **equal** to the `lhs` parameters.  For example, if
we have wanted `C Int beta` we can refine `beta ~ Bool` based on the `C Int
Bool` instance.

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
