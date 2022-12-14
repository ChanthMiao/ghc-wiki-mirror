# Support for deriving `Functor`, `Foldable`, and `Traversable` instances


GHC 6.12.1 introduces an extension to the `deriving` mechanism allowing for automatic derivation of `Functor`, `Foldable`, and `Traversable` instances using the `DeriveFunctor`, `DeriveFoldable`, and `DeriveTraversable` extensions, respectively. Twan van Laarhoven [first proposed this feature](https://mail.haskell.org/pipermail/haskell-prime/2007-March/002137.html) in 2007, and [opened a related GHC ticket](https://gitlab.haskell.org/ghc/ghc/ticket/2953) in 2009.

## Example

```haskell
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data Example a = Ex a Char (Example a) (Example Char)
  deriving (Functor, Foldable, Traversable)
```


The derived code would look something like this:

```haskell
instance Functor Example where
    fmap f (Ex a1 a2 a3 a4) = Ex (f a1) a2 (fmap f a3) a4

instance Foldable Example where
    foldr f z (Ex a1 a2 a3 a4) = f a1 (foldr f z a3)
    foldMap f (Ex a1 a2 a3 a4) = mappend (f a1) (foldMap f a3)

instance Traversable Example where
    traverse f (Ex a1 a2 a3 a4) = fmap (\b1 b3 -> Ex b1 a2 b3 a4) (f a1) <*> traverse f a3
```

## Algorithm description

`DeriveFunctor`, `DeriveFoldable`, and `DeriveTraversable` all operate using the same underlying mechanism. GHC inspects the arguments of each constructor and derives some operation to perform on each argument, which depends of the type of the argument itself. In a `Functor` instance, for example `fmap` would be applied to occurrences of the last type parameter, but `id` would be applied to other type parameters. Typically, there are five cases to consider. (Suppose we have a data type `data A a = ...`.)

1. Terms whose type does not mention `a`
1. Terms whose type mentions `a`
1. Occurrences of `a`
1. Tuple values
1. Function values


After this is done, the new terms are combined in some way. For instance, `Functor` instances combine terms in a derived `fmap` definition by applying the appropriate constructor to all terms, whereas in `Foldable` instances, a derived `foldMap` definition would `mappend` the terms together.

### `DeriveFunctor`


A comment in [TcGenDeriv.hs](http://git.haskell.org/ghc.git/blob/9f968e97a0de9c2509da00f6337b612dd72a0389:/compiler/typecheck/TcGenDeriv.hs#l1476) lays out the basic structure of `DeriveFunctor`, which derives an implementation for `fmap`.

```wiki
For the data type:

  data T a = T1 Int a | T2 (T a)

We generate the instance:

  instance Functor T where
      fmap f (T1 b1 a) = T1 b1 (f a)
      fmap f (T2 ta)   = T2 (fmap f ta)

Notice that we don't simply apply 'fmap' to the constructor arguments.
Rather
  - Do nothing to an argument whose type doesn't mention 'a'
  - Apply 'f' to an argument of type 'a'
  - Apply 'fmap f' to other arguments
That's why we have to recurse deeply into the constructor argument types,
rather than just one level, as we typically do.

What about types with more than one type parameter?  In general, we only
derive Functor for the last position:

  data S a b = S1 [b] | S2 (a, T a b)
  instance Functor (S a) where
    fmap f (S1 bs)    = S1 (fmap f bs)
    fmap f (S2 (p,q)) = S2 (p, fmap f q)

However, we have special cases for
         - tuples
         - functions

More formally, we write the derivation of fmap code over type variable
'a for type 'b as ($fmap 'a 'b).  In this general notation the derived
instance for T is:

  instance Functor T where
      fmap f (T1 x1 x2) = T1 ($(fmap 'a 'b1) x1) ($(fmap 'a 'a) x2)
      fmap f (T2 x1)    = T2 ($(fmap 'a '(T a)) x1)

  $(fmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(fmap 'a 'a)          =  f
  $(fmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(fmap 'a 'b1) x1, $(fmap 'a 'b2) x2)
  $(fmap 'a '(T b1 b2))  =  fmap $(fmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(fmap 'a '(b -> c))   =  \x b -> $(fmap 'a 'c) (x ($(cofmap 'a 'b) b))

For functions, the type parameter 'a can occur in a contravariant position,
which means we need to derive a function like:

  cofmap :: (a -> b) -> (f b -> f a)

This is pretty much the same as $fmap, only without the $(cofmap 'a 'a) case:

  $(cofmap 'a 'b)          =  \x -> x     -- when b does not contain a
  $(cofmap 'a 'a)          =  error "type variable in contravariant position"
  $(cofmap 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> ($(cofmap 'a 'b1) x1, $(cofmap 'a 'b2) x2)
  $(cofmap 'a '[b])        =  map $(cofmap 'a 'b)
  $(cofmap 'a '(T b1 b2))  =  fmap $(cofmap 'a 'b2)   -- when a only occurs in the last parameter, b2
  $(cofmap 'a '(b -> c))   =  \x b -> $(cofmap 'a' 'c) (x ($(fmap 'a 'c) b))
```

`DeriveFunctor` is special in that it can recurse into function types, whereas `DeriveFoldable` and `DeriveTraversable` cannot (see the section on covariant and contravariant positions).

### `DeriveFoldable`


Another comment in [TcGenDeriv.hs](http://git.haskell.org/ghc.git/blob/9f968e97a0de9c2509da00f6337b612dd72a0389:/compiler/typecheck/TcGenDeriv.hs#l1725) reveals the underlying mechanism behind `DeriveFoldable`:

```wiki
Deriving Foldable instances works the same way as Functor instances,
only Foldable instances are not possible for function types at all.
Given (data T a = T a a (T a) deriving Foldable), we get:

  instance Foldable T where
      foldr f z (T x1 x2 x3) =
        $(foldr 'a 'a) x1 ( $(foldr 'a 'a) x2 ( $(foldr 'a '(T a)) x3 z ) )

-XDeriveFoldable is different from -XDeriveFunctor in that it filters out
arguments to the constructor that would produce useless code in a Foldable
instance. For example, the following datatype:

  data Foo a = Foo Int a Int deriving Foldable

would have the following generated Foldable instance:

  instance Foldable Foo where
    foldr f z (Foo x1 x2 x3) = $(foldr 'a 'a) x2

since neither of the two Int arguments are folded over.

The cases are:

  $(foldr 'a 'a)         =  f
  $(foldr 'a '(b1,b2))   =  \x z -> case x of (x1,x2) -> $(foldr 'a 'b1) x1 ( $(foldr 'a 'b2) x2 z )
  $(foldr 'a '(T b1 b2)) =  \x z -> foldr $(foldr 'a 'b2) z x  -- when a only occurs in the last parameter, b2

Note that the arguments to the real foldr function are the wrong way around,
since (f :: a -> b -> b), while (foldr f :: b -> t a -> b).

One can envision a case for types that don't contain the last type variable:

  $(foldr 'a 'b)         =  \x z -> z     -- when b does not contain a

But this case will never materialize, since the aforementioned filtering
removes all such types from consideration.
```


In addition to `foldr`, `DeriveFoldable` also generates a definition for `foldMap` as of GHC 7.8.1 (addressing #7436). The pseudo-definition for `$(foldMap)` would look something like this:

```wiki
  $(foldMap 'a 'b)         = \x -> mempty     -- when b does not contain a
  $(foldMap 'a 'a)         = f
  $(foldMap 'a '(b1,b2))   = \x -> case x of (x1, x2) -> mappend ($(foldMap 'a 'b1) x1) ($(foldMap 'a 'b2) x2)
  $(foldMap 'a '(T b1 b2)) = \x -> foldMap $(foldMap 'a 'b2) x -- when a only occurs in the last parameter, b2
```

### `DeriveTraversable`


From [TcGenDeriv.hs](http://git.haskell.org/ghc.git/blob/9f968e97a0de9c2509da00f6337b612dd72a0389:/compiler/typecheck/TcGenDeriv.hs#l1800):

```wiki
Again, Traversable is much like Functor and Foldable.

The cases are:

  $(traverse 'a 'a)          =  f
  $(traverse 'a '(b1,b2))    =  \x -> case x of (x1,x2) -> (,) <$> $(traverse 'a 'b1) x1 <*> $(traverse 'a 'b2) x2
  $(traverse 'a '(T b1 b2))  =  traverse $(traverse 'a 'b2)  -- when a only occurs in the last parameter, b2

Like -XDeriveFoldable, -XDeriveTraversable filters out arguments whose types
do not mention the last type parameter. Therefore, the following datatype:

  data Foo a = Foo Int a Int

would have the following derived Traversable instance:

  instance Traversable Foo where
    traverse f (Foo x1 x2 x3) =
      fmap (\b2 -> Foo x1 b2 x3) ( $(traverse 'a 'a) x2 )

since the two Int arguments do not produce any effects in a traversal.

One can envision a case for types that do not mention the last type parameter:

  $(traverse 'a 'b)          =  pure     -- when b does not contain a

But this case will never materialize, since the aforementioned filtering
removes all such types from consideration.
```

### Covariant and contravariant positions


One challenge of deriving `Functor` instances for arbitrary data types is handling function types. To illustrate this, note that these all can have derived `Functor` instances:

```haskell
data CovFun1 a = CovFun1 (Int -> a)
data CovFun2 a = CovFun2 ((a -> Int) -> a)
data CovFun3 a = CovFun3 (((Int -> a) -> Int) -> a)
```


but none of these can:

```haskell
data ContraFun1 a = ContraFun1 (a -> Int)
data ContraFun2 a = ContraFun2 ((Int -> a) -> Int)
data ContraFun3 a = ContraFun3 (((a -> Int) -> a) -> Int)
```


In `CovFun1`, `CovFun2`, and `CovFun3`, all occurrences of the type variable `a` are in *covariant* positions (i.e., the `a` values are produced), whereas in `ContraFun1`, `ContraFun2`, and `ContraFun3`, all occurrences of `a` are in *contravariant* positions (i.e., the `a` values are consumed). If we have a function `f :: a -> b`, we can't apply `f` to an `a` value in a contravariant position, which precludes a `Functor` instance.


Most type variables appear in covariant positions. Functions are special in that the lefthand side of a function arrow reverses variance. If a function type `a -> b` appears in a covariant position (e.g., `CovFun1` above), then `a` is in a contravariant position and `b` is in a covariant position. Similarly, if `a -> b` appears in a contravariant position (e.g., `CovFun2` above), then `a` is in a covariant position and `b` is in a contravariant position.


If we annotate covariant positions with `p` (for positive) and contravariant positions with `n` (for negative), then we can examine the above examples with the following pseudo-type signatures:

```wiki
CovFun1/ContraFun1 :: n -> p
CovFun2/ContraFun2 :: (p -> n) -> p
CovFun3/ContraFun3 :: ((n -> p) -> n) -> p
```


Since `ContraFun1`, `ContraFun2`, and `ContraFun3` all use the last type parameter in at least one `n` position, GHC would reject a derived `Functor` instance for each of them.

## Requirements for legal instances


This mechanism cannot derive `Functor`, `Foldable`, or `Traversable` instances for all data types. Currently, GHC checks if a data type meets the following criteria:

1. The data type has at least one type parameter. (For example, `data NoArg = NoArg` cannot have a `Functor` instance.)
1. The data type's last type parameter cannot be used contravariantly. (see the section on covariant and contravariant positions.)
1. The data type's last type parameter cannot be used in the "wrong place" in any constructor's data arguments. For example, in `data Right a = Right [a] (Either Int a)`, the type parameter `a` is only ever used as the last type argument in `[]` and `Either`, so both `[a]` and `Either Int a` values can be `fmap`ped. However, in `data Wrong a = Wrong (Either a a)`, the type variable `a` appears in a position other than the last, so trying to `fmap` an `Either a a` value would not typecheck.

>
> Note that there are two exceptions to this rule: tuple and function types.

1. The data type's last type variable cannot used in a `-XDatatypeContexts` constraint. For example, `data Ord a => O a = O a deriving Functor` would be rejected.


In addition, GHC performs checks for certain classes only:

1. For derived `Foldable` and `Traversable` instances, a data type cannot use function types. This restriction does not apply to derived `Functor` instances, however.
1. For derived `Functor` and `Traversable` instances, the data type's last type variable must be truly universally quantified, i.e., it must not have any class or equality constraints. This means that the following is legal:

```haskell
data T a b where
    T1 :: a -> b -> T a b      -- Fine! Vanilla H-98
    T2 :: b -> c -> T a b      -- Fine! Existential c, but we can still map over 'b'
    T3 :: b -> T Int b         -- Fine! Constraint 'a', but 'b' is still polymorphic

deriving instance Functor (T a)

{-
instance Functor (T a) where
    fmap f (T1 a b) = T1 a (f b)
    fmap f (T2 b c) = T2 (f b) c
    fmap f (T3 x)   = T3 (f x)
-}
```

but the following is not legal:


```
data T a b where
    T4 :: Ord b => b -> T a b  -- No!  'b' is constrained
    T5 :: b -> T b b           -- No!  'b' is constrained
    T6 :: T a (b,b)            -- No!  'b' is constrained
```

This restriction does not apply to derived `Foldable` instances. See the following section for more details.

### Relaxed universality check for `DeriveFoldable`

`DeriveFunctor` and `DeriveTraversable` cannot be used with data types that use existential constraints, since the type signatures of `fmap` and `traverse` make this impossible. However, `Foldable` instances are unique in that they do not produce constraints, but only consume them. Therefore, it is permissible to derive `Foldable` instances for constrained data types (e.g., GADTs).


For example, consider the following GADT:

```haskell
data T a where
    T1 :: Ord a => a -> T a
```


In the type signatures for `fmap :: Functor t => (a -> b) -> t a -> t b` and `traverse :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)`, the `t` parameter appears both in an argument and the result type, so pattern-matching on a value of `t` must not impose any constraints, as neither `fmap` nor `traverse` would typecheck.

`Foldable`, however, only mentions `t` in argument types:

```haskell
class Foldable t where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldr1 :: (a -> a -> a) -> t a -> a
    foldl1 :: (a -> a -> a) -> t a -> a
    toList :: t a -> [a]
    null :: t a -> Bool
    length :: t a -> Int
    elem :: Eq a => a -> t a -> Bool
    maximum :: forall a. Ord a => t a -> a
    minimum :: forall a. Ord a => t a -> a
    sum :: Num a => t a -> a
    product :: Num a => t a -> a
```


Therefore, a derived `Foldable` instance for `T` typechecks:

```haskell
instance Foldable T where
    foldr f z (T1 a) = f a z -- foldr :: Ord a => (a -> b -> b) -> b -> T a -> b
    foldMap f (T1 a) = f a   -- foldMap :: (Monoid m, Ord a) => (a -> m) -> T a -> m
```


Deriving `Foldable` instances for GADTs with equality constraints could become murky, however. Consider this GADT:

```haskell
data E a where
    E1 :: (a ~ Int) => a   -> E a
    E2 ::              Int -> E Int
    E3 :: (a ~ Int) => a   -> E Int
    E4 :: (a ~ Int) => Int -> E a
```


All four `E` constructors have the same "shape" in that they all take an argument of type `a` (or `Int`, to which `a` is constrained to be equal). Does that mean all four constructors would have their arguments folded over? While it is possible to derive perfectly valid code which would do so:

```haskell
instance Foldable E where
    foldr f z (E1 e) = f e z
    foldr f z (E2 e) = f e z
    foldr f z (E3 e) = f e z
    foldr f z (E4 e) = f e z

    foldMap f (E1 e) = f e
    foldMap f (E2 e) = f e
    foldMap f (E3 e) = f e
    foldMap f (E4 e) = f e
```


it is much harder to determine which arguments are equivalent to `a`. Also consider this case:

```haskell
data UnknownConstraints a where
    UC :: Mystery a => Int -> UnknownConstraints a
```


For all we know, it may be that `a ~ Int => Mystery a`. Does this mean that the `Int` argument in `UC` should be folded over?


To avoid these thorny edge cases, we only consider constructor arguments (1) whose types are *syntactically* equivalent to the last type parameter and (2) in cases when the last type parameter is a truly universally polymorphic. In the above `E` example, only `E1` fits the bill, so the derived `Foldable` instance is actually:

```haskell
instance Foldable E where
    foldr f z (E1 e) = f e z
    foldr f z (E2 e) = z
    foldr f z (E3 e) = z
    foldr f z (E4 e) = z

    foldMap f (E1 e) = f e
    foldMap f (E2 e) = mempty
    foldMap f (E3 e) = mempty
    foldMap f (E4 e) = mempty
```


To expound more on the meaning of criterion (2), we want not only to avoid cases like `E2 :: Int -> E Int`, but also something like this:

```haskell
data HigherKinded f a where
    HigherKinded :: f a -> HigherKinded f (f a)
```


In this example, the last type variable is instantiated with `f a`, which contains one type variable `f` applied to another type variable `a`. We would *not* fold over the argument of type `f a` in this case, because the last type variable should be *simple*, i.e., contain only a single variable without any application.


For the original discussion on this proposal, see #10447.

## Alternative strategy for deriving `Foldable` and `Traversable`



We adapt the algorithms for `-XDeriveFoldable` and `-XDeriveTraversable` based on that of `-XDeriveFunctor`. However, there is an important difference between deriving the former two typeclasses and the latter one (as of GHC 8.2, addressing [Trac \#11174](https://ghc.haskell.org/trac/ghc/ticket/11174)), which is best illustrated by the following scenario:


```haskell
data WithInt a = WithInt a Int# deriving (Functor, Foldable, Traversable)
```


The generated code for the `Functor` instance is straightforward:

```haskell
instance Functor WithInt where
  fmap f (WithInt a i) = WithInt (f a) i
```


But if we use too similar of a strategy for deriving the `Foldable` and `Traversable` instances, we end up with this code:

```haskell
instance Foldable WithInt where
  foldMap f (WithInt a i) = f a <> mempty

instance Traversable WithInt where
  traverse f (WithInt a i) = fmap WithInt (f a) <*> pure i
```


This is unsatisfying for two reasons:

1. The `Traversable` instance doesn't typecheck! `Int#` is of kind `#`, but `pure` expects an argument whose type is of kind `*`. This effectively prevents `Traversable` from being derived for any datatype with an unlifted argument type (see [Trac \#11174](https://gitlab.haskell.org/ghc/ghc/issues/11174)).

1. The generated code contains superfluous expressions. By the `Monoid` laws, we can reduce `f a <> mempty` to `f a`, and by the `Applicative` laws, we can reduce `fmap WithInt (f a) <*> pure i` to `fmap (\b -> WithInt b i) (f a)`.


We can fix both of these issues by incorporating a slight twist to the usual algorithm that we use for `-XDeriveFunctor`. The differences can be summarized as follows:

1. In the generated expression, we only fold over arguments whose types mention the last type parameter. Any other argument types will simply produce useless `mempty`s or `pure`s, so they can be safely ignored.

1. In the case of `-XDeriveTraversable`, instead of applying `ConName`, we apply `\b_i ... b_k -> ConName a_1 ... a_n`, where

- `ConName` has `n` arguments
- `{b_i, ..., b_k}` is a subset of `{a_1, ..., a_n}` whose indices correspond to the arguments whose types mention the last type parameter. As a consequence, taking the difference of `{a_1, ..., a_n}` and `{b_i, ..., b_k}` yields the all the argument values of `ConName` whose types do not mention the last type parameter. Note that `[i, ..., k]` is a strictly increasing???but not necessarily consecutive???integer sequence.

   For example, the datatype

```haskell
data Foo a = Foo Int a Int a
```

   would generate the following `Traversable` instance:

   ```haskell
   instanceTraversableFoowhere
     traverse f (Foo a1 a2 a3 a4)=
       fmap (\b2 b4 ->Foo a1 b2 a3 b4)(f a2)<*> f a4
   ```


Technically, this approach would also work for `-XDeriveFunctor` as well, but we decide not to do so because:

1. There's not much benefit to generating, e.g., `(\b -> WithInt b i) (f a)` instead of `WithInt (f a) i`.

1. There would be certain datatypes for which the above strategy would generate `Functor` code that would fail to typecheck. For example:

   ```
   dataBar f a =Bar(forall f.Functor f => f a)derivingFunctor
   ```

   With the conventional algorithm, it would generate something like:

   ```haskell
   fmap f (Bar a) = Bar(fmap f a)
   ```

   which typechecks. But with the strategy mentioned above, it would generate:

   ```
   fmap f (Bar a) = (\b -> Bar b) (fmap f a)
   ```

   which does not typecheck, since GHC cannot unify the rank-2 type variables in the types of `b` and `fmap f a`.

## Future work


There are more classes in `base` that we could derive!


In particular, the `Bifunctor` class (born from the [bifunctors](http://hackage.haskell.org/package/bifunctors) library) [was added](https://gitlab.haskell.org/ghc/ghc/issues/9682) to `base` in GHC 7.10, and the `Bifoldable` and `Bitraversable` classes (also from `bifunctors`) [were added](https://gitlab.haskell.org/ghc/ghc/issues/10448) to `base` in GHC 8.2. All three classes could be derived in much the same way as their cousins `Functor`, `Foldable`, and `Traversable`. The existing algorithms would simply need to be adapted to accommodate two type parameters instead of one. The [Data.Bifunctor.TH](http://hackage.haskell.org/package/bifunctors-5.3/docs/Data-Bifunctor-TH.html) module from the `bifunctors` library demonstrates an implementation of the following proposal using Template Haskell.


In GHC 8.0, higher-order versions of the `Eq`, `Ord`, `Read`, and `Show` typeclasses were added to `base` in the `Data.Functor.Classes` module (which originally lived in the `transformers` library). These classes are generalized to work over datatypes indexed by one type parameter (for `Eq1`, `Ord1`, `Read1`, and `Show1`) or by two type parameters (`Eq2`, `Ord2`, `Read2`, and `Show2`). Haskell programmers have been able to derive `Eq`, `Ord`, `Read`, and `Show` for a long time, so it wouldn't be hard at all to envision a deriving mechanism for `Eq1`, `Eq2`, and friends which takes advantage of tricks that `DeriveFunctor` uses. The [deriving-compat](http://hackage.haskell.org/package/deriving-compat-0.3) library demonstrates proofs-of-concept for deriving [Eq1/2](http://hackage.haskell.org/package/deriving-compat-0.3/docs/Data-Eq-Deriving.html), [Ord1/2](http://hackage.haskell.org/package/deriving-compat-0.3/docs/Data-Ord-Deriving.html), [Read1/2](http://hackage.haskell.org/package/deriving-compat-0.3/docs/Text-Read-Deriving.html), and [Show1/2](http://hackage.haskell.org/package/deriving-compat-0.3/docs/Text-Show-Deriving.html) using Template Haskell.

### Classes


The `Bifunctor`, `Bifoldable`, and `Bitraversable` classes are defined as follows:

```haskell
class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

class Bifoldable p where
    bifoldMap :: Monoid m => (a -> m) -> (b -> m) -> p a b -> m
    bifoldr :: (a -> c -> c) -> (b -> c -> c) -> c -> p a b -> c

class (Bifunctor t, Bifoldable t) => Bitraversable t where
    bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> t a b -> f (t c d)
```


Each class contains further methods, but they can be defined in terms of the above ones. Therefore, we need only derive implementations for them. This also mirrors how the algorithms currently work in the one-parameter cases, as they only implement `fmap`, `foldMap`, `foldr`, and `traverse`.


The typeclasses in `Data.Functor.Classes` are defined as follows:

```haskell
class Eq1 f where
    liftEq :: (a -> b -> Bool) -> f a -> f b -> Bool

class (Eq1 f) => Ord1 f where
    liftCompare :: (a -> b -> Ordering) -> f a -> f b -> Ordering

class Read1 f where
    liftReadsPrec :: (Int -> ReadS a) -> ReadS [a] -> Int -> ReadS (f a)
    liftReadList  :: (Int -> ReadS a) -> ReadS [a]        -> ReadS [f a]

class Show1 f where
    liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int ->  f a  -> ShowS
    liftShowList  :: (Int -> a -> ShowS) -> ([a] -> ShowS)        -> [f a] -> ShowS

class Eq2 f where
    liftEq2 :: (a -> b -> Bool) -> (c -> d -> Bool) -> f a c -> f b d -> Bool

class (Eq2 f) => Ord2 f where
    liftCompare2 :: (a -> b -> Ordering) -> (c -> d -> Ordering) -> f a c -> f b d -> Ordering

class Read2 f where
    liftReadsPrec2 :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b] -> Int -> ReadS (f a b)
    liftReadList2  :: (Int -> ReadS a) -> ReadS [a] -> (Int -> ReadS b) -> ReadS [b]        -> ReadS [f a b]

class Show2 f where
    liftShowsPrec2 :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS) -> Int ->  f a b - > ShowS
    liftShowList2  :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> (Int -> b -> ShowS) -> ([b] -> ShowS)        -> [f a b] -> ShowS
```

### Algorithms


A pseudo-code algorithm for generating a `bimap` implementation is:

```wiki
We write the derivation of bimap code over the last two type variables
'a and 'b, for the given type 'c, as ($bimap 'a 'b 'c). We refer bimap's
first and second map functions as f and g, respectively.

  $(bimap 'a 'b 'c)             =  \x -> x     -- when c does not contain a or b
  $(bimap 'a 'b 'a)             =  f
  $(bimap 'a 'b 'b)             =  g
  $(bimap 'a 'b '(c1,c2))       =  \x -> case x of (x1,x2) -> ($(bimap 'a 'b 'c1) x1, $(bimap 'a 'b 'c2) x2)
  $(bimap 'a 'b '(T c1 c2 c3))  =  bimap $(bimap 'a 'b 'c2) $(bimap 'a 'b 'c3)   -- when a and b only occur in the last two parameters, c2 and c3
  $(bimap 'a 'b '(T c1 c2 c3))  =  fmap  $(bimap 'a 'b 'c3)                      -- when a and b only occur in the last parameter, c3
  $(bimap 'a 'b '(c -> d))      =  \x e -> $(bimap 'a 'b 'd) (x ($(cobimap 'a 'b 'c) e))

For functions, the type parameters, 'a and 'b, can occur in contravariant positions,
which means we need to derive a function like:

  cobimap :: (a -> b) -> (c -> d) -> (f b d -> f a c)

This is pretty much the same as $bimap, only without the $(cobimap 'a 'b 'a) and $(cobimap 'a 'b 'b) cases:

  $(cobimap 'a 'b 'c)             =  \x -> x     -- when c does not contain a or b
  $(cobimap 'a 'b 'a)             =  error "type variable in contravariant position"
  $(cobimap 'a 'b 'b)             =  error "type variable in contravariant position"
  $(cobimap 'a 'b '(c1,c2))       =  \x -> case x of (x1,x2) -> ($(cobimap 'a 'b 'c1) x1, $(cobimap 'a 'b 'c2) x2)
  $(cobimap 'a 'b '(T c1 c2 c3))  =  bimap $(cobimap 'a 'b 'c2) $(cobimap 'a 'b 'c3)   -- when a and b only occur in the last two parameters, c2 and c3
  $(cobimap 'a 'b '(T c1 c2 c3))  =  fmap  $(cobimap 'a 'b 'c2)                        -- when a and b only occur in the last parameter, c3
  $(cobimap 'a 'b '(c -> d))      =  \x e -> $(cobimap 'a 'b 'd) (x ($(bimap 'a 'b 'c) e))
```


This algorithm isn't terribly different from the one above for generating an `fmap` implementation, and that's the point. It's simply generalizing the same ideas to work over a typeclass of kind `* -> * -> *`. The algorithms for generating `foldMap`/`foldr` and `traverse` can be generalized to generate `bifoldMap`/`bifoldr` and `bitraverse`, respectively. For example, here's what the algorithm for `bifoldMap` would look like:

```wiki
  $(bifoldMap 'a 'b 'c)            = \x -> mempty     -- when c does not contain a or b
  $(bifoldMap 'a 'b 'a)            = f
  $(bifoldMap 'a 'b 'b)            = g
  $(bifoldMap 'a 'b '(c1,c2))      = \x -> case x of (x1, x2) -> mappend ($(bifoldMap 'a 'b 'c1) x1) ($(bifoldMap 'a 'b 'c2) x2)
  $(bifoldMap 'a 'b '(T c1 c2 c3)) = bifoldMap $(bifoldMap 'a 'b 'c2) $(bifoldMap 'a 'b 'c3) -- when a and b only occur in the last two parameters, c2 and c3
  $(bifoldMap 'a 'b '(T c1 c2 c3)) = foldMap $(bifoldMap 'a 'b 'c3)                          -- when a and b only occur in the last parameter, c3
```


(The caveats in [https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor\#AlternativestrategyforderivingFoldableandTraversable](https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor#AlternativestrategyforderivingFoldableandTraversable) apply.)


There's one part of the `bifoldMap` algorithm that deserves futher discussion: the overlapping cases for `T c1 c1 c3`. Whenever an argument to a constructor has a type  where each of the last two type variables mention `a` or `b`, we opt to generate `bifoldMap` instead of `foldMap`. We *could* go the other way, though. For instance, the following is a valid implementation of `Bifoldable` for `newtype T a b = T (Either a b)`:

```haskell
instance Bifoldable T where
  bifoldMap _ g (T e) = foldMap g e
```


But this is unsatisfying for a couple of reasons, though. One obvious issue is that this definition blatantly ignores the first argument to `bifoldMap`, preventing users from folding over the `a` type parameter. Another problem is that doing this would be inconsistent with how `bimap` and `bitraverse` are generated. Unlike with `bifoldMap`, parametricity forces there to be one definition for `bimap` and `bitraverse` (see [https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor\#RelaxeduniversalitycheckforDeriveFoldable](https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/DeriveFunctor#RelaxeduniversalitycheckforDeriveFoldable) for more info):

```haskell
instance Bifunctor T where
  bimap f g (T e) = T (bimap f g e)

instance Bitraversable T where
  bitraverse f g (T e) = fmap T (bitraverse f g e)
```


Therefore, it feels far more natural to generate this `Bifoldable` instance:

```haskell
instance Bifoldable T where
  bifoldMap f g (T e) = bifoldMap f g e
```


This also ensures that [bifoldMapDefault](http://hackage.haskell.org/package/bifunctors-5.3/docs/Data-Bitraversable.html#v:bifoldMapDefault) gives the same result as `bifoldMap`.

#### Corner case: GADTs


Consider the following code:

```haskell
data Both a b where
  BothCon :: x -> x -> Both x x

deriving instance Bifoldable Both
```

What should be the definition of `bifoldMap` for `Both`? We have a choice, since both the function argument of type `(a -> m)` and of type `(b -> m)` can be applied to either argument. In such a scenario, the second fold function takes precedence over the first fold function, so the derived `Bifoldable` instance would be:

```haskell
instance Bifoldable Both where
  bifoldMap _ g (BothCon x1 x2) = g x1 <> g x2
```


This definition ensures that `bifoldMap id = foldMap` for a derived `Foldable` instance for `Both`.

#### `Data.Functor.Classes`


Deriving `Eq1/2`, `Ord1/2`, and `Show1/2` could be done in a very similar way to deriving `Foldable/Bifoldable`. For instance, you can substitute in `liftEq`/`liftEq2` in place of `foldMap`/`bifoldMap` in the above algorithm and have a sensible way to generate `liftEq`/`liftEq2` expressions. In addition, `Eq1/2`, `Ord1/2`, and `Show1/2` can all be derived for GADTs as well.

`Read1/2` could not be derived for GADTs, since it mentions type variables in the return types of its methods, but it could still be derived using the same principles as deriving `Foldable`/`Bifoldable`.