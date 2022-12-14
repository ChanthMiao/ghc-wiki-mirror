# Type Indexed Type Representations Proposal

## Overview


This page describes a new API for type indexed type representations, basic `Dynamic` functionality, static pointers and distributed closures.
It is a specific realisation of the ideas in [DistributedHaskell](distributed-haskell).


Just as in [DistributedHaskell](distributed-haskell), we provide 4 APIs

- `Data.Typeable`: type-indexed type representation.  This replaces the existing `Typeable` class, and the kind of `TypeRep` changes to `TypeRep :: k -> *`.

- `Data.Dynamic`: dynamically-typed values; replaces the existing `Data.Dynamic`.  The API is almost unchanged.

- `Data.StaticPtr`: static pointers. The idea is that this will ultimately by the foundation for the [Cloud Haskell](https://hackage.haskell.org/package/distributed-static) package.

- `Control.DistributedClosure`: serialisable closures.  The idea is that this will ultimately by the foundation for the Cloud Haskell `distributed-static` package.


Each is described in more detail below.

## Transition Plan


We propose to

- *Outright replace* the existing `Typeable` class with the new one; ditto the `TypeRep` type.
  This will change the API.
  This seems better than inventing new names for everything (e.g. class `TypeableT`, data type `TypeRepT`).
  Not many packages use `TypeRep` explicitly, we want to encourage those that do to switch over.

- GHC magic for the `Typeable` class will apply to the new class.

- To ease the transition we will provide both

  - The old API via a (deprecated) new module `Data.Typeable710`.
  - The old API via the exiting module `Data.Typeable` but with new names for (deprecated) old types and functions.

- Update the existing `Dynamic` type to be based on the new `Typeable` class.
  This will keep the same API, except we have to drop the function `dynTypeRep :: Dynamic -> TypeRep ?`, as we cannot return an indexed `TypeRep`.
  We allow pattern matching on the `Dynamic` type as an alternative.

- The static pointer and closure modules are new.


(Code will be forthcoming: it implements the library side of `Data.Typeable` and `Data.StaticPtr` and all of `Data.Dynamic` and `Control.DistributedClosure`.
The GHC support for deriving `Typeable` and the `static` keyword (and thus building the static pointer table) is not done, but there is a mock-up in library code.
We provide the new `Typeable` and `Dynamic` in `Data.TypeableT` and `Data.DynamicT` to avoid name clashes for now.)

## Questions


See also the individual questions sections below.

- Now is the easiest time to rename things - do you have suggestions for better naming schemes?
- It remains to be seen if this implementation is suitable as a foundation for the Cloud Haskell packages.
  The polymorphic static pointer support is different, and I'm not entirely certain that CH doesn't need full rank-1 polymorphism support (but I am fairly confident).
  At the very least, there is more overhead (on the wire), and some performance tests should be done before committing to anything.
- Any comments would be gladly received!


There is #11011 for discussion of this proposal.

---

## Data.Typeable


For `Data.Typeable` we ultimately need Richard Eisenberg's kind equalities.
But until GHC gets kind equalities we offer a variant ("homogeneous case") that doesn't need them, but has an extra `unsafeCoerce` or two, and returns `Bool` rather than a type equality on kind-heterogeneous comparisons.


### Without kind equalities


```
data TypeRep (a :: k) -- abstract
typeRepFingerprint :: TypeRep a -> Fingerprint
appR :: TypeRep (a :: k -> k') -> TypeRep (b :: k) -> TypeRep (a b)

class Typeable (a :: k) where
  typeRep :: TypeRep a

-- GHC has magic built-in support for Typeable instances
-- but the effect is similar to declarations like these:
instance (Typeable c, Typeable a) => Typeable (c a)
instance Typeable Bool
instance Typeable (->)

withTypeRep :: TypeRep a -> (Typeable a => b) -> b
-- c.f. Trac #2439

eqRR :: TypeRep (a :: k1) -> TypeRep (b :: k2) -> Bool
eqRRHom :: TypeRep (a :: k) -> TypeRep (b :: k) -> Maybe (a :~: b)

data GetApp (a :: k) where
  GA :: TypeRep (a :: k1 -> k2) -> TypeRep (b :: k1) -> GetApp (a b)
getAppR :: TypeRep (a :: k) -> Maybe (GetApp a)

data G1 c a where
  G1 :: TypeRep (a :: k) -> G1 (c :: k -> k') (c a)
getR1 :: TypeRep (ct :: k1 -> k) -> TypeRep (c'at :: k) -> Maybe (G1 ct c'at)
-- Implementation uses an unsafeCoerce

data G2 c a where
  G2 :: TypeRep (a :: k1) -> TypeRep (b :: k2) -> G2 (c :: k1 -> k2 -> k3) (c a b)
getR2 :: TypeRep (c :: k2 -> k1 -> k) -> TypeRep (a :: k) -> Maybe (G2 c a)
-- Implementation uses an unsafeCoerce

-- rest are for convenience
typeOf  :: Typeable a => (a :: *) -> TypeRep a
getFnR  :: TypeRep (a :: *) -> Maybe (G2 (->) a)
castR   :: TypeRep (a :: *) -> TypeRep (b :: *) -> a -> Maybe b
cast    :: (Typeable (a :: *), Typeable (b :: *)) => a -> Maybe b
gcastR  :: TypeRep (a :: k) -> TypeRep (b :: k) -> c a -> Maybe (c b)
gcast   :: (Typeable (a :: k), Typeable (b :: k)) => c a -> Maybe (c b)
gcastR1 :: TypeRep (t :: k1 -> k2) -> TypeRep (t' :: k1 -> k2) -> c (t a) -> Maybe (c (t' a))
gcast1  :: (Typeable (t :: k1 -> k2), Typeable (t' :: k1 -> k2)) => c (t a) -> Maybe (c (t' a))
gcastR2 :: TypeRep (t :: k1 -> k2 -> k3) -> TypeRep (t' :: k1 -> k2 -> k3) -> c (t a b) -> Maybe (c (t' a b))
gcast2 :: (Typeable (t :: k1 -> k2 -> k3), Typeable (t' :: k1 -> k2 -> k3)) => c (t a b) -> Maybe (c (t' a b))
```


Notes:

- Many of these functions come in two variants:

  - one which takes an explicit `TypeRep` argument, and
  - one that take an implicit `TypeRep` argument via a `Typeable a` constraint.

>
>
> We use a consistent naming scheme: put an `R` suffix on variants that take an explicit `TypeRep` parameter, no suffix for `Typeable` constraint versions.
>
>

- Note that the type `(:~:)` comes from `Data.Type.Equality`.
  And `Fingerprint` is from `GHC.Fingerprint`.

- `typeRepFingerprint` is not used internally (although it could be for `eqRR`), but is provided for users.
  e.g. to add consistency checks when using `Control.DistributedClosure` that two nodes agree on their static pointer table (or at least the types agree, if not the values).

- Note also `eqRR` is not hugely useful as (if it returns True) we know that types and kinds are the same, but GHC doesn't, so `unsafeCoerce` is often needed.

- The `withTypeRep` function is potentially useful, and illustrates a generally useful pattern: see [Typeable/WithTypeable](typeable/with-typeable).

### Key differences from GHC 7.10

- The key difference is that `TypeRep` becomes type-indexed, so that if `x :: TypeRep [Int]` then `x` is a runtime-inspectable type representation for `[Int]`.
  It is poly-kinded, of course, so that `TypeRep Maybe` is fine.

- In class `Typeable`, the `typeRep` method therefore no longer needs a proxy argument.
  Indeed the class dictionary precisely is a single type representation.

- Functions for constructing and destructing `TypeRep`s differ, in particular destructing needs a GADT return type to deal with existentially hidden `TypeRep` indices.

- The new API does not expose `TyCon`, and is therefore a much smaller API; see questions below.

### With kind equalities


Once we have kind equalities, we have a kind-heterogeneous `(:~~:) :: k1 -> k2 -> *`.
Now we do not `unsafeCoerce` in `getR1` and the like, so we can now just export `getAppR` and leave the rest to the users.


The changes are that now:

- `eqRR :: TypeRep (a :: k1) -> TypeRep (b :: k2) -> a :~~: b` (i.e. now returns `a:~~:b` rather than `Bool`), and is more useful (doesn't force us to use `unsafeCoerce`)
- `getR1` and `getR2` don't need `unsafeCoerce`, and we can generalise `G1`, `getR1` etc. to be poly-kinded i.e. `getR1 :: TypeRep (a :: k1 -> k) -> TypeRep (b :: k2) -> Maybe (G1 a b)` where `k /= k2`


.
We obviously may want to provide (and deprecate) `getR1`, `eqRRHom` etc. for compatibility, but they now can be written entirely safely in user-code.


(I am not yet sure whether it would be useful to keep the homogeneous equality functions around --- potentially enforcing kind homogeneity could be useful)

### Trusted code base


The TCB consists of (in the homogeneous case), the implementation of `data TypeRep`, `class Typeable` and its implementations, `eqRR` and `eqRRHom` (comparison of `TypeRep`s), `getR1` and `getR2` (decomposing `TypeRep`s).
As is currently done for `Typeable`, the instances for `Typeable` should be "magically" provided by GHC.


In the kind-heterogeneous case, `getR1` and `getR2` come out of the TCB.

### Questions

- How many `getR1`, `getR2` etc should we provide?

- Currently, equality checks for `TypeRep`s are done recursively rather than using fingerprints, and once no-kinds is merged, it will actually build equality evidence out of smaller equality evidince.
  However, using fingerprints and one `unsafeCoerce` may be more efficient (especially if fingerprints are cached in the datatype), but is argueably less principled, and certainly brings more code into the TCB.
  Which method do we want to use?
  Or perhaps provide both `eqRR` with fingerprints and `eqRRParanoid` recursively?

- Do we want explicit names for some type representations?
  Perhaps `typeRepBool` etc., just for Prelude defined types.
  (It is nice to avoid writing `typeRep :: TypeRep Bool`)

- `TyCon` is used internally but is now entirely hidden from the user.
  Is there a use-case where this is not desirable?  By way of background, the internal representatation looks like this

  ```wiki
  data TypeRep (a :: k) where
    TRCon :: TyCon a -> TypeRep a
    TRApp :: TypeRep a -> TypeRep b -> TypeRep (a b)
  ```

  where the `TyCon a` is a now-internal data type describing a type constructor.

- For compatability, the old API is exported from `Data.Typeable` with `710` as a suffix on everything, and under the old names from `Data.Typeable.Typeable710`.
  Comments on naming scheme are welcome!

- Currently, all fingerprints match between the old, `base`, `TypeRep`, the new `TypeRep` and the compatability `TypeRep710`.
  Is this the correct route, or should we force them to be different, or perhaps different in a predictable way (xor with `"new_____"`, `"old_____"` and `"compat__"` or some-such)?

- A related ticket: [7897](https://gitlab.haskell.org/ghc/ghc/issues/7897) (MakeTypeRep fingerprints be proper, robust fingerprints) describes a situation where the data type can change, but the fingerprint remains the same (as it is a hash of "\<package-key\> \<module\> \<type name\>"), but it only really affects the main package.
  The conclusion appears to be to keep the status quo.

- Naming question: is `withTypeRep` or `withTypeable` preferred?

- `withTypeRep` is not actually needed in the current code base, as most functions have a variant which takes a plain `TypeRep`.
  (However, we do use `Dict (Typeable a)` in the Static Pointer Table code, which we could potentially do without if we use `withTypeRep`.)
  Do we want to provide it?
  (Note that one cannot write it safely in source Haskell, but it is trivial in Core, as (Typeable is a one-method class, implemented basically as a newtype, and is a singleton, so there are no incoherence issues)).

- Relatedly, `withTypeRep` is currently in its own module, as I am not particularly comfortable with the contortions one needs in source Haskell to write it.
  Should it be made an first-class member of the API and exported from `Data.Typeable`?

---

## Data.Dynamic


This should be a fairly seamless changeover, since `Dynamic` is abstract currently (although we cannot provide a `dynTypeRep :: Dynamic -> TypeRep ?` function - this does not seem oft-used & can be avoided by pattern matching as our `Dynamic` is not abstract).


The API follows the current API, except missing `dynTypeRep`, as detailed above.
We provide variants of functions that take explicit `TypeRep` arguments.


```
data Dynamic where
  Dynamic :: TypeRep a -> a -> Dynamic

toDynR :: TypeRep a -> a -> Dynamic
toDyn  :: Typeable a => a -> Dynamic
fromDynamicR :: TypeRep a -> Dynamic -> Maybe a
fromDynamic  :: Typeable a => Dynamic -> Maybe a

fromDynR :: TypeRep a -> Dynamic -> a -> a
fromDyn  :: Typeable a => Dynamic -> a -> a

dynApp   :: Dynamic -> Dynamic -> Dynamic  -- Existing function; calls error on failure
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic

data SDynamic s where
  SDynamic :: TypeRep a -> s a -> SDynamic s

toSDynR :: TypeRep a -> s a -> SDynamic s
toSDyn :: Typeable a => s a -> SDynamic s
fromSDynamicR :: TypeRep a -> SDynamic s -> Maybe (s a)
fromSDynamic :: Typeable a => SDynamic s -> Maybe (s a)
fromSDynR :: TypeRep a -> SDynamic s -> s a -> s a
fromSDyn :: Typeable a => SDynamic s -> s a -> s a
```


Notes

- These is no trusted code here; i.e. no `unsafeCorece`s in the implementation.

- `Dynamic` is *not* abstract, so you can pattern match on it. If it were abstract we'd need to add

  ```wiki
  unpackDynamic :: Dynamic -> (forall a. TypeRep a -> a -> r) -> r
  ```

- `SDynamic` is a mid-point between fully dynamic & fully static types.
  We statically know some "Shape" information, but not all info about type.
  e.g., `SDynamic Maybe` contains a value that is definitely a `Maybe ty` for some type `ty`, but the type `ty` can vary between values of type `SDynamic Maybe`.

>
>
> One use-case is in the implementation of `StaticPtr`.
>
>

### Questions

- Do we want to provide `dynApp` which calls `error` instead of returning `Nothing`?
  It seems to be much less used than `dynApply`, and personally I dislike that style.
- Since we are modifying things, perhaps we should think about a more invasive change (or alternatively an extension to the API!)
  `Maybe` is not very informative, perhaps we should change to `Either String` or `Either FromError` and `Either ApplyError`.
  Note in `dynApply` two errors are currently conflated:

  - "first arg is not a function",
  - "function expects different type to arg given".

  Also, it is not particularly easy to print out what the type mis-match was in `fromDynamic`.

---

## Data.StaticPtr


Note that the static pointer support requires a static pointer table in a different form to what GHC already supports, and an extension to the `static` keyword (or a new `polystatic` keyword).
Recall:

- `static` is a language construct, and `static <expr>`, has type `StaticPtr t` if `<expr>` has type `t`.
- In `static <expr>`, the free variables (and type variables) of `<expr>` must all be bound at top level.
- The implementation could work by giving `<expr>` a top-level definition with a new name, `static34 = <expr>`, then adding a row in the static pointer table recording that the `StaticPtr` of name "\<expr\>-key" points to `static34`, of type `t`, and a reflection: `TypeRep t`.

  - This `TypeRep t` enables us to do a dynamic typecheck when deserialising a `StaticPtr`, which is serialised as just its key in the static pointer table.


See below for a description of `polystatic`.



The API:


```
data Dict c where
  Dict :: forall c . c => Dict c

-- A StaticPtr is just a /code/ pointer to a monomorphic value
data StaticPtr (a :: *) -- abstract

deRefStaticPtr :: StaticPtr a -> a

putSDynStaticPtr :: SDynamic StaticPtr -> Put
getSDynStaticPtr :: Get (SDynamic StaticPtr)
instance Binary (SDynamic StaticPtr)

putStaticPtr :: StaticPtr a -> Put
getStaticPtr :: TypeRep a -> Get (StaticPtr a)
instance Typeable a => Binary (StaticPtr a)

-- A Static is either a StaticPtr, a "polymorphic code pointer", or a syntax tree of applications of such.
data Static (a :: *) -- abstract

deRefStatic :: Static a -> a

staticMono :: StaticPtr a -> Static a
staticApp :: Static (a -> b) -> Static a -> Static b

putSDynStatic :: SDynamic Static -> Put
getSDynStatic :: Get (SDynamic Static)
instance Binary (SDynamic Static)

putStatic :: Static a -> Put
getStatic :: TypeRep a -> Get (Static a)
instance Typeable a => Binary (Static a)
```


Notes

- To serialise a static pointer, we just serialise its name

  - What a name actually comprises is fairly flexible:

    - If all binaries have the same SPT, we could just have an numeric index into the SPT
    - If we want to support overlapping SPTs, we could do a (package,module,name) triple
      Since we have a safe lookup function, this choice cannot impact type-safety
- We don't actually expore a `lookupSPT` function, but that is the content of `getStaticPtr`.
- It would be nice to have a different keyword and type (as currently: `StaticPre` vs `Static`) for polymorphic support, as the serialization of a `StaticPtr` is small, but for a polymorphic `Static`, may be suprisingly large.

### Polymorphism


We may wish to be able to have `staticReverse = static reverse :: StaticPtr (forall a. [a] -> [a])`, but this is not possible.
If we attempted it, we would need to put a row in the static pointer table along the lines of `("staticReverse-key", reverse :: forall a. [a] -> [a], TypeRep (forall a. [a] -> [a]))`, but the type representation runs into impredicativity issues.


Warning: polymorphism seems to be a tricky issue: it certianly took up the vast majority of my time and thinking!
We may wish to not support it at all - see "Possible approaches to avoid polymorphism".

#### Our Approach


Our approach for closures, however, needs polymorphism support: we wish for some class `Serializable (a :: *)` and a function `closurePure :: Serializable a => a -> Closure a`, which lets us take any `Int` or `Bool` or `[Maybe (Int, Bool)]` etc and create a closure.
Our approach is that, roughly `Serializable` has a method `binDict :: Static (Dict (Binary a))`, and a `closurePure x` is serialised as this static dictionary and `encode x :: Bytestring`.
To write `instance Serializable a => Serializable (Maybe a)` we need a static function `sMaybeDB :: Static (Dict (Binary a) -> Dict (Binary (Maybe a)))`, which is polymorphic.
Then the instance can be written:


```
instance Serializable b => Serializable (Maybe b) where
  binDict = sMaybeDB `staticApp` binDict
```


The problem now is that we don't know how to put `sMaybeDB` into the SPT, as it has a polymorphic type.



Thankfully, we actually turn out to need just "the polymorphic function `f` with its one free type variable instantiated at `a`", where `a` is given by a *static* `Typeable` dictionary, and we need this instantiated "`f @ a`" to be serialisable.
(Note that we may use multiple types `a` at different places, or polymorphism wouldn't be necessary!)
As on [StaticPointers](static-pointers), we propose to only deal with parametric polymorphism, and not type-class polymorphism, which users can reduce to the former using the Dict Trick (see \[[DistributedHaskell](distributed-haskell)\].
We limit ourselves to one free type variable of kind `*` and rank 1 polymorphism.
(Our approach could be extended with a 2-polymorphic flavour of `Static` easily (or 3- etc), but it is not clear how to do one flavour which is polymorphic of arbitrary arity.)


So we want:

- if `<expr> :: forall a . Typeable a => t` (`a` occurs in `t`), then `polystatic <expr>` has type `forall a . Static (Dict (Typeable (a :: *))) -> Static t`, where `a` can occur in `t`, but all other (value and type) variables are bound at top level.
- This is put into a "polymorphic pointer table" (PPT) as a row `("<expr>-key", expr' :: forall (a :: *) . Dict (Typeable a) -> (t, TypeRep t))`
- Note that

  ```wiki
  sMaybeDB :: Static (Dict (Typeable (a :: *))) -> Static (Dict (Binary a) -> Dict (Binary (Maybe a)))
  sMaybeDB = polystatic (\(Dict :: Dict (Binary a)) -> Dict :: Dict (Binary (Maybe a)))
  ```

  So `sMaybeDB` is not `Static`, so we never try to serialise it, just things of the form `sMaybeDB (staticTypeDictMaybeInt :: Static (Dict (Typeable (Maybe Int))))`.
  This we can serialise as

  - a tag to show this is a polymorphic thing (as opposed to a `staticApp` or a plain `StaticPtr`)
  - then `"sMaybeDB-key"`
  - finally the serialization of `staticTypeDictMaybeInt` (which will itself probably be `sMaybeDB staticTypeDictInt`, where `staticTypeDictInt` is finally a plain `StaticPtr`, which is serialized as `"staticTypeDictInt-key"`).

  We can then decode in the obvious way, making dynamic typechecks where necessary.
- The implementation could work by giving `<expr>` a top-level definition with a new name, `polystatic34 = \(Dict :: Dict(Typeable a))  -> <expr>`, then `expr'` in the PPT becomes the code pointer to `polystatic34`
- Note the types won't line up nicely to naively have the PPT being a list, so we currently use type families to have a type-level function, `PolyTag`, so instead of `t` above, we have `PolyTag MaybeDBTag a` instead (where `MaybeDBTag` is a new data type whose only role is as an argument to PolyTag.
  Then we can have the homgeneous rows (Key, tag , forall a . Dict (Typeable a) -\> (PolyTag tag a, TypeRep (PolyTag tag a))).


Then our instance can be nicely written:
(note that we also need a `Static (Dict (Typeable a))` in `Serializable a`, as we need to pass that to `sMaybeDB`, so we have to add a bit more code to build a `Static (Dict (Typeable (Maybe a)))` also.


```
--Our polymorphic "wrap a `Maybe` around this Typeable/Binary dictionary" functions:
-- these just point to enteries in the Polymorphic Pointer Table
sMaybeDB :: Static (Dict (Typeable (a :: *))) -> Static (Dict (Binary a) -> Dict (Binary (Maybe a)))
sMaybeDB = polystatic (\(Dict :: Dict (Binary a)) -> Dict :: Dict (Binary (Maybe a)))

sMaybeDT :: Static (Dict (Typeable (a :: *))) -> Static (Dict (Typeable (Maybe a)))
sMaybeDT = polystatic (Dict :: Dict (Typeable (Maybe a)))

class (Binary a, Typeable a) => Serializable a where
  binDict :: Static (Dict (Binary a))
  typDict :: Static (Dict (Typeable a))

instance Serializable b => Serializable (Maybe b) where
  binDict = sMaybeDB typDict `staticApp` binDict
  typDict = sMaybeDT typDict
```


Note that to serialise a polymorphic pointer, we send its name and a description of the (static) type at which the polymorphism is "applied", which may itself be a deeply nested combination of static applications and more polymorphism.
This is why it may be surprisingly large!
Thus it is more efficient to declare commonly used static values monomorphically if possible (see the `Control.DistributedClosure` notes for an example).

### Possible alternate approaches to polymorphism


Other approaches we may take to `Serializable` and `closurePure`:

#### The `rank1dynamic` approach


We actually support more polymorphism, at the expense of lying to GHC!
The approach of `rank1dynamic` is basically casting everything to `Any` (or similar), and implementing our own typechecker.
This needs `unsafeCoerce`, whereas our approach doesn't, but has the advantage of not requiring `Static`-`Typeable`-ness, but requires a different, rank-1 `Typeable`.
It supports "proper" polymorphism: in `rank1dynamic`, you can create a dynamic reverse function which then can be extracted at any type, but with our approach the type is fixed on creation.
This also doesn't have the one-type-variable constraint.

#### Don't support nice `Serializable` instances


This is the "abandon ship" approach.
We don't support any polymorphism or any `instance Serializable b => Serializable (Maybe b)` instances.
This forces the user to write `Serializable` instances for each type they are interested in, not just each type former.
One benefit though, is the instances will be trivial, as there is no polymorphism going on, and the `typDict` member can be removed.


```
instance Serializable (Maybe [Bool]) where
  binDict = static (Dict :: Dict (Binary (Maybe [Bool])))
```


This will just add a row in the SPT for each static dictionary.

#### The GHC magic approach


Like the "abandon ship" approach, we don't support polymorphism, but we do support nice `Serializable` instances.
Again we add a row in the SPT for each dictionary at each type we are interested in, but this time, GHC gains magic to write the trivial instances automatically.
(Warning, this is the most speculative approach!)
We slightly generalise our goal, from `Binary` to arbitrary classes.



We define a new class (note `SC :: Constraint -> Constraint`), where `SC` stands for "Static Constraint"


```
class c => SC c where
  dict :: Static (Dict s)
```


These act as normal, except when GHC comes to solve one where `c` has no free type variables, it solves `c`, and takes a dictionary `d :: Dict c` of that and essentially splices in `static d`.
Thus the effect is to generate, on the fly,


```
instance SC (Binary (Maybe [Bool])) where
  dict = static (Dict :: Dict (Binary (Maybe [Bool])))
```


Thus we can write


```
data Closure a where
  ...
  ClosurePure :: SC (Binary a) => a -> Closure a
  ...

f :: Serializable [a] => a -> Closure [a]
f x = closurePure [x,x]

g :: Serializable [Maybe b] => Closure [Maybe b]
g y = f $ Just y
```


So far, this is all non-magical, but when the user finally says `g True`, we need to find a `Serializable [Maybe Bool]` instance.
But this is exactly a static `Binary [Maybe Bool]` instance, and a `Binary [Maybe Bool]` is trivially solveable, as (morally) a top level value with no free variables (or type variables).
So GHC can just stuff that into the SPT and be done!
Thus, the following works (without having to think about manually instanciating `Serializable`)

```wiki
g True :: Closure [Maybe Bool]
```


One problem with magic "StaticConstraint" class is that you must declare the *types* you are interested in at compile time.
This is as opposed to the *type formers* that our polymorhism proposal requires.
i.e. you can only accept `Int`, `[Int]` and `[[Int]]`, rather than the "closure under type application" of `Int` and `[] :: * -> *` ??? i.e. `Int`, `[Int]`, `[[Int]]`, `[[[Int]]]`, ad infinitum.


One benefit with all these alternatives is that they would all avoid the issue of non-obvious large serialisations (as they never build a large "polymorphic/static application" tree to describe a static dictionary.

### A reason to support polymorphism


We may want a function `purify :: SDynamic Closure -> SDynamic Closure` which converts (some) arbitrary closures into `closurePure` format.
(Imagine asking a powerfull remote node to factorise large numbers for you, or some other hard problem).
By "some", we mean some types, i.e. for some `a`s we make `Closure a`s pure, and leave other `Closure b`s alone.
If the set of interesting `a`s is finite, (just `Int` and `Integer`) in the example above, then we are easilly able to do this under all the above proposals.
However, if we want all combinations of, say, `Int`, `Bool`, `[]` and `Maybe`, then we cannot do it under the non-polymorphic approaches, but we can under our proposal.



We must write something akin to


```
purify :: SDynamic Closure -> SDynamic Closure
purify (SDynamic (tr :: TypeRep a) (cval :: Closure a))
  = let val :: a = unclosure cval
    in SDynamic tr (closurePure val)  -- NOTE: this fails to typecheck, as we need `Serializable a`
```


Exersise: implement this!
(Hint: consider `getSerializeable :: TypeRep a -> Maybe (Dict (Serializable a))` which recurses on the `TypeRep`, doing dynamic checks to see if it is `Int` or `Bool` or `[_]` or `Maybe _` at each stage.)

### Trusted Code Base


Just the RTS and GHC support for building the static pointer table.
(The "`rank1dynamic`" alternative approach obviously has a much larger TCB, and the "magic GHC" approach has the magic in the TCB, of course!).

### Questions

- Should `StaticPtr` be merged with `Static`?

  - For: Simplicity, smaller API.
  - Against: It is nice to syntactally know that a `StaticPtr` will have a small serialisation (as it is just a key into the SPT).
    Recall a `Static` may be a big thing, built from lots of `staticApp`s and polymorphism.

- Naming of `deRef*`: for StaticPtr, this was chosen for consistency with current GHC.
  Consistency between `Static` and `StaticPtr` is nice, but `deRefStatic` makes the operation sound trivial (follow a pointer?), but we may need to do arbitrary computations to evaluate applications.

  - Option 1: leave as is
  - Option 2: rename `deRefStatic` to `unStatic` (then consistent with Closures)
  - Option 3: rename both `deRef*` to `un*`

- `Dict` should probably live somewhere else. Where?

- Recall our comment above that `closurePure x` is serialised as a static Binary dictionary and `encode x :: Bytestring`.
  This is slightly different to what we are doing at the moment, which is encoding it as a `closureS (polystatic decode' `staticApp` binDict) `closureApp` (encode x)`, where `decode' :: Dict (Binary a) -> ByteString -> a`.
  I am starting to favour serialising the dictionary, does anyone else have a preference?

- The static "polymorphism" support is a bit kludgy - any comments on this would be most helpful!

---

## Control.DistributedClosure

`Closure`s are based on the fact that both `Static`s and `ByteString`s are easilly serialisable.
The `Serialisable` class and `closurePure` use this by noting that if we have a 'static' decoding function `sf` for `a`, then we can serialise `sf` and `encode a :: ByteString`, and this is a serialisation of `a` itself.
(Note that the sole use of `Serialisable` is to implement `closurePure`.


Note however, that we require a fully-fledged 'static' `Typeable` and `Binary` dictionary, this enables us to be able to write instances like `instance Serializable b => Serializable (Maybe b)`.
These instances are where we require our polymorphism support in `Data.StaticPtr`


```
data Closure (a :: *) -- abstract
unclosure :: Closure a -> a

closureSP :: StaticPtr a -> Closure a
closureS :: Static a -> Closure a
closureEnc :: ByteString -> Closure ByteString
closureApp :: Closure (a -> b) -> Closure a -> Closure b

-- | A class for those types for which we have /static/ evidence of their 'Binary' and 'Typeable'
-- nature, and so can serialise them (via 'closurePure')
class (Binary a, Typeable a) => Serializable a where
  binDict :: Static (Dict (Binary a))
  typDict :: Static (Dict (Typeable a))

closurePure :: Serializable a => a -> Closure a

putSDynClosure :: SDynamic Closure -> Put
getSDynClosure :: Get (SDynamic Closure)
instance Binary (SDynamic Closure) where

putClosure :: Closure a -> Put
getClosure :: TypeRep a -> Get (Closure a)
instance Typeable a => Binary (Closure a) where
```

### Notes


Being based on our polymorphism support, `closurePure` can be inefficient, in the sense that it will expand in to a lot of data to send across the network.
This is because, it serialises a static decoder for the required type, as well as the value.
This static decoder is built automatically by the `Serializable` class, but this machinery heavily relies upon the polymorphism support of static pointers.
For example, at the time of writing, `closurePure $ Just [Just False,Nothing]` serialises as 162 bytes, of which only 12 are the data.
(These numbers may fluctuate slightly with implementation changes, but the general problem remains.)
However, it is possible to avoid using `closurePure` is some situations, for example, instead of
`f x = (closureS $ static not) `closureApp` (closurePure x)`
one could write
`f x = (closureS $ static (not . (decode :: ByteString -> Bool))) `closureApp` (closureEnc $ encode x)`,
which "bakes in" the correct decoder to a static pointer.
