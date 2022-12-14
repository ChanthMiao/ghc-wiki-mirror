# Safer and more expressive type representations


This page is the main root page for a re-design of the `Typeable` class, to make **type-indexed** type representations.  


The main ticket for tracking progress is #11011. There is currently an active implementation effort which is being documented at [Typeable/BenGamari](typeable/ben-gamari)


Relevant links

- [A reflection on types](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/), the paper that describes the overall vision.  This is essential reading.
- [Ben Prices's Git respository](https://github.com/brprice/typeableT), built summer 2015, of a prototype version of the ideas.
- [Typeable/BenGamari](typeable/ben-gamari) Ben Gamari's notes on his on-going implementation effort
- [TypeableT](typeable-t) really a more concrete variant of this page
- [Typeable/WithTypeable](typeable/with-typeable) about `withTypeable`.
- [GhcKinds/PolyTypeable](ghc-kinds/poly-typeable) another rather duplicated page.


Also relevant is the [root page for distributed Haskell](distributed-haskell), which was a major driving force for the design described here.


The names of functions and type constructors is totally up for grabs.

# Status

See the ~Typeable ticket for tickets.


## Goal



Consider `Dynamic`:


```haskell
data Dynamic where
  Dyn :: TypeRep -> a -> Dynamic
```


We'd like to write `dynApply`:


```haskell
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
```


which succeeds only when the application is type correct. But how?  Let's try


```haskell
dynApply (Dyn tf f) (Dyn tx x) = ???
```


We need some way to decompose `tf` (the type representation for `f`), ask if it is an arrow type, and if so extract the type representation for the argument and result type.  Then we need to compare the argument type with `xt`'s representation. If so, we are good to go. 



But at the moment we don't have any *type-safe* way to decompose `TypeRep`.  Indeed `dynApply` is defined like this right now:


```haskell
dynApply (Dyn tf f) (Dyn tx x)
  = case funResultTy tf tx of
       Just tr -> Just (Dyn tr (unsafeCoerce f x))
```


where `funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep` does the decomposition of `tf`, checking that it is of form `tx -> tr`. 


The `unsafeCoerce` makes `dynApply` part of the Trusted Code Base (TCB).  That might not be so bad, but it is a poster-child for a raft of other similar functions (e.g. in Cloud Haskell). 


So **our main goal is to be able to write `dynApply` in a type-safe way** so that it is not part of the TCB.

---

## Step 1: Type-indexed type representations



The first obvious thing is that we must make a connection between the type-rep arg of `Dyn` and the value itself. Something like this:


```haskell
data Dynamic where
  Dyn :: TTypeRep a -> a -> Dynamic
```


Here we are using a *type-indexed* type representation, `TTypeRep`.  Now the connection betweeen the two is visible to the type system.



We can re-define the `Typeable` class and `TypeRep` thus:


```haskell
class Typeable a where
  tTypeRep :: TTypeRep a     -- No need for a proxy argument!

data TypeRep where
  TypeRep :: TTypeRep a -> TypeRep

typeRep :: Typeable a => proxy a -> TypeRep
-- The current typeRep function
typeRep (p :: proxy a) = TypeRep (tTypeRep :: TTypeRep a)
```


It is helpful to have both `Typeable a` (the class) and `TTypeRep a` (the value).

- It is sometimes convenient to pass a `TTypeRep` around implicitly (via a `Typeable a =>` constraint).  
- But in tricky cases, it is also often much clearer (and less laden with proxy arguments) to pass it around as an ordinary, named value.  


We can get from `Typeable a` to `TTypeRep` by using the class method `tTypeRep`.  But what about the other way round? 
We need to add the following primitive: 


```haskell
withTypeable :: TTypeRep a -> (Typeable a => b) -> b
```


(This seems both simpler and more useful than making the Typeable class recursive through TypeRep data declaration.) See more on the [sub-page](typeable/with-typeable).



We can also compare two `TTypeReps` to give a statically-usable proof of equality:


```haskell
eqTT :: TTypeRep a -> TTypeRep b -> Maybe (a :~: b)
eqT  :: (Typeable a, Typeable b) => Maybe (a :~: b)

data a :~: b where  -- Defined in Data.Typeable.Equality
   Refl :: a :~: a
```


(`eqT` and `:~:` exist already as part of the exports of `Data.Typeable`.) 

---

## Step 2: Decomposing functions



If we want to decompose `TTypable`, at least in the function arrow case, we need a function like this:


```haskell
decomposeFun :: TTypeRep fun
             -> r
             -> (forall arg res. (fun ~ (arg->res)) 
                              => TTypeRep arg -> TTypeRep res -> r)
             -> r
-- (decomposeFun tf def k) sees if 'tf' is a function type
-- If so, it applies 'k' to the argument and result type
-- representations; if not, it returns 'def'
```


This function is part of `Typeable`, and replaces `funResultTy`.



Now we can write `dynApply`, in a completely type-safe way, outside the TCB:


```haskell
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn tf f) (Dyn tx x)
  = decomposeFun tf Nothing $ \ ta tr ->
    case eqTT ta tx of
      Nothing   -> Nothing
      Just Refl -> Just (Dyn tr (f x))
```


**Pattern synonyms**.  An alternative, rather nicer interface for `decomoposeFun` would use a [pattern synonym](pattern-synonyms) instead of continuation-passing style.  Here is the signature for the pattern synonym:


```haskell
pattern TRFun :: fun ~ (arg -> res)
              => TTypeRep arg 
              -> TTypeRep res 
              -> TTypeRep fun
```


which looks (by design) very like the signature for a GADT data constructor.  Now we can use `TRFun` in a pattern, thus:


```haskell
dynApply :: Dynamic -> Dynamic -> Maybe Dynamic
dynApply (Dyn (TRFun ta tr) f) (Dyn tx x)
  = case eqTT ta tx of
      Nothing   -> Nothing
      Just Refl -> Just (Dyn tr (f x))
dynApply _ _ = Nothing
```


Is that not beautiful?  The second equation for `dynApply` is needed in case the `TRFun` pattern does not match.

---

## Step 3: Extracting kind equalities


Suppose we have a `TTypeRep (a :: k)`. It might be nice to get a `TTypeRep (k :: *)`
out of it. (Here, we assume `* :: *`, but the idea actually works without that assumption.) An example is an attempt to typecheck the *expression*


```haskell
typeOf :: Typeable (a :: k) => Proxy a -> TypeRep
```


where `typeOf :: Typeable a => a -> TypeRep` is a long-standing part of the `Typeable` API. To typecheck that expression, we need a `Typeable (Proxy a)` dictionary. Of course, to build such a thing, we need `Typeable k`, which we have no way of getting.



So, we now include


```haskell
kindRep :: TTypeRep (a :: k) -> TTypeRep k
```


in the API. I (Richard) conjecture that this is all possible without kind equalities, but would be rather awkward due to GHC's insistence that kind parameters be implicit. See #10343, which inspired this bit.

---

## Step 4: Decomposing arbitrary types


It is all very well being able to decompose functions, but what about decomposing other types, like `Maybe Int`?  



To do this it is natural to regard types as built from type constructors and binary application, like this:


```haskell
data TTypeRep (a :: k) :: * where
    TRApp :: TTypeRep a -> TTypeRep b -> TTypeRep (a b)
    TRCon :: TTyCon a -> TTypeRep a

-- TTyCon must be polykinded, so that we can have
--    (TTyCon Maybe), (TTyCon Either), (TTyCon Int) etc
-- TTyCon :: forall k. k -> *
```


(We could, and ultimately should, use pattern synonyms again, but it's more concrete to use a GADT for now.  Perhaps surprisingly, it is actually fine to expose `TTypeRep` concretely, with its constructors; we can't construct ill-formed `TTypeRep`s.)



While this GADT is expressible in GHC now (note the existential kind in TRApp), **it is not very useful without kind equalities**.   (GHC does not currently support kind equalities, but Richard Eisenberg is working that very question.)  Why?  Here is the type of `TRApp` in its full glory, with normally-invisible kind args in angle brackets:


```haskell
TRApp :: forall k1 k2. forall (a :: k1 -> k2) (b :: k1).
         TTypeRep <k1->k2> a -> TTypeRep <k1> b -> TTypeRep <k2> (a b)
```


Or, to be really explicit about the existentials:


```haskell
TRApp :: forall k2 (c:k2).                      -- Universal
         forall k1 (a :: k1 -> k2) (b :: k1).   -- Existential
         (c ~ a b)
      => TTypeRep <k1->k2> a 
      -> TTypeRep <k2>     b 
      -> TTypeRep <k2>     c
```


Now suppose we want to implement `decomposeFun`.  We should be able to do this outside the TCB, i.e. without `unsafeCoerce`:


```haskell
arrowCon :: TTyCon (->)   -- The type rep for (->)

decomposeFun :: TTypeRep fun
             -> r
             -> (forall arg res. (fun ~ (arg->res)) 
                              => TypeRep arg -> TTypeRep res -> r)
             -> r
decomposeFun tr def kont
  = case tr of
      TRApp (TRApp (TRCon c) r1) r2 -> case eqTyCon arrowCon c of 
          Just HRefl -> kont r1 r2
          Nothing    -> def
      _ -> default 
```


But look at the arguments of `eqTyCon`:


```haskell
   arrowCon :: TTyCon <*->*->*>   (->)
   c        :: TTyCon <k1->k2->*> tc
```


where `k1` and `k2` are existential kinds bound by the two nested `TRApp` constructors, and `tc` the existential bound by the inner `TRApp`.  But `kont` is expecting `arg` and `res` to have kind `*`!  So we need proofs that `k1 ~ *` and `k2 ~ *`.



The real work is done by `eqTyCon`:


```haskell
eqTyCon :: forall (k1 k2 :: *). 
           forall (a :: k1) (b :: k2). 
           TTyCon <k1> a -> TTyCon <k2> b -> Maybe (a :~~: b)
```


where `:~~:` is a *kind-heterogeneous* version of `:~:`:


```haskell
data (a::k1) :~~: (b::k2) where
  HRefl :: forall (a::k).  a :~~: a
```


Or, to write the type of `HRefl` with its constraints explicit:


```haskell
HRefl :: forall k1 k2. forall (a::k1) (b::k2).
         (k1 ~ k2, a ~ b) 
      => a :~~: b
```


That is, `HRefl` encapsulates a proof of kind equality as well as one of type equality.  


So Step 3 (allowing `TypeRep` to be fully decomposed in a type safe way) absolutely requires kind equalities.

---

## Other points

### Fast comparison of `TypeRep`


The current implementation of `TypeRep` allows constant-time comparison based on fingerprints.  To support this in the new scheme we would want to add a fingerprint to every `TypeRep` node. But we would not want clients to see those fingerprints, lest they forge them.


Conclusion: make `TypeRep` abstract.  But then how can we pattern-match on it?  Pattern synonyms seem to be exactly what we need.

### Trusted computing base



With all of this, what is left in the TCB?  The `TTyCon` type is a new abstract type, and the comparison (based on fingerprints) must be in the TCB.


```haskell
TTyCon a  --- abstract type
eqTyCon :: forall k1 k2. forall (a :: k1) (b :: k2). 
           TTyCon a -> TTyCon b -> Maybe (a :~~: b)
tyConKind :: TTyCon (a :: k) -> TTypeRep (k :: *)
```


Note that `TTyCon` represents type constructors already applied to any kind arguments. Even with kind equalities, we have no way of creating a representation for `Proxy :: forall k. k -> *`, as that would require an impredicative kind for the implicit kind argument to `TTyCon`.



`withTypeable` could be written in core (and perhaps could be generalized to other constraints) but not in source Haskell:


```haskell
withTypeable :: TypeRep a -> (Typeable a => b) -> b
```


See more [here](typeable/with-typeable).


Implementing the "fast comparison" idea above will require putting more code in the TCB because of interaction with fingerprints. But the functions above are morally the only ones that need be trusted.

### What about structure information?


Related but different issue: [https://ghc.haskell.org/trac/ghc/ticket/7897](https://ghc.haskell.org/trac/ghc/ticket/7897)

---

## Step by step


We can do steps 1 and 2 (and probably 3) without kind equalities, although the 
implementation of `decomposeFun` will use `unsafeCoerce` and will be part of the TCB. However, this will all likely happen after merging with kind equalities.

## Proposed API



This version assumes homoegeneous equality (as GHC is today, July 2, 2015). Below is the version with heterogeneous equality.


```haskell
data TTypeRep (a :: k)        -- abstract
data TTyCon (a :: k)          -- abstract

-- Costructors
trCon :: forall k (a :: k). TTyCon a -> TTypeRep a  -- in TCB
trApp :: forall k k1 (a :: k1 -> k) (b :: k1). 
         TTypeRep a -> TTypeRep b -> TTypeRep (a b)  -- in TCB

-- Destructors
pattern type TRCon :: forall k (a :: k). TyCon a -> TTypeRep a
pattern type TRApp :: forall k. exists k1 (a :: k1 -> k) (b :: k1). 
                      TTypeRep a -> TTypeRep b -> TTypeRep (a b)
pattern type TRFun :: TTypeRep arg 
                   -> TTypeRep res 
                   -> TypeRep (TTypeRep (arg->res))


-- For now, homogeneous equalities
eqTyCon :: forall k (a :: k) (b :: k). 
           TTyCon a -> TTyCon b -> Maybe (a :~: b)
eqTT    :: forall k (a :: k) (b :: k). 
           TTypeRep a ->T TypeRep b -> Maybe (a :~: b)
```


This assumes heterogeneous equality (that is, with kind equalities).


```haskell
data TTypeRep (a :: k)        -- abstract
data TTyCon (a :: k)          -- abstract

tyConKind :: forall k (a :: k). TTyCon a -> TTypeRep k

-- Costructors
trCon :: forall k (a :: k). TTyCon a -> TTypeRep a
trApp :: forall k k1 (a :: k1 -> k) (b :: k1). 
         TTypeRep a -> TTypeRep b -> TTypeRep (a b)

-- Destructors
pattern type TRCon :: forall k (a :: k). TyCon a -> TTypeRep a
pattern type TRApp :: forall k. exists k1 (a :: k1 -> k) (b :: k1). 
                      TTypeRep a -> TTypeRep b -> TTypeRep (a b)

-- this definition to go into Data.Type.Equality
data (a :: k1) :~~: (b :: k2) where
   HRefl :: a :~~: a

eqTyCon :: forall k1 k2 (a :: k1) (b :: k2). 
           TTyCon a -> TTyCon b -> Maybe (a :~~: b)

-- the following definitions can be defined on top of the interface
-- above, but would be included for convenience
pattern type TRFun :: TTypeRep arg -> TTypeRep res
                   -> TTypeRep (arg -> res)

eqTT    :: forall k1 k2 (a :: k1) (b :: k2). 
           TTypeRep a -> TTypeRep b -> Maybe (a :~~: b)

typeRepKind :: forall k (a :: k). TTypeRep a -> TTypeRep k

class forall k (a :: k). Typeable k => Typeable a where
  tTypeRep :: TTypeRep a
```


**SLPJ**: Do we need both `:~:` and `:~~:`?



**RAE**: I don't think it's strictly necessary, but keeping the distinction might be useful. `:~:` will unify the kinds of its arguments during type inference, and that might be what the programmer wants. I don't feel strongly here.


Some of this design is motivated by the desire to allow flexibility in the implementation to allow for fingerprinting for fast equality comparisons. Naturally, the fingerprints have to be in the TCB. If it weren't for them, though, the `TypeRep` type could be exported concretely.


Could we simplify this a bit by removing `TyCon`? **RAE**: No.


The class declaration for `Typeable` is highly suspect, as it is manifestly cyclic. However, (forgetting about implementation) this doesn't cause a problem here, because all kinds have kind `*`. Thus, we know the cycle terminates. Implementation is an open question, but I (RAE) surmise that this barrier is surmountable.

**RAE:** How can we get a `TTyCon` for a known-at-compile-time tycon? I want something like `tyConRep @(->)` that will give something of type `TTyCon (->)`. The problem is that the type argument to `tyConRep` might not be a bare type constructor... but I would hate to have such a function return a `Maybe`, as it would be very annoying in practice, and it could be checked at compile time. I almost wonder if a new highly-magical language primitive would be helpful here.

**Iceland_jack**: I recall a conversation about making `:~:` a pattern synonym for `:~~:`

## Kind-polymorphism

### `unsafeCoerce` can be written


As painfully demonstrated (painful in the conclusion, not the demonstration!) in [comment:16:ticket:9858](https://gitlab.haskell.org/ghc/ghc/issues/9858), `Typeable` can now (7.10.1 RC1) be abused to write `unsafeCoerce`. The problem is that today's `TypeRep`s ignore kind parameters.



Drawing this out somewhat: we allow only `Typeable` instances for unapplied constructors. That is, we have


```haskell
deriving instance Typeable Maybe
```


never


```haskell
deriving instance Typeable (Maybe Int)
```


However, what if we have


```haskell
data PK (a :: k)
```


? `PK` properly has *two* parameters: the kind `k` and the type `a :: k`. However, whenever we write `PK` in source code, the `k` parameter is implicitly provided. Thus,
when we write


```haskell
deriving instance Typeable PK
```


we actually supply a kind parameter `k0`. GHC tries to find a value for `k0`, fails, and leaves it as a skolem variable. We get an instance `forall k. Typeable (PK k)`. But, of course, `PK *` and `PK (* -> *)` should have *different* `TypeRep`s. Disaster!


### A stop-gap solution

**NB:** This was not implemented. The "medium-term solution" below was, as of 7.10.1.

[comment:16:ticket:9858](https://gitlab.haskell.org/ghc/ghc/issues/9858) is a demonstration that the boat is taking on water! Fix the leak, fast!


The "obvious" answer -- don't supply the `k` here -- doesn't work. The instance for `PK` would become `Typeable <forall k. k -> *> PK`, where a normally-implicit kind parameter is supplied in angle brackets. This is an **impredicative kind**, certainly beyond GHC's type system's abilities at the moment, especially in a type-class argument.



One (somewhat unpleasant) way forward is to allow kind parameters to be supplied in `Typeable` instances, but still restrict type parameters. So, we would have


```haskell
deriving instance Typeable (PK :: * -> *)
deriving instance Typeable (PK :: (* -> *) -> *)
```


which would yield instances `Typeable <* -> *> (PK <*>)` and `Typeable <(* -> *) -> *> (PK <* -> *>)` -- two separate instances with different, and distinguishable `TypeRep`s. What's painful here is that, clearly, there are an *infinite* number of such instantiations of `PK`. We could maybe provide a handful of useful ones, but we could never provide them all.


That, then, is the current plan of attack. (**No, it's not.**) `Typeable` instance heads must include concrete instantiations of all kind parameters but no type parameters. `base` will provide several instantiations for poly-kinded datatypes, but users may have to write orphan instances to get others. `AutoDeriveTypeable`  will ignore poly-kinded datatype definitions -- users must write explicit instances if they want to.

**Question:** With this design, orphan instances will be unavoidable. Given that two different authors may introduce the same orphan instances, would it work to mark every `Typeable` instance as `{-# INCOHERENT #-}`? For this to work out, we would need a guarantee that the fingerprints of the instances are the same regardless of originating module.

### Medium-term solution

**This is implemented in 7.10.**


Although it is impossible to create all necessary `Typeable` instances for poly-kinded constructors at the definition site (there's an infinite number), it *is* possible to create `Typeable` "instances" on demand at use sites. The idea (originally proposed in [comment:20:ticket:9858](https://gitlab.haskell.org/ghc/ghc/issues/9858)) is to add some custom logic to the solver to invent `Typeable` evidence on demand. Then, whenever the solver needs to satisfy a `Typeable` constraint, it will just recur down to the type's leaves and invent evidence there.


For poly-kinded type constructors, we still need to worry about kind parameters. This is easy, actually: we just come up with some mechanism with which to incoporate kind parameters into a `TypeRep`s fingerprint. One such mechanism would be to use the current fingerprint-generation algorithm (used on types) and just apply it to the kinds. Of course, kinds are different from types, but there's no reason we can't use the same algorithm. Problem solved.


Some drawbacks:

- There is no way to inspect a `TypeRep`'s kind or its kind parameters. But that's OK for now.
- Under this proposal, *everything* is `Typeable`. But maybe that's not so bad.
- It seems much more efficient to generate type constructor fingerprints at definition sites. Doing so would add to the code size of every type-declaring module. Worse, we would have to add data constructor fingerprints, too, considering the possibility of promotion.
- More complexity in the solver.

### Long-term solution



We actually don't have a good long-term solution available. We thought that `* :: *` and kind equalities would fix this, but they don't. To see why, let's examine the type of `trApp`:


```haskell
trApp :: forall k k1 (a :: k1 -> k) (b :: k1). 
         TTypeRep a -> TTypeRep b -> TTypeRep (a b)
```


In the `* :: *` world, it would be reasonable to have `TTypeRep`s for kinds, assuming we have `TTypeRep`s always take explicit kind parameters. So, we might imagine having a `TTypeRep` for `PK` (call it `pkRep`) and a `TTypeRep` for `Bool` (call it `boolRep`). Does `pkRep `trApp` boolRep`` type-check? Unfortunately, no. We have


```haskell
pkRep   :: TTypeRep <forall k. k -> *> PK
boolRep :: TTypeRep <*> Bool
```


but `trApp` says that `a` must have type `k1 -> k` for some `k1` and `k`. Here `PK` would be the value for `a`, but `PK`'s kind is `forall k. k -> *`, which doesn't fit `k1 -> k` at all! We would need to generalize `trApp` to


```haskell
trApp2 :: forall (k1 :: *) (k :: k1 -> *)
                 (a :: pi (b :: k1). k b) (b :: k1).
          TTypeRep <pi (b :: k1). k b> a -> TTypeRep <k1> b
       -> TTypeRep <k b> (a b)
```


Note that the kind of `a` is a ??-type, dependent on the choice of `b`, and that `x -> y` would be considered shorthand for `pi (_ :: x). y`. While Richard's branch includes support for such types, there are no plans for type-level lambda or higher-order unification, both of which would be necessary to make this definition usable. For example, calling `trApp2` on `PK` would *still* fail, because we try to match `pi (b :: k1). k b` with `forall k2. k2 -> *`. The `k2` is not the last parameter of the body of the forall, so straightforward unification fails. We must choose `k1 := *, k := \x. x -> *` to get the types to line up. Urgh.

### Why kind equalities, then?


Given the fact that Richard's branch doesn't solve this problem, what is it good for? It still works wonders in the mono-kinded case, such as for decomposing `->`. It's just that poly-kinded constructors are still a pain.
