# Implementation of `withTypeable`


This page collects ideas about creating local instances, centering on the example of

```wiki
withTypeable :: TTypeRep a -> (Typeable a => r) -> r
```


Currently, `withTypeable` cannot be written in Haskell, but it is straightforward to write in Core. Can we fix this?


A facility like this is needed by Edward Kmett: see [2439\#comment:25](https://gitlab.haskell.org/ghc/ghc/issues/2439)

## Fully general approach



One sledgehammer of a solution is to have some way of creating any local instance, anywhere. We can imagine some primitive/keyword -- call it `withdict` -- that takes all the members of a class and magically creates an instance. So, if we have


```
class Super a => C a where
  meth1 :: a -> a
  meth2 :: [a] -> Bool
```


then `withdict` can be like

```wiki
withdict :: Super a => (a -> a) -> ([a] -> Bool) -> (C a => r) -> r
```


This violates instance coherency, of course, so perhaps the facility is available only with `-XIncoherentInstances`. But, as stated, this is a sledgehammer we may not need to swing quite yet.

## One-method approach


If we don't want the sledgehammer, we could restrict ourselves to single-element classes. Currently, GHC implements single-element classes by making them, essentially, a `newtype`. Thus

```wiki
class D a where
  meth :: a
```


is like

```wiki
newtype D a = MkD { meth :: a }
```


except that `D` has kind `* -> Constraint` instead of the expected `* -> *`.


The proposal here is to lock in this approach as part of the definition of the GHC/Haskell language. Unfortunately, continuing our example with `D`, we can't have `Coercible a (D a)`, because the kinds don't line up -- we have `a :: *` yet `D a :: Constraint`. But, we *can* do this: `Coercible (a -> r) (D a => r)`. And because the `Coercible` solver doesn't much care about the `*` vs. `Constraint` distinction, it is up to the task.

### But that's impredicative!


The constraint `Coercible (a -> r) (D a => r)` is indeed impredicative, and we can't reasonably expect GHC to infer the type `coerce :: (a -> r) -> (D a => r)`. But, with explicit type application (#5296), this is straightforward. Indeed, in the proposed implementation ([Phab:D1138](https://phabricator.haskell.org/D1138)), we can (almost) say `coerce :: (D a => r) -> (a -> r)`. The only thing holding us back is that the `Coercible` mechanism requires newtypes' constructors to be in scope in order to unwrap them. Dictionary constructors are never in scope, so this check fails.


But, if we simply say that `-XIncoherentInstances` allows the `Coercible` solver to skip this check for class dictionaries, then we're home free. Problem solved.

## Incoherence


Any approach to local instances is going to butt up against coherence. Except that incoherence is impossible with **singleton** types, like the `Typeable` class we started with. So we might consider relaxing the `-XIncoherentInstances` requirement if the types involved are singletons.


The problem here is that I can't imagine a good way for GHC to detect singletons. We could put in some silly syntactic checks, but these will always be wrong, I think. [Scherer and R??my (2015)](http://gallium.inria.fr/~scherer/research/unique_inhabitants/unique_stlc_sums-long.pdf) suggest that this is not an easy problem.

## Alternatives


Simon and Iavor have suggested a class-based approach here. Richard thinks that using classes is unnecessary, especially because GHC can't verify that a certain type is a singleton. Here are the proposals:

### Simon


Here is one idea, which amounts to abstracting the pattern of a class and singleton type:

```wiki
class SingClass (c :: Constraint) where
  type SingType c :: *
  singVal :: c => SingType c
  singCls :: SingType c -> (c => b) -> b
```


Then you can say

```wiki
deriving instance SingClass (KnownNat a)
```


and GHC generates the Right Thing, something like

```wiki
instance SingClass (KnownNat a) where
  type SingType (KnownNat a) = SNat a
  singVal = natSing  -- natSing is the method of KnownNat
  singCls = ???can only be written in Core..
```


This `deriving` would fail if the class didn???t have the required form, namely

```wiki
class c a where
  op :: <some type>
```

### Iavor


I think that it is probably sufficient to have just a single special class called `Singleton`, and two functions:

```wiki
theOne :: Singleton s => s
withSingleton :: s -> (Singleton s => a) -> a
```


I think that this should be a derived class, and when we derive an instance for a specific type, we could have an analysis to determine if this type is a singleton.  My first thought is that the analysis should work something like this:

1. Non-empty:  find one constructor, whose result type unifies with the target type (let's call it `C`); if we can't find such a constructor, then reject the deriving.
1. One constructor: try to find another constructor whose result type unifies with the target type;  if we can find such a constructor, then reject the deriving.
1. Consistent constructor: derive `Singleton` for all the fields of `C`; while doing this, you may assume that the current type is a member of `Singleton`.
1. The definition of the one element is `C` applied to `theOne` for each field.


I am not sure if this is quite right as, for example, it would accept types like this `data T = C T` but perhaps that's OK?
