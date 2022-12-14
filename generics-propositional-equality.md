
This page sketches the ideas how to equip `GHC.Generics` with type-level reasoning facilities. This page describes *work in progress*. Initially responsible for this page is Gabor Greif.

# Status Nov 2014


At the HacBerin 2014 GGR and JPM discussed the issue and we settled it in a way so that no changes to GHC are necessary. The trick is to obtain type equality witness from the Rep T and push that down to its innards. This way we need no propeq for constructors etc. Also Pedro is working towards a clean solution utilizing DataKinds.

# Motivation


the [gdiff library](https://hackage.haskell.org/package/gdiff) requires a family GADT for describing the constructors of datatypes that one wants to `diff` and `patch`. This family has to cover (identify) all appearing data type constructors in the *value tree* transitively. Given two such constructor identifiers `gdiff` appeals to propositional equality (called `decEq` in the library) to get hold of the constructor's data in a type-safe manner.

[GHC.Generics](http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.7.0.0/GHC-Generics.html) provides a `Rep t p` (representation of the data structure at the type level) for every data type when the user demands `deriving Generic`. Metadata is attached to parts of this representation which can be queried for names, modules, fixity, etc. at runtime.


Marriage of `GHC.Generics` with `gdiff` appears straightforward weren't there one detail: the metadata is not available at the type level, so `gdiff`'s requirement for propositional equality cannot be satisfied.


To support this propositional equality in `GHC.Generics`, we have to equip the {datatype, constructor and selector} metatypes with type-level information so that we can use `Data.Type.Equality`-provided functions (e.g. `sameNat`, `sameSymbol`) on them.

# Metadata in `GHC.Generics`

- for data types: `D1 meta f p`
- for data constructors: `C1 meta f p` (potentially more than one per data type)
- for selectors: `S1 meta f p` (potentially more than one per constructor)


Here `meta` is a private (phantom) type constructor which parametrises the `Datatype`, `Constructor` and `Selector` instances. The corresponding class constraint's methods give (runtime access) to the metainformation.


An example session with GHCi is provided below to illustrate the current system:

```wiki
Prelude> :m +GHC.Generics 
Prelude GHC.Generics> :kind! Rep Bool ()
Rep Bool () :: *
= M1
    D
    GHC.Generics.D1Bool
    (M1 C GHC.Generics.C1_0Bool U1 :+: M1 C GHC.Generics.C1_1Bool U1)
    ()
```

# Implementation Idea


In order to [TypeLevelReasoning](type-level-reasoning) to work data types must be indexed by some type-level decoration (e.g. `GHC.TypeLits`' `Nat` and `Symbol` kinds).


The idea is to change typecheck/TcGenGenerics to create
`meta = Constr "Mod" "Bool" "True"`
instead of `meta = GHC.Generics.C1_1Bool`


This idea is (partly) implemented on branch `wip/generics-propeq`. Here is a GHCi dialogue:

```wiki
Prelude GHC.Generics> :kind! Rep Bool ()
Rep Bool () :: *
= M1
    D
    (Dat "GHC.Generics" "Bool")
    (M1 C (Constr (Dat "GHC.Generics" "Bool") "False") U1
     :+: M1 C (Constr (Dat "GHC.Generics" "Bool") "True") U1)
    ()
```


Given this metainformation reflected at the type level, propositional equality
can be implemented by resorting to `KnownSymbol` and `sameSymbol` from `GHC.TypeLits`.


There is a small problem, though: We get *orphan instance* warnings because `Dat` and `Constr` do not carry the original datatype as an index.

## An Aside: why `Datatype`?



Consider this current constraint on data types:


```
class Datatype d where
  datatypeName :: t d f a -> [Char]
  moduleName :: t d f a -> [Char]
  ...
```


Given the fact that for `d = (Dat "GHC.Generics" "Bool")` both pieces of information can be reified from the type-level, why do we need this constraint at all?

# The Conservative Approach



After observing that `Datatype` is essentially just `KnownSymbol ?? KnownSymbol` we can ask the question:


>
>
> Can we distill a type-level equality witness from `Datatype` constraints?
>
>


Unsurprisingly the answer is "yes". `GHC.Generics` could provide a function

```wiki
sameDatatype :: (Datatype l, Datatype r) => Proxy l -> Proxy r -> Maybe (l :~: r)
```


and implement it in the same unsafe fashion as `GHC.TypeLits` does for `sameSymbol`.


This should be sufficient to satisfy `gdiff`'s requirements on propositional equality.


The only con that I see with this approach is that `module GHC.Generics` gets additional
dependencies on `import Data.Proxy`, `import Unsafe.Coerce` and `import Data.Type.Equality`.



For `Constructor` and `Selector` we would need to add further base class constraints:


```
class Datatype c => Constructor c where
  ...

class Constructor s => Selector s where
  ...
```


GHC would need to also instantiate these base constraints for constructors and selectors.

**Update**: I have implemented this in part, and it works. Unfortunately it can be used to subvert the type system, by comparing two different constructors with `sameDatatype` and obtain type equality for the constructors *as long as they are from the same ADT*. I am working on an improved scheme to fix this.
