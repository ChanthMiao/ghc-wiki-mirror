# Notes on the FC-based intermediate language


These notes describe the new intermediate language for GHC.  The
intermediate language is based on System F with algebraic datatypes
and explicit type coercions (hereafter FC) (see the [paper](http://research.microsoft.com/%7Esimonpj/papers/ext%2Df/fc-popl.pdf)).  This note mostly focuses
on the type system and also discuss how some source-level features are
represented in the intermediate language.


Most of the system is fairly standard, with the exception of
coercions.  A coercion `c`, is a type-level term, with a kind of the
form `T1 :=: T2`. (`c :: T1 :=: T2`) is a proof that a term of type `T1`
can be coerced to type `T2`.  It is used as argument of a cast
expression; if `t :: T1` then `(t `cast` c) :: T2`.  

# What's New?


If you are already familiar with the intermediate language and its type system as it existed in 6.4.2 (and probably before) then
the key aspects that have changed are (and are further described in several sections below):

- Coercions have been added - The syntax of types now includes coercions which are evidence for type equalities.  There are distinguished coercion variables and a new variant of `TyCon`, with constructor `CoercionTyCon`.  There is also a new `Expr` variant, with constructor `Cast`, which performs a cast given an expression and evidence of the safety of the cast.

- Kinds are now Types - The type `Kind` is just a synonym for `Type`.  There are special PrimitiveTyCons that represent kinds.

- Newtypes are implemented with coercions - The previous ad-hoc mechanism has been replaced with one that uses coercions.

- GADTs are implemented with coercions - The complex typing rules for case expressions on GADTs have been removed and instead the data constructors of GADTs carry coercions inside them.  Consequently, typechecking Core is simpler (though type inference is just as hard as ever).


For anyone not familiar with the system as it existed previously, the rest of this note attempts to describe most of the type system of the intermediate language.

## Types


The representation of types is defined (as the datatype `Type`) in
TypeRep, and most of the useful functions on types are defined in
Type.  TypeRep exports the representation concretely, and should
probably not be used outside the few places it is already used.  Type
re-exports everything useful from TypeRep, but exports the
representation abstractly.  The datatype `Type` really represents a
single syntactic category that includes types, coercions, kinds, and
super-kinds.

### Type Variables


Type variables, of type `Var`, and associated construction and
manipulation functions are defined in the Var module.  There are two
data constructors that make type variables, `TyVar` and `TcTyVar`.
`TcTyVar`s can be mutable tyvars that are instantiated during type
checking.  After typechecking, all `TcTyVar`s are turned to `TyVar`s.
`TyVar`s carry a `Bool` field, `isCoercionVar`, which is `True` if the type
variable is a coercion variable and `False` otherwise.  The function
`isCoVar` should be used to test if a type variable is a coercion
variable.

### Type Constructors


Type constructors, of datatype `TyCon`, are defined in the TyCon module
and exported abstractly.  There are several different sorts of type
constructors; the most important for understanding the overall
intermediate language type system are: 
  

- `AlgTyCon`, which are for tycons for datatypes and newtypes and have a field of type `AlgTyConRhs` which specified whether it is a datatype or newtype and contains further information for each;
- `PrimTyCon`, which are for built-in primitive tycons, and are also used to represent base kinds;  
- `CoercionTyCon`, which are for special tycons which are meant to represent syntactic forms (and not really type constructors), so they must be saturated to have a kind;
- `SuperKindTyCon`, which are tycons that are used to represent super-kinds, also called sorts (which classify kinds as either coercion kinds, CO, or type kinds, TY), `SuperKindTyCon`s are unique in never having a kind.  


All `TyCon`s but `SuperKindTyCon` and `CoercionKindTyCon` carry their kind
in a field called `tyConKind`, and `CoercionKindTyCons` carry their
kinding rule (a function with type `[Type] -> Kind`) in a field called
`coKindFun`.

### Kinds are Types


We have (as of August 2006) unified types and kinds as members of the
datatype `Type`.  `Kind` is just a synonym for `Type`.  Basic kinds are now
represented using type constructors, e.g. the kind `*` is represented as

```wiki
star = TyConApp liftedTypeKindTyCon []
```


where `liftedTypeKindTyCon` is a built-in `PrimTyCon`.  The arrow type
constructor is used as the arrow kind constructor, e.g. the kind \`\* -\>
\*\` is represented internally as

```wiki
FunTy star star
```


Kinds and types may be distinguished by looking at their "Kind" using
the typeKind function.  The "Kind" of a kind is always one of the
sorts TY (for kinds that classify normal types) or CO (for kinds that
classify coercion evidence).  The coercion kind, `T1 :=: T2`, is
represented by `PredTy (EqPred T1 T2)`.


GHC has a relatively complicated kind structure...


There's a little subtyping at the kind level.  Here is the picture for
type-kinds (kinds of sort TY).

```wiki
		 ?
		/ \
	       /   \
	      ??   (#)
	     /  \
            *   #

where	*    [LiftedTypeKind]   means boxed type
	#    [UnliftedTypeKind] means unboxed type
	(#)  [UbxTupleKind]     means unboxed tuple
	??   [ArgTypeKind]      is the lub of *,#
	?    [OpenTypeKind]	means any type at all

In particular:

	error :: forall a:?. String -> a
	(->)  :: ?? -> ? -> *
	(\(x::t) -> ...)	Here t::?? (i.e. not unboxed tuple)
```

### Coercions and Coercion Kinds


Coercions are type-level terms which act as evidence for type
equalities and are classified by a new sort of kind (with the form 
`T1 :=: T2`).  Most of the coercion construction and manipulation functions
are found in the Coercion module.


The syntax of coercions extends the syntax of types (and the type
`Coercion` is just a synonym for `Type`).  By representing coercion
evidence on the type level, we can take advantage of the existing
erasure mechanism and keep non-termination out of coercion proofs
(which is necessary to keep the system sound).  The syntax of
coercions and types also overlaps a lot.  A normal type is evidence
for the reflexive coercion, i.e.,

```wiki
Int :: Int :=: Int
```


Coercion variables are
used to abstract over evidence of type equality, as in

```wiki
(/\c::(a :=: Bool). \x::a. if (x `cast` c) then 0 else 1) :: (a :=: Bool) => a -> Int
```


There are also coercion constants that are introduced by the compiler
to implement some source language features (newtypes for now,
associated types soon and probably more in the future).  Coercion
constants are represented as `TyCon`s made with the constructor
`CoercionTyCon`. 


Coercions are type level terms and can have normal type constructors applied
to them.  The action of type constructors on coercions is much like in
a logical relation.  So if `c1 :: T1 :=: T2` then

```wiki
[c1] :: [T1] :=: [T2]
```


and if `c2 :: S1 :=: S2` then

```wiki
c1 -> c2 :: (T1 -> S1 :=: T2 -> S2)
```


The sharing of syntax means that a normal type can be looked at as
either a type or as coercion evidence, so we use two different kinding
relations, one to find type-kinds (implemented in Type as \`typeKind ::
Type -\> Kind\`) and one to find coercion-kinds (implemented in Coercion as
`coercionKind :: Coercion -> Kind`).


Coercion variables are distinguished from type variables, and
non-coercion type variables (just like any normal type) can be used as
the reflexive coercion, while coercion variables have a particular
coercion kind which need not be reflexive.  

### GADTs


The internal representation of GADTs is as regular algebraic datatypes that carry coercion evidence as arguments.  A declaration like

```wiki
data T a b where
  T1 :: a -> b -> T [a] (a,b)
```


would result in a data constructor with type

```wiki
T1 :: forall a b. forall a1 b1. (a :=: [a1], b :=: (a1, b1)) => a1 -> b1 -> T a b
```


This means that (unlike in the previous intermediate language) all data constructor return types have the form `T a1 ... an` where
`a1` through `an` are the parameters of the datatype.  


However, we also generate wrappers for GADT data constructors which have the expected user-defined type, in this case

```wiki
$wT1 = /\a b. \x y. T1 [a] (a,b) a b [a] (a,b) x y
```


Where the 4th and 5th arguments given to `T1` are the reflexive coercions

```wiki
[a]   :: [a] :=: [a]
(a,b) :: (a,b) :=: (a,b)
```


 


### Representation of coercion assumptions


In most of the compiler, as in the FC paper, coercions are abstracted
using `ForAllTy cv ty` where `cv` is a coercion variable, with a kind of
the form `PredTy (EqPred T1 T2)`.  However, during type inference it is
convenient to treat such coercion qualifiers in the same way other
class membership or implicit parameter qualifiers are treated.  So
functions like `tcSplitForAllTy` and `tcSplitPhiTy` and `tcSplitSigmaTy`,
treat `ForAllTy cv ty` as if it were `FunTy (PredTy (EqPred T1 T2)) ty`
(where `PredTy (EqPred T1 T2)` is the kind of `cv`).  Also, several of the `dataCon`XXX functions treat coercion members of the data constructor
as if they were dictionary predicates (i.e. they return the `PredTy (EqPred T1 T2)` with the theta).

### Newtypes are coerced types


The implementation of newtypes has changed to include explicit type coercions in the place of the previously used ad-hoc mechanism.  
For a newtype declared by

```wiki
newtype T a = MkT (a -> a)
```


the `NewTyCon` for `T` will contain n`t_co = CoT` where `CoT t : T t :=: t -\>
t`.  This `TyCon` is a `CoercionTyCon`, so it does not have a kind on its
own; it basically has its own typing rule for the fully-applied
version.  If the newtype `T` has k type variables hen `CoT` has arity at
most k.  In the case that the right hand side is a type application
ending with the same type variables as the left hand side, we
"eta-contract" the coercion.  So if we had

```wiki
newtype S a = MkT [a]
```


then we would generate the arity 0 coercion `CoS : S :=: []`.  The
primary reason we do this is to make newtype deriving cleaner.  If the coercion
cannot be reduced in this fashion, then it has the same arity as the tycon.


In the paper we'd write

```wiki
axiom CoT : (forall t. T t) :=: (forall t. [t])
```


and then when we used `CoT` at a particular type, `s`, we'd say

```wiki
CoT @ s
```


which encodes as `(TyConApp instCoercionTyCon [TyConApp CoT [], s])`


But in GHC we instead make `CoT` into a new piece of type syntax
(like `instCoercionTyCon`, `symCoercionTyCon` etc), which must always
be saturated, but which encodes as

```wiki
TyConApp CoT [s]
```


In the vocabulary of the paper it's as if we had axiom declarations
like

```wiki
axiom CoT t :  T t :=: [t]
```


The newtype coercion is used to wrap and unwrap newtypes whenever the constructor or case is used in the Haskell source code.


Such coercions are always used when the newtype is recursive and are optional for non-recursive newtypes.  Whether or not they are used can be easily changed by altering the function mkNewTyConRhs in iface/BuildTyCl.lhs.

## Core (the intermediate language)

- Exprs

- Casts

- Typechecking

- Environments and substitution

## Simplification

- exprIsConApp_maybe

- simplExpr

### Loose Ends


Some loose ends that came up during implementation of FC:

- there is a strange unsafeCoerce that we could not figure out the purpose of in the FFI, a warning is currently emitted when it is used

- removed the -DBREAKPOINT definition in the Makefile because it induced a module loop, we should probably fix this
