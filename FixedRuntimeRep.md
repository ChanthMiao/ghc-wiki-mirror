This page outlines a plan to move the representation polymorphism checks that currently occur in the zonker and the desugarer to the typechecker.

* Tickets:
  * #17201 
  * #17113
  * #13105 
  * #17536 
  * #17907 Full test coverage
  * #18170 (MP: probably) (SLD: first step towards a proper solution)
  * #20277 Unboxed sum types
  * #20330 Primops (arguments past their arity)
* Merge request: !6164


# Table of contents

- [Motivation](#motivation)
- [Broad outline](#broad-outline)
- [Details](#details)
  * [Emitting FixedRuntimeRep constraints](#emitting-fixedruntimerep-constraints)
    + [Where specifically are we emitting these constraints?](#where-specifically-are-we-emitting-these-constraints)
  * [Solving FixedRuntimeRep constraints](#solving-fixedruntimerep-constraints)
    + [User-written instances](#user-written-instances)
    + [User-written Givens](#user-written-givens)
    + [Typed Template Haskell](#typed-template-haskell)
  * [Reporting unsolved FixedRuntimeRep constraints](#reporting-unsolved-fixedruntimerep-constraints)
    + [CtOrigins](#ctorigins)
    + [reportWanteds](#reportwanteds)
    + [Don't suggest "add FixedRuntimeRep"](#dont-suggest-add-fixedruntimerep)
    + [Don't allow FixedRuntimeRep constraints to be deferred](#dont-allow-fixedruntimerep-constraints-to-be-deferred)
  * [Evidence for FixedRuntimeRep and code generation](#evidence-for-fixedruntimerep-and-code-generation)
    + [Alternative 1: store the representation](#alternative-1-store-the-representation)
    + [Alternative 2: cast to a fixed representation using a kind coercion](#alternative-2-cast-to-a-fixed-representation-using-a-kind-coercion)
      - [Implementation plan](#implementation-plan)
        * [PHASE 1](#phase-1)
        * [PHASE 2](#phase-2)

# Motivation

There are several downsides to checking representation polymorphism in the desugarer, as evidenced by the tickets #17201 #17113 #13105 #17536. For instance, one might need to do type-family reduction in order to determine the `RuntimeRep`. So it seems more natural to do these checks in the typechecker instead, where one has access to all this information.

Similar mechanisms can be implemented in other circumstances:
  * constrain a type to be of the form `TYPE blah` or `Constraint` (TypeLike), see #15979 ([this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/15979#note_213564) in particular), #16139, #19573, ...
  * linear types: check for submultiplicity; see #18756. 

# Broad outline

We want to enforce that a `RuntimeRep` is concrete, expressed using only applications and (non-synonym) constructors. For example, the following are OK:

  - `BoxedRep Lifted`
  - `TupleRep '[]`
  - `SumRep '[ TupleRep '[], IntRep ]`

the following are not:

  - `rep` (for a skolem type variable `rep :: RuntimeRep`),
  - `BoxedRep v` (for a skolem type variable `v :: Levity`),
  - `F Bool` (for a type family `F`).

To do this, we introduce a special predicate and associated type-class (similar to the internal unlifted primitive equality `(~#)` and the user-facing boxed equality `(~)`):

```haskell
type Concrete# :: forall k. k -> TYPE (TupleRep '[])

type Concrete :: forall k. k -> Constraint
class Concrete ty
```

Whenever a situation arises in which a type `ty` must be representation-monomorphic, we emit a `Concrete# ty` Wanted constraint (see §[Emitting FixedRuntimeRep constraints](#emitting-fixedruntimerep-constraints)).

Solving `Concrete# ty` will provide us with evidence that `ty :: TYPE rep` for some concrete `rep :: RuntimeRep` (see §[Evidence for FixedRuntimeRep and code generation](#evidence-for-fixedruntimerep-and-code-generation) for what this evidence is and why it is important).

If any of these Wanted `Concrete#` constraints go unsolved, we then report a representation polymorphism error to the user (see §[Reporting unsolved FixedRuntimeRep constraints](#reporting-unsolved-fixedruntimerep-constraints)).

# Details
## Emitting Concrete# constraints

Whenever we want to check that a type `ty` is not representation-polymorphic, we emit a `Concrete# ty` constraint.    

### Where specifically are we emitting these constraints?

Under the `GHC.Tc.Gen` module hierarchy. For instance, we simply add a call to `hasFixedRuntimeRep` in the following functions:
  - `GHC.Tc.Gen.App.tcEValArg` for function applications,
  - `GHC.Tc.Gen.Bind.tcPolyBinds` for bindings,
  - `GHC.Tc.Gen.Pat.tc_pat` for patterns,
  - `GHC.Tc.Gen.Expr.tcRecordField` for record fields,
  - ...

## Solving Concrete# constraints

The constraint solver needs to be able to solve these newly emitted `Concrete#` constraints. To do this, we add global instances in `GHC.Tc.Instance.Class.matchGlobalInst`, in the same way as for existing built-in constraints such as `Coercible`, `Typeable`, etc.

### User-written instances

One subtletly is due to Backpack: we want to allow a signature to declare an abstract `RuntimeRep` that is instantiated to a fixed `RuntimeRep` later. To allow the signature to typecheck, users must write an instance (this works much the same as user-defined instance for `KnownNat`, as outlined in Note [Instances of built-in classes in signature files] in `GHC.Tc.Validity`), which is picked up in `matchConcrete`.

Note that we must allow users to declare that e.g. a list of `RuntimeRep`s is concrete, to allow more general reasoning:

```haskell
data Rep :: RuntimeRep
data Reps :: [RuntimeRep]
instance Concrete Rep
instance Concrete Reps

foo :: (x :: TYPE (SumRep ( Rep ': IntRep ': Reps ': Reps ))) -> ...
foo x = ...
```

Here we need to be able to see that the `RuntimeRep` `SumRep ( Rep ': IntRep ': Reps ': Reps )` is concrete, and we can only really do that if we allow users to define that `Reps` is concrete.

This is one important reason why we use a `Concrete :: forall k. k -> Constraint` class, as opposed to something specialised to `RuntimeRep` like `FixedRuntimeRep :: RuntimeRep -> Constraint` (which was the original design).

### User-written Givens

Users should not be allowed to specify Given `Concrete` constraints, e.g.:

```haskell
identity :: forall rep (a :: TYPE rep). Concrete rep => a -> a
identity x = x
```

If we allowed this, we would solve the `Concrete# rep` Wanted (from the binder `x` in the body of `identity`) using the provided Given, which is no good as we still can't compile this function as we don't know the representation of `x`.

### Typed Template Haskell

There are, however, circumstances in one might want to accept user-specified `Concrete` Givens constraints, such as with Typed Template Haskell, where we could allow representation-polymorphic typed Template Haskell expressions as long as they are monomorphised at the splice site, as in the following example:

```haskell
module Mod1 where

repPolyApp :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2)
           . CodeQ ((a -> b) -> a -> b)
repPolyApp = [|| \f x -> f x ||]

---------------------------------------------------------

module Mod2 where
import Mod1

ok1 :: Int
ok1 = $$repPolyApp (+1) 7

ok2 :: Int#
ok2 = $$repPolyApp (+# 1#) 7#
```

`repPolyApp` is rejected as is, as `r1` is not known to have a fixed runtime representation which causes a problem with the function application `f x`. However, one could imagine:

```
repPolyApp :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                     (a :: TYPE r1) (b :: TYPE r2)
           .  CodeC (Concrete r1)
           => CodeQ ((a -> b) -> a -> b)
repPolyApp = [|| \f x -> f x ||]
```

in which `CodeC (Concrete r1)` indicates that the `FixedRuntimeRep r1` constraint will be satisfied at the splice-site.  

For the moment, we simply avoid emitting `Concrete#` Wanted constraints when type-checking typed Template Haskell quotes (the evidence would be thrown away anyway). This can change in the future once improvements to typed Template Haskell (such as the introduction of the `CodeC` constraint combinator) are implemented.

## Reporting unsolved FixedRuntimeRep constraints

### CtOrigins

When we emit a new wanted `Concrete#` constraint, we also pass a `CtOrigin` that provides further information about the nature of the check (are we checking a function application, a pattern match, ...?). When the error is reported, this further information will be supplied to the user.

### reportWanteds

We add a case to `reportWanteds` for `Concrete` constraints, to get a custom error message instead of `Could not deduce 'Concrete rep'`.

### Don't suggest "add Concrete"

We don't want to suggest the user add a `Concrete` constraint to their type signature, so we add a check to `GHC.Tc.Errors.ctxtFixes` to prevent GHC from making that suggestion.

### Don't allow Concrete constraints to be deferred

We don't want to allow `Concrete` constraints to be deferred, as polymorphic runtime representations can cause the code generator to panic. So we ensure that constraints whose `CtOrigin` is a `FixedRuntimeRepOrigin` can't be deferred in `GHC.Tc.Errors.nonDeferrableOrigin`.

## Evidence for Concrete# and code generation

What kind of evidence do we want the solving of `Concrete#` constraints to produce? We need to obtain enough information so that code-generation is possible (e.g. so that we know what kind of register to use when doing a function call).

### Alternative 1: store the representation

One idea is to directly store the representation of all binders and function arguments in Core, perhaps as `[PrimRep]`. This would entail changing Core rather significantly.    

<details>
  <summary>Click to read more about this alternative.</summary>

A `[PrimRep]` (that is, a list of `PrimRep`s) describes the representation of a value, as it will be used after unarisation. It must be a list to account for the possibility of unboxed tuples. Note that `PrimRep` includes a constructor `VoidRep`, but we will always assume that we will not use this constructor. See `Note [VoidRep]` in `GHC.Types.RepType` for details.

The solver can thus use `[PrimRep]` as evidence:

```haskell
newtype CodeGenRep = MkCodeGenRep [PrimRep]   -- no VoidReps here
newtype TcCodeGenRep = MkTcCodeGenRep (TcRef (Maybe CodeGenRep))
```

A new `TcEvDest` corresponding to `TcCodeGenRep` will also need to be added, to be used by `FixedRuntimeRep`.    

This evidence is then stored in TTG extension fields at `GhcTc` pass (e.g. in `XApp` for `HsExpr`, in `XWildPat`, `XVarPat`, ... in `Pat`, etc).    

To pass this on after desugaring, we modify Core so that arguments and binders have associated `CodeGenRep`:

```haskell
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   CodeGenRep b (Expr b)   -- this is different
  | Let   (Bind b) (Expr b)
  | Case  CodeGenRep (Expr b) b Type [Alt b]   -- this is different
  | Cast  (Expr b) CoercionR
  | Tick  CoreTickish (Expr b)

data Arg b
  = ExprArg CodeGenRep (Expr b)
  | TypeArg Type
  | CoercionArg Coercion
```

Here, we refactored `Expr` to move the `Type` and `Coercion` constructors out to `Arg`.    
To avoid this refactoring, we could instead define `type Arg b = (Maybe CodeGenRep, Expr b)`, and enforce the invariant that the first element is `Nothing` iff the second element is `Type` or `Coercion`. This seems more error-prone, however.    

Note that we don't change `Bind` as only the `BoxedRep Lifted` representation is allowed there. 

**Pro**: the `CodeGenRep`s are exactly what the code generator needs; it doesn't need to go inspecting types to determine the runtime representation.    
**Con**: we are potentially storing a lot of `PrimRep`s, which might bloat `Core` programs (e.g. `1+2+3+4+5+6+7+8` would store many `LiftedRep` `PrimRep`s).    
**Con**: we don't have a good way of linting the `PrimRep`s.

</details>

### Alternative 2: cast to a fixed representation using a kind coercion

The evidence for a `Concrete# ty` constraint is a kind coercion

```haskell
co :: ty ~# concrete_ty
```

where `concrete_ty` is a tree of constructors and applications like `mkTyConApp tYPETyCon [mkTyConApp intRepTyCon []]` or `mkTyConApp tYPETyCon [mkTyConApp tupleRepTyCon [mkTyConApp nilTyCon [runtimeRepTy]]]`. No variables, type synonyms or type families, etc.    

We can re-use `HoleDest` at the typechecker stage to store a mutable hole to be filled in with evidence, later, by the constraint solver.      

The idea is that, while typechecking, we insert casts using these coercions, so that the code-generator only ever sees function arguments and binders that have a fixed runtime representation.

That is, we would never end up with Core like:

```haskell
f = /\ ( a :: F Int ). \ ( x :: a ). some_expression
```

but rather:

```haskell
f = /\ ( a :: F Int ). \ ( x :: ( a |> kco ) ). some_expression
```

where `kco` is the evidence for `Concrete# (F Int)`.    
Then the type of `x`, `a |> kco`, has kind `TYPE rep` where `rep` is a definite `RuntimeRep` such as `'IntRep`. This is what we wanted: we know which representation the binder `x` has, so the code generator can do its usual work. Calling `typePrimRep` will return the `[PrimRep]` that correspond to the `RuntimeRep` rep, which we recall is guaranteed to be a tree of constructors and applications. This is important, as `kindPrimRep` can't handle type-family applications such as `TYPE (G Rep)` (as it doesn't have access to a typechecking environment, needed to look up type-family instances, etc).

#### Implementation plan

It is possible to implement this alternative in two steps.

##### PHASE 1
Don't insert any casts. That is, after constraint solving (and zonking), we insist that the coercion evidence obtained for any `Concrete# ty` constraint must be `Refl`, and not a more general coercion.    
This will not handle situations such as type family reduction of a `RuntimeRep`, but will be sufficient to subsume all the previous representation-polymorphism checks that are done in the desugarer.

##### PHASE 2
Introduce casts, in order to allow type-family reduction in `RuntimeRep`s.    
These casts have knock-on consequences: additional casts will be required, to ensure everything remains well-typed.    

To illustrate, suppose that we are part-way through type-checking a function application `f e`. So far, say that we've determined the following types:

```haskell
f :: a -> b
e :: a
```

Now we perform a representation-polymorphism check on the application `f e`. Supposing all goes well:

  - we emit a Wanted `Concrete a` constraint,
  - this constraint gets solved with evidence `kco`.

We now perform the cast `a |> kco` as explained [above](#alternative-2-cast-to-a-fixed-representation-using-a-kind-coercion), in order to achieve representation monomorphism in Core.    
However, we can't simply end there, as we must now also insert a cast in the type of `f`, because it had expected an argument of type `a` when we instead desire to pass an argument of type `a |> TYPE kco`. So we cast in the opposite direction in the "argument" parameter of the function type:

```
f :: ( a -> b ) |> fun_co
```

where

```
fun_co = (->) ( SymCo kco ) ( Refl b )
```

Each situation in which we desire to insert casts to allow for this extended form of representation polymorphism will require a similar treatment: adjust the surrounding types so that everything remains well-typed.