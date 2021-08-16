This page outlines a plan to move the representation polymorphism checks that currently occur in the zonker and the desugarer to the typechecker.

* Tickets: #17201 #17113 #13105 #17536
* Merge request: !6164


# Table of contents

- [Motivation](#motivation)
- [Broad outline](#broad-outline)
- [Details](#details)
  * [Emitting FixedRuntimeRep constraints](#emitting-fixedruntimerep-constraints)
    + [Where specifically are we emitting these constraints?](#where-specifically-are-we-emitting-these-constraints)
  * [Solving FixedRuntimeRep constraints](#solving-fixedruntimerep-constraints)
  * [Reporting unsolved FixedRuntimeRep constraints](#reporting-unsolved-fixedruntimerep-constraints)
    + [CtOrigins](#ctorigins)
    + [Don't suggest "add FixedRuntimeRep"](#dont-suggest-add-fixedruntimerep)
    + [Don't allow FixedRuntimeRep constraints to be deferred](#dont-allow-fixedruntimerep-constraints-to-be-deferred)
  * [Evidence for FixedRuntimeRep and code generation](#evidence-for-fixedruntimerep-and-code-generation)
    + [Alternative 1: store the representation](#alternative-1-store-the-representation)
    + [Alternative 2: cast to a fixed representation using a kind coercion](#alternative-2-cast-to-a-fixed-representation-using-a-kind-coercion)
      - [Implementation plan](#implementation-plan)
        * [Step 1](#step-1)
        * [Step 2](#step-2)

# Motivation

There are several downsides to checking representation polymorphism in the desugarer, as evidenced by the tickets #17201 #17113 #13105 #17536. For instance, one might need to do type-family reduction in order to determine the `RuntimeRep`. So it seems more natural to do these checks in the typechecker instead, where one has access to all this information.

Similar mechanisms can be implemented in other circumstances:
  * constrain a type to be of the form `TYPE blah` or `Constraint` (TypeLike), see #15979 ([this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/15979#note_213564) in particular), #16139, #19573, ...
  * linear types: check for submultiplicity; see #18756. 

# Broad outline

Introduce a new built-in class:

```haskell
type FixedRuntimeRep :: RuntimeRep -> Constraint
class FixedRuntimeRep rep where {}
```

Whenever a situation arises in which a `RuntimeRep` must be monomorphic, we emit a `FixedRuntimeRep rep` Wanted constraint. The constraint solver attempts to solve these Wanted constraints; if it can't, a type error is reported that tells the user that a representation-polymorphic type isn't allowed. Otherwise, it must provide some kind of evidence that code generation is possible: see the section [Evidence for FixedRuntimeRep and code generation](#evidence-for-fixedruntimerep-and-code-generation).

# Details
## Emitting FixedRuntimeRep constraints

The point of emitting a `FixedRuntimeRep` constraint is to allow the typechecker to determine whether the `RuntimeRep` is actually fixed (e.g. performing type-family reduction if necessary). When encountering a type `ty :: k`,  to ensure it is representation-monomorphic, we first require that `k` be of the form `TYPE r`. If we can immediately determine that `r` is a specific `RuntimeRep` (e.g. `IntRep`), we're done; otherwise, we emit a `FixedRuntimeRep r` constraint, to be solved by the constraint solver.    

Note that we must look through type synonyms to avoid unnecessarily emitting `FixedRuntimeRep` constraints, such as when handling the common case `type Type = TYPE ('BoxedRep 'Lifted)`.    

### Where specifically are we emitting these constraints?

Under the `GHC.Tc.Gen` module hierarchy. For instance, we simply add a call to `hasFixedRuntimeRep` in the following functions:
  - `GHC.Tc.Gen.App.tcEValArg` for function applications,
  - `GHC.Tc.Gen.Bind.tcPolyBinds` for bindings,
  - `GHC.Tc.Gen.Pat.tc_pat` for patterns,
  - `GHC.Tc.Gen.Expr.tcRecordField` for record fields,
  - ...

## Solving FixedRuntimeRep constraints

The constraint solver needs to be able to solve these newly emitted `FixedRuntimeRep` constraints. To do this, we add global instances in `GHC.Tc.Instance.Class.matchGlobalInst`, in the same way as for existing built-in constraints such as `Coercible`, `Typeable`, etc.

## Reporting unsolved FixedRuntimeRep constraints

### CtOrigins

When we emit a new wanted `FixedRuntimeRep` constraint, we also pass a `CtOrigin` that provides further information about the nature of the check (are we checking a function application, a pattern match, ...?). When the error is reported, this further information will be supplied to the user.

### Don't suggest "add FixedRuntimeRep"

We don't want to suggest the user add a `FixedRuntimeRep` constraint to their type signature, so we add a check to `GHC.Tc.Errors.ctxtFixes` to prevent GHC from making that suggestion.

### Don't allow FixedRuntimeRep constraints to be deferred

We don't want to allow `FixedRuntimeRep` constraints to be deferred, as polymorphic runtime representations can cause the code generator to panic. So we ensure that constraints whose `CtOrigin` is a `FixedRuntimeRepOrigin` can't be deferred in `GHC.Tc.Errors.nonDeferrableOrigin`.

## Evidence for FixedRuntimeRep and code generation

What kind of evidence do we want the solving of `FixedRuntimeRep` constraints to produce? We need to obtain enough information so that code-generation is possible (e.g. so that we know what kind of register to use when doing a function call).

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

The evidence for a `FixedRuntimeRep rr` constraint is a (nominal) kind coercion between types of kind `RuntimeRep`; its LHS type is `rr` and its RHS type is `rep` for some `rep` which is a tree of constructors and applications like `mkTyConApp intRepTyCon []` or `mkTyConApp tupleRepTyCon [mkTyConApp nilTyCon [runtimeRepTy]]`. No variables, type synonyms or type families, etc.    

We can re-use `HoleDest` at the typechecker stage to store a mutable hole to be filled in with evidence, later, by the constraint solver.      

The idea is that, while typechecking, we insert casts using these coercions, so that the code-generator only ever sees function arguments and binders that have a fixed runtime representation.

That is, we would never end up with Core like:

```haskell
f = /\ ( a :: TYPE (F Int) ). \ ( x :: a ). some_expression
```

but rather:

```haskell
f = /\ ( a :: TYPE (F Int) ). \ ( x :: ( a |> TYPE kco ) ). some_expression
```

where `kco` is the evidence for `FixedRuntimeRep (F Int)`.    
Then the type of `x`, `a |> TYPE kco`, has kind `TYPE rep` where `rep` is a definite `RuntimeRep` such as `'IntRep`. This is what we wanted: we know which representation the binder `x` has, so the code generator can do its usual work. Calling `typePrimRep` will return the `[PrimRep]` that correspond to the `RuntimeRep` rep, which we recall is guaranteed to be a tree of constructors and applications. This is important, as `kindPrimRep` can't handle type-family applications such as `TYPE (F Int)` (as it doesn't have access to a typechecking environment, needed to look up type-family instances, etc).

#### Implementation plan

It is possible to implement this alternative in two steps.

##### Step 1
Don't insert any casts. That is, after constraint solving (and zonking), we insist that the coercion evidence obtained for any `FixedRuntimeRep rr` constraint must be `Refl`, and not a more general coercion.    
This will not handle situations such as type family reduction of a `RuntimeRep`, but will be sufficient to subsume all the previous representation-polymorphism checks that are done in the desugarer.

##### Step 2
Introduce casts, in order to allow type-family reduction in `RuntimeRep`s.    
These casts have knock-on consequences: additional casts will be required, to ensure everything remains well-typed.    

To illustrate, suppose that we are part-way through type-checking a function application `f e`. So far, say that we've determined the following types:

```haskell
f :: a -> b
e :: a
```

Now we perform a representation-polymorphism check on the application `f e`. Supposing all goes well:

  - we find that `a :: TYPE rr`,
  - we emit a Wanted `FixedRuntimeRep rr` constraint,
  - this constraint gets solved with evidence `kco`.

We now perform the cast `a |> TYPE kco` as explained [above](#alternative-2-cast-to-a-fixed-representation-using-a-kind-coercion), in order to achieve representation monomorphism in Core.    
However, we can't simply end there, as we must now also insert a cast in the type of `f`, because it had expected an argument of type `a` when we instead desire to pass an argument of type `a |> TYPE kco`. So we cast in the opposite direction in the "argument" parameter of the function type:

```
f :: ( a -> b ) |> fun_co
```

where

```
fun_co = (->) ( SymCo (GRefl a (TYPE kco)) ) ( Refl b )
```

Each situation in which we desire to insert casts to allow for this extended form of representation polymorphism will require a similar treatment: adjust the surrounding types so that everything remains well-typed.