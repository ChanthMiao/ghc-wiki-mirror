This page outlines a plan to move the representation polymorphism checks that currently occur in the zonker and the desugarer to the typechecker.

* Tickets: #17201 #17113 #13105 #17536
* Merge request: !6164

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

Whenever a situation arises in which a `RuntimeRep` must be monomorphic, we emit a `FixedRuntimeRep rep` Wanted constraint. The constraint solver attempts to solve these Wanted constraints; if it can't, a type error is reported that tells the user that a representation-polymorphic type isn't allowed. Otherwise, it produces evidence, to be passed onto the code generator (see the section [Evidence for FixedRuntimeRep and code generation](#evidence-for-fixedruntimerep-and-code-generation)).

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

## Evidence for FixedRuntimeRep and code generation

The evidence for a `FixedRuntimeRep` is the information necessary for compiling the program, which will allow the code generator to know what runtime representation to use (e.g. to know which kind of register to use when compiling a function call).    
This would replace the existing calls to `typePrimRep`, which panic when encountering type family applications like `Id IntRep`.    

Firstly, evidence from solving `FixedRuntimeRep` constraints in the typechecker will be stored in TTG extension fields at `GhcTc` pass (e.g. in `XApp` for `HsExpr`, in `XWildPat`, `XVarPat`, ... in `Pat`, etc).    
Secondly, this information needs to persist in some fashion through desugaring, so that the code generator has enough information. Here are some of the ideas considered so far:

### Alternative 1: Evidence = `[PrimRep]`

A `[PrimRep]` (that is, a list of `PrimRep`s) describes the representation of a value, as it will be used after unarisation. It must be a list to account for the possibility of unboxed tuples. Note that `PrimRep` includes a constructor `VoidRep`, but we will always assume that we will not use this constructor. See `Note [VoidRep]` in `GHC.Types.RepType` for details.

The solver can thus use `[PrimRep]` as evidence:

```haskell
newtype CodeGenRep = MkCodeGenRep [PrimRep]   -- no VoidReps here
newtype TcCodeGenRep = MkTcCodeGenRep (TcRef (Maybe CodeGenRep))
```

A new `TcEvDest` corresponding to `TcCodeGenRep` will also need to be added, to be used by `FixedRuntimeRep`.    

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

Note that we don't change `Bind` as only the `BoxedRep Lifted` representation is allowed there. We have also refactored `Expr` to move the `Type` and `Coercion` constructors out to `Arg`. Otherwise, we could have something like `type Arg b = (Maybe CodeGenRep, Expr b)` with an invariant that the first element is `Nothing` iff the second element is `Type` or `Coercion`, but that seems more error-prone.

**Advantage**: the `CodeGenRep`s are exactly what the code generator needs; it doesn't need to go inspecting types to determine the runtime representation.    
**Disadvantage**: we are potentially storing a lot of `PrimRep`s, which might bloat `Core` programs (e.g. `1+2+3+4+5+6+7+8` would store many `LiftedRep` `PrimRep`s).    
**Disadvantage**: we don't have a good way of linting the `PrimRep`s.

### Alternative 2: Evidence is a coercion

The evidence for a `FixedRuntimeRep k` constraint is a (nominal) coercion whose LHS type is `k` and whose RHS type is `TYPE rep` where `rep` is a tree of constructors and applications like `mkTyConApp intRepTyCon []` or `mkTyConApp tupleRepTyCon [mkTyConApp nilTyCon [runtimeRepTy]]`. No variables, type synonyms or type families, etc.    

We can re-use `HoleDest` at the typechecker stage to store a mutable hole to be filled by the evidence.    

Changes to Core:

```haskell
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   CoercionN b (Expr b)   -- this is different
  | Let   (Bind b) (Expr b)
  | Case  CoercionN (Expr b) b Type [Alt b]   -- this is different
  | Cast  (Expr b) CoercionR
  | Tick  CoreTickish (Expr b)
  | Type  Type
  | Coercion Coercion
```

That is, we add coercions that prove that binders have a fixed runtime representation. We don't do this for arguments, which we instead directly cast by such a coercion. Then, when we need to know the representation for the argument, we can always look at the argument's type's kind, which will always be a constructor/application tree.

**Advantage**: We can lint the coercions.    
**Advantage**: We can re-use some infrastructure around `CoercionHole`s.
**Advantage**: The changes to Core are fewer.
**Disadvantage**: the code generator will need to inspect the coercions to obtain the relevant `PrimRep`s.

### Alternative 3 = 1 + 2

Store both the coercion and `[PrimRep]` in `Core`.

**Advantage**: the code-generator has the `PrimRep`s it needs -- no need to recalculate the representation from the coercion. 
**Advantage**: we can lint everything nicely.    
**Disadvantage**: we are potentially storing a lot of `PrimRep`s.