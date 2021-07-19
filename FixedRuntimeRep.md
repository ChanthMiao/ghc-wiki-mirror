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
class FixedRuntimeRep rep where
  fixedRuntimeRep :: RuntimeRep
```

Whenever a situation arises in which a `RuntimeRep` must be monomorphic, we emit a `FixedRuntimeRep rep` Wanted constraint. The constraint solver attempts to solve these Wanted constraints; if it can't, a type error is reported that tells the user that a representation-polymorphic type isn't allowed. Otherwise, it produces evidence: the specific `RuntimeRep`. This evidence is then passed on to the code generator.

# Details
## Emitting FixedRuntimeRep constraints

The whole point of emitting a `RuntimeRep` constraint is to allow the typechecker to determine whether the `RuntimeRep` is actually fixed (e.g. performing type-family reduction if necessary). When encountering a type `ty :: k`, one has several options to ensure it is representation-monomorphic:
  1. require that `k` be of the form `TYPE r` for a specific `RuntimeRep` `r`. This is no good: we might have `TYPE (Id IntRep)` which requires a type family reduction.
  2. require that `k` be of the form `TYPE r` for some as-yet-unspecified `r,` and emit a `FixedRuntimeRep r` constraint,
  3. emit a `FixedRuntimeRep (GetRuntimeRep k)` where `GetRuntimeRep` is a type family with only equation `GetRuntimeRep (TYPE r) = r`.

The third option would lead to more programs being accepted, as it would allow programs in which type-family reduction is necessary to discover that the kind `k` is of the form `TYPE r`.    
I (@sheaf) have chosen (3.) for the moment, but if it turns out to cause problems it will be very easy to pivot back to (2.).

### Where specifically are we emitting these constraints?

Under the `GHC.Tc.Gen` module hierarchy:
  - `GHC.Tc.Gen.App` for function applications,
  - `GHC.Tc.Gen.Bind` for bindings,
  - `GHC.Tc.Gen.Pat` for patterns,
  - ...

## Solving FixedRuntimeRep constraints

The constraint solver needs to be able to solve these newly emitted `FixedRuntimeRep` constraints. To do this, we add global instances in `GHC.Tc.Instance.Class.matchGlobalInst`, in the same way as for existing built-in constraints such as `Coercible`, `Typeable`, etc.

## Reporting unsolved FixedRuntimeRep constraints

### CtOrigins

When we emit a new wanted `FixedRuntimeRep` constraint, we also pass a `CtOrigin` that provides further information about the nature of the check (are checking a function application, a pattern match, ...?). When the error is reported, this further information will be supplied to the user.

### Don't suggest "add FixedRuntimeRep"

We don't want to suggest the user add a `FixedRuntimeRep` constraint to their type signature, so we add a check to `GHC.Tc.Errors.ctxtFixes` to prevent GHC from making that suggestion.

## Evidence for FixedRuntimeRep and code generation

The evidence for a `FixedRuntimeRep` is the information necessary for compiling the program, which will allow the code generator to know what runtime representation to use (e.g. to know which kind of register to use when compiling a function call).    
This would replace the existing calls to `typePrimRep`, which panic when encountering type family applications like `Id IntRep`.    

Firstly, evidence from solving `FixedRuntimeRep` constraints in the typechecker will be stored in TTG extension fields at `GhcTc` pass (e.g. in `XApp` for `HsExpr`, in `XWildPat`, `XVarPat`, ... in `Pat`, etc).    
Secondly, this information needs to persist in some fashion through desugaring, so that the code generator has enough information. Here are some of the ideas considered so far:

### Alternative 1: Evidence = [PrimRep]

The solver generates `[PrimRep]` as evidence:

```haskell
newtype CodeGenRep = MkCodeGenRep [PrimRep]
newtype TcCodeGenRep = MkTcCodeGenRep (TcRef (Maybe CodeGenRep))
```

A new `TcEvDest` corresponding to `TcCodeGenRep` will also need to be added, to be used by `FixedRuntimeRep`.    

To pass this on after desugaring, we modify Core so that arguments and binders have associated `[PrimRep]`:

```haskell
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   PrimRep b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  PrimRep (Expr b) b Type [Alt b]
  | Cast  (Expr b) CoercionR
  | Tick  CoreTickish (Expr b)

data Arg b
  = ExprArg PrimRep (Expr b)
  | TypeArg Type
  | CoercionArg Coercion
```
Note that we don't change `Bind` as only the `BoxedRep Lifted` representation is allowed there.

**Advantage**: the primreps are exactly what the code generator needs; it doesn't need to go inspecting types to determine the runtime representation.
**Disadvantage**: we are potentially storing a lot of `PrimRep`s, which might bloat `Core` programs (e.g. `1+2+3+4+5+6+7+8` would store many `LiftedRep` `PrimRep`s).
**Disadvantage**: we don't have a good way of linting the `PrimRep`s.

### Alternative 2: Evidence is a coercion

The evidence for a `FixedRuntimeRep k` constraint is a coercion whose LHS is k and whose RHS is `TYPE rep` where `rep` is a tree of constructors and applications like `mkTyConApp intRepTyCon []` or `mkTyConApp tupleRepTyCon [mkTyConApp nilTyCon [runtimeRepTy]]`. No variables, type synonyms or type families, etc.    

We can re-use `HoleDest` at the typechecker stage to store a mutable hole to be filled by the evidence.    

Changes to Core:

```haskell
data Expr b
  = Var   Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   Coercion b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  Coercion (Expr b) b Type [Alt b]
  | Cast  (Expr b) CoercionR
  | Tick  CoreTickish (Expr b)
  | Type  Type
  | Coercion Coercion
```

That is, we add coercions that prove that binders have a fixed runtime representation. We don't do this for arguments, which we instead directly cast by such a coercion.

**Advantage**: we can lint the coercions.
**Disadvantage**: the code generator will need to inspect the coercions to obtain the relevant `PrimRep`s.

### Alternative 3 = 1 + 2

Store both the coercion and `[PrimRep]` in `Core`.

**Advantage**: the code-generator has the `PrimRep`s it needs.
**Advantage**: we can lint everything nicely.
**Disadvantage**: we are potentially storing a lot of `PrimRep`s.