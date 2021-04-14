A big shortcoming of GHC is that it does not support sharing in types,
in any way, shape, or form.  That can lead to exponentially large
types in exotic situtions.  This page explores a design that would
address this problem.

### Let-bindings in types

A starting point might be to allow let-bindings in types
```
  let a = <type> in <type>
```
in the structure of types themselves
```
  data Type = TyVarTy Var
            | AppTy Type Types
            | ...
            | Let TyVar Type Type   -- NEW
```
Now `coreView`, which is always called by type manipulating functions, can push the Let out of the way.  E.g.
```
splitTyConApp_maybe ty
  | Just ty' <- coreView ty = splitTyConApp ty'
  | TyConApp tc tys <- ty   = Just (tc, tys)
  | otherwise               = Nothing
```
How does `coreView` do that?
```
coreView :: Type -> Maybe Type
coreView (Let tv rhs (TyConApp tc tys))
  = Just (TyConApp tc (map (Let tv rhs) tys))
...
```
Here we've had to duplicate `rhs` but we're taking the type apart, so maybe we have to put up with it.

### Substitutions in types

A major operation on types that destroys sharing is *substitution*:
```
substTy :: Subst -> Type -> Type
```
But a substitution is really just a bunch of let's, so perhaps we can just wrap the type in those lets.
Or better still, just wrap it in the subtitution.  Intead of a let-binding, let's have a delayed substitution
```
  data Type = TyVarTy Var
            | AppTy Type Types
            | ...
            | SubstTy Subst Type   -- NEW

substTy :: Subst -> Type -> Type
substTy s ty = SubstTy s ty
```
So `substTy` takes constant time (!), and we instead do the work in
`coreView`, where we "push down" the substitution:
```
coreView (SubstTy s (TyConApp tc tys))
  = Just (TyConApp tc (map (SubstTy s) tys))
coreView (SubstTy s (TyVarTy tv))
  = lookupTyVar s tv
```
Having a full `Subst` has the big advantage that a `Subst` carries an in-scope set,
so we can push down past a forall:
```
coreView (SubstTy s (ForAllTy bndr ty))
  = Just (ForAllTy bndr' (SubstTy s' ty)
  where
    (s', bndr') = substTyVarBndr s bndr
```
where `substTyVarBndr` is what we already have. (It clones `bndr` if necessary,
and substitutes its kind.)

## Composing substitutions

How does `coreView` handle `SubstTy s1 (SubstTy s2 ty)`?  Perhaps
```
coreView (SubstTy s ty)
  | Just ty' <- coreView ty = coreView (SubstTy s ty')
```
but that looks like a bit of a mess -- and makes `coreView` recursive, which it isn't supposed to be.
Moreover the pushing-down business would get expensive: to push down 10 lets, you'd need 10 iterations
of `coreView`.

An obvious, highly plausible possibility is to *combine* the substitutions:
```
mkSubstTy :: Subst -> Type -> Type
mkSubstTy s1 (SubstTy s2 ty) = SubstTy (s1 `composeSubst` s2) ty
```
After all, a substitution is a multi-let!  So the invariant would be that we never
get a nested SubstTy.

## Eliminating clutter

A big disadvantage of this approach is that we might have silly types like
```
SubstTy <big-subst> (TyConApp intTyCon [])
```
which hang onto a big substitution that isn't used.  More generally
```
SubstTy <big-subst> <type mentioning only 'a'>
```
where we hang onto a big substitution when we only need the binding for one variable 'a'.

One solution would be to treat *types* like we do *terms*: de-clutter them in the Mighty Simplifier.
Currently, the Simplifier simply applies `substTy` to Types.  But it could easily
* Walk over the type gathering occurrence information (i.e free variables)
* Prune the substitutions in `SubstTy` nodes to mention only the free vars of the body.
* Completely substitute out variables that only occur once.

This is very analogous to what we do with terms.  It would only happen in the
Simplifier, not in "smart constructors" for Type.

## What else would need to change?

* Although quite a lot of Type-users would not change (because they go via accces functions like `splitTyConApp_maybe`), there are quite a few that would.  For example `mapType` and `foldType` would need to be


## Would this be a step forward?

I don't know if this approach would be better than what we have.

* I think it would improve matters for some corner cases with big type blow-up.
* I worry that it might make matters worse for the common case.  Or maybe not?   Perhaps delaying substitution would be good in the common case too?
* I worry that we might get new space leaks.

