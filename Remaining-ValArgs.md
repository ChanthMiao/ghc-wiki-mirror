The representation-polymorphism checks ensure that arguments and lamda binders have a fixed runtime representation. However, there is one tricky situation: partial applications of functions with representation-polymorphic arguments. GHC manipulates several such functions.

## Functions with representation-polymorphic arguments in GHC

### Wired-in `Id`s

Some examples:

```haskell
coerce :: forall r (a :: TYPE r) (b :: TYPE r). Coercible a b => a -> b
unsafeCoerce# :: forall a r (a :: TYPE q) (b :: TYPE r). a -> b
```

Others: `rightSection`, `leftSection`, `oneShot`, `inline`, `seq`.

### Unboxed tuples and sums

```haskell
(# , #) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                  (a :: TYPE r1) (b :: TYPE r2)
        . a -> b -> (# a, b #)
(# _ | #) forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                 (a :: TYPE r1) (b :: TYPE r2)
        . a -> (# a | b #)
```

### UnliftedNewtypes

With `-XUnliftedNewtypes`, newtype constructors can have representation-polymorphic arguments:

```haskell
type N :: forall r. TYPE r -> TYPE r
newtype N a where
  MkN :: forall r (a :: TYPE r). a -> N a
```

## Typechecking remaining arguments

In `tcRemainingValArgs`, we typecheck a partial application, ensuring that it can be eta-expanded. For example:

```haskell
bad :: forall r (a :: TYPE r). a -> a
bad = coerce
```

We reject this partial application of coerce, as we have not instantiated it at a specific `RuntimeRep`.

```haskell
fine :: forall (a :: TYPE IntRep). a -> a
fine = coerce
```

Here we can elaborate to

```haskell
fine = \@(a :: TYPE IntRep) (x :: a) -> coerce @IntRep @a @a 
```

Finally, in some situations we need to allow rewriting in `RuntimeRep`s (see [PHASE 2 of FixedRuntimeRep](FixedRuntimeRep#phase-2)):

```haskell
type RR :: RuntimeRep
type family RR where { RR = IntRep }
tricky :: forall (a :: TYPE RR). a -> a
tricky = coerce @RR
```

In this last case, we need to insert a cast that serves as evidence that `RR ~# IntRep`: otherwise, if we left it as an opaque `RR`, we wouldn't know what kind of register to put the argument in when calling `tricky`.

## PHASE 2 for remaining value arguments

There are several moving parts to an implementation that accepts [PHASE 2](FixedRuntimeRep#phase-2) for remaining value arguments.

Consider another example:

```haskell
type family RR where { RR = FloatRep }
tup x = (#,#) @LiftedRep @RR (chr x)
```

The goal is to elaborate it as follows:

```haskell
tup :: forall (a :: TYPE RR). Int -> a -> (# Char, a #)
tup = /\ (a :: TYPE RR). \(i:Int) (x:a).
     let fun :: (a |> TYPE FloatRep) -> (# Char, a |> TYPE FloatRep #)
         fun = ((#,#) @LiftedRep @RR @Char @a (chr i)) |> co1
     in (\(y:: a |> co2 :: TYPE FloatRep). (fun y)) |> co3
where
  co1 :: (a -> (# Char, a #))  ~  ((a |> TYPE FloatRep) -> (# Char, a |> TYPE FloatRep #))
  co2 :: TYPE RR ~ TYPE FloatRep
  co3 :: (# Char, a |> TYPE FloatRep #)  ~  (# Char, a #)
```

The idea is that `tcRemainingValArgs` should take in an application and a "remaining type" (`app_res_rho`), and insert casts and eta-expansions to produce another application of the same "remaining type" in which all `Id`s with representation-polymorphic arguments have been saturated.

In this case:

  - we cast the partial application `(#,#) @LiftedRep @RR @Char @a (chr i)` so that the next argument has `RuntimeRep` `FloatRep` instead of `RR`,
  - we eta-expand at `RuntimeRep` `FloatRep`,
  - we let-bind the previous arguments to avoid losing sharing,
  - we cast back so that we don't change the overall type of the application.

### Side-goal: saturation of data constructors

This approach is interesting for a second reason: the typechecking of data constructors works by eta-expanding in order to make multiplicities match up.

For example, we have:

```haskell
Just :: forall (a :: Type). a %1 -> Maybe a
```

We want to accept `map Just`, but `map` expects a function of type ` a -> b` and not `a %1 -> b`. So we need to eta-expand `Just`: `map (\x -> Just x)`.

We can perform this eta-expansion of data constructors in the same way in `tcRemainingValArgs`, which would saturate all partial applications of data constructors.

This is in contrast to the current approach, which eta-expands data constructors fully.

Example:

```haskell
newtype N (a :: TYPE r) = MkN a

foo = MkN @IntRep
```

This newtype constructor application is currently eta-expanded fully, introducing a representation-polymorphic lambda:

```haskell
( \ @(r :: RuntimeRep) @(a :: TYPE r) (x :: a) -> MkN @r @a x )
   @IntRep
```

Were we to implement eta-expansion in `tcRemainingValArgs`, we would instead produce:

```haskell
\ @(a :: TYPE IntRep) (x :: a) -> MkN @IntRep @a x
```

which is unproblematic. This means we could get rid of the logic in Core Lint which skips representation-polymorphism checks in the output of the desugarer (which is necessary to avoid erroring on such problematic lambdas which get beta-reduced away by the optimiser).

### Implementation details

The most up-to-date implementation of this plan can be found in commit 303d7a994545ebc6f8f67c8bf5f7d3335295d58a. The crux of the logic is in `GHC.Tc.Gen.Head.tcRemainingValArgs`:

```haskell
tc_args :: Arity
        -> (Int -> FixedRuntimeRepContext)
        -> TcM (HsWrapper, [HsExprArg 'TcpTc])
tc_args arity mk_frr_ctxt
  = do { let partial_application = nb_applied_val_args < arity
       ; if not partial_application
         then return (idHsWrapper, applied_args)
         else
    do { (rem_arg_mults_and_cos, rem_res_rho, rem_wrap)
           <- check_remaining_args
                (nb_applied_vis_val_args + 1)
                (nb_applied_val_args + 1)
                rem_arg_tys
                app_res_rho

       ; (let_wrap, final_args) <- letBindValArgs applied_args

       ; return (     let_wrap
                  <.> mkWpCastN (mkTcSymCo fun_co_new_mults)
                  <.> rem_wrap
                  <.> mkWpCastN fun_co_orig_mults
                , final_args) } }

  where

    check_remaining_args :: Int -- visible value argument index, starting from 1
                                -- only used to report the argument position in error messages
                         -> Int -- value argument index, starting from 1
                                -- used to count up to the arity to ensure we don't check too many argument types
                         -> [(Scaled Type, AnonArgFlag)] -- run-time argument types
                         -> TcRhoType
                         -> TcM ( [(Mult, TcCoercionN)]
                                   -- (rem_arg_mult, rem_arg_co)
                                   -- where rem_arg_co is evidence that this remaining value argument
                                   -- has a fixed RuntimeRep
                                , TcRhoType
                                   -- the result type fun_res_rho
                                , HsWrapper )
                                   -- the wrapper that performs the eta-expansion
    check_remaining_args _ i_val _ res_rho
      | i_val > arity
      = return ([], res_rho, idHsWrapper)
    check_remaining_args i_visval !i_val ((Scaled mult arg_ty, af) : tys) res_rho
      | let res_rho' = funResultTy res_rho
      = do { (arg_co, new_arg_ty) <- hasFixedRuntimeRep (mk_frr_ctxt i_visval) arg_ty
           ; let i_visval' = case af of { InvisArg -> i_visval; VisArg -> i_visval + 1}
           ; (arg_cos, rem_res_rho, eta_wrap) <- check_remaining_args i_visval' (i_val + 1) tys res_rho'
           ; let wrap = WpFun idHsWrapper eta_wrap (Scaled mult new_arg_ty)
           ; return ((mult, arg_co) : arg_cos, rem_res_rho, wrap) }

```

So we perform a representation-polymorphism check on the remaining value arguments, obtaining a coercion `arg_co`. We eta-expand for this argument using a `WpFun` `HsWrapper`. Finally, we let-bind previous arguments with a new form of `HsWrapper`, `WpHsLet`. Because the multiplicities can have changed (as discussed in the previous sections), we need to insert some additional casts at different sets of multiplicities (these are the two `mkWpCastN`s in the code above).