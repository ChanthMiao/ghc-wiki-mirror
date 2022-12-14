
This page discusses the interaction of Trees That Grow with derived for `Data`.  (But it might apply to manual instances for, say, `Outputable` too.)


This page is part of [ImplementingTreesThatGrow](implementing-trees-that-grow).

## Example



Here's our example:


```
  type family XOverLit p
  data HsOverLit p = OverLit (XOverLit p) (HsExpr p)

  data GhcPass (c :: Pass)
  deriving instance Typeable c => Data (GhcPass c)

  data Pass = Parsed | Renamed | Typechecked

  type instance XOverLit (GhcPass 'Parsed     ) = PlaceHolder
  type instance XOverLit (GhcPass 'Renamed    ) = Name
  type instance XOverLit (GhcPass 'Typechecked) = Type

```


We want a `Data` instance for this type.

### PLAN A



I propose


```
 deriving instance (Typeable p, Data (XOverLit (GhcPass p))) =>
    Data (HsOverLit (GhcPass p))

```


But that gives rise to big constraint sets; for each data constructor
we get another `X` field, and another constraint in the `Data` instance.


This is what is currently implemented in GHC master. It works, but every time the constraint set is enlarged as the next step of Trees that Grow goes in, the time taken to compile GHC increases.

### PLAN B



Alan would like to see


```
deriving instance Data (HsOverLit (GhcPass p))
```


which should have all the information available to the derivation process, since `p` can only have one of three values and each of them has a type family instance for `XOverLit`.


This "does not compile".



Alan has discovered that the three instances:


```
  deriving instance Data (HsOverLit (GhcPass 'Parsed ))
  deriving instance Data (HsOverLit (GhcPass 'Renamed))
  deriving instance Data (HsOverLit (GhcPass 'Typechecked))
```


**will** compile, but only with GHC 8.2.1, not with 8.0.2, due to a flaw in the standalone deriving process.


That is: instead of one `Data` instance for the `HsSyn` traversals,
make three.


\[The spurious constraint problem was resolved by including `deriving instance Typeable c => Data (GhcPass c)`, as recommended by \@RyanGlScott \]

### PLAN E


Proceed as per Plan A, but move all the standalone deriving code to a new file, which will produce orphan instances.


Inside this, use CPP to detect a modern enough compiler (GHC 8.2.1) to generate via Plan B, otherwise fall back to Plan A.


Eventually improve the standalone deriving sufficiently that it is able to generate a single traversal, instead of the three.


So for day-to-day work ghc devs can use GHC 8.2.1, and we confirm is still works with GHC 8.0.2 less frequently.

### PLAN F


(This is a bit like Plan C/D, but without the stupid error.)


Suppose we declared out extension fields like this

```wiki
data HsOverLit p = OverLit (GhcExt (XOverLit p)) (HsExpr p)

newtype GhcExt x = Ext x
```


Now we could execute on Plan C by saying

```wiki
instance Data (GhcExt x) where
  gmapM f x = x
  ...
```


The downside is that every pattern match and construction would need to wrap and unwrap with `Ext`.


But actually we are going to need to do that anyway!  We are planning to abolish the alternation of `Located t` and `t` by putting the SrcSpan for the construct in its extension field. Like this

```wiki
data GhcExt x = Ext SrcSpan x
```


So we have to do this wrapping business anyway!


This approach would mean that every client of `HsSyn` would have to have a `GhcExt` in every node, with a `SrcSpan`. If that's not OK (and it probably isn't ok) we could do this:

```wiki
data HsOverLit p = OverLit (XOverLit p) (HsExpr p)
type instance XOverLit (GhcPass p) = GhcExt (GhcXOverlit p)
```


The price is that there are two type-level functions for each constructor.  The benefit is that we can do Plan C.


Actually, in the common case where GHC does not use any extension fields for a constructor `K` we could dispense with the second function thus

```wiki
type instance XK (GhcPass p) = GhcExt PlaceHolder
```

### Plan G


Let's focus on `Outputable` for now. The underlying problem is that to have a *single* pretty printer that does a good job of printing *every instantation* of `HsExpr x` is really hard:

- There are many, many fields within `HsExpr`, of type `X1 x`, `X2 x`, ... `X100 x`, where the `Xi` are all type functions. So a truly-generic pretty-printer for `HsExpr` would have to take 100 pretty-printers as its arguments.

- It'd be difficult to do layout that worked regardless of the way in which the type was instantiated.


Here's a way to get very small instance contexts, at the cost of writing a GHC-specific pretty-printer.  (Leaving the challenge of a truly generic pretty printer for another day.)

```wiki
data ThePass p where
  ThePassPs :: ThePass GhcPs
  ThePassRn :: ThePass GhcRn
  ThePassTc :: ThePass GhcTc

class GhcPassC p where
  thePass :: ThePass p

data HsExpr p
  = HsVar (XVar p) Int
  | ...

type instance XVar (GhcPass p) = GhcXVar p
type family GhcXVar p where
  GhcXVar GhcPs = RdrName
  GhcXVar GhcRn = Name
  GhcXVar GhcTc = Id

instance (GhcPassC p => Outputable (HsExpr (GhcPass p)) where
   ppr (HsVar x i) = ppr i <+> case (thePass @p) of
                                 ThePassPs -> ppr x -- RdrName
                                 ThePassRn -> ppr x -- Name
                                 ThePassTc -> ppr x -- Id
```


So every `Outputable` instance takes `GhcPassC p` as a context, which is just a simple data contructor.  Then we dispatch
on that constructor when we want to pretty-print that piece.


This would be a huge step forward over the pretty printer GHC has had for ages, where the pass-specific structures are
entirely un-printable.  And it'd be simple and efficient.


I have no idea how this would affect `Data`.

### Plan H



The performance issue of Plan A presumably comes from the fact that we have instance search running every time we are typechecking an instance with a large constraint set. That is:


```
type LargeConstraintSet p =
  ( Outputable (XIPBinds    p)
  , Outputable (XViaStrategy p)
  , ...
  , ...
  , ...
  , ... -- and so on, for each extension field
  )

instance LargeConstraintSet p => Outputable (HsExpr p)
instance LargeConstraintSet p => Outputable (HsPat p)
instance LargeConstraintSet p => Outputable (HsType p)
instance LargeConstraintSet p => Outputable (HsBinds p)
instance (LargeConstraintSet idL, LargeConstraintSet idR, Outputable body) => Outputable (StmtLR idL idR body)
...
```


Every time we add something to the `LargeConstraintSet`, typechecking each of these instances slows down. 



Instead, we could create a single constraint, `OutputableX`, and hide the large constraint set inside it:


```
class OutputableX p where
  withOutputableX :: (LargeConstraintSet p => r) -> r
```


and then


```
instance OutputableX p => Outputable (HsExpr p)
instance OutputableX p => Outputable (HsPat p)
instance OutputableX p => Outputable (HsType p)
instance OutputableX p => Outputable (HsBinds p)
instance (OutputableX idL, OutputableX idR, Outputable body) => Outputable (StmtLR idL idR body)
```


becomes fast, as it only has one constraint: `OutputableX`. Operationally, we have `LargeConstraintSet p` stored inside `OutputableX`, but for the typechecker, this is hidden information.



We will have only three instances of `OutputableX`, defined as follows:


```
instance OutputableX (GhcPass 'Parsed) where
  withOutputableX r = r

instance OutputableX (GhcPass 'Renamed) where
  withOutputableX r = r

instance OutputableX (GhcPass 'Typechecked) where
  withOutputableX r = r
```


Therefore, we have to do instance resolution for `LargeConstraintSet` only once per pass (to define the above instances).


Now, whenever a piece of code needs something from the large constraint set, it calls `withOutputableX` explicitly:

```wiki
-  ppr (IPBinds ds bs) = pprDeeperList vcat (map ppr bs)
+  ppr (IPBinds ds bs) = withOutputableX @p $
+                        pprDeeperList vcat (map ppr bs)
```


Inside `withOutputableX @p`, we have `LargeConstraintSet p` in context.

### Plan H\*



Plan H relies on the assumption that calls to `withOutputableX` will not lead to the same kind of performance degradation. This is an untested, but hopefully true assumption. In case it proves to be wrong, we can augment this plan by having separate methods for each constraint:


```
class OutputableX p where
  withOutputableXIPBinds :: (Outputable (XIPBinds p) => r) -> r
  withOutputableXViaStrategy :: (Outputable (XViaStrategy p) => r) -> r
  ...
  ...
  ... -- and so on, for each extension field
```


With Plan H\*, the code will be asking only for the constraint that it actually needs:

```wiki
-  ppr (IPBinds ds bs) = pprDeeperList vcat (map ppr bs)
+  ppr (IPBinds ds bs) = withOutputableXIPBinds @p $
+                        pprDeeperList vcat (map ppr bs)
```


This means that we're not dealing with `LargeConstraintSet` at all, we're working with individual parts of it.

## Outdated/Infeasible Plans

### PLAN C (Infeasible)


It is still painful generating three virtually identical chunks of traversal code.
So suppose we went back to


```
  deriving instance (...)
                 => Data (OverLit (GhcPass p)) where???
```


We'd get unresolved constraints like `Data (XOverLit (GhcPass p))`.  Perhaps we
could resolve them like this


```
  instance Data (XOverLIt (GhcPass p)) where
     gmapM f x = x
     ...etc...
```


That is: a no-op `Data` instances.  Traversals would not traverse extension fields.
That might not be so bad, for now!

### PLAN D (Infeasible)


If there were cases when we really did want to look at those extension fields,
we still could, by doing a runtime test, like this:


```
  instance IsGhcPass p => Data (XOverLIt (GhcPass p)) where
     gmapM = case ghcPass @p of
                IsParsed -> gmapM
                IsRenamed -> gmapM
                IsTypechecked -> gmapM
     ...etc...
```


Here I'm positing a new class and GADT:


```
class IsGhcPass p where
  ghcPass :: IsGhcPassT p

data IsGhcPassT p where
  IsParsed      :: IsGhcPass Parsed
  IsRenamed     :: IsGhcPass Renamed
  IsTypechecked :: IsGhcPass Typechecked

instance IsGhcPass Parsed where
  ghcPass = IsParsed
...etc...
```


Now, instead of passing lots of functions, we just pass a single GADT value
which we can dispatch on.


We can mix Plan C and D.


Alan:


Note:

-  We are solving a compilation time problem for GHC stage1/stage2. The produced compiler has the same performance regardless of which derivation mechanism.

- Ideally we would like to end up with `data` instances for an arbitrary `OverLit p`, which is an outcome for Plan A. i.e. Plan A will play nice with external index types,for GHC API users.
