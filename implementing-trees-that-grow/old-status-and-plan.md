Based on reading and git log, @Ericson2314 doesn't think much of this stuff is very useful at this point, at least on the main TTG page, and so moved it here.

## Status

We are following the General Plan (below), but we merge Step 1 and Step 3.
We also split the overall work into multiple smaller patches, by working on a set of datatypes at a time.
   

### Step 0 (Done, Accepted, and Landed)

[Patch D3609](https://phabricator.haskell.org/D3609)

### Step 1 & 3

#### Patch 1 (Done)

[Patch D4147](https://phabricator.haskell.org/D4147) implements TtG for

- `ValBinds`
- `HsPat`
- `HsLit`
- `HsOverLit`
- `HsType`
- `HsTyVarBndr`
- `HsAppType`
- `FieldOcc`
- `AmbiguousFieldOcc`


Overall, the implementation follows TtG paper with some details captured below.
Further details can be found in the patch description, and also there is a related [ticket](https://ghc.haskell.org/trac/ghc/ticket/14429).

## General Plan


We have the following practical plan, which makes it possible to change the AST and the related code gradually, without touching the entire code base with a few git commits.
(I expect to fill in and update the details, as we go.)
It is done in four steps.

### Step 0


We replace the current `HsSyn` with one using a type parameter that can enable the growable base AST, as proposed in [D3609](https://phabricator.haskell.org/D3609).

### Step 1


We replace the current `HsSyn` with 

1. a growable base AST (following our latest design described in our JUCS paper); 
1. a set of extensions exactly as they are right now in GHC; and
1. a set of pattern synonyms as a (temporary) shim to avoid the need for changing the rest of the code base.


  
It will hopefully allow us to reuse the AST for Template Haskell (TH), or even refactoring the parser (to replace Haskell-Src-Exts, etc)   
At this stage, we WILL have some slow downs in GHC's own compile time (and possibly run time) due to the decomposition (and then recomposition), but we hopefully regain some speed by the next steps.
(Arguably the benefit of extensibility, by far, outweighs the \*tolerable\* slow downs)


Some further details on this step:

- a module AST.hs is added to compile/hsSyn where it holds all the declarations for growable AST:

  1. ADTs,
  1. extension type families, and
  1. constraint synonyms `ForallX`
- each ADT definition in hsSyn files which is going to be an extension to the growable AST in AST.hs is replaced with its extension description:

  1. type synonyms relating the datatype to their corresponding base growable datatype in AST.hs, via `GHC` index (e.g., `HsLit pass` is an `GHC` specific instance of `AST.HsLit` hence `AST.HsLit (GHC pass)`)
  1. pattern synonym type signatures for each constructor, which are exactly (even with documentations) as before but in GADT-style.
  1. COMPLETE pragma to suppress totality check error messages
  1. pattern synonym bodies, for each constructor, which basically define which fields / constructors are considered as extensions
  1. extension type family instances (and a datatype for `New` constructors), declaring type of extensions
- to ease inlining in the next steps, "Hs" suffix in all names are removed
- besides importing AST module as qualified, we add the following pragmas (if needed):

  1. `{-# LANGUAGE PatternSynonyms, TypeFamilies, SynonymInstances, FelxibleInstances #-}`
  1. `{-# OPTIONS_GHC -fno-warn-orphans #-}`
- when two constructors of /the same/ datatype, have /the same/ field name, we have to rename one of them, as record pattern synonyms cannot mimic this behaviour   
- in a couple of files there are functions with type annotations including types like `Located (body GhcTc)` which should be changed to a type like `Located (body (GHC GhcTc))`  


All these are pretty mechanical, and I use a set of primitive macros to do parts of the job for me.

### Step 2


We reorganise the way source locations are stored, by moving from the current alternating design to a direct style (as discussed in the Summer of Haskell project).


It will give us a cleaner parser (design/code wise), and speed ups.

### Step 3


Possibly, we remove the pattern synonyms to avoid a layer of indirection and get some speed up. 

### Step 4

We work on refactoring, by then redundant, bits and pieces of TH by either just removing them (like the HsSyn-TH translator) or reusing the ones in the compiler.

## Naming conventions



This is a quick capture of a discussion on \#ghc, about the naming conventions for TTG extension constructors


```haskell
data Foo p = F Int p

-- becomes

data Foo p = F (XF p) Int (Idp p)
           | XFoo (XXFoo p)
```


The extension point name is the constructor name (`F`) preceded by an X (`XF`), and it comes first in the fields of the constructor.


The additional constructor is the data type name preceded by X (`XFoo`), and it's extension payload has `XX`, so the `XXFoo` above.



Where a constructor has the same name as the data type, e.g.


```haskell
data Foo p = Foo Int p

-- becomes

data Foo p = Foo (XCFoo p) Int (Idp p)
           | XFoo (XXFoo p)
```


the constructor extension point is prefixed with `XC`, so `XCFoo` above.


In terms of usage, the extension points should only be used to capture changes of use for the data type or constructor between phases of compilation.


Any changes due to producing a new version of GHC should proceed as normal, adding or changing constructors as required.


In the above example the `XFoo` variant could be produced by the renamer or typechecker capturing additional information that is completely different from the input type.

## Experiment 1


There is an experimental implementation at [https://github.com/alanz/ghc/tree/wip/new-tree-one-param](https://github.com/alanz/ghc/tree/wip/new-tree-one-param).


The intention is to

1. replace the current `hsSyn` parameter which is one of `RdrName`, `Name` or `Id` for the output of the *parser*, *renamer* or *typechecker* respectively with an explicit parameter for the pass; and

1. add extension points to `HsLit.hs` to get a feel for how it will impact things.


 
It does this by adding a `HsExtension` module to `hsSyn`, defining as


```haskell
data GhsPs -- Parser phase
data GhcRn -- Renamer
data GhcTc -- Typechecker
data GhcTh -- Template Haskell. Currently unused
```


This is a deviation from the *Trees that Grow* paper ([http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf)) section 4.2 which suggests


```haskell
data GHC (c :: Component)
data Component = Compiler Pass | TemplateHaskell
data Pass = Parsed | Renamed | Typechecked
```


The deviation is due to a current problem in the implementation which requires the tag type to appear in the `hsSyn` AST types, and the requirement for `Data` instances for it.  See below for details.



This is not important for the experiment however, as in practice we would define type synonyms of the form


```haskell
type GhcPs = GHC '(Compiler Parsed)
type GhcRn = GHC '(Compiler Renamed)
...
```


The `HsLit` module is amended as


```haskell
data HsLit x
  = HsChar (XHsChar x) {- SourceText -} Char
      -- ^ Character
  | HsCharPrim (XHsCharPrim x) {- SourceText -} Char
      -- ^ Unboxed character
...
  | HsDoublePrim (XHsDoublePrim x) FractionalLit
      -- ^ Unboxed Double
```


Each constructor gets its own tag type, derived mechanically from the constructor name for ease of reference when being used.  The extension point is used for the `SourceText` where this is needed. But note that it is also added to constructors without `SourceText`, such as `HsDoublePrim`.



We then define the type families in `HsExtension.hs` for each extension point, as


```haskell
type family XHsChar x
type family XHsCharPrim x
...
type family XHsDoublePrim x
```


For each compiler pass we define the specific mappings


```haskell
-- GHCP
type instance XHsChar       GhcPs = SourceText
type instance XHsCharPrim   GhcPs = SourceText
...
type instance XHsDoublePrim GhsPc = ()

-- GHCR
type instance XHsChar       GhcRn = SourceText
type instance XHsCharPrim   GhcRn = SourceText
...
type instance XHsDoublePrim GhcRn = ()

...
```


These are all the same at the moment, and `()` is used for points not requiring anything for any of the passes.


One of the eventual goals (for \@alanz anyway) is to be able to pass an AST using different annotations on it to later passes of the compiler.



To facilitate this, some type classes are defined for the `SourceText` and providing initial/default values.


```haskell
class HasSourceText a where
  -- Provide setters to mimic existing constructors
  noSourceText  :: a
  sourceText    :: String -> a

  setSourceText :: SourceText -> a
  getSourceText :: a -> SourceText

-- Named constraint to simplify usage
type SourceTextX x =
  ( HasSourceText (XHsChar x)
  , HasSourceText (XHsCharPrim x)
  ...
  )

```

```haskell
class HasDefault a where
  def :: a

-- Named constraint to simplify usage
type HasDefaultX x =
  ( HasDefault (XHsChar x)
  , HasDefault (XHsCharPrim x)
...
  , HasDefault (XHsDoublePrim x)
  )
```


These have the expected instances for the two types used in GHC


```haskell
instance HasSourceText SourceText where
  noSourceText    = NoSourceText
  sourceText s    = SourceText s

  setSourceText s = s
  getSourceText a = a

```

```haskell
instance HasDefault () where
  def = ()

instance HasDefault SourceText where
  def = NoSourceText
```

### PostXXX types



The paper also proposes explicitly using extension points for the `PostRn` and `PostTc` usages. This has not been done in the current experiment, which has the limited goals set out above. The types have been replaced with updated ones parameterised by the pass variable though


```haskell
type family PostTC x ty -- Note [Pass sensitive types]
type instance PostTc GhcPs ty = PlaceHolder
type instance PostTc GhcRn ty = PlaceHolder
type instance PostTc GhcTc ty = ty

-- | Types that are not defined until after renaming
type family PostRN x ty  -- Note [Pass sensitive types]
type instance PostRn GhcPs ty = PlaceHolder
type instance PostRn GhcRn ty = ty
type instance PostRn GhcTc ty = ty
```

### When we actually *need* a specific id type



Many functions and data types need to refer to variables that used to be simply the AST type parameter.  This ability is provided through the `IdP` type family


```haskell
-- Maps the "normal" id type for a given pass
type family IdP p
type instance IdP GhcPs = RdrName
type instance IdP GhcRn = Name
type instance IdP GhcTc = Id
```


So we end up with


```haskell
data Sig pass
    TypeSig
       [Located (IdP pass)]  -- LHS of the signature; e.g.  f,g,h :: blah
       (LHsSigWcType pass)   -- RHS of the signature; can have wildcards
...
```

### Experiences


Once the `hsSyn` AST was converted, the conversion process for other modules is straightforward, as it is basically mapping

- `RdrName` to `GhcPs`
- `Name` to `GhcRn`
- `Id`  to `GhcTc`


in type signatures, and making sure that where the original was used "as is" to now wrap it in `IdP`.


In some cases adding `SourceTextX` or `HasDefaultX` constraints is also required. 

### Problems



The `Data` instance for the index type is required due to the following kind of construct


```haskell
  | ValBindsOut
        [(RecFlag, LHsBinds idL)]
        [LSig GHCR] -- AZ: how to do this?
```


This has `GHCR` as the hard coded index type for `LSig`.


I presume this can be dealt with by either

1. Somehow adding a constraint that `IdP idL ~ Name` (where `idL` is the AST index type; or

1. Making `ValBindsOut` only appear as a constructor when the pass type is `GHCR`, or has the `IdP idL ~ Name` constraint.


Can this be done? How?

### Further steps

1. Implement the `PostRn` and `PostTc` mechanism as per *Trees that Grow*.

1. Sort out the `Data` problem for the parameter type.

1. Add further extension points to the AST.

1. Use of type synonyms, if required.


 


## Experiment 2



\@simonpj wrote


>
>
> I was talking to Ben, Simon et al about your big patch [https://phabricator.haskell.org/D3935](https://phabricator.haskell.org/D3935), which \> is Step 1 of [ https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow](https://ghc.haskell.org/trac/ghc/wiki/ImplementingTreesThatGrow).
>
>
>
> To us it seems that separating out Step 3 is a pretty big detour:
>
>
> - It involves defining a massive collection of pattern synonyms that we will then discard.
>
> - It defines a massive file AST.hs that defines all the HsSyn data types, all moved from HsPat, HsType etc ??? but those declarations will all move back again in Step 3.
> - Each synonym has the comments from the original data constructor carefully transferred to it; then in Step 3 we will transfer them back to the data constructor.
>
> - There is some faff associated with record-field-name clashes that we can revert in step 3.
>
>
> So could we just do Step 1 and Step 3 at once?
>
>
>
> Of course, that means that all pattern matches must be dealt with.   But many of them use field names anyway, and so will be minimally changed.  And it???s totally straightforward what to do.
>
>
>
> If you prefer, you could do it one data type at a time.  We already have the right type parameters.
>
>


This experiment is taking place at [https://github.comhttps://gitlab.haskell.org/ghc/ghc/tree/wip/ttg-2017-10-13](https://github.comhttps://gitlab.haskell.org/ghc/ghc/tree/wip/ttg-2017-10-13)

### Rough notes based on starting the work (AZ)

1. The design is inherently layered, so information has to appear in at least two places.


Pieces

- The actual data structure containing the extension points

```haskell
        data Pat x
          = WildPat
              (XWildPat x)
          ...
```

- The type family definition per extension point

```haskell
        type family XWildPat   x
```

- A convenience constraint naming all extension points for a given data type

```haskell
        type ForallXPat c x = ...
```

- The type instance definition, giving a concrete type for a given type tag

```haskell
        type instance
         XWildPat   (GHC pass) = PostTc pass Type
```

1. In current implementation

```haskell
    type
      Pat pass = AST.Pat (GHC pass)
```


effectively forces all extension points to be in the GHC "namespace"

1. Argument for patterns

```haskell
    data HsValBindsLR idL idR
      = -- | Value Bindings In
        --
        -- Before renaming RHS; idR is always RdrName
        -- Not dependency analysed
        -- Recursive by default
        ValBindsIn
            (XValBinds idL idR)
            (LHsBindsLR idL idR) [LSig idR]

        -- | Value Bindings Out
        --
        -- After renaming RHS; idR can be Name or Id Dependency analysed,
        -- later bindings in the list may depend on earlier ones.
      | NewValBindsLR
          (XNewValBindsLR idL idR)
      -- | ValBindsOut
      --       [(RecFlag, LHsBinds idL)]
      --       [LSig GhcRn] -- AZ: how to do this?
```

1. We need to define a way of combining extensions

```haskell
    plusHsValBinds :: HsValBinds a -> HsValBinds a -> HsValBinds a
    plusHsValBinds (ValBindsIn x1 ds1 sigs1) (ValBindsIn x2 ds2 sigs2)
      = ValBindsIn (x1 `mappend` x2) (ds1 `unionBags` ds2) (sigs1 ++ sigs2)
```

>
>
> Is `mappend` the right thing to use?
>
>

1. Current implementation defines

```haskell
    pattern
      ValBindsIn a b
        = AST.ValBinds   NoFieldExt a b
```

>
>
> which means that any tag using the extension will break in GHC code.
>
>

1. what are we trying to achieve with TTG?

  - Pass arbitrary AST through GHC and have it processed?
    What minimal constraints?
  - GHC processes AST, but other tools can then post-process?
  - What about alternate GhcPs representation, one for IDE usage?
  - Who owns the extensions?

    - alternate representations for e.g. GhcPs IDE vs non-IDE
    - phase specific changes e.g. ValBindsOut
    - Other uses, non-GHC

    Basic question is when should a GHC dev make a modification to
    the core data type, and when use an extension point?

1. A pattern locks in a particular use of an extension point.

```haskell
    pattern
      ValBindsOut a b
        = NewValBindsLR (NValBindsOut a b)
```

>
>
> it is not possible to pattern match on the type params on the LHS
> so the following does not parse
>
>

```haskell
    pattern
      ValBindsOut (GhcPass a) (GhcPass b)
        = NewValBindsLR (NValBindsOut (GhcPass a) (GhcPass b))
```
