## Implementation Plan

### Add MetaML-style quotes


The [Template Haskell Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal) contains a sub-proposal to
[Add MetaML-style quotes](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal#PartB:AddMetaML-styletypedquotes).  This requires a more detailed design.



In the case of terms (only), we know from MetaML that we can have typed quotations. These are rather useful as the metaocaml exploration of the [Shonan Challenge report](http://okmij.org/ftp/meta-programming/Shonan-challenge.pdf) and [ code](https://github.com/StagedHPC/shonan-challenge) (amongst others) shows.  Right now, however, all this work is done in (the recently reborn) [ metaocaml](http://okmij.org/ftp/ML/MetaOCaml.html) rather than in (Template) Haskell because of the availability of typed quotes and splices for increased correctness.


### Concrete Steps


1.   **Add a new abstract type of typed expressions** `TExp a`.  Internally, this can be just an AST (possibly re-using `Exp` or something internal to GHC) and its type. 

1.   **Add a new term quotation form** `[|| e ||]`, called a typed quote; the type of the quote is `TExp ty`, where `ty` is the type of `e`. In the type-system jargon, this is the "introduction form" for `TExp`. The *value* of this quote is the (typed) AST for `e`.

1.   **Add a new splice form** `$$e`, called a typed splice. The term `e` must have type `TExp ty`, and the splice `$$e` then has type ty. This is the "elimination form" for `TExp`. It can, of course, only be used in a context which is expecting a `TExp ty`.  

1.   **Add a constant which takes a typed quote and returns an untyped one**: `unType :: TExp a -> Q Exp` 

1.   **Run these new typed splices in the typechecker, not the renamer.**

1.   **Use renaming of binding forms to insure capture-avoidance** (a la MetaML).

1.   **Cross-stage persistence will remain unchanged.**  To be able to use an identifier at future stages, it must be fully available, which means that it needs to be defined in a previous compilation unit if it will be spliced (by name) into a term.


The justification for (2) and (3) are classical.  For ensuring type-safety (at the current state of knowledge), it is important that `TExp` be abstract, as sound typed-expression manipulation is very hard to achieve, especially in the presence of binders.  A future extension may open this up, whenever this particular tough nut is cracked, but for now we must have (1).  In theory, `unType` (4) is not needed; in practice, it probably will be, but mainly for top-level splices.  (5) is obvious: type information needs to be available for typed splices, and this is not available in the renamer.  (6) comes from MetaML, and basically just means that the renamer will be applied to typed splices as well.  (7) documents a non-change \[which is a little awkward in code, was not present in the old metaocaml, but is actually in the new metaocaml -- see the [data constructor restriction](http://okmij.org/ftp/ML/MetaOCaml.html#ctors)).

### Syntax


As for syntax, it would obviously be best if one could simply use `[| |]` for typed quotes as, in the Haskell world, one would want to have as many things as possible be typed.  But this is entirely unrealistic: surely there will be "correct" TH code out there which will not type.  It would be quite interesting to see what these codes are, but it would also be unfair to inflict such random pain on TH users.  Furthermore, as `[| |]` would be typed only for terms (at least until a lot more research is done, to provide "types" for the other syntactic categories of Haskell), this would likely end up more frustrating than anything else.  Thus, at present, a new syntactic category is best.  As was documented in the proposal, there are a lot of options here, but it is probably simplest to just double-up, aka `[|| e ||]` and `$$e`, rather than steal another sort of generalized bracket for this purpose.  


Of course, it would be nice to also use Unicode for something nicer; ??? e ??? is certainly tempting. ??? e ??? too.  Using ''denotation brackets'' would definitely be wrong though.  But, to go back to logic, most tempting is actually ??? e ??? which logicians have used for quite some time.  Then splice could be denoted by ??? e ??? .  The official suggestion is for the latter two, aka unicode points 2e22, 2e23, 2e24 and 2e25 respectively.

### Other Details


Quite a number of other items have already been described in detail in the [Proposal](http://hackage.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal), so there is no point in repeating them here.

### Scope Extrusion and effects


Since effects in Haskell are typed quite differently than in ocaml, [scope extrusion](http://okmij.org/ftp/ML/MetaOCaml.html#got-away) is unlikely to be much of a problem.  However, these items should be tested.


Note that there should be no restriction on `a` to be a pure type, i.e. it could be `m a` for some `Monad m`.  Normal Haskell typing rules would just apply.  Since top-level declarations cannot be generated as (typed) terms, all such effects would have to be present in the environment already for things to be typeable.

### Implementation Details


It is implicit in the above, but should be made explicit: untyped quotes/splices/quasiquotes should continue to work as they are -- which in detail means that they should be run in the renamer.  Typed quotes/splices/quasiquotes, should be run by the typechecker.

## Implementation Status


Implementation work on Typed Template Haskell is being done on the `th-new` branch.

### Compiling the `th-new` branch

1. Set up a ghc tree as you normally would. See [Building/GettingTheSources](building/getting-the-sources). Check out out the `th-new` branch, e.g.,
  `$ git clone -b th-new http://darcs.haskell.org/ghc.git/`

1. Check out the `th-new` branch in `libraries/template-haskell`.

1. Check out the `th-new` branch in `testsuite`.

1. Configure and build as you would normally (see Building). Note that if you want to use the GHC API to perform run time compilation, you will have to build a static GHCi by adding the following to your `mk/build.mk`. See #7774.

```wiki
DYNAMIC_BY_DEFAULT = NO
DYNAMIC_GHC_PROGRAMS = NO
```

### Features


1. **A new abstract type of typed expressions** `TExp a`.  Internally, this is just a Template Haskell `Exp` with a phantom type.

1. **A new term quotation form** `[||e||]`, called a typed quote. The type of the quote is `Q (TExp tau)`, where `tau` is the type of `e`. In the type-system jargon, this is the "introduction form" for `TExp`. The *value* of this quote is an action in the `Q` monad that computes the (typed) AST for `e`.

1. **A new splice form** `$$e`, called a typed splice. The term `e` must have type `Q (TExp tau)`, and the splice `$$e` then has type tau. This is the "elimination form" for `TExp`.

1. **A constant which takes a typed quote and returns an untyped one**: `unType :: TExp a -> Exp`. There is also a constant `unTypeQ :: Q (TExp a) -> Q Exp`.

1. **Typed splices are run in the typechecker.**

1. **Un-typed splices are run in the renamer.**

1. **Cross-stage persistence is remain unchanged.**

1. **(Un-typed) pattern and local declaration splices are supported.**

1. **Ability to add new top-level declarations from within a top-level splice.** Any top-level splice may add a new top-level declaration using the function `addTopDecl :: [Dec] -> Q ()`. Bindings introduced by these top level declarations are immediately available for use. Only functions and values may be bound by declarations added with `addTopDecl`, and the introduced binders must be `NameU`'s, i.e., generated with `newName` instead of `mkName`.

### Remaining Issues/Questions

**Pattern Splices and Bindings**


Top-level pattern splices are run in the renamer, so they may *introduce binders*. However, nested pattern splices, i.e., pattern splices that occur in a bracket, *are not* run in the renamer, so they *may not introduce binders*. This is arguably inconsistent and undesirable, although top-level splices really are different beasts from nested splices.


This behavior is also inconsistent with quasiquoters. Because *all* quasiquoters are run in the renamer, they may introduce binders even when they are nested within brackets!

**Name Splices**


It would be nice to be able to write code like this:

```wiki
foo = [d|data `T = ...|]
```


where `T` is of type `Name`. Right now, generating a data type declaration with a gensym'd name requires using the smart constructors provided by the Template Haskell library.



Our proposal is to introduce ``n` as syntax for a *name splice*. In an expression context, ``n` would produce a variable expression for the variable with `Name` `n`; in a pattern context, it would bind the variable with `Name` `n`; in a type constructor context it would produce a declaration for the type constructor with `Name` `n`.



What about a type context? Should it produce a type variable? And in an expression context, how to we produce a data constructor by name?
