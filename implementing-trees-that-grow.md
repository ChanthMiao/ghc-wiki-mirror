# Implementation of Trees that Grow


In this page, we discuss the overall plan and details of implementing Trees that Grow in GHC.   Re-engineering `HsSyn` is a major exercise that touches a lot of code, so we need to move carefully.  This page outlines the plan.

### External info

- [The Trees that Grow paper](http://www.jucs.org/jucs_23_1/trees_that_grow/jucs_23_01_0042_0062_najd.pdf)
- The motivation and some background information can be found at the [report](https://ghc.haskell.org/trac/ghc/wiki/NativeMetaprogramming) of a related Summer of Haskell project.
- Main GHC branch: `wip/GrowableAST`
- #14490: TTG snags

### "Child" wiki pages

- [Notes about instances](implementing-trees-that-grow/instances)
- [Potential use for adding IDE support](implementing-trees-that-grow/ide-support)
- [A set of advises for GHC developers on how to work with TTG](implementing-trees-that-grow/trees-that-grow-guidance)
- [Handling Source Locations](implementing-trees-that-grow/handling-source-locations)
- [Handling API Annotations via TTG](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow/in-tree-api-annotations)
- [Old Status and Plan](https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grow/old-status-and-plan)


Main protagonists: Shayan Najd and Alan Zimmerman. 

## Goals

### Long term

The long term goal is to use a single data type for

- GHC itself (currently ~~`HsSyn`~~ `GHC.HS.*`)
- Template Haskell (currently the types in the `template-haskell` library)
- `hs-src-exts`, a popular library for processing Haskell

This data type will need to be defined in another package, of course.

### Short term

The shorter term plan is to validate the idea by applying it to GHC.  That is, re-engineer `GHC.HS.*` along the lines of Trees That Grow.

A major benefit is that we believe that this re-engineering will:

- Completely subsume Alan Zimmerman's [Api Annotations](https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations), making them much easier to use.
- Allow us to get rid of the annoying alternation between `t` and `Located t`, which pervades `GHC.HS.*`

### Getting from short to long term

@Ericson2314 thinks the key is to "bud off" the AST module, so that it's increasingly ready to be shared between GHC and other packages per the long term goal. That means trying to separate the AST datatypes from as much GHC-specific machinery as possible.

Here are some issues relating to that:

- #18936
- #19218

## Status

The main work in recent months has been incrementally moving phase-specific stuff into extension points.
This is trackeded in #16830.

The larger picture has stayed, fairly constant, looking like:

```haskel
module GHC.Hs.Extension

data NoExtField = ...
data NoExtCon = ...

data GhcPass = ...

type family XConA p
type family XConB p
...
```
```haskell
module GHC.Hs.Foo

Data Foo p = ...

type instance XConA GhcPs = ...
type instance XConA GhcRn = ...
type instance XConA GhcTc = ...

...
```

This does mean that the AST and GHC stuff like `GhcPass` and it's instances is thoroughly mixed together.

# Plan

## #18936

The next step per #18936 would be to change things up like this:

```haskel
module Language.Haskell.Syntax.Extension

data NoExtField = ...
data NoExtCon = ...

type family XConA p
type family XConB p
...
```
```haskel
module GHC.Hs.Extension.GhcPass

data GhcPass = ...
```
```haskell
module Language.Haskell.Syntax.Foo

import Language.Haskell.Syntax.Extension

Data Foo p = ...
```
```haskell
module GHC.Hs.Foo

import GHC.Hs.Extension.GhcPass
import Language.Haskell.Syntax.Foo

type instance XConA GhcPs = ...
type instance XConA GhcRn = ...
type instance XConA GhcTc = ...

...
```

Now we hava a nice separation between the parts that ought to be GHC-specific and those that out to be GHC-agnostic.

### Cleaving dependencies

But, at firstt, the `Language.Haskell.Syntax` parts will still be tied to the rest of GHC in unfortunate ways, as the new dependencies test added in !4778 shows.

 - Imports of the type checker: WIP fix in !4782

 - `Outputable`. We could just move remaining instances to `Language.Haskell.Syntax.Foo`, but I have some fondness for making `Language.Haskell.Syntax.*.Ppr` modules in hopes that someday `Outputable` itself can become GHC-agnostic.

 - `hs-boot` cycles. `Name`s, `Id`s, `Var`s, are somewhat jumbled.
    Vars are mixed with Tc unification vars, and typed vars rerefernece `DataCon` and tons of other stuff via that.

    Some of these specific issues here are to be solved elsewhere, e.g. #18758 / [design/type-refactor](design/type-refactor) about separating the Haskell and Core notions of a type.
    But we need not be blocked on all that; we can just make more extension points, because even though the use of these types are not ntoday GHC-stage-specific, they certainly are GHC-specific.

Or at least the beginning of it, more extension points might be needed for phase-agnostic but GHC-specific stuff.

## #19218

At long least, we move the `Language.Haskell.Syntax` modules to a separate package.
We might want to make sure we have #10827 done by then so it's as easy to load the split GHC in GHCi as it is to load the original monolith.
