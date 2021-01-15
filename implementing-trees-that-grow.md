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

The long term goal is to use a single collection of Haskell-syntax data types for multiple clients:

- GHC itself (currently `GHC.Hs.*`)
- Haddock
- Template Haskell (currently the types in the `template-haskell` library)
- `hs-src-exts`, a popular library for processing Haskell

These data types will need to be defined in a separate package, say `haskell-syntax` so it can be properly shared between each of these without dragging in extraneous definitions.

It should also live outside the `GHC.*` module namespace to indicate it's not GHC-specific. Perhaps `Language.Haskell.Syntax`.

### Short term

The shorter term plan is to validate the idea by applying it to GHC:
* Create a new sub-tree, still within the main GHC codebase, rooted at `Language.Haskell.Syntax`.  This will eventually become a separate package `haskell-syntax`.
* Minimise, and eventually eliminate, dependencies from `Language.Haskell.*` to `GHC.*`
* Keep `GHC.Hs` for GHC-specific code that manipulates the syntax tree.
* In parallel, use the TTG extension points to implement Alan Zimmerman's [Api Annotations](https://ghc.haskell.org/trac/ghc/wiki/ApiAnnotations), making them much easier to use. See !2418.


### Getting from short to long term

@Ericson2314 thinks the key is to "bud off" the AST module, so that it's increasingly ready to be shared between GHC and other packages per the long term goal. That means trying to separate the AST datatypes from as much GHC-specific machinery as possible.

Here are some issues relating to that:

- #18936
- #19218

## Status

The main work in recent months has been incrementally moving phase-specific stuff into extension points.
This is trackeded in #16830.

The larger picture, however, has stayed fairly constant, looking like:

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

## Plan

### #18936

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

The avoidance of `GHC.*` for the ghc-agnostic modules was not part of the original issue, but from a comment from @simonpj in https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4778#note_323748.
I think it's a great idea: since we won't be able to abstract away all the interactions with the rest of GHC that should go in the end, it's crucial to indicate to the programmers the intent of budding off these modules into a separate package before it's actually released.
Otherwise, other backsliding is likely as other PRs accidentally introduce new entanglements with GHC even as the people TTG seek to get rid of them, and we go in circles.

### Cleaving dependencies

But, at first, the `Language.Haskell.Syntax` parts will still be tied to the rest of GHC in unfortunate ways, as the new dependencies test added in !4778 shows.

 - Imports of the type checker: WIP fix in !4782

 - `Outputable`. We could just move remaining instances to `Language.Haskell.Syntax.Foo`, but I have some fondness for making `Language.Haskell.Syntax.*.Ppr` modules in hopes that someday `Outputable` itself can become GHC-agnostic.

 - `hs-boot` cycles. `Name`s, `Id`s, `Var`s, are somewhat jumbled.
    Vars are mixed with Tc unification vars, and typed vars rerefernece `DataCon` and tons of other stuff via that.

    Some of these specific issues here are to be solved elsewhere, e.g. #18758 / [design/type-refactor](design/type-refactor) about separating the Haskell and Core notions of a type.
    But we need not be blocked on all that; we can just make more extension points, because even though the use of these types are not ntoday GHC-stage-specific, they certainly are GHC-specific.

Or at least the beginning of it, more extension points might be needed for phase-agnostic but GHC-specific stuff.

### Open vs closed world design

Let's start with a specific example `GhcPass`.
This is (modulo newtypes) a sum type, with a variant for each stage.
This allows singleton-related machinery like `IsPass` in order to *exhaustively* dispatch on each stage within a function. (See `Note [IsPass]`.)

The alternative method is used with `CollectPass`, a class for collecting the bound variables of patterns.
It is needed because Haddock uses it with a non-`GhcPass` type for its own TTG type family instances. 
Unlike with variants for stages, there is no way to exhaustively case on `Type`, and so we have the implicit parameter in the form of the type class dictionary for how to handle the variable cases.

Trying to pull out the AST from GHC raises these sorts of questions, because we have a bunch of stage agnostic helper functions today that are almost ghc-agnostic.
In many cases however, they still require `GhcPass` because something transitively calls something that needs to case on the stage.
We might be tempted to refactor those to live in the `Language.Haskell.Syntax` modules, but it will make the types more complex.

It is @Ericson2314's view not to worry about generalizing things now.
If it's GHC specific, just keep it that way, and instead focus on modularity.
This is because modularity leads to multiple downstream consumers, and balancing the needs of multiple actually existing downstream consumers---not anticipating the needs of multiple hypothetical consumers---is what make libraries great.
So if we have an AST package, and if GHC and other things that use it start duplicating functionality, then let's worry about the quality of our abstractions, but not before.
Yes, @Ericson2314 has long been interested in seeing GHC use more high level abstractions, but trying to push GHC in that direction seems all around worse than just letting an ecosystem of packages evolve that naturally.

### #19218

At long least, we move the `Language.Haskell.Syntax` modules to a separate package.
We might want to make sure we have #10827 done by then so it's as easy to load the split GHC in GHCi as it is to load the original monolith.
