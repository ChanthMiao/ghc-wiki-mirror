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
