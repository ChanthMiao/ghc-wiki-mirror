# Overview

Consider the following STG program:
```
let f = {w,y,z,a,b,c} \n {x} -> M
    g = {y,z,a,q} \n {x} -> N
in ... f ... g ...
```
When this let-expression is evaluated, a closure for `f` containing the values
of its free variables in the current environment. Additionally, a closure will be
constructed for `g` containing many of the same variables, i.e. `y`, `z`, and `a`. Instead of duplicating these local 
copies of the variables, this document proposes that the STG program above is
translated into the following program:
```
letenv e = {y,z,a} in
let f = {w,e,b} \n {x} -> caseenv e of {y,z} -> M
    g = {e,q} \n {x} -> caseenv e of {y,z} -> N
in ... f ... g ...
```
The variables `y`, `z`, and `a`, which are both needed in the closures for `f` and `g`, are placed in a new environment object in the heap via the new `letenv` binder. The definitions of `f` and `g` are changed so that `e` replaces the shared free variables. And lastly, the new heap object is deconstructed with `caseenv` which adds the bindings of `y`, `z`, and `a` back into the environments for `M` and `N`.

What is gained in this case? The first program will allocate two closures with a total of 10 cells (assuming the variables have equal size and ignoring the info pointer for now) for free variables in the heap, whereas the second will allocate three heap objects with a total of 8 cells for free variables.

The idea of sharing the free variables of closures is not new. This work takes inspiration from the closure sharing techniques found in SML/NJ [Appel and Trevor. Optimizing Closure Environment Representations. 1988.; Appel. Compiling with Continuations. 1992.; Shao and Appel. Space-Efficient Closure Representations. 1994.], the Orbit Scheme compiler [Adams et. al. 1986], and the Rabbit Scheme compiler [Steele. 1978].

# Implementation

## STG Language Extension

`letenv` and `caseenv` just form the pretty syntax for examples. In the implementation, `GenStgRhs` must be extended with a new constructor for binding environments:
```
data GenStgRhs pass = StgRhsEnv [Id] | ...
```
Unlike function applications which accept variable and literal arguments, environments will only every refer to variables since they are there only for free variables.

`caseenv` is an extension to `GenStgExpr`:
```
data GenStgExpr pass = StgCaseEnv Id [Id] (GenStgExpr pass) | ...
```
The first argument of the constructor is the heap-bound environment that is being opened up; it will always be a variable. The second and third arguments operate like an alternative: the second is the list of variables to be bound in the third expression.

## Analyses

Appel and Trevor in "Optimizing Closure Environment Representations" compare a number of different ways of representing closures which may share free variables. The example above is just one of many. Therein, we create shared environments for closures declared in the same let-block.

Another example of a shared closure structure is found in the discussion of [#14461](https://gitlab.haskell.org/ghc/ghc/-/issues/14461) and is also found in [Appel and Trevor. 1988.] referred to as path compression. For example:
```
let f = {a,b,c,d} \n {x} ->
                 case d + x > 42 of
                   True  -> let g = {a,b,c,x} \n {y} -> M in ... g ...
                   False -> let h = {a,b} \n {z} -> N in ... h ...
in ... f ...
```
Here the inner closures make use of the free variables of an outer closure for a total of 10 cells. We could share the closures as the following:
```
letenv e = {a,b} in
let f = {e,c,d} \n {x} -> caseenv e of {a,b} ->
                 case d + x > 42 of
                   True  -> let g = {e,c,x} \n {y} -> caseenv e of {a,b} -> M
                            in ... g ...
                   False -> let h = {e} \n {z} -> caseenv e of {a,b} -> N
                            in ... h ...
in ... f ...
```
which allocates a total of 9 cells. It could also be structured as the following for the same number of cells:
```
letenv e = {a,b,c} in
let f = {e,d} \n {x} -> caseenv e of {a,b,c} ->
                 case d + x > 42 of
                   True  -> let g = {e,x} \n {y} -> caseenv e of {a,b} -> M
                            in ... g ...
                   False -> let h = {a,b} \n {z} -> caseenv e of {a,b} -> N
                            in ... h ...
in ... f ...
```

**TODO**

## Code Generation

**TODO**

## Formal Semantics

There is an extension to the STG abstract machine (as described in "Implementing lazy functional languages on stock hardware: the STG-machine") that is editable on [Overleaf](https://www.overleaf.com/2445276151rfsrtkwwhmgb).