Motivation: see #14461

Development Branch: `wip/stg-clos-env-share`. To activate the transformation give the `-fstg-clos-env-share` flag.

# Overview

Consider the following STG program:
```
let f = {w,y,z,a,b,c} \n [x] -> M
    g = {y,z,a,q} \n [x] -> N
in ... f ... g ...
```
When this let-expression is evaluated, a closure for `f` containing the values
of its free variables in the current environment. Additionally, a closure will be
constructed for `g` containing many of the same variables, i.e. `y`, `z`, and `a`. Instead of duplicating these local 
copies of the variables, this document proposes that the STG program above is
translated into the following program:
```
let e = env {y,z,a} in
let f = {w,e,b} \n {x} -> case-env e of {y,z} -> M
    g = {e,q} \n {x} -> case-env e of {y,z} -> N
in ... f ... g ...
```
The variables `y`, `z`, and `a`, which are both needed in the closures for `f` and `g`, are placed in a new environment object in the heap via the new `letenv` binder. The definitions of `f` and `g` are changed so that `e` replaces the shared free variables. And lastly, the new heap object is deconstructed with `caseenv` which adds the bindings of `y`, `z`, and `a` back into the environments for `M` and `N`.

What is gained in this case? The first program will allocate two closures with a total of 10 cells (assuming the variables have equal size and ignoring the info pointer for now) for free variables in the heap, whereas the second will allocate three heap objects with a total of 8 cells for free variables.

The idea of sharing the free variables of closures is not new. This work takes inspiration from the closure sharing techniques found in SML/NJ [Appel and Trevor. Optimizing Closure Environment Representations. 1988.; Appel. Compiling with Continuations. 1992.; Shao and Appel. Space-Efficient Closure Representations. 1994.], the Orbit Scheme compiler [Adams et. al. 1986], and the Rabbit Scheme compiler [Steele. 1978].

# Implementation

## STG Language Extension

`env {...}` and `case-env` just form the pretty syntax for examples. In the implementation, `GenStgRhs` must be extended with a new constructor for binding environments:
```
data GenStgRhs pass = StgRhsEnv DIdSet | ...
```
Unlike function applications which accept variable and literal arguments, environments will only every refer to variables since they are there only for free variables. `StgRhsEnv`s have runtime representation `BoxedRep Unlifted` and its fields are free to have any monomorphic representation.

`case-env` is an extension to `GenStgExpr`:
```
data GenStgExpr pass = StgCaseEnv Id DIdSet (GenStgExpr pass) | ...
```
The first argument of the constructor is the heap-bound environment that is being opened up; it will always be a variable of boxed, unlifted runtime representation. The second and third arguments operate like an alternative: the second is the list of variables to be bound in the third expression.

#### Alternative design without `StgCaseEnv`

SG wondered why we need `StgCaseEnv` when its semantics is identical to a regular `StgCase` on a record of boxed, unlifted representation. Why not reuse `StgCase`? Well, then we'd have to extend `type StgAlt = (AltCon, [Id], StgExpr)` with a new case for `CaseEnv`s, because closure envs don't have a DataCon we can reference in `AltCon`. So either we'd have to make `StgAlt` a sum type or add a new alternative to `AltCon`, which is also used in `Core`, where it'd be very weird having to consider closure envs. Maybe `data StgAltCon = Plain AltCon | CloEnv`? Still a bit weird. Ultimately, it may be simpler to stick to extending STG expression syntax.

## Analyses (How We Choose Introduce Shared Closure Environments)

Appel and Trevor in "Optimizing Closure Environment Representations" compare a number of different ways of representing closures which may share free variables. The example above is just one of many (that particular instance creates shared environments for closures declared in the same let-block). Because there are so many ways that we may choose to share closures, the transformation is structured so that we may decide (perhaps, by some unimplemented meta-analysis) whether or not to apply a shared environment once it has been found.

### General Rules for Closure Sharing

1. It does not appear wise to share a closure environment with less than 2 variables. This is because there is the addition of an info_pointer in the shared environment and a pointer to the shared environment in every closure that makes use of it.

2. Adding shared environments may result in *space leaks* if a closure makes use of a shared environment with variables that it doesn't need. For instance, consider the following program where `f` does not appear free in `Q` but `g` does:
```
let f = {a,b,c,d} \n [x] -> M in
let g = {a,b,c} \n [x] -> N in
... 
case f 3 of 
  z -> Q
```
If we create a shared environment for all of the variables of `f`, then we have the following:
```
let e = env {a,b,c,d} in
let f = {e} \n [x] -> case-env e of {a,b,c,d} -> M in
let g = {e} \n [x] -> case-env e of {a,b,c} -> N in
... 
case f 3 of 
  z -> Q
```
Since `f` does not appear free in `Q`, we could garbage collect its environment after it is used in the case-expression. However, using this particular shared closure we must keep around all of `f`'s environment following the case. `d` is not required by `g` but we cannot garbage collect it; therefore, we have a space leak.

3. Should we share an environment with a closure created in a particular branch of a case-expression? This is not so clear. If we do, then with Analysis 2 (below) we can get a 4.1% decrease in allocations in the `event` test from nofib; however, we see a 0.8% increase in the programs `last-piece` and `solid`. If we do not, then the best we have is a 3.3% decrease in allocations for `mate`.

### Analysis 1 (Unimplemented)

Another example of a shared closure structure is found in the discussion of [#14461](https://gitlab.haskell.org/ghc/ghc/-/issues/14461) and which notices where:
```
let f = {a,b,c,d} \n {x} ->
                 case d + x > 42 of
                   True  -> let g = {a,b,c,x} \n {y} -> M in ... g ...
                   False -> let h = {a,b} \n {z} -> N in ... h ...
in ... f ...
```
Here the inner closures make use of the free variables of an outer closure for a total of 10 cells. We could share the closures as the following:
```
let e = env {a,b} in
let f = {e,c,d} \n {x} -> case-env e of {a,b} ->
                 case d + x > 42 of
                   True  -> let g = {e,c,x} \n {y} -> case-env e of {a,b} -> M
                            in ... g ...
                   False -> let h = {e} \n {z} -> case-env e of {a,b} -> N
                            in ... h ...
in ... f ...
```
which allocates a total of 9 cells. It could also be structured as the following for the same number of cells:
```
let e = env {a,b,c} in
let f = {e,d} \n {x} -> case-env e of {a,b,c} ->
                 case d + x > 42 of
                   True  -> let g = {e,x} \n {y} -> case-env e of {a,b} -> M
                            in ... g ...
                   False -> let h = {a,b} \n {z} -> case-env e of {a,b} -> N
                            in ... h ...
in ... f ...
```

### Analysis 2:

Given the current set of free variable that we may want to create a shared environment for, if a sub-expression allocates an environment with a superset of those variable, then it is safe to use a shared environment. For instance,
```
let f = {a,b,c} \n [] -> M in
...
let g = {a,b,c,d,f} \n [] -> N in
...
```
will construct the following shared environment:
```
let e = env {a,b,c} in
let f = {e} \n [] -> case-env e of {a,b,c} -> M in
...
let g = {e,d,f} \n [] -> case-env e of {a,b,c} -> N in
...
```

This appears to be rather conservative as it does not create shared environments for most of nofib. Below is the table of non-zero results for the version of this analysis which will share inside of case-expressions:

| Program       |   Alloc-diff|
|---------------|-------|
|   constraints | -1.1% |
|  cryptarithm2 | +0.1% |
|         event | -4.1% |
|   exact-reals | -0.7% |
|        expert | +0.1% |
|fannkuch-redux | -0.4% |
|           fem | +0.3% |
|           fft | +0.1% |
|          fish | -0.7% |
|         fluid | -0.5% |
|        gamteb | -0.1% |
|           hpg | +0.3% |
|           ida | -1.3% |
|       knights | -0.6% |
|    last-piece | -0.9% |
|          lift | +0.1% |
|        mandel | -1.6% |
|          mate | -3.3% |
|        parser | +0.1% |
|        simple | -0.5% |
|         solid | +0.9% |
|     wave4main | -0.9% |
|       -1 s.d. | -0.7% |
|       +1 s.d. | +0.4% |
|       Average | -0.1% |



## Code Generation

In generating code for first class environments, we treat them in much the same way as the environment part of thunks and function closures. That is, we construct them by allocating space in the heap and placing the values of the free variables---which may be pointers and non-pointers---at that location. This means that we need an info pointer like thunks and function closures. For their elimination (case-env), we simply pull the free variables out and bind them to the correct
registers. Note that no entry or evaluation of the case-env scrutinee need take place.

However, there are somethings that are not the same as the function
and thunk closures which the GHC heap is not used to managing. Currently, we lie to GHC about them. Consider this STG program with a first class environment:

```
main =
  let a = 1
      b = 2
      c = 3 in
  let e = env {a,b,c} in
  let f = {env} \n [x,y] ->
            case-env e of
              {a,b,c} -> a + b + c + x + y
  in f 34 2
```

The first lie is with the type of an environment which must be given when an environment is generated by the analysis. **TODO: SB recommends adding a new type `Env :: TYPE (BoxedRep Unlifted)**

```
  let {
     e :: 0 -> 0
     [LclId] =
        env {a,b,c}; } in
```

We introduce these objects into the untyped STG language so the only important thing here is that the type we use has the correct representation: a pointer to a heap object.

The second place we lie is the static information that is compiled with a first class environment. We tell Cmm that an environment has as a thunk heap representation:

```
 env_e_entry() { //  []
         { info_tbls: [(cEntry,
                        label: env_e_info
                        rep: HeapRep 3 ptrs { Thunk }
                        srt: Nothing)]
           stack_info: arg_space: 8
         }
     {offset
        cEntry: // global
           goto cEntry;
     }
 },
```

Of course, first class environments are *never* entered, so this entry code is useless as well. Since, GHC's heap is used to dealing with closures it was easiest to provide useless entry code.

This should suffice for now as we experiment with the performance effects of different closure environment sharing analyses.


## Formal Semantics

There is an extension to the STG abstract machine (as described in "Implementing lazy functional languages on stock hardware: the STG-machine") that is editable on [Overleaf](https://www.overleaf.com/2445276151rfsrtkwwhmgb).