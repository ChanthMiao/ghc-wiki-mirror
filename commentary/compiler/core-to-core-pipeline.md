# Core-to-Core optimization pipeline


After the source program has been [typechecked](commentary/compiler/type-checker) it is desugared into GHC's intermediate language [Core](commentary/compiler/core-syn-type). The Core representation of a program is then optimized by a series of correctness preserving Core-to-Core passes. This page describes the overall structure of the Core-to-Core optimization pipeline. Detailed descriptions of optimizations are available [in the published papers](commentary/compiler/core-to-core-pipeline#further-reading). An overview of the whole compiler pipeline is available [here](commentary/compiler/hsc-main).

## Optimizations during desugaring


At the end of desugaring we run the `simpleOptPgm` function that performs some simple optimizations: eliminating dead bindings, and inlining non-recursive bindings that are used only once or where the RHS is trivial. The rest of Core optimisations is performed by the Core-to-Core pipeline.

## The pipeline



The structure of the Core-to-Core pipeline is determined in the `getCoreToDo` function in the [compiler/GHC/Core/Opt/Pipeline.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/Opt/Pipeline.hs) module. Below is an ordered list of performed optimisations. These are enabled by default with `-O1` and `-O2` unless the description says a specific flag is required. The simplifier, which the pipeline description below often refers to, is described in detail in [the next section](commentary/compiler/core-to-core-pipeline#simplifier).


- **Static Argument Transformation**: tries to remove redundant arguments to recursive calls, turning them into free variables in those calls.  Only enabled with `-fstatic-argument-transformation`.  If run this pass is preceded with a "gentle" run of the simplifier.

- **Vectorisation**: run the [Data Parallel Haskell](data-parallel) [vectoriser](data-parallel/vectorisation). Only enabled with `-fvectorise`. TODO does `-Odph` imply `fvectorise`?

- **Simplifier, gentle run**

- **Specialisation**: specialisation attempts to eliminate overloading. More details can be found in the comments in [compiler/GHC/Core/Opt/Specialise.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/Opt/Specialise.hs).

- **Full laziness, 1st pass**: floats let-bindings outside of lambdas. This pass includes annotating bindings with level information and then running the float-out pass. In this first pass of the full laziness we don't float partial applications and bindings that contain free variables - this will be done by the second pass later in the pipeline. See "Further Reading" section below for pointers where to find the description of the full laziness algorithm.

- **Simplifier, main run**: run the main passes of the simplifier (phases 2, 1 and 0). Phase 0 is run with at least 3 iterations.

- **Float in, 1st pass**: the opposite of full laziness, this pass floats let-bindings as close to their use sites as possible. It will not undo the full laziness by sinking bindings inside a lambda, unless the lambda is one-shot. At this stage we have not yet run the demand analysis, so we only have demand information for things that we imported.

- **Call arity**: attempts to eta-expand local functions based on how they are used. If run, this pass is followed by a 0 phase of the simplifier. See Notes in [compiler/GHC/Core/Opt/CallArity.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Core/Opt/CallArity.hs) and the relevant paper.

- **Demand analysis, 1st pass** (a.k.a. strictness analysis): runs the [demand analyser](commentary/compiler/demand) followed by worker-wrapper transformation ([JFP paper](http://ittc.ku.edu/~andygill/papers/wrapper.pdf)) and 0 phase of the simplifier. This pass tries to determine if some expressions are certain to be used and whether they will be used once or many times (cardinality analysis). We currently don't have means of saying that a binding is certain to be used many times. We can only determine that it is certain to be one-shot (ie. used only once) or probable to be one shot. Demand analysis pass only annotates Core with strictness information. This information is later used by worker/wrapper pass to perform transformations. CPR analysis is also done during demand analysis.

- **Exitification**: The goal is to pull as much code out of recursive functions as possible, as the simplifier is better at inlining into call-sites that are not in recursive functions.

- **Full laziness, 2nd pass**: another full-laziness pass. This time partial applications and functions with free variables are floated out.

- **Common Sub-expression-elimination**: eliminates expressions that are identical.

- **Float in, 2nd pass**

- **Check rules, 1st pass**: this pass is not for optimisation but for troubleshooting the rules. It is only enabled with `-frule-check` flag that accepts a string pattern. This pass looks for rules beginning with that string pattern that could have fired but didn't and prints them to stdout.

- **Liberate case**: unrolls recursive functions once in their own RHS, to avoid repeated case analysis of free variables. It's a bit like the call-pattern specialisation but for free variables rather than arguments. Followed by a phase 0 simplifier run. Only enabled with `-fliberate-case` flag.

- **Call-pattern specialisation**: Only enabled with `-fspec-constr` flag. 'Inlining for recursive functions': Specialises recursive functions for call patterns involving arguments in WHNF, so that the simplifier can weed out emerging redexes in the body. Details are in the paper [Call-pattern Specialisation for Haskell Programs](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/spec-constr.pdf)

- **Check rules, 2nd pass**

- **Simplifier, final**: final 0 phase of the simplifier.

- **Demand analysis, 2nd pass** (a.k.a. late demand analysis): this pass consists of demand analysis followed by the worker-wrapper transformation and phase 0 of the simplifier. The reason for this pass is that some opportunities for discovering strictness were not visible earlier; and optimisations like call-pattern specialisation can create functions with unused arguments which are eliminated by late demand analysis. Only run with `-flate-dmd-anal`.

- **Demand analysis, final pass** A final pass of the demand analyser, not followed by worker-wrapper. This one is always run if there was any previous run of the demand analyser: As mentioned in the cardinality paper this is meant to detect single entry thunks. Another round of WW would invalidate that information.

- **Check rules, 3rd pass**


The plugin mechanism allows to modify the above pipeline dynamically.

## Simplifier


Simplifier is the workhorse of the Core-to-Core optimisation pipeline. It performs all the local transformations: (TODO this list is most likely not comprehensive)

- constant folding
- applying the rewrite rules
- inlining
- case of case
- case of known constructor
- eta expansion and eta reduction
- combining adjacent casts
- pushing a cast out of the way of an application e.g. ??`(f |> k) a` ??==\> ??`f (a |> k1) |> k2` for suitable `k1`, `k2`.

### Simplifier phases


Each run of the simplifier is assigned with a phase number: 2, 1 or 0. Phase numbers are used for control of interaction between the rules and `INLINE`/`NOINLINE` pragmas - see sections [9.31.6.5](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#phase-control) and [ 9.32.3](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#how-rules-interact-with-inline-noinline-pragmas) of the user guide. There are many 0 phases because we use the simplifier to propagate the effects of other passes and once we've gone through the main runs of the simplifier we no longer have to worry about rules and inlinings - these have been already dealt with. There is also a special initial phase, which is used at the beginning of the pipeline by the "gentle" simplifier runs.

### Simplifier iterations


Each single run of the simplifier consists of many iterations. Each iteration tries to apply all the optimisations. The simplifier run ends when a fixpoint is reached or when a predesignated number of iterations has been performed (the default is 4 and it can be controlled via the `-fmax-simplifier-iterations` flag.

### Simplifier settings


Simplifier settings can be further tweaked by modifying the fields of `SimplifierMode` record. It is possible to turn on and off the following optimisations:

- rewrite rules
- inlining
- case-of-case transformation
- eta-expansion


The so-called "gentle" run disables the case-of-case transformation but does inlining, applies rules and does and eta-expansion. The reason it disables case-of-case is that it makes full laziness work better. FIXME that's what the comment says, and yet we run the full-laziness after the main run of the simplifier.


Even when we disable inlining bindings which only occur once in the body can still be inlined.

## Other documentation

- [Commentary/Compiler/OptOrdering](commentary/compiler/opt-ordering) discusses the motivation for ordering the optimisations in the pipeline. This may or may not be up to date.
- You can use `-dverbose-core2core` flag to dump the Core after almost all passes in the pipeline.

## Further reading

- [Compilation by Transformation in Non-Strict Functional Languages](https://www.microsoft.com/en-us/research/publication/compilation-transformation-non-strict-functional-languages/) (a.k.a. the Santos' thesis): basic source of information about core-to-core transformations. Describes local transformations, full laziness, static argument transformation and many others. Note: Santos' description of inlining is superseeded by "Secrets of the GHC inliner" (see below).

- [Let-floating: moving bindings to give faster programs](http://research.microsoft.com/pubs/67060/float.ps.gz), Simon Peyton Jones, Will Partain, and Andre Santos, ICFP 1996. Describes the let floating and full laziness optimisation passes. It mostly repeats Santos' thesis.

- [A transformation-based optimiser for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/comp-by-trans-scp.ps.gz), SL Peyton Jones and A Santos, Science of Computer Programming 32(1-3), pp3-47, September 1998.  Gives a very simplified overview of Core and then goes into discussing: inlining, veta reduction, optimizing conditionals, unboxed data types and strictness analysis, and let floating (including full laziness). Again, this paper repeats the Santos' thesis, except for the unboxed data types description, where it repeats "Unboxed values as first class citizens in a non-strict functional language". 

- [Secrets of the GHC inliner](http://research.microsoft.com/en-us/um/people/simonpj/papers/inlining/index.htm), Simon Peyton Jones and Simon Marlow, Journal of Functional Programming 12(4), July 2002, pp393-434.  As the name of the paper rightly implies, this paper gives full details of the inliner.

- [Playing by the rules: rewriting as a practical optimisation technique in GHC](http://research.microsoft.com/en-us/um/people/simonpj/papers/rules.htm), Simon Peyton Jones, Andrew Tolmach and Tony Hoare, Haskell Workshop 2001.  Describes the RULES mechanism.

- [Call-pattern Specialisation for Haskell Programs](https://research.microsoft.com/en-us/um/people/simonpj/papers/spec-constr/spec-constr.pdf), Simon Peyton Jones, ICFP 2007. Describes the specialisation optimiser.

- [Modular, Higher-Order Cardinality Analysis in Theory and Practice](http://research.microsoft.com/en-us/um/people/simonpj/papers/usage-types/cardinality-popl14.pdf), Ilya Sergey, Dimitrios Vytiniotis, Simon Peyton Jones, POPL 2014. Describes cardinality analysis (a part of demand analysis) and optimisations that it enables or improves (eg. let-floating). Also available as a more detailed [ JFP journal version](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/12/cardinality-jfp-1.pdf).

- [Constructed Product Result Analysis for Haskell](http://research.microsoft.com/en-us/um/people/simonpj/papers/cpr/cpr.ps.gz), Clem Baker-Finch, Kevin Glynn, and Simon Peyton Jones, Journal of Functional Programming 14(2), 211???245, March 2004. Describes optimisation that allows to return tuple components in registers (for functions that return tuples).

- [Call arity](http://www.joachim-breitner.de/publications/CallArity-TFP.pdf), by Joachim Breitner. Describes the call arity optimisation. Also see [ his thesis](http://www.joachim-breitner.de/thesis) for even more details.

## TODO

- Does handling of unboxed values described in "Unboxed values as first class citizens in a non-strict functional language" fit in the scope of this page?
