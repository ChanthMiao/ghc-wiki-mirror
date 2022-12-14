
This list overviews the active branches in the main GHC repository.

# Active branches

- `master`: Primary development branch for GHC HEAD.

- `wip/llf`: **Late Lambda Lift**.  Nick Frisby. We lift some lambdas before CorePrep. I'm still determining when to 'not' lift a lambda. My terse notes [here](late-lam-lift).

- `wip/ext-solver`: Iavor S. Diatchki.  Integrate an extrenal SMT solver with the constrain solver.  Mostly for working with type-level naturals at the moment.

- `ghc-lwc2`: **Lightweight concurrency substrate support**. KC Sivaramakrishnan. This implements an evolution of Peng Li's original concept of implementing a lot of the concurrency substrate in the RTS *in Haskell*. The design is described in a paper, "Composable Scheduler Activations for Haskell", found here: [http://research.microsoft.com/en-us/um/people/simonpj/papers/lw-conc/lwc-hs13.pdf](http://research.microsoft.com/en-us/um/people/simonpj/papers/lw-conc/lwc-hs13.pdf).

- `data-kind-syntax`: [Syntax for ''kind-only'' definitions](ghc-kinds/kinds-without-data) (i.e. without the type definition when resorting to promotion). Ready for review and merge.

- `wip/orf`: **Overloaded Record Fields**. Adam Gundry. See [Records/OverloadedRecordFields](records/overloaded-record-fields).

- `wip/amp`: **Applicative/Monad Proposal**. Austin Seipp. This implements the AMP, which will be available in GHC 7.10.

- `wip/ermsb`: **Extended rep-movsb (ERMSB) support for Intel processors**. Austin Seipp. Modern Intel processors feature extremely fast (yet simple!) string copy primitives, which are taken advantage of by simply using the existing `rep movsb` instruction. For many workloads, these copies are competetive with fast AVX-based copies (on my Haswell machine, the difference between the two was indistinguishable). This should result in superior copy performance for Ivy Bridge processors and later, which support the ERMSB addition. This also adds support for `-march` and `-mcpu` to the compiler.

- `wip/generics-propeq`: **Propositional equality for `GHC.Generics`**. Gabor Greif. For interworking of `hackage://gdiff` with `GHC.Generics` we need parametrized meta-data types. The idea is to replace `GHC.Generics.D1Bool` by `GHC.Generics.Dat "GHC.Types" "Bool"` and `GHC.Generics.C1_0Bool` (aka. `True`) by `GHC.Generics.Constr "GHC.Types" ("True", 1)`, and so on. This opens them up to type-level reasoning with `KnownSymbol`, `sameSymbol` etc.

- `wip/gadtpm`: **Pattern Matching Coverage/Exhaustiveness Checker**. George Karachalias. Notes (to be updated soon) can be found [here](pattern-match-check) and [here](pattern-match-check-implementation).

# Limbo branches


These branches are not merged, but they are also not totally dead ended, and their status is currently uncertain.

- `coloured-core`: **Support for terminal color codes in `-ddump-simpl` output**. Thomas Schilling.

- `supercompiler`: **Max's Supercompiler**. Max Bolingbroke. This implements the ideas present in Max's PhD thesis, seen here: [http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-835.html](http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-835.html)

- `local-gc`: **Capability-local garbage collection**. Simon Marlow & Simon PJ. As described in "Multicore Garbage Collection with Local Heaps": [http://simonmar.github.io/bib/papers/local-gc.pdf](http://simonmar.github.io/bib/papers/local-gc.pdf)

- `wip/nested-cpr`: **Nested CPR**. Works, but results where underwhelming. See [NestedCPR](nested-cpr). Joachim Breitner.

- `wip/cbv-conv-thunk`: **Opportunistic evaluation of thunks**. This is a side-line of `wip/nested-cpr`: Nested CPR requires a convergence analysis, and it might be worth evaluating them (#7596). Seems to be not as useful as hoped. Joachim Breitner.

- `wip/common-context`: **Common context transformation** which can reduce code size and possibly produce more join points. See [NestedCPR](nested-cpr#common-context). Joachim Breitner.

# Uncertain


The actual status of these branches, including whether they have been merged and/or superseded, is not clear.

- `ghc-spj`: **???**. Simon PJ.

- `wip/exprArity`: Contains one patch left over when Joachim merged SPJ???s `better-ho-cardinality` branch.

# Archived branches


This is a list of branches that are not developed and are not suitable for `master` for some reason, but might prove useful at some other time

- `wip/T10613`: [Demand Analysis Instrumentation](commentary/compiler/demand#instrumentation)

# Dead/merged branches


This is a list of inactive branches which are dead or have been merged into the tree:

- `late-lam-lift`: **deprecated llf branch**. Please --- someone who can --- delete this branch. It is an old branch that ought to be a wip/ branch but was started before we had that convention. Nick Frisby updated it and push it to `wip/llf` on 19 August 2014.
