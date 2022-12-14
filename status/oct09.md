# GHC status October 2009


We are just about to make our annual major release, of GHC 6.12.1 (in the following we will say "GHC 6.12" to refer to GHC 6.12.1 and future patch-level releases along the 6.12 branch).


GHC continues to be very active, with many opportunities
for others to get involved.  We are particularly eager to find partners
who are willing to take responsibility for a particular platform 
(e.g. Sparc/Solaris, currently maintained by Ben Lippmeier); see [Platforms](platforms).

## The GHC 6.12 release


We usually try to make a major release of GHC immediately after ICFP.
It has been somewhat delayed this year, but we expect to release 
GHC 6.12 during November or December 2009.  Apart from the myriad of
new bug fixes and minor enhancements, the big new things in 6.12 are:

- Considerably improved support for parallel execution.  GHC 6.10
  would execute parallel Haskell programs, but performance was often
  not very good.  Simon Marlow has done lots of performance tuning
  in 6.12, removing many of the accidental (and largely invisible)
  gotchas that made parallel programs run slowly.

- As part of this parallel-performance tuning, Satnam Singh and Simon
  Marlow have developed ThreadScope, a GUI that lets you see what is
  going on inside your parallel program.  It's a huge step forward
  from "It takes 4 seconds with 1 processor, and 3 seconds with 8
  processors; now what?".  ThreadScope will be released separately
  from GHC, but at more or less the same time as GHC 6.12.

- Dynamic linking is now supported on Linux, and support for other
  platforms will follow.  Thanks for this most recently go to
  the [Industrial Haskell Group](http://industry.haskell.org) (thank you \[IHG\]!) who
  pushed it into a fully-working state; dynamic linking is the culmination of the work of
  several people over recent years.
  One effect of dynamic linking is that binaries shrink dramatically, because the run-time
  system and libraries are shared.  Perhaps more importantly, it is possible
  to make dynamic plugins from Haskell code that can be used from other
  applications.

- The I/O libraries are now Unicode-aware, so your Haskell programs
  should now handle text files containing non-ascii characters, without special effort.

- The package system has been made more robust, by associating each
  installed package with a unique identifier based on its exposed ABI.
  Now, cases where the user re-installs a package without recompiling
  packages that depend on it will be detected, and the packages with broken dependencies will
  be disabled.  Previously, this would lead to obscure compilation errors,
  or worse, segfaulting programs.

  This change involved a lot of
  internal restructuring, but it paves the way for future improvements
  to the way packages are handled.  For instance, in the future we
  expect to track profiled packages independently of non-profiled ones,
  and we hope to make it possible to upgrade a package in an ABI-compatible
  way, without recompiling the packages that depend on it.  This latter
  facility will be especially important as we move towards using
  more shared libraries.

- There are a variety of small language changes, including

  - Some improvements to data types: record punning,
    declaring constructors with class constraints, GADT syntax for
    type families etc.
  - You can omit the "`$`" in a top-level Template Haskell splice, which
    makes the TH call look more like an ordinary top-level declaration with
    a new keyword.
  - We're are deprecating `mdo` for recursive do-notation, in favour of
    the more expressive `rec` statement.
  - We've concluded that the implementation of impredicative
    polymorphism is unsustainably complicated, so we are re-trenching.
    It'll be deprecated in 6.12 (but will still work), and will be either
    removed or replaced with something simpler in 6.14.


For more detail, see the release notes in the [6.12 User manual](http://www.haskell.org/ghc/dist/current/docs/html/users_guide/index.html) \[UserManual\], which mention many things skipped over here.


Internally, GHC 6.12 has a totally re-engineered build system, with much-improved
dependency tracking [Building](building).  While there have been lots of teething problems, things are settling down and the new system is a huge improvement over the old one.  The main improvement is that you can usually just say `make`, and everything will be brought up to date (before it was often necessary to `make clean` first).  Another improvement is that the new system exposes much more parallelism in the build, so GHC builds faster on multicores.

## GHC and the Haskell platform


Another big change with GHC 6.12 is that Hackage and the Haskell Platform is
allowing GHC HQ to get out of the libraries business.  So the plan is

- We release GHC 6.12 with very few libraries
- Bill Library Author downloads GHC 6.12 and tests his libraries
- The next Haskell Platform release packages GHC 6.12 with these tested libraries
- Joe User downloads the Haskell Platform.
- Four months later there's a new HP release, still with GHC 6.12,
  but with more or better libraries.  The HP release cycle is
  decoupled from GHC


So if you are Joe User, you want to wait for the HP release.  Don't
grab the GHC 6.12 release.  It'll be perfectly usable, but only if you
use (an up to date) cabal-install to download libraries, and accept that
they may not be tested with GHC 6.12.

## What's hot for the next year


GHC continues to be a great substrate for research.  Here are the main things
we are working on at the moment.

### Type systems


Type families have proved a great success.  From the outside it might
seem that they are done -- after all, they are in GHC 6.10 -- but the
internals are quite fragile and it's amazing that it all works as well as
it does.  (Thanks to Manuel's work.)  Tom Schrijver, Dimitrios
Vytiniotis, Martin Sulzmann, and Manuel Chakravarty have been working
with Simon PJ to understand the fundamentals and, in the light of that
insight, to re-engineer the implementation into something more robust.
We have developed the "OutsideIn" algorithm, which gives a much nicer
account of type inference than our previous story of type inference.
The new approach is described in [Complete and Decidable Type Inference for GADTs](http://research.microsoft.com/~simonpj/papers/gadt) 
\[ICFP09a\]. More controversially, we now believe that local let/where
bindings should not be generalised -- 
see [Let should not be generalised](http://research.microsoft.com/~simonpj/papers/constraints/index.htm) \[LetGen\].  Dimitrios is building a
prototype that embodies these ideas, which we'll then transfer into
GHC.


Meanwhile, Dimitrios, Simon, and Stephanie Weirich are also working on
fixing one of GHC's more embarrassing bugs (Trac #1496),
whereby an interaction of type families and the newtype-deriving can
persuade GHC to generate type-unsound code. It's remained un-fixed
because the obvious approaches seem to be hacks, so the cure was as
bad as the disease.  We think we are on to something; stay tuned.

### Intermediate language and optimisation


Although it is, by design, invisible to users, GHC's intermediate language
and optimisation passes have been receiving quite a bit of attention.
Some highlights

- Read Max Bolingbroke's paper on [Strict Core](http://www.cl.cam.ac.uk/~mb566/papers/tacc-hs09.pdf) \[MaxB\], a possible new
  intermediate language for GHC.  Adopting Strict Core would be a Big 
  Change, however, and we have not decided to do so (yet).

- Simon PJ totally re-engineered the way that INLINE pragmas are 
  implemented, with the goal of making them more robust and 
  predictable [http://www.haskell.org/pipermail/cvs-ghc/2009-October/050881.html !InlinePatch](http://www.haskell.org/pipermail/cvs-ghc/2009-October/050881.html !InlinePatch).  There's a new CONLIKE pragma which
  affects rule matching.

- Peter Jonsson did an internship in which he made a start on turning
  GHC into a supercompiler.  Neil Mitchell's [terrific PhD thesis](http://community.haskell.org/~ndm/thesis/) suggested
  that supercompliation works well for Haskell \[!NeilM\], and Peter has been working on
  supercompilation for Timber as part of his [own PhD](http://www.csee.ltu.se/~pj/papers/scp/index.html) \[!PeterJ\].
  The GHC version isn't ready for prime time yet, but Simon PJ (now
  educated by Peter and Neil) is keen to pursue it.

- An internal change in GHC 6.12 is the addition of "annotations", a
  general-purpose way for a programmer to add annotations to
  top-level definitions that can be consulted by a core-to-core pass,
  and for a core-to-core pass to pass information to its successors
  [Annotations](annotations).
  We expect to use these annotations increasingly in GHC itself.

### Parallelism


Most of the changes in this area in GHC 6.12.1 were described in our ICFP'09 paper [Runtime Support for Multicore Haskell](http://www.haskell.org/~simonmar/bib/multicore-ghc-09_abstract.html) \[ICFP09b\].  The highlights:

- Load-balancing of sparks is now based on lock-free work-stealing queues.

- The overhead for *running* a spark is significantly less, so GHC can take advantage of finer-grained parallelism

- The parallel GC is now much more locality-aware.  We now do parallel GC in young-generation collections by default, mainly
  to avoid destroying locality by moving data out of the CPU cache on which it is needed.  Young-generation collections
  are parallel but not load-balanced.  There are new RTS flags to control parallel GC behaviour.

- Various other minor performance tweaks.


In the future we plan to focus on the GC, with the main goal being to implement independent per-CPU collection.  The other area we plan to look at is changing the GC policy for sparks, as described in our ICFP'09 paper; this will need a corresponding change to the Strategies library to avoid relying on the current "sparks are roots" GC policy, which causes difficulties for writing parallel code that exploits speculation.

### Data Parallelism


Data Parallel Haskell has seen few user-visible changes since the last report.  Nevertheless, Roman Leshchinskiy has been busy improving many of the fundamental building blocks behind the scenes.  These changes were necessary as DPH was able to generate very fast parallel code for simple examples, but the optimisation infrastructure was too fragile ??? i.e., small changes to other parts of GHC (most notably, the Simplifier) or to the DPH libraries could lead to dramatic performance regressions.  Over the last few months, Roman has been working on making the system more robust, while Simon PJ improved and extended parts of GHC's existing optimisation infrastructure (such as the Inliner and other aspects of the Simplifier) to support Roman's efforts.  As a first consequence of this recent work, the divide-and-conquer quickhull benchmark (computing a convex hull) is now significantly faster than the corresponding list-based implementation [http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs QuickHull](http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs QuickHull).  This is an important milestone as quickhull uses dynamically nested parallelism whose depth is not statically bound.



Gabriele Keller implemented a first prototype of a new library API for *regular multi-dimensional* arrays to complement the existing irregular, nested arrays.  For regular computations on dense matrices, relaxation methods and similar, regular arrays (as opposed to nested arrays) are more convenient and expose additional opportunities for optimisation.  Gabriele obtained very encouraging first results with a sequential version that uses a new fusion technique, which we are calling *delayed arrays* [http://www.scribd.com/doc/22091707/Delayed-Regular-Arrays-Sep09 RegLibBench](http://www.scribd.com/doc/22091707/Delayed-Regular-Arrays-Sep09 RegLibBench).



In parallel with the implementation of regular, multi-dimensional arrays as part of DPH, Sean Lee and Manuel Chakravarty are implementing almost the same regular-array API as an EDSL in `Data.Array.Accelerate`.  The EDSL implementation restricts the expressiveness of the array language, but at the same time enables us to experiment with more ambitious backends ??? especially with GPU code generation via CUDA and related technologies. More details are in the video of Manuel's talk from the Haskell Implementors Workshop in Edinburgh [http://justtesting.posterous.com/running-haskell-array-computations-on-a-gpu AccelerateTalk](http://justtesting.posterous.com/running-haskell-array-computations-on-a-gpu AccelerateTalk).

### Code generation


For the last two years we have been advertising a major upheaval in
GHC's back end.  Currently a monolithic "code generator" converts
lambda code (the STG language) into flat `C--`; "flat" in the sense
that the stack is manifested, and there are no function calls.
The upheaval splits this in to a pipeline of passes, with a relatively-simple
conversion of lambda code into `C--` (with function calls), followed
by a succession of passes that optimise this code, and flatten it
(by manifesting the stack and removing calls).


John Dias is the principal architect of this new path, and it is in GHC
already; you can switch it on by saying `-fnew-codegen`.  What remains is
(a) to make it work 100% (currently 99%, which is not good enough); (b)
commit to it, which will allow us to remove gargantuan quantities of
cruft; (c) exploit it, by implementing cool new optimisations at the `C--` level;
(d) take it further by integrating the native code generators into the 
same pipeline.  You can read more on the wiki [CodeGen](commentary/compiler/new-code-gen-pipeline).


Several passes of the new code generation pipeline are supported by Hoopl,
a Haskell library that makes it easy to write dataflow analyses and optimisations
over `C--` code [http://research.microsoft.com/\~simonpj/papers/c-- Hoopl](http://research.microsoft.com/~simonpj/papers/c-- Hoopl).  We think Hoopl is pretty cool, and have well-advanced
ideas for how to improve it a lot more.


All of this has taken longer than we hoped.  Once the new pipeline is in place
we hope that others will join in.  For example, David Terei did an interesting
undergraduate project on using LLVM as a back end for GHC \[Terei\], and 
Krzysztof Wos is just beginning an undergraduate project on optimisation in the new pipeline.
We are particularly grateful to Ben Lippmeier for his work on the SPARC native code generator.

## Bibliography: papers

- \[ICFP09a\] "Complete and Decidable Type Inference for GADTs", Tom Schrijvers, Simon Peyton Jones, Martin Sulzmann, and Dimitrios Vytiniotis. ICFP'09.  [http://research.microsoft.com/\~simonpj/papers/gadt](http://research.microsoft.com/~simonpj/papers/gadt)

- \[ICFP09b\] "Runtime Support for Multicore Haskell", Simon Marlow, Satnam Singh, and Simon Peyton Jones, ICFP 2009. [http://www.haskell.org/\~simonmar/bib/multicore-ghc-09_abstract.html](http://www.haskell.org/~simonmar/bib/multicore-ghc-09_abstract.html)

- \[LetGen\] "Let should not be generalised", Dimitrios Vytiniotis, Simon Peyton Jones, and Tom Schrijvers, submitted to TLDI'10.  [http://research.microsoft.com/\~simonpj/papers/constraints/index.htm](http://research.microsoft.com/~simonpj/papers/constraints/index.htm)

- \[Hoopl\] "Hoopl: dataflow optimisation made simple", Norman Ramsey, John Dias, and Simon Peyton Jones, rejected by POPL 2010. [http://research.microsoft.com/\~simonpj/papers/c--](http://research.microsoft.com/~simonpj/papers/c--)

- \[Terei\] "Low Level Virtual Machine for Glasgow Haskell Compiler", David A. Terei, BSc Thesis. [http://www.cse.unsw.edu.au/\~pls/thesis/davidt-thesis.pdf](http://www.cse.unsw.edu.au/~pls/thesis/davidt-thesis.pdf)

- \[MaxB\] "Types are calling conventions", Max Bolingbroke and Simon Peyton Jones, Haskell Symposium 2009. [http://www.cl.cam.ac.uk/\~mb566/papers/tacc-hs09.pdf](http://www.cl.cam.ac.uk/~mb566/papers/tacc-hs09.pdf)

- \[InlinePatch\] The big INLINE patch [http://www.haskell.org/pipermail/cvs-ghc/2009-October/050881.html](http://www.haskell.org/pipermail/cvs-ghc/2009-October/050881.html)

- \[NeilM\] "Transformation and Analysis of Functional Programs", Neil Mitchelll, PhD thesis, University of York, 2009. [http://community.haskell.org/\~ndm/thesis/](http://community.haskell.org/~ndm/thesis/)

- \[PeterJ\] "Positive supercompliation for a higher order call-by-value language", Peter Jonsson and Johan Nordlander, POPL 2009. [http://www.csee.ltu.se/\~pj/papers/scp/index.html](http://www.csee.ltu.se/~pj/papers/scp/index.html)

- \[IHG\] The Industrial Haskell Group. [http://industry.haskell.org](http://industry.haskell.org)

- \[UserManual\] GHC 6.12 user manual.  [http://www.haskell.org/ghc/dist/current/docs/html/users_guide/index.htm](http://www.haskell.org/ghc/dist/current/docs/html/users_guide/index.htm)

- \[QuickHull\] DPH QuickHull source code [http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs](http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs)

- \[RegLibBench\] Dense matrix-matrix multiplication benchmark with delayed, regular arrays [http://www.scribd.com/doc/22091707/Delayed-Regular-Arrays-Sep09](http://www.scribd.com/doc/22091707/Delayed-Regular-Arrays-Sep09)

- \[AccelerateTalk\] "Haskell Array, Accelerated (Using GPUs)", Manuel M T Chakravarty, presented at the *Haskell Implementors Workshop 2009*, Edinburgh. [http://justtesting.posterous.com/running-haskell-array-computations-on-a-gpu](http://justtesting.posterous.com/running-haskell-array-computations-on-a-gpu)

## Bibliography: wiki


All these URLs should be preceded with [https://gitlab.haskell.org/trac/ghc/wiki](https://gitlab.haskell.org/trac/ghc/wiki)

- \[Building\] GHC's new build system [Building](building)
- \[Platforms\] Platforms that GHC supports [Platforms](platforms)
- \[Annotations\] Annotations in GHC [Annotations](annotations)
- \[CodeGen\] The new codegen pipeline [Commentary/Compiler/NewCodeGenPipeline](commentary/compiler/new-code-gen-pipeline)
