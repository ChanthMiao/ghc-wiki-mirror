# GHC status April 2010


In the past 6 months we have made the first 2 releases from the 6.12 branch. 6.12.1 came out in December, while 6.12.2 was released in April. The 6.12.2 release fixes many bugs relative to 6.12.1, including closing 81 trac tickets. For full release notes, and to download it, see the GHC webpage ([http://www.haskell.org/ghc/download_ghc_6_12_2.html](http://www.haskell.org/ghc/download_ghc_6_12_2.html)). We plan to do one more release from this branch before creating a new 6.14 stable branch.


GHC 6.12.2 will also be included in the upcoming Haskell Platform release ([http://hackage.haskell.org/platform/](http://hackage.haskell.org/platform/)). The Haskell platform is the recommended way for end users to get a Haskell development environment.

### Ongoing work


Meanwhile, in the HEAD, the last 6 months have seen more than 1000 patches pushed from more than a dozen contributors. As the following graph shows, tickets are still being opened faster than we can close them, with the total open tickets growing from around 700 to almost 800. We will be looking in the near future at improving the effectiveness of the way we use the bug tracker.

![](apr10/GHC_trac_tickets.png)

### Language changes


We have made only a few small language improvements.
The most significant ones concern quasi-quotations, implementing
suggestions from Kathleen Fisher:

- Quasi-quotes can now appear as a top-level declaration, or in a type, as well
  as in a pattern or expression.
- Quasi-quotes have a less noisy syntax (no "$").  


Here's an example that illustrates both:

```wiki
f x = x+1
[pads| ...blah..blah... |]
```


The second declaration uses the quasi-quoter called `pads` (which must
be in scope) to parse the "...blah..blah..", and return a list of
Template Haskell declarations, which are then spliced into the program
in place of the quote. 

### Type system


Type families remain the hottest bit of GHC's type system.  Simon
PJ has been advertising for some months that he intends to completely
rewrite the constraint solver, which forms the heart of the type
inference engine, and that remains the plan although he is being slow
about it.  The existing constraint solver works surprisingly
well, but we have lots of tickets demonstrating bugs in corner cases.
An upcoming epic (70-page) JFP paper "Modular type inference with local assumptions" 
brings together all the key ideas; watch Simon's home page.

### The mighty simplifier


One of GHC's most crucial optimisers is the Simplifier, which is
responsible for many local transformations, plus applying inlining and
rewrite-rules.  Over time it had become apparent that the
implementation of INLINE pragmas wasn't very robust: small changes in
the source code, or small wibbles in earlier optimisations, could mean
that something with an INLINE pragma wasn't inlined when it should be,
or vice versa.


Simon PJ therefore completely re-engineered the way INLINE pragmas are
handled:

- GHC now takes a "snapshot" of the original RHS of a function with an INLINE pragma.

- The function is now optimised as normal, but when the function is
  inlined it is the snapshot, not the current RHS, that is inlined.

- The function is inlined only when it is applied to as many arguments as the LHS of its original definition.  Consider;

  ```wiki
  f1, f2 :: Int -> Int -> Int
  {-# INLINE f1 #-}
  f1 x = \y -> <blah>
  {-# INLINE f2 #-}
  f2 x y = <blah>
  ```

  Here `f1` will be inlined when it is applied to one argument, but `f2` will only be inlined if it appears applied to two arguments.  This turns out to be helpful in reducing gratuitous code bloat.


Another important related change is this. Consider

```wiki
{-# RULE "foo" flip (flop x) = <blah> #-}
test x = flip y ++ flip y
  where
    y = flop x
```


GHC will not fire rule "foo" because it is scared about duplicating the redex
`(flop x)`.  However, if you declare that `flop` is CONLIKE, thus

```wiki
{-# NOINLINE [1] CONLIKE flop #-}
```


This declares that an application of `flop` is cheap enough that even a shared 
application can participate in a rule application.  The CONLIKE pragma is a modifier
on a NOINLINE (or INLINE) pragma, because it really only makes sense to match 
`flop` on the LHS of a rule if you know that `flop` is not going to be inlined
before the rule has a chance to fire.

### The back end


GHC's back end has been a ferment of activity.  In particular,

- David Terei made a LLVM back end for GHC [https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM Terei](https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM Terei).  It's not part of the HEAD, but we earnestly hope that it will become so.

- John Dias, Norman Ramsey, and Simon PJ made a lot of progress on Hoopl, our new representaion for control flow graphs, and accompanying functions for dataflow analysis and transformation.  There is a paper [http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/ Hoopl](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/ Hoopl), and Hoopl itself is now a standalone, re-usable Cabal package, which makes it much easier for others to use.


The downside is that the code base is in a state of serious flux:

- We still have two back-end pipelines, because we don't trust the new one to drop the old one.  See [NewCodeGen](commentary/compiler/new-code-gen).
- We are in the midst of pushing the new Hoopl into GHC.

### Runtime system work (SimonM)


There has been a lot of restructuring in the RTS over the past few months, particularly in the area of parallel execution.  The biggest change is to the way "blackholes" work: these arise when one thread is evaluating a lazy computation (a "thunk"), and another thread or threads demands the value of the same thunk.  Previously, all threads waiting for the result of a thunk were kept in a single global queue, which was traversed regularly.  This lead to two performance problems.  Firstly, traversing the queue is O(n) in the number of blocked threads, and we recently encountered some benchmarks in which this was the bottleneck.  Secondly, there could be a delay between completing a computation and waking up the threads that were blocked waiting for it.  Fortunately, we found a design that solves both of these problems, while adding very little overhead.


We also fixed another pathological performance case: when a large numbers of threads are blocked on an MVar and become unreachable at the same time, reaping all these threads was an O(n<sup>2</sup>) operation.  A new representation for the queue of threads blocked on an MVar solved this problem.


At the same time, we rearchitected large parts of the RTS to move from algorithms involving shared data structures and locking to a message-passing style.  As things get more complex in the parallel RTS, using message-passing let us simplify some of the invariants and move towards having less shared state between the CPUs, which will improve scaling in the long run.


The GC has seen some work too: the goal here is to enable each processor ("capability" in the internal terminology) to collect its private heap independently of the other processors.  It turns out that this is quite tricky to achieve in the context of the current architecture, but we have made some progress in this direction by privatising more of the global state and simplifying the GC data structures by removing the concept of "steps", while keeping a simple aging policy which is what steps gave us previously.

### Data Parallel Haskell


In the last months, our focus has been on improving the scalability of the [http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs Quickhull](http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs Quickhull) benchmark, and this work is still ongoing.  In addition, Roman has invested significant energy into the increasingly popular [ http://hackage.haskell.org/package/vector-0.6.0.1 vector package](http://hackage.haskell.org/package/vector-0.6.0.1 vector package) and the [ http://hackage.haskell.org/package/NoSlow NoSlow](http://hackage.haskell.org/package/NoSlow NoSlow) array benchmark framework.  Package vector is our next-gen sequential array library, and we will replace the current sequential array component (dph-prim-seq) with package vector sometime in the next few months.


We completed a first release of the regular, multi-dimensional array library introduced in the previous status report.  The library is called Repa and is available from Hackage [http://hackage.haskell.org/package/repa Repa package](http://hackage.haskell.org/package/repa Repa package). The library supports shape-polymorphism and works with both the sequential and parallel DPH base library.  We discuss the use and implementation of Repa in a draft paper [ http://www.cse.unsw.edu.au/\~chak/papers/KCLPL10.html Repa](http://www.cse.unsw.edu.au/~chak/papers/KCLPL10.html Repa).  We have shown that Repa can produce efficient and scalable code for FFT and relaxation algorithms and would be very interested to hear from early adopters who are willing to try Repa out in an application they care about.


At the start of the year, Ben Lippmeier has joined the project.  He has started to improve our benchmarks infrastructure and worked on Repa.

### Other miscellaneous stuff

- GHC makes heavy use of sets and finite maps.  Up till now it has used
  its own home-grown `UniqFM` and `FiniteMap` modules.  Milan Straka (visiting as
  an intern from the Czech Republic) has 

  - Made GHC use the `containers` package instead, which happily makes 
    compilation go a few percent faster.
  - Developed some improvments to `containers` that makes it go faster still.
    So `UniqFM` and `FiniteMap` are finally dead.  Hurrah for Hackage!

- The [http://research.microsoft.com/threadscope Threadscope](http://research.microsoft.com/threadscope Threadscope) tool for visualising parallel execution was released.  The tool is ripe for improvement in many ways, if you're interested in helping let us know

### Nightly builds


For some time, it's been clear to us that Buildbot is not the perfect tool for our nightly builds. The main problem is that it is very susceptible to network wibbles, which means that many of our builds fail due to a network issue mid-build. Also, any customisation beyond that anticipated by the configuration options provided requires some messy python coding, poking around inside the buildbot classes. Additionally, we would like to implement a "validate-this" feature, where developers can request that a set of patches is validated on multiple platforms before being pushed. We couldn't see an easy way to do this with buildbot.


When the darcs.haskell.org hardware was upgraded, rather than installing buildbot on the new machine, we made the decision to implement a system that better matched our needs instead. The core implementation is now complete, and we have several machines using it for nightly builds.


We're always keen to add more build slaves; please see [Builder](builder) if you're interested. Likewise, patches for missing features are welcome! The (Haskell) code is available at [http://darcs.haskell.org/builder/](http://darcs.haskell.org/builder/)

# Bibliography

- \[Builder\] The GHC builder package [https://gitlab.haskell.org/trac/ghc/wiki/Builder](https://gitlab.haskell.org/trac/ghc/wiki/Builder)

- \[Hoopl\] "Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation", Norman Ramsey, John Dias, and Simon Peyton Jones, submitted to ICFP'10.  [http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/](http://research.microsoft.com/en-us/um/people/simonpj/papers/c--/)

- \[NewCodeGen\] The glorious new code generator [https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/NewCodeGen](https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/NewCodeGen)

- \[NoSlow\] The NoSlow array benchmark framework [http://hackage.haskell.org/package/NoSlow](http://hackage.haskell.org/package/NoSlow)

- \[Quickhull\] The Quickhull DPH benchmark [http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs](http://darcs.haskell.org/packages/dph/examples/quickhull/QuickHullVect.hs)

- \[Repa\] "Regular, shape-polymorphic, parallel arrays in Haskell", Gabriele Keller, Manuel M. T. Chakravarty, Roman Leshchinskiy, Simon Peyton Jones, and Ben Lippmeier, submitted to ICFP'10. [http://www.cse.unsw.edu.au/\~chak/papers/KCLPL10.html](http://www.cse.unsw.edu.au/~chak/papers/KCLPL10.html)

- \[Repa package\] The Repa Cabal package [http://hackage.haskell.org/package/repa](http://hackage.haskell.org/package/repa)

- \[Terei\] The LLVM back end for GHC [https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM](https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Compiler/Backends/LLVM)

- \[Threadscope\] The Threadscope tool [http://research.microsoft.com/threadscope](http://research.microsoft.com/threadscope)

- \[vector package\] The vector Cabal package [http://hackage.haskell.org/package/vector-0.6.0.1](http://hackage.haskell.org/package/vector-0.6.0.1)
