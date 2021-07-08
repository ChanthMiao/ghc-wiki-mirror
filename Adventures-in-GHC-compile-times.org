# Table of Contents

1.  [What](#orgdd8bc8e)
2.  [Ideas](#orgb8d9a98)
    1.  [IntMap becomes unbalanced](#org7782028)
        1.  [Hypothesis](#orgbaca21f)
        2.  [Status](#orgcebbfa7)
        3.  [Evidence](#orge972291)
        4.  [The Fix](#orge53459a)
        5.  [Relevant Issues](#orgda45030)
        6.  [Relevant Merge Requests](#orgb90ee69)
        7.  [Courses of Action](#org7ce9740)
    2.  [IntMap `lookup` performs allocation](#orgbb4dd28)
        1.  [Hypothesis](#orgdc6e967)
        2.  [Status](#orgddfdba1)
        3.  [Evidence](#org826f279)
        4.  [The Fix](#orgc262c7e)
        5.  [Relevant Issues](#orge0fe75d)
        6.  [Relevant Merge Requests](#org1709652)
        7.  [Relevant Patches](#org231e0e0)
        8.  [Courses of Action](#orgf1c302c)
    3.  [Avoid allocations in substitutions in the simplifier](#org2266d42)
        1.  [Hypothesis](#orgfac5b8b)
        2.  [Status](#org56b0d9c)
        3.  [Evidence](#org0611ba5)
        4.  [The Fix](#org15cf9a6)
        5.  [Relevant Issues](#org92fe1d5)
        6.  [Relevant Merge Requests](#org7fe3821)
        7.  [Relevant Patches](#org22a0f36)
        8.  [Courses of Action](#orgfb5cc07)
    4.  [Optimize the pretty printing during code generation](#orgc85ab47)
        1.  [Hypothesis](#org98c4938)
        2.  [Status](#orga772cfc)
        3.  [Evidence](#org85c749d)
        4.  [The Fix](#orgd9b6ba1)
        5.  [Relevant Issues](#org69abc74)
        6.  [Relevant Merge Requests](#org5193ec0)
        7.  [Relevant Patches](#org7086e38)
        8.  [Courses of Action](#orged8a666)
3.  [Knowledge Sharing](#orgc4a8d6a)
    1.  [Is every IntMap necessary?](#org3aaac88)


<a id="orgdd8bc8e"></a>

# What

This page serves as a central repository for the effort to track and improve
GHC compiler performance, where performance in this context specifically means
build times. I (Jeff) will try to track ideas, issues, and relevant merge
requests for future work as well.


<a id="orgb8d9a98"></a>

# Ideas


<a id="org7782028"></a>

## IntMap becomes unbalanced


<a id="orgbaca21f"></a>

### Hypothesis

`IntMap` is used throughout the compiler, typically storing `Unique` for
keys. The idea here is that the `IntMap` is becoming very unbalanced. While
this isn't a problem in and of itself because under the hood `IntMap`'s are
Patricia trie in specific cases if the keys share a long prefix of bits
then the spine of the tree needs to be rebuilt for every insertion, thus
leading to performance degradation. See [this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/19820#note_351497) for a more precise
discussion.


<a id="orgcebbfa7"></a>

### Status

Unconfirmed. In progress, working on [novel intmap implementation](#org7ce9740)


<a id="orge972291"></a>

### Evidence

To gather evidence that the unbalancing is happening we need to either
inspect the heap or print the trees during a build. Any other ideas
appreciated.


<a id="orge53459a"></a>

### The Fix

The current `Unique` implementation is big endian, i.e., it stores the "key"
in the most significant bits of an `Int`. There are several paths forward:

1.  hash the keys to make the probability of a long prefix more unlikely
    thereby increasing
2.  move the keys to little endian to observe a difference


<a id="orgda45030"></a>

### Relevant Issues

-   <https://gitlab.haskell.org/ghc/ghc/-/issues/19820>
-   <https://gitlab.haskell.org/ghc/ghc/-/issues/18541#note_292432> see Sylvain
    Henry's comment on `IntMap.lookup`
-   <https://github.com/haskell/containers/pull/340#issuecomment-610400875>


<a id="orgb90ee69"></a>

### Relevant Merge Requests

(1) has been tried in [!5068](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5068) by [@sgraf812](https://gitlab.haskell.org/sgraf812), who implemented the key hash but
didn't fix compilation errors


<a id="org7ce9740"></a>

### Courses of Action

1.  Retrieve direct evidence of the tree becoming unbalanced
2.  Revive [!5068](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5068), fix the compilation errors and benchmark
3.  Create a patch with [novel IntMap implementation](https://github.com/haskell/containers/pull/340) and benchmark
4.  Use a mutable map for the bits of the compiler which are already in IO.
    This hasn't been tried but would be a significant change as we would have
    to add the `hashtables` package as a boot or core library (not sure
    which). Although [benchmarks by Chris Done](https://github.com/haskell-perf/dictionaries) indicate a 1000x speedup over
    pure IntMap's.
5.  Ed Kmett has also experimented with Clojure-style RRB trees in his [here](https://github.com/ekmett/transients).
    The implementation is unfinished and there are no benchmarks. We could
    finish the implementation, benchmark it against the standard `IntMap`, if
    it looks good then patch it into the compiler and benchmark.


<a id="orgbb4dd28"></a>

## IntMap `lookup` performs allocation


<a id="orgdc6e967"></a>

### Hypothesis

IntMap lookup performs allocation due to the key being specialized in its
definition. See SPJ's breakdown [here](https://gitlab.haskell.org/ghc/ghc/-/issues/20069).


<a id="orgddfdba1"></a>

### Status

Confirmed without direct evidence.


<a id="org826f279"></a>

### Evidence

By inspection of source code. Also noticed in [this comment](https://gitlab.haskell.org/ghc/ghc/-/issues/18541#note_292432), however not
confirmed with direct evidence. See Sebastian's [comment](https://gitlab.haskell.org/ghc/ghc/-/issues/20069#note_362952) about the `go`
closure.


<a id="orgc262c7e"></a>

### The Fix

Sylvain Henry has a patch [here](https://gitlab.haskell.org/ghc/ghc/-/issues/18541#note_292432) but only tested the intmap-benchmarks.


<a id="orge0fe75d"></a>

### Relevant Issues

-   <https://gitlab.haskell.org/ghc/ghc/-/issues/19820> The low-hanging fruit
    issue kicked off by Richard Eisenberg's ticky ticky profile.
-   <https://gitlab.haskell.org/ghc/ghc/-/issues/18541#note_292432> see Sylvain
    Henry's comment on `IntMap.lookup`
-   <https://gitlab.haskell.org/ghc/ghc/-/issues/20069> SPJ's IntMap issue


<a id="org1709652"></a>

### Relevant Merge Requests


<a id="org231e0e0"></a>

### Relevant Patches

-   see <https://gitlab.haskell.org/ghc/ghc/-/issues/18541#note_292432>


<a id="orgf1c302c"></a>

### Courses of Action

-   Implement and benchmark Sylvain Henry's patch, benchmark it for building
    entire packages not just the intmap-benchmark


<a id="org2266d42"></a>

## Avoid allocations in substitutions in the simplifier


<a id="orgfac5b8b"></a>

### Hypothesis

Benchmarking indicates that a large amount of allocations occur in the
simplifier. We should seek to understand why that is the case.


<a id="org56b0d9c"></a>

### Status

Unexplored


<a id="org0611ba5"></a>

### Evidence


<a id="org15cf9a6"></a>

### The Fix


<a id="org92fe1d5"></a>

### Relevant Issues

-   [Opportunity for increased sharing during substitution](https://gitlab.haskell.org/ghc/ghc/-/issues/19537)
-   [Annotating Core to avoid unnecessary traversal of large subexpressions](https://gitlab.haskell.org/ghc/ghc/-/issues/19538)


<a id="org7fe3821"></a>

### Relevant Merge Requests

-   Sylvain Henry implemented a fix only in `Tidy` in [!5267](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5267) but there is a bug
    and some variables aren't correctly renamed leading to test failures.


<a id="org22a0f36"></a>

### Relevant Patches


<a id="orgfb5cc07"></a>

### Courses of Action

1.  Read through [!5267](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5267)
2.  Fix [!5267](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5267) benchmark it. Try it out in `GHC.Core.substExpr` and
    `GHC.Core.TyCo.Subst`


<a id="orgc85ab47"></a>

## Optimize the pretty printing during code generation


<a id="org98c4938"></a>

### Hypothesis

Code generation is a significant chunk of compile time. According to Matt
Pickering some pretty printing functions perform a lot of allocation during
this phase which leads to a slow down.


<a id="orga772cfc"></a>

### Status

Unexplored


<a id="org85c749d"></a>

### Evidence


<a id="orgd9b6ba1"></a>

### The Fix

We'll need to optimize pretty printing. Exactly what needs optimization, and
how is to be determined.


<a id="org69abc74"></a>

### Relevant Issues


<a id="org5193ec0"></a>

### Relevant Merge Requests


<a id="org7086e38"></a>

### Relevant Patches


<a id="orged8a666"></a>

### Courses of Action

1.  benchmark pretty printing during code generation to identify candidate
    functions for optimization.
2.  Ticky profile these functions to get some hard evidence.


<a id="orgc4a8d6a"></a>

# Knowledge Sharing

It would be nice to know:


<a id="org3aaac88"></a>

## Is every IntMap necessary?

-   Consider this passage from Richard Eisenberg, in ghc-devs Vol215 issue 5:

    > One piece I'm curious about, reading this thread: why do we have so many IntMaps
    > and operations on them? Name lookup is a fundamental operation a compiler must
    > do, and that would use an IntMap: good. But maybe there are other IntMaps used
    > that are less necessary. A key example: whenever we do substitution, we track an
    > InScopeSet, which is really just an IntMap. This InScopeSet remembers the name
    > of all variables in scope, useful when we need to create a new variable name
    > (this is done by uniqAway). Yet perhaps the tracking of these in-scope variables
    > is very expensive and comprises much of the IntMap time. Might it be better just
    > to always work in a monad capable of giving fresh names? We actually don't even
    > need a monad, if that's too annoying. Instead, we could just pass around an
    > infinite list of fresh uniques. This would still be clutterful, but if it grants
    > us a big speed improvement, the clutter might be worth it.
    >
    > The high-level piece here is that there may be good things that come from
    > understanding where these IntMaps arise.