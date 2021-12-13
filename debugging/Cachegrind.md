With the addition of DWARF symbols in GHC 8.10 we can use low level profiling techniques to debug GHC and other Haskell programs. This wiki page will walk you through using one of these tools, [cachegrind](https://valgrind.org/docs/manual/cg-manual.html), to debug compiler performance.

### Why cachegrind?
Cachegrind is part of the `valgrind` tool suite, it simulates an input program on a virtual machine to inspect that program's interactions with CPU caches. These interactions can be very important and can kill what is seemingly good code.

### What kind of information does this give me
Cachegrind in particular is useful because it returns line-by-line haskell source code annotated with instruction counts, and CPU cache information, such as instruction cache misses, and data read and write misses. **This is particularly useful for finding hot spots in GHC and your code**

### I'm completely new here, links?
- [This post](https://www.tweag.io/blog/2021-11-04-haskell-profiling-cachegrind/) gives a good overview of `cachegrind` on a simple haskell program, and how to read the cachegrind report
- [This post](http://igoro.com/archive/gallery-of-processor-cache-effects/) discusses some interesting cache effects, how to measure them, and the amount of performance impact they can have on your code. Read it, it is worth it!
- [The cachegrind manual](https://valgrind.org/docs/manual/cg-manual.html) also has good examples and describes the cachegrind report in detail.

### The Scenario
Imagine you have a performance MR open on GHC and you notice that the CI shows something like the following:

![image](uploads/a2e513382d449fff6db12c198fa3ab86/image.png)

Now you want to figure out what is going on. Here is the process:
1. Create two `ghc` directories, one for `master` one for your branch. I'll call these `ghc-master` and `ghc`.

2. Build both `ghc-master` and `ghc` with `debug_info` and `ticky_ghc`. If you're using `hadrian` it will look like:
```
hadrian/build clean && ./boot && ./configure && hadrian/build -j --flavour=default+debug_info+ticky_ghc
```

3. Pick a test case that shows a large regression, the larger the better. For example, `T9961` shows a regression over `100%` which means we should get a good signal from using it.

4. Go to `ghc-master` directory and have your `ghc-master` build the test case with:
```
$ cd ghc-master
$ valgrind --tool=cachegrind _build/stage1/bin/ghc -g -O2 -fforce-recomp -ddump-to-file -ddump-simpl -ddump-stg-final ./testsuite/tests/perf/compiler/T9961.hs
```
Some extra words of caution:
- **Ensure CPU frequency scaling is off, and your CPU is quiet, this can impact the measurement!** (I recommend [cpupower](https://wiki.archlinux.org/title/CPU_frequency_scaling#Disabling_Turbo_Boost) to check. See also [this](https://easyperf.net/blog/2019/08/02/Perf-measurement-environment-on-Linux#8-use-statistical-methods-to-process-measurements) article on getting consistent benchmarking results on Linux)
- The flag **-g** is required to produce the DWARF symbols that `valgrind` will use. **If you forget -g, or you don't build with debug_info, you'll get a report of only the RTS and a lot of ??? symbols**

5. Repeat (4) for `ghc` instead of `ghc-master`. 

6. From 4, and 5 you should have a file called `cachegrind.out.XXXX` in the `ghc` and `ghc-master` root directories. `XXXX` was the `pid` of the cachegrind process, so don't expect these to be the same between `ghc` and `ghc-master`.

7. call the helper utility `cg_annotate` on your `cachegrind.out.XXXX` files. I (Jeff) usually dump these to a more descriptive name to keep `ghc-master` vs branch straight:
```
$ cd /path/to/ghc-master
$ cg_annotate cachegrind.out.7849 > ghc-master.cachegrind
$ cd /path/to/ghc
$ cg_annotate cachegrind.out.1810173 > ghc-foldl.cachegrind   ## or ghc-<my-experiment>.cachegrind
```
You can also pipe the report into `vim` for a quick look:
```
cg_annotate cachegrind.out.1810173 | vim -
```

Now you should have two `cachegrind` reports to compare. Here are the things to look at:
1. The overall instruction count, if you see a large increase in `ghc` compared to `ghc-master` then this means you've successfully detected a change!
2. The table of instruction count and information by function. For example, in https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7005 only code in `OccurAnal.hs` changed, thus I only focused on the results prepended with `OccurAnal...`. This table is also useful for zeroing in on a hot spot.
3. The last section of the cachegrind report is the main event. It is a line-by-line annotated haskell source code with instruction count and cache information. For example, in [this comment](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7005#note_393748)

### How do I know when I've found something

Differences of a few thousand instructions should not be much of a concern. Differences of a few hundred thousand indicate a hot spot. For example, consider the differences in instruction count from cachegrind reports from MR!7005:

##### ghc-master
```
--------------------------------------------------------------------------------
-- Auto-annotated source: /store/Programming/ghc//compiler/GHC/Core/Opt/OccurAnal.hs
--------------------------------------------------------------------------------
Ir                  I1mr             ILmr         Dr                 D1mr            DLmr           Dw                 D1mw            DLmw           
         .                .            .                  .               .              .                  .               .              .           occAnalRecBind :: OccEnv -> TopLevelFlag -> ImpRuleEdges -> [(Var,CoreExpr)]
         .                .            .                  .               .              .                  .               .              .                          -> UsageDetails -> WithUsageDetails [CoreBind]
         .                .            .                  .               .              .                  .               .              .           -- For a recursive group, we
         .                .            .                  .               .              .                  .               .              .           --      * occ-analyse all the RHSs
         .                .            .                  .               .              .                  .               .              .           --      * compute strongly-connected components
         .                .            .                  .               .              .                  .               .              .           --      * feed those components to occAnalRec
         .                .            .                  .               .              .                  .               .              .           -- See Note [Recursive bindings: the grand plan]
       585 ( 0.00%)      49 ( 0.00%)  18 ( 0.01%)       303 ( 0.00%)     16 ( 0.00%)     6 ( 0.00%)        97 ( 0.00%)      1 ( 0.00%)     1 ( 0.00%)  occAnalRecBind !env lvl imp_rule_edges pairs body_usage
   109,354 ( 0.00%)     327 ( 0.00%) 122 ( 0.05%)    41,236 ( 0.00%)  2,818 ( 0.01%)    19 ( 0.00%)    25,032 ( 0.00%)     57 ( 0.00%)    12 ( 0.00%)    = foldr (occAnalRec rhs_env lvl) (WithUsageDetails body_usage []) sccs
         .                .            .                  .               .              .                  .               .              .             where

```

##### MR!7005
```
--------------------------------------------------------------------------------
-- Auto-annotated source: /store/Programming/ghc//compiler/GHC/Core/Opt/OccurAnal.hs
--------------------------------------------------------------------------------
Ir                  I1mr             ILmr         Dr                 D1mr            DLmr           Dw                 D1mw            DLmw           
         .                .            .                  .               .              .                  .               .              .           occAnalRecBind :: OccEnv -> TopLevelFlag -> ImpRuleEdges -> [(Var,CoreExpr)]
         .                .            .                  .               .              .                  .               .              .                          -> UsageDetails -> WithUsageDetails [CoreBind]
         .                .            .                  .               .              .                  .               .              .           -- For a recursive group, we
         .                .            .                  .               .              .                  .               .              .           --      * occ-analyse all the RHSs
         .                .            .                  .               .              .                  .               .              .           --      * compute strongly-connected components
         .                .            .                  .               .              .                  .               .              .           --      * feed those components to occAnalRec
         .                .            .                  .               .              .                  .               .              .           -- See Note [Recursive bindings: the grand plan]
         .                .            .                  .               .              .                  .               .              .           -- See Note [Strict left folds in Occurance Analysis]
   287,856 ( 0.00%)   2,797 ( 0.01%) 204 ( 0.08%)   147,492 ( 0.00%)    904 ( 0.00%)    47 ( 0.00%)    63,235 ( 0.00%)  4,819 ( 0.03%)    20 ( 0.00%)  occAnalRecBind !env lvl imp_rule_edges pairs body_usage
   110,381 ( 0.00%)     144 ( 0.00%)  43 ( 0.02%)    47,441 ( 0.00%)    989 ( 0.00%)     0             31,414 ( 0.00%)      5 ( 0.00%)     1 ( 0.00%)    = foldr' (occAnalRec rhs_env lvl) (WithUsageDetails body_usage []) sccs
         .                .            .                  .               .              .                  .               .              .             where
```

Notice that we can observe the change that was made: `foldr` in `occAnalRecBind` on `ghc-master` becomes `foldr'` in MR!7005, specifically in commit 17b49f49acf502d9f712d962ab95d4c5d239a590, and that this change induced over `200k` _more_ instructions associated to `occAnalRecBind` (see the `Ir` column on the left most side). Definitely a regression, indeed in commit 21318a24fe215facbbabfbbbb5503a071581c2b9 this was reverted and allocations returned to normal.

Differences of a `.` to a few hundred thousand or more are definitely worth investigating. A `.` means that the events `cachegrind` tracks were not applicable to the line in question. So a change from a `.` to a lot of instructions means a lot of new events began originating from code generated from that line.

### Great tool and technique, when do I use it?
Use this technique in conjunction with [tickyticky](https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging/ticky-ticky) profiling to zero in on the source of the performance regression. [This comment](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7005#note_393748) shows the triage:
1. Do a tickyticky profile to identify problematic candidates
2. Then check the core output to see if anything sticks out.
3. Now run cachegrind to further narrow down your search by instruction count.

May the debugging gods smile upon your soul!