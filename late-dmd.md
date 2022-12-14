# Late Demand analysis


Notes about running demand analysis a second time, late in the pipeline.

## Status


Commits

- [c080f727ba5f83921b842fcff71e9066adbdc250](/trac/ghc/changeset/c080f727ba5f83921b842fcff71e9066adbdc250/ghc) - discussed by this page
- [\[a7920ef6eefa5578c89b7cda0d6be207ee38c502/testsuite\]](/trac/ghc/changeset/a7920ef6eefa5578c89b7cda0d6be207ee38c502/testsuite) - I updated tests' expected behavior in the presence of c080f727
- [e4a1d2d0a71bf335a04eaf93deb440b709f9430e](/trac/ghc/changeset/e4a1d2d0a71bf335a04eaf93deb440b709f9430e/ghc) - SPJ finished what c080f727 started by further simplifying things I hadn't recognized as possible
- [34728de0f059d8e076981448392203f2501aa120](/trac/ghc/changeset/34728de0f059d8e076981448392203f2501aa120/ghc) - I updated the documentation and a source Note for -flate-dmd-anal and -ffun-to-thunk


The -flate-dmd-anal flag runs the demand analysis a second time just before CorePrep, with a subsequent run of the Simplifier.  Cf #7782.


It's not on by default yet, but we hope -O2 will eventually imply it, perhaps even for the GHC 7.8 release. UPDATE (2018-02-21): nofib results with `-flate-dmd-anal` were not conclusive, see #6087 for results.


The bulk of this patch merely simplifies the treatment of wrappers in interface files; see "Removing the clever .hi files scheme" below.

### TODO

- Ask the performance czars and community for help in determining if we should make -O2 imply -flate-dmd-anal.

- That might involve investigating the more-reliable-looking slowdowns in the New performance numbers section. No slow down was apparent on both platforms (so far), but a couple looked reliable on a given platform. eg typecheck on the big server showed the same slowdown regardless of -flate-dmd-anal on the nofib tests (ie same in 10 and 11) and also regardless of mode=norm or mode=slow. Thus it smells like some change in a library function that the main loop of typecheck uses consistently is getting a slowdown. But it's very hard to tell from the numbers and it takes a lot of time to investigate that sort of thing.

  - To proceed: perhaps measure mode=slow on the MacBook Pro. Also build the libraries with ticky on the big server to search for the hypothetical library function that is slowing down typecheck.

### Relation to other tickets


There are some tickets documenting runtime bugs that can be cleaned up by running the demand analyzer (followed by a simplifier run) a second time at the end of the pipeline: #4941, #5302, #6087. #6070 ? Others?

---

## Removing the clever .hi files scheme


Running the demand analyzer twice breaks some expectations of the .hi file format. Prior to this commit, GHC regenerated the wrapper's body from the its strictness signature and worker id. Now, instead, the body is simply encoded just like any other InlineStable.


This change???

1. simplifies a special case; there's plenty of knock-on code elimination from no longer having ids in UnfoldingSource,
1. increases the size of .hi files (see below),
1. accordingly increases compile time a bit (eg \~ +1% over nofib),
1. accommodates the late demand analysis (see below)
1. similarly accommodates the -ffun-to-thunk flag


Simplifying the .hi scheme was the easiest way to enable `-flate-dmd-anal` and make `-ffun-to-thunk` safe to use. **It is possible to revert back to the clever .hi scheme**. It will however require some care in order to safely interoperate with `-flate-dmd-anal`, `-ffun-to-thunk`, and any future work that similarly effects the accuracy of the clever .hi file scheme's regeneration phase.

### Effect on .hi file size


The comparison in this section page uses [ef017944600cf4e153aad686a6a78bfb48dea67a](/trac/ghc/changeset/ef017944600cf4e153aad686a6a78bfb48dea67a/ghc) as the base commit ??? after measuring, I rebased my patch to apply it to [33c880b43ed72d77f6b1d95d5ccefbd376c78c78](/trac/ghc/changeset/33c880b43ed72d77f6b1d95d5ccefbd376c78c78/ghc)


Removing the clever .hi file scheme for wrappers results as expected in an increase of .hi file size.


In $TOPDIR/libraries, there's an extra 569,509 bytes of .hi file.


Here's the files with a growth \>10K.

```wiki
(bytes growth,file)
(11103,"base/dist-install/build/GHC/Arr.hi")
(12479,"template-haskell/dist-install/build/Language/Haskell/TH/Lib.hi")
(12756,"binary/dist-install/build/Data/Binary/Class.hi")
(15727,"random/dist-install/build/System/Random.hi")
(29348,"base/dist-install/build/Data/Data.hi")
(30497,"template-haskell/dist-install/build/Language/Haskell/TH/Syntax.hi")
(37081,"Cabal/Cabal/dist-install/build/Distribution/PackageDescription.hi")
(64200,"ghc-prim/dist-install/build/GHC/Classes.hi")
```


Here's the files with a growth \>10%.

```wiki
(0.10163132137030995,"Cabal/Cabal/dist-install/build/Distribution/Simple/Bench.hi")
(0.1067165410638649,"hoopl/dist-install/build/Compiler/Hoopl/XUtil.hi")
(0.11125552378476736,"base/dist-install/build/Control/Monad.hi")
(0.11311653959856854,"time/dist-install/build/Data/Time/Calendar/Private.hi")
(0.12166183143643532,"transformers/dist-install/build/Data/Functor/Compose.hi")
(0.1584435579816642,"hoopl/dist-install/build/Compiler/Hoopl/Combinators.hi")
(0.21422422135168143,"ghc-prim/dist-install/build/GHC/Classes.hi")
```

### Main Benefit of Removal


The clever .hi scheme caused CoreLint errors when combined with -flate-dmd-anal. Trying to build GHC with -flate-dmd-anal on the libraries incurs a panic

```wiki
ghc-stage1: panic! (the 'impossible' happened)
  (GHC version 7.7.20130830 for x86_64-apple-darwin):
	applyTypeToArgs
    Expression: base:GHC.Real.$weven{v reB} [gid]
                  @ a{tv a13L} [tv]
                  ww_a5VQ{v} [lid]
                  ww_a5VU{v} [lid]
                  ww_a5W7{v} [lid]
                  w_a5VK{v} [lid]
    Type: forall a{tv a2aJ} [tv].
          base:GHC.Real.Real{tc 2e} a{tv a2aJ} [tv] =>
          (a{tv a2aJ} [tv] -> a{tv a2aJ} [tv] -> a{tv a2aJ} [tv])
          -> a{tv a2aJ} [tv] -> ghc-prim:GHC.Types.Bool{(w) tc 3c}
    Args: [TYPE a{tv a13L} [tv], ww_a5VQ{v} [lid], ww_a5VU{v} [lid],
           ww_a5W7{v} [lid], w_a5VK{v} [lid]]

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug
```


The issue here is that the second demand analysis in GHC.Real refines the strictness of GHC.Real.even.

```wiki
first Str=DmdType <S(SLLLLLLLL),U(U,A,A,U,A,A,A,A,A)>

                  <L,U>

second Str=DmdType <S(S(LS(SLLLLLLL)L)LLLLLLLL),
                    1*U(1*U(U(A,A,A,A,A,A,U),
                            U(U,A,A,A,A,A,A,A),
                            A),
                    A,
                    A,
                    1*C1(C1(U)),
                    A,A,A,A,A)>

                   <L,U>
```


Since there are three leaf Us in the first signature, the worker's type takes **three** value arguments. But the second strictness for GHC.Real.even is recorded in the .hi file, and it has four leaf Us. So when the importing module regenerates the body of the GHC.Real.even wrapper, it applies the worker to **four** value arguments. Boom.


Similar to -flate-dmd-anal, abandoning the clever .hi scheme lets us safely import code compiled with/without -ffun-to-thunk from a module compiled without/with -ffun-to-thunk.

- Compile A.hs with -ffun-to-thunk
- Compile a file B.hs that imports A.hs without -ffun-to-thunk


If demand analysis removes all the value arguments from a function f in A.hs and B.hs uses that function, compilation of B.hs will crash. The problem is that the regeneration of the body of f in B will attempt to apply f to a `realWorld#` argument because there is no -ffun-to-thunk flag. However, f no longer accepts any arguments, since it was compiled with -ffun-to-thunk. Boom.

---

## Effect on .hi file size and .a file size


The comparison in this section page uses [ef017944600cf4e153aad686a6a78bfb48dea67a](/trac/ghc/changeset/ef017944600cf4e153aad686a6a78bfb48dea67a/ghc) as the base commit ??? after measuring, I rebased my patch to apply it to [33c880b43ed72d77f6b1d95d5ccefbd376c78c78](/trac/ghc/changeset/33c880b43ed72d77f6b1d95d5ccefbd376c78c78/ghc)


The second demand analysis generates more worker/wrapper splits, so it also generates larger .hi files and larger .o files. The numbers in this section measure the difference between `-O2 -flate-dmd-anal` and `-O2 -fno-late-dmd-anal`. This is on my 64 bit Mac OS X.



It's based on the size of the .hi and .a files in $TOPDIR/libraries.


<table><tr><th>                   </th>
<th>.hi bytes</th>
<th>.a bytes
</th></tr>
<tr><th>no late-dmd</th>
<th>                  </th>
<th>                  
</th></tr>
<tr><th>     late-dmd</th>
<th>                  </th>
<th>                  
</th></tr>
<tr><th>  difference  </th>
<th> +552,057   </th>
<th> +684,696 
</th></tr></table>


These are the big .hi changes over 10K.

```wiki
(growth bytes,  module)
(35807,"base/dist-install/build/Data/Data.hi")
(54562,"template-haskell/dist-install/build/Language/Haskell/TH/Syntax.hi")
(59000,"Cabal/Cabal/dist-install/build/Distribution/PackageDescription.hi")
(69900,"template-haskell/dist-install/build/Language/Haskell/TH/Lib.hi")
```


These are the big .hi changes over 10%.

```wiki
(growth%,  module)
(0.10158001494608733,"haskeline/dist-install/build/System/Console/Haskeline/Command.hi")
(0.10499966324675034,"hoopl/dist-install/build/Compiler/Hoopl/MkGraph.hi")
(0.11207246180884142,"haskeline/dist-install/build/System/Console/Haskeline/Command/Undo.hi")
(0.11254620966637761,"transformers/dist-install/build/Control/Applicative/Lift.hi")
(0.11394046020649104,"base/dist-install/build/GHC/Event/Thread.hi")
(0.11417453220731909,"dph/dph-lifted-base/dist-install/build/Data/Array/Parallel/PArray/Reference.hi")
(0.11493796526054591,"hoopl/dist-install/build/Compiler/Hoopl/XUtil.hi")
(0.11842105263157894,"dph/dph-prim-seq/dist-install/build/Data/Array/Parallel/Unlifted/Sequential/Extracts.hi")
(0.1252496671105193,"base/dist-install/build/Control/Concurrent/QSemN.hi")
(0.13623208379272325,"base/dist-install/build/Numeric.hi")
(0.174892616905746,"haskeline/dist-install/build/System/Console/Haskeline/Backend/DumbTerm.hi")
(0.17564356435643563,"base/dist-install/build/Data/Ratio.hi")
(0.1764402762032361,"base/dist-install/build/Control/Concurrent/QSem.hi")
(0.2952895972676818,"dph/dph-lifted-copy/dist-install/build/Data/Array/Parallel/Lifted/TH/Repr.hi")
(0.3762859126952084,"template-haskell/dist-install/build/Language/Haskell/TH/Lib.hi")
```


These are the big .a changes over 10K.

<table><tr><th> growth bytes </th>
<th> module
</th></tr>
<tr><th>-19408</th>
<th>libHSdph-prim-par-0.8.0.1.a 
</th></tr>
<tr><th>-16976</th>
<th>libHSdph-prim-seq-0.8.0.1.a 
</th></tr>
<tr><th>10440</th>
<th>libHShoopl-3.10.0.0.a 
</th></tr>
<tr><th>11120</th>
<th>libHStransformers-0.3.0.0.a 
</th></tr>
<tr><th>11472</th>
<th>libHSold-time-1.1.0.1.a 
</th></tr>
<tr><th>22584</th>
<th>libHStime-1.4.0.2.a 
</th></tr>
<tr><th>30168</th>
<th>libHSdph-lifted-copy-0.8.0.1.a 
</th></tr>
<tr><th>35224</th>
<th>libHSvector-0.9.1.a 
</th></tr>
<tr><th>44448</th>
<th>libHScontainers-0.5.0.0.a 
</th></tr>
<tr><th>48408</th>
<th>libHShaskeline-0.7.0.4.a 
</th></tr>
<tr><th>115104</th>
<th>libHStemplate-haskell-2.9.0.0.a 
</th></tr>
<tr><th>120936</th>
<th>libHSbase-4.7.0.0.a 
</th></tr>
<tr><th>237088</th>
<th>libHSCabal-1.17.0.a 
</th></tr></table>

---

## Performance numbers


These numbers in this section come from [c080f727ba5f83921b842fcff71e9066adbdc250](/trac/ghc/changeset/c080f727ba5f83921b842fcff71e9066adbdc250/ghc), building the libraries/nofib tests with various combinations of `-fno-late-dmd-anal` and `-flate-dmd-anal`.


I use these abbreviations in the following tables

```wiki
00 - no late dmd analysis on either libs or nofib tests
10 - late demand analysis on libs, but not on nofib tests
11 - late demand analysis on both libs and nofib tests
```

`build.mk` included

```wiki
SRC_HC_OPTS     = -O -H64m
GhcStage1HcOpts = -O -fasm
GhcStage2HcOpts = -O2 -fasm
GhcHcOpts       = -Rghc-timing
GhcLibHcOpts    = -O2

SplitObjs          = NO

DYNAMIC_BY_DEFAULT   = NO
DYNAMIC_GHC_PROGRAMS = NO
```


The changes in binary size were the same on my two tests platforms so far (both 64-bit). It looks like essentially we're seeing the effects of an increase in the size of the base library. The smallest programs increased by +1.1% in both 10 and 11. Other programs usually had \~0.1% difference in 10 and 11. nucleic2 has about a +1% from 10 to 11, but that is a known anomaly ?????cf the discussion in "old performance numbers" below.

```wiki
Binary Sizes

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
        -1 s.d.                -----           +0.4%           +0.4%
        +1 s.d.                -----           +0.7%           +0.7%
        Average                -----           +0.6%           +0.6%
```

### 64-bit MacBook Pro


2.7Ghz Core i7 MacBook Pro, 16GB memory, 64-bit.
One processor with two cores; each core has 25 KB L2 cache, with a (shared) 4MB L3 cache.

#### mode=norm NoFibRuns=30

```wiki
Allocations

-- NB nucleic2 and cryptarithm2 are explained in the "Old performance numbers" section below.

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
       cichelli             80307264           +0.0%          -22.9%
        mandel2              1041544           +0.0%          -21.4%
reverse-complem            150153040          -13.2%          -13.2%
          fasta            401153024           -9.1%           -9.1%
      integrate            474063360           +0.0%           -5.1%
   k-nucleotide           4125099504           -0.0%           -4.8%
        knights              1968072           +0.0%           -3.8%
         fulsom            323486224           +0.0%           -2.6%
      transform            696343224           +0.0%           -2.4%

       -- everything else changed less

       nucleic2             87567072           +0.0%           +3.4%
   cryptarithm2             24028936           +0.0%           +4.2%

        -1 s.d.                -----           -1.9%           -4.8%
        +1 s.d.                -----           +1.5%           +3.1%
        Average                -----           -0.2%           -0.9%
```

```wiki
Run Time

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
           life                 0.23          -13.0%          -13.0%

       -- everything else changed less

   binary-trees                 0.61           +6.3%           +5.9%

        -1 s.d.                -----           -3.5%           -4.1%
        +1 s.d.                -----           +2.9%           +2.3%
        Average                -----           -0.4%           -0.9%
```

```wiki
Elapsed Time

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
      compress2                 0.23          -14.2%          -17.7%
      typecheck                 0.20           +2.0%           -8.9%
           life                 0.26          -12.3%           -6.2%
         simple                 0.24           -9.0%           -4.9%

       -- everything else changed less

            hpg                 0.21           -1.9%           +6.7%
reverse-complem                 0.27          +13.5%          +12.8%

        -1 s.d.                -----           -5.7%           -5.6%
        +1 s.d.                -----           +4.2%           +4.3%
        Average                -----           -0.9%           -0.8%
```

### Dual 64-bit Xeon


Two processors, each 2.40GHz Xeon E5620, 12MB cache, 48GB memory, 64-bit.  cf [http://ark.intel.com/products/47925](http://ark.intel.com/products/47925), both processors have four cores (so eight "threads" via Hyper-Threading).

#### mode=norm NoFibRuns=30

```wiki
Allocations

-- NB nucleic2 and cryptarithm2 are explained in the "Old performance numbers" section below.

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
       cichelli             80307264           +0.0%          -22.9%
        mandel2              1041544           +0.0%          -21.4%
reverse-complem            150153040          -13.2%          -13.2%
          fasta            401153024           -9.1%           -9.1%
      integrate            474063360           +0.0%           -5.1%
   k-nucleotide           4125099504           -0.0%           -4.8%
        knights              1968072           +0.0%           -3.8%
         fulsom            323486224           +0.0%           -2.6%
      transform            696343224           +0.0%           -2.4%
            ida            128551480           +0.0%           -1.2%
        parstof              3102544           +0.0%           -1.4%
         simple            226411568           -0.0%           -1.0%

       -- everything else changed less

           bspt             12285840           +0.0%           +1.2%
       nucleic2             87567496           +0.0%           +3.4%
   cryptarithm2             24028936           +0.0%           +4.2%

        -1 s.d.                -----           -1.9%           -4.8%
        +1 s.d.                -----           +1.5%           +3.1%
        Average                -----           -0.2%           -0.9%
```

```wiki
Run Time


-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
         simple                 0.27           -2.6%           -6.4%
      transform                 0.39           -1.3%           -5.1%
          fasta                 0.59           -2.5%           -4.7%

       -- everything else changed less

          kahan                 0.30           +3.6%           +3.9%
   binary-trees                 0.88           +7.2%           +6.9%
      typecheck                 0.24           +8.3%           +8.3%
         hidden                 0.49           +4.1%          +10.2%

        -1 s.d.                -----           -1.7%           -3.0%
        +1 s.d.                -----           +2.9%           +3.5%
        Average                -----           +0.6%           +0.2%
```

```wiki
Elapsed Time

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
         simple                 0.27           -2.6%           -6.8%
      transform                 0.39           -1.3%           -5.1%
          fasta                 0.59           -2.7%           -3.7%

       -- everything else changed less

   binary-trees                 0.88           +7.3%           +6.9%
      typecheck                 0.24           +8.3%           +8.3%
         hidden                 0.49           +4.1%          +10.1%

        -1 s.d.                -----           -1.6%           -2.9%
        +1 s.d.                -----           +3.1%           +3.6%
        Average                -----           +0.7%           +0.3%
```

#### mode=slow NoFibRuns=30

```wiki
Allocations

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
       cichelli             80307264           +0.0%          -22.9%
        mandel2              1041544           +0.0%          -21.4%
reverse-complem           1500677840          -13.2%          -13.2%
          fasta           4005660304           -9.1%           -9.1%
      integrate            948063920           +0.0%           -5.1%
   k-nucleotide          41144014840           +0.0%           -4.9%
         fulsom            323486224           +0.0%           -2.6%
      transform           1389145136           +0.0%           -2.4%
         genfft           1796463848           +0.0%           -1.2%
            ida            733628984           +0.0%           -1.0%
        parstof              3102544           +0.0%           -1.4%
         simple            226411568           -0.0%           -1.0%

       -- everything else changed less

           bspt             12285840           +0.0%           +1.2%
       nucleic2             87567496           +0.0%           +3.4%
   cryptarithm2             24028936           +0.0%           +4.2%

        -1 s.d.                -----           -1.9%           -4.7%
        +1 s.d.                -----           +1.5%           +3.1%
        Average                -----           -0.2%           -0.9%
```

```wiki
Run Time

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
         mandel                 0.22           -9.1%           -9.1%
      transform                 0.80           -0.3%           -8.7%
reverse-complem                 1.39           -5.9%           -6.1%
         simple                 0.26           -1.4%           -5.2%
          fasta                 5.84           -3.9%           -4.2%
    gen_regexps                 1.01           -4.6%           -4.7%

       -- everything else changed less

      paraffins                 1.00           +0.2%           +3.4%
      typecheck                 0.49          +10.2%           +8.2%
         hidden                 0.49           +4.1%          +10.2%

        -1 s.d.                -----           -2.6%           -3.3%
        +1 s.d.                -----           +2.9%           +2.7%
        Average                -----           +0.1%           -0.3%
```

```wiki
Elapsed Time

-------------------------------------------------------------------------------
        Program                   00              10              11
-------------------------------------------------------------------------------
         mandel                 0.22           -9.1%           -9.1%
      transform                 0.80           +0.0%           -8.5%
reverse-complem                 1.39           -5.9%           -5.8%
         simple                 0.27           -2.1%           -5.2%
          fasta                 5.86           -3.9%           -4.2%
    gen_regexps                 1.01           -4.5%           -4.6%

       -- everything else changed less

      paraffins                 1.00           +0.2%           +3.7%
      typecheck                 0.49          +10.2%           +8.2%
         hidden                 0.49           +4.5%          +10.2%

        -1 s.d.                -----           -2.6%           -3.2%
        +1 s.d.                -----           +2.9%           +2.8%
        Average                -----           +0.1%           -0.3%
```

---

## Old performance numbers


NB These were from April 2013.


Here's the effects on nofib. Run time didn't seem to change as drastically.  The "X/Y" column headers mean "library-flags/test-flags" given to GHC when compiling the respective bit.

```wiki
Allocations

-------------------------------------------------------------------------------
        Program                O2/O2     late-dmd+O2/O2    late-dmd+O2/late-dmd+O2
-------------------------------------------------------------------------------
   cryptarithm2             25078168           +0.0%           +8.0%
       nucleic2             98331744           +0.0%           +3.2%

       -- everything else changed less

       cichelli             80310632           +0.0%          -22.9%
          fasta            401159024           -9.1%           -9.1%
         fulsom            321427240           +0.0%           -2.6%
   k-nucleotide           4125102928           -0.0%           -4.8%
        knights              2037984           +0.0%           -3.7%
        mandel2              1041840           +0.0%          -21.4%
        parstof              3103208           +0.0%           -1.4%
reverse-complem            155188304          -12.8%          -12.8%
         simple            226412800           -0.0%           -1.0%
```


All other changes less than 1% allocation.
Note that it improves a couple tests significantly just via changes in the base libraries.


For cryptarithm2, (cf remarks in #4941)

- 4% increase allocation is due to reboxing
- 4% is due to dead closures, because the fix in #4962 isn't working for some reason.


For nucleic2, in var_most_distant_atom, an let-bound function is inlined after w/w, and hence grows numerous closures by a significant amount. I'm not sure where to lay the blame for this. Note however, that just making nucleic2's data types use strict !Float fields changes its allocation -72.4%, so maybe this "bad practice" corner case is a small issue.
