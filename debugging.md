# Debugging


This part of the wiki collects all the information related to debugging GHC. That includes debugging the compiler itself, the libraries, the runtime system, the code generator, or the build system.

- If you're debugging a compiler panic or some problem in GHC itself, then go to [Debugging/Compiler](debugging/compiler)

- If the compiled program crashes or panics, then go to [Debugging/CompiledCode](debugging/compiled-code)

- For debugging the runtime system, see [Debugging/RuntimeSystem](debugging/runtime-system)

- For debugging when running under GHCi, see [Commentary/Compiler/Backends/GHCi](commentary/compiler/backends/GHCi)

- Performance debugging. 

  - [Debugging/ProfilingGhc](debugging/profiling-ghc): Profiling the compiler itself.  
  - [Debugging/TickyTicky](debugging/ticky-ticky): for debugging performance-related issues in compiled code.  Typically for performance debugging of the Simplifier and Core-level optimisations.
  - [Debugging/CacheGrind](debugging/Cachegrind): Identify hot spots with instruction counts per line of GHC source code.
  - [Debugging/LowLevelProfiling](debugging/low-level-profiling): way to investigate low-level performance, typically for performance debugging of the code generator or RTS.
  - [Debugging/CabalTest](debugging/cabal-test): the "`Cabal` test" for comparing performance of GHC builds

- Build failures.  If you're trying to debug a build failure, then you probably want to look at

  - [Building/Troubleshooting](building/troubleshooting): Fixing common problems in a GHC build
  - [Building/Modifying](building/modifying#debugging): Debugging the build system

- Use [count_lines](debugging#) to count the number of lines of code in the compiler

- Use [compareSizes](debugging#comparesizes) to compare the sizes of corresponding .o or .hi files in two trees.

- If know the last good commit, [bisection](bisection) may be useful to identify which commit introduced the bug.

---

## compareSizes


The `compareSizes` program compares the sizes of corresponding files in two trees:

```wiki
$ ./compareSizes --hi ~/ghc/darcs/ghc ~/ghc/6.12-branch/ghc
        Size | Change | Filename
      25644 | -0.99% | compiler/stage1/build/Demand.hi
      21103 | -0.98% | compiler/stage2/build/Demand.hi
     180044 | -0.98% | libraries/base/dist-install/build/GHC/Classes.hi
       6415 | -0.58% | .../Data/Array/Parallel/Prelude/Base/Tuple.hi
       6507 | -0.57% | .../Data/Array/Parallel/Prelude/Base/Tuple.hi
   [...]
       3264 |  3.16% | .../Parallel/Unlifted/Sequential/Flat/Enum.hi
      51389 |  3.30% | .../build/Language/Haskell/Extension.hi
       1415 | 72.18% | libraries/base/dist-install/build/Data/Tuple.hi
   28752162 | -0.00% | TOTAL
```


Flags:

- `--o` to compare object files.
- `--hi` to compare interface files \[DEFAULT\]


There's a hack to avoid descending into '\*_split' directories. 


The source for `compareSizes` is in `$(TOP)/utils/compare_sizes`.