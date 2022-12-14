
GHC development continues to push forward with a new super-major release.
GHC 8.0.1 was released in May 2016 and has been surprisingly stable for a
release with so much churn. Nevertheless, there were issues, many of which will
soon be fixed with the 8.0.2 patchlevel release which is due to be released in
mid-November 2016.


Following 8.0.2 focus will turn to the quickly-approaching 8.2 release.
GHC 8.2.1, the first release in the 8.2 series, will likely be released in
February 2016. This release is largely intended to be a consolidation and bugfix
release, with significantly fewer new features than the last 8.0 release.
In particular, we have focused efforts on improving compilation speed.
Over the course of the release we have developed tools for
better tracking GHC's performance and used these tools to identify and resolve a
number of performance issues. The result is significant improvement in
compilation time and allocations on a large fraction of `nofib` tests.

# Major changes in GHC 8.2


While the emphasis of 8.2 is on performance, stability, and consolidation,
there are a few new features which will likely appear,

## Libraries, source language, and type system

- **Indexed `Typeable` representations**. While GHC has long supported runtime type reflection through the `Typeable` typeclass, its current incarnation requires care to use, providing little in the way of type-safety. For this reason the implementation of types like `Data.Dynamic` must be implemented in terms of `unsafeCoerce` with no compiler verification.

>
>
> GHC 8.2 will address this by introducing [indexed type representations](typeable), leveraging the type-checker to verify programs using type reflection. This allows facilities like `Data.Dynamic` to be implemented in a fully type-safe manner. See the [paper](http://research.microsoft.com/en-us/um/people/simonpj/papers/haskell-dynamic/) for an description of the proposal and the [ Wiki](https://ghc.haskell.org/trac/ghc/wiki/Typeable/BenGamari) for the current status of the implementation.
>
>

- **Backpack**: Backpack has merged with GHC, Cabal and cabal-install, allowing you to write libraries which are parametrized by signatures, letting users decide how to instantiate them at a later point in time. If you want to just play around with the signature language, there is a new major mode `ghc --backpack`; at the Cabal syntax level, there are two new fields `signatures` and `mixins` which permit you to define parametrized packages, and instantiate them in a flexible way. More details are on the [Backpack wiki page](backpack).

- **`deriving` strategies**: GHC now provides the programmer with a precise mechanism to distinguish between the three ways to derive type class instances: the usual way, the `GeneralizedNewtypeDeriving` way, and the `DeriveAnyClass` way. See the [DerivingStrategies wiki page](commentary/compiler/deriving-strategies) for more details.

- **New `base` classes**: The `Bifoldable`, and `Bitraversable` typeclasses are now included in the `base` library.

- **Unboxed sums**: GHC 8.2 has a new language extension, `UnboxedSums`, that enables unboxed representation for non-recursive sum types. GHC 8.2 doesn't use unboxed sums automatically, but the extension comes with new syntax, so users can manually unpack sums. More details can be found in [the wiki page](https://gitlab.haskell.org/trac/ghc/wiki/UnpackedSumTypes).

## Runtime system

- **Compact regions** ([paper](http://ezyang.com/papers/ezyang15-cnf.pdf)). This runtime system feature allows a referentially "closed" set of heap objects to be collected into a "compact region", allowing cheaper garbage collection, heap-object sharing between processes, and the possibility of inexpensive serialization.

- **Better profiling support**: The cost-center profiler now better integrates with the GHC event-log. Heap profile samples can now be dumped to the event log, allowing heap behavior to be more easily correlated with other program events.

- **More robust DWARF output**: GHC's support for DWARF debugging information has been gradually stabilizing over the last few releases. While GHC 8.0 was a significant improvement over 7.10, a number of infelicities in the implementation rendered it unsafe for production use. GHC 8.2 will hopefully be the first release where DWARF debugging can be considered stable.

>
>
> With stable stack unwinding support comes a number of opportunities for new serial and parallel performance analysis tools (e.g. statistical profiling) and debugging. As GHC's debugging information improves, we expect to see tooling developed to support these applications. See the [DWARF status page](https://ghc.haskell.org/trac/ghc/wiki/DWARF/80Status) for futher information.
>
>

- **Better support for NUMA machines**: Machines with non-uniform memory access costs are becoming more and more common as core counts continue to increase. The runtime system is now better equipped to efficiently run on such systems.

- **Experimental changes to the scheduler** that enable the number of threads used for garbage collection to be lower than the `-N` setting.

- **Support for `StaticPointers` in GHCi**. At long last programs making use of the `StaticPointers` language extension will have first-class interpreter support.

- **Improved idle CPU usage**: A long-standing regression resulting in unnecessary wake-ups in an otherwise idle program was fixed. This should lower CPU utilization and improve power consumption for some programs.

## Miscellaneous

- **Compiler Determinism**: GHC 8.0.2 is the first release of GHC which produces deterministic interface files. This helps consumers like `nix` and caching build systems, and presents new opportunities for compile-time improvements.

- **Hadrian**: 8.2 will hopefully be the first release to ship with Hadrian, our new Shake-based build system developed by Andrey Mokhov and his collaborators. This new build system is significantly more maintainable than our aging `make`-based system, will lead to improved compilation times on some platforms, and is better equipped to incorporate features like build artifact caching. If you???re interested in helping out, take a look at the list of issues that are blocking the merge into GHC: [https://github.com/snowleopard/hadrian/issues/239](https://github.com/snowleopard/hadrian/issues/239).
