## Overview

There is a plan at https://gitlab.haskell.org/ghc/ghc/-/issues/14095 to improve the incrementality of compilation by splitting the compilation of each module between typechecking and desugaring. This would provide faster feedback to programmers about what went wrong with their build, and hopefully improve compile times.

Simultaneously, IOHK would like to have more useful intermediate files in order to resume compilation after typechecking with a separate compiler backend.

This wiki page can hopefully serve as a point of reference so that we don't step on each other's toes and generally coordinate the overlap in these efforts, and make it apparent to all GHC developers what we're working toward.

## Detailed Roadmap

### Phase 1: Parallelise `--make` alone, don't worry about `.hi` files, use existing in-memory data structures

This Phase is being implemented as detailed in https://gitlab.haskell.org/ghc/ghc/-/issues/14095

#### Some ideas for cleanup of Recompilation avoidance code
 
      1. https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3204 We get rid of `(RecompRequired, Maybe ModIface)` and replace it with a new datatype to ensure that when recompilation is not required, we do in fact have an iface.

      2. https://gitlab.haskell.org/ghc/ghc/-/issues/18205 currently we sometimes "censor" the existence of an old iface when we need to recompile, and sometimes don't. We tried just being consistent: always have the old iface to calculate the old fingerprint if it exists and we are rebuilding. However, that broke tests -- evidently the seemingly arbitrary state of diagnostic information (whether we have the old fingerprint) actually affects the "business logic" of what recompilation we do. This is no good. We should fix the code so the "should we recompile?" logic in one place is strictly sufficient, and it's safe to always have the old fingerprint.

         - Once the above is fixed, `GHC.Iface.Recomp.checkVersions` should not return an iface. It is currently either `Nothing` or the same iface that was passed in, with the choice based on the spooky magic the issue documents. Instead, it can simply return whether to recompile, and the (sole) caller bundles the iface as evidence.

      3. `GHC.Iface.Recomp.checkVersions` should not have side effects. It currently does some hackery with the external package state in order to make interface lookups look at the correct file (.hi versus .hi-boot). The comment says that this is furthermore only needed in one-shot mode, and not in `--make`. This `checkVersions` function is morally pure. Let's make it that way by avoiding this fiddling with external state.

         - In moving the EPS stuff elsewhere, we might have a chance to better disentangle batch mode and one shot mode, which are currently wrestling each other in a way that just leaves both parties saddled with extra complexity and confusion.

      4. Loading in one place. `GHC.Iface.Recomp.*` currently loads interfaces based on things changing, i.e. it's somewhat push-based. Other code loads interfaces on demand - fully pull based. Should we make sense of this in a more consistent way? @Ericson2314 hopes so, which is why `checkOldIface` !3204 still returns the whole interface even though it's one caller just needs the fingerprint. Maybe it will make sense to revisit that, but John didn't want to change it in !3204.

 

#### Future work that can be done

      - Backpack? Backpack already does something akin to split type checking and codegen today, and we should integrate with that.
        - https://gitlab.haskell.org/ghc/ghc/-/issues/10871 is the original phase 2 "richer hi files" issue. Directly solving it is a phase two thing, but perhaps we can anticipate it in the all in-memory case.

      - Multiple home units? If the main thrust of this is finished, we can make sure fine-grained `--make` works across units.
        - Probably needed to do the prior step in a non-adhoc way
        - Chance to delete the `.bkp` file special cases, if we can do multi-home unit, with sigs / delayed code gen, in the regular case.

### Phase 2: Write intermediate data structures needed to resume compilation in hi files. Write separate hi files per phase to avoid contention.

   0. There exists a [branch by Ed Yang](https://github.com/ezyang/ghc/commit/13615ca4e4bf759f323de22a3d182b06c4050f38) which implements fat interfaces from which compilation can be resumed. We probably want to determine how helpful this might be, if at all. The description mentions resuming typechecking after loading the fat interface, so it's unclear whether these fat interfaces are the right ones for our split, though they may be after all.

   1. Work out where to cut the compilation process for best results. Between typechecking and desugaring? Between desugaring and Core->Core?

   2. Figure out what data actually need to go into the finer-grained .hi files. The general goal will be to be able to resume compilation with after the point at which we stopped compilation earlier.

   3. Figure out a format for GHC output that allows us to add new interface files without disturbing tools that only care about the final interface files too much (e.g. a directory). If this is a significant change, it may require work in downstream tools to support it (e.g. cabal). Can be done in parallel with earlier steps. (Discussion: all _file_ formats shouldn't require changes in Cabal, and a directory format is a small (but breaking) change to make it recursively copy the directories for `install`.)

   4. Actually implement serialisation and the driver code to use it.

### Phase 3: Use the new capabilities in downstream tools

   1. Teach build tools (e.g. cabal) to make use of the additional available parallelism.

      - The IDE work, with it's experimentation with shake-ifying the driver, might derived the most benefit from this work, as the separated concerns and fine-grained building blocks from the "cleanup" stage should make this experimentation easier and more fruitful.

   2. Come up with a workflow for external tools (e.g. plugins) to write and read their own extra data into the interface directory (or whatever it is).


### Notes about .hi files splitting (for backup, copied from 14095)

The first step is to split `hi` files just as we split `ModDetails`: there should be one file (e.g. `hi-tc` corresponding to `ModDetailsTc`) and another file (Maybe just `hi` for now) corresponding to `ModDetailsCg`. Then, there is the issue of the guts. We need to ge the result of type checking and also serialize it to disk, so the downstream invocation for the same module can pick up the work. It would be really cool if we could actually use `hie` files for this, as they store the whole type-checked AST.

Probably the easier part is splitting the `hi` file, as tracked in #17843. We won't be able to leverage the parallelism without complicated blocking on other files being written (since the `*Guts` can only be conveyed in memory) that I don't think it worth it to write and then throw away, but I think that's OK. It's good to keep up with the `ModDetails` split for sake of code quality and keeping the batch mode and one-shot code from drifting apart further.