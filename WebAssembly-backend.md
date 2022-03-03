# WebAssembly backend

This page is a working document about adding a WebAssembly backend to GHC.

## Relevant links

- [WebAssembly goals](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-goals): bullet-pointed list of what WebAssembly support means, and how the goals may be accomplished.
- [Project milestones](https://terrorjack.notion.site/Project-Milestones-02-22-2022-edition-9b751df9025a46d1b68b077083c83683): a living notion document that has finer grained milestones. The linked kanban board is also available publicly.
- [Draft MR](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7632): the central working branch. This is for unreg codegen for now, our wasm32 NCG will be delivered in a separate MR.

## FAQ

### Which timeframe?

We aim for 9.6.

### Which process?

We have a central working branch and associated draft MR against `master`. Whenever we identify some self-contained changeset that’s stable enough, we’ll discuss and open smaller MRs.

There will be certain changesets that are inevitably large (e.g. the whole `wasm32` NCG once finished). We won’t attempt to split those into smaller pieces though, but they are expected to come later.

Another problem is submodules. We have patches against some boot libs that are out-of-tree. When we feel confident those patches are stable enough, we will submit PRs, and bump submodule commit in our working branch accordingly.

### How does the wasm backend interact with the on-going plans to make GHC runtime retargetable? Is the current plan for wasm to merge before runtime retargetability, therefore requiring the user to build a new compiler to use the backend?

Yes. We don’t expect to interact with RTS retargetability for the time being. To use wasm backend, users will need to do their own build (although we expect to do bindist releases at some point for better user experience)

### Is the expectation that the wasm backend passes the entirety of the testsuite; if not, will it be a separate testsuite way?

Yes. But of course, a lot of existing test cases won’t work due to missing system capability or RTS features. We expect to leverage the existing skip feature of testsuite, identify and annotate the wont-work cases so they are skipped when we’re testing against wasm.

### Is the wasm RTS going to be merged into the tree?

Good news is, there’s no separate wasm RTS! The existing C RTS will be cross-compiled to wasm to implement all RTS functionality (that’s also the most crucial strategic shift in the asterius project)

But there will indeed be some patches to RTS, build system and such, to add wasm-specific logic.

### Is the RTS going to support the full feature surface supported by GHC's C RTS?

Yes! Well, terms and conditions apply, due to constraints imposed by the wasm and wasi spec.

### How are "ways" interpreted in the context of wasm? For instance, "dyn" makes little sense in this context. Depending upon the RTS implementation, the same might be said of -debug, -threaded, and -eventlog

The way convention of wasm are expected to be identical to native archs.

We won’t implement `dyn` way for now, given the non-trivial workload to implement dynamic linker for wasm.

The threaded RTS won’t work yet, we’ll focus on the non-threaded RTS for the time being. Other flavours like debug, eventlog, prof, etc should work.

### What are we going to do about GHC's many native-code-centric flags (e.g. -fPIC, -shared, -msse2). Will these be errors if used when targetting wasm?

We can either ignore them, issue a warning or panic. Either way it’ll be trivial to implement, so it’s not worth concerning.

### Are there plans to support the Ticky-Ticky profiler? Cost-centre profiling?

They work out of the box already.

### Is there a design for JavaScript FFI? Have we considered whether this should go through the GHC Proposals Process given that it is essentially adding new syntax?

This needs thorough discussion with the GHCJS folks.

### What is the plan for handling the various C bits in base/text/bytestring/etc.? Specifically, has there been any motion on discussions with upstreams regarding upstreaming the wasm implementations?

They mostly work out of the box already. In some cases some patching is still needed (e.g. `simdutf` in `text`), we’ll propose PRs accordingly.

### Will GHC need to be aware of the system's wasm runtime (e.g. know the path to wasmtime)? Presumably yes as we will at very least need to know how to start iserv.

This is the duty of the `iserv` implementation for wasm. It may pick up the runtime from `PATH`, or expect the runtime to be configured by GHC build system and available in `settings`, either way is trivial to implement.

### What needs to happen to put proper wasm backend support in Cabal? Is upstream aware of this plan? Presumably c-sources support will require wasi-sdk support in both Cabal and GHC?

We expect most existing logic in `Distribution.Simple.GHC` work out of the box, as long as the `wasm32-wasi-ghc` and `wasm32-wasi-ghc-pkg` executables are properly configured.