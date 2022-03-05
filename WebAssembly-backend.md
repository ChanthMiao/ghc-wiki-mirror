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

## Questions from IRC, ghc-devs, etc

### Does it make sense to have both a JS and wasm backend? AFAIK JS can interoperate with wasm.

GHCJS is much more tested in real commercial projects, so we (wasm people) believe it makes sense.

### So is this new repo totally replacing the Asterius repo on GitHub?

Yes. But it'll take more work to implement JS interoperability that's supported in Asterius right now.

### Have you considered using LLVM's wasm32 target, instead of implementing wasm32 NCG? Given LLVM's support for wasm looks promising?

Yes. We even considered modifying the existing LLVM pipeline, so that we only emit LLVM bitcode as object files, to take advantage of LLVM LTO.

We did a simple experiment: enforce the no-lto/thin-lto/full-lto flag globally (in wasi-sdk's sysroot build, and in ghc's unreg codegen, given that's what we have for now). Conclusions are somewhat counter-intuitive: LTO brings no visible benefit to run-time performance, even slightly increases wasm code size, and has very significant link-time performance penalty. So it seems emitting plain wasm object files instead of LLVM bitcode is a saner choice.

Even without considering LTO, one may still prefer LLVM codegen to avoid needing to write NCG, or to bet LLVM's quality of compile-time optimization. But note that there're also downsides:

- Compile-time is slow. This is a known problem for other targets as well.
- It's complex. There are parts (e.g. mangler) that doesn't make sense for wasm, wired-in assumptions (e.g. global regs on the stack) that may emit slower code, etc.

The "complex" part is really what made us decide to go for an NCG in the beginning: we can tune a lot of runtime conventions (cmm tail calls, register passing, etc) by ourselves, compile-time will be much faster, and most importantly, we have much more knowledge about how wasm works, compared to how LLVM works (let alone how GHC's LLVM codegen works!!). So we believe trading the extra workload to get predictability of our work process is totally worth it.

Also, having an NCG for wasm32 doesn't salt the earth and prevent LLVM codegen from being improved to work better for wasm32! It's still possible that LLVM codegen will emit faster code. Just keep in mind it's not a silver bullet everywhere :)

### What about WebGHC?

We are aware of previous efforts in WebGHC and appreciate the exploratory work a lot! Still, we decide to do things differently in a lot of places, including but not limited to:

- The syscall layer. WebGHC relies on webabi to emulate various linux syscalls in JavaScript, but we choose WASI standard. You shouldn't pay for JavaScript when you don't use it. We also believe modifying the RTS to fit in WASI is more reliable than emulating syscalls and make unmodified RTS work on that layer.
- How to do blocking syscalls, or more broadly speaking, how the Haskell thread model fits in JavaScript concurrency model when targetting the web. WebGHC uses either Web Workers or binaryen asyncify to emulate blocking behavior, both of which come with extra overhead. We'd like to avoid those overhead, and most importantly, waiting for async JS imports to return shouldn't block other threads, the two different concurrency models should work seamlessly. Same with the previous point, this involves quite some modifications to GHC RTS.
- Choice of underlying C/C++ toolchain. WebGHC uses emscripten, while we go for wasi-sdk.
- WebGHC disables the libffi dependency, at the cost of not supporting dynamic foreign exports. We preserve that ability using our libffi wasm32-wasi port.
- WebGHC Template Haskell only work via compiling twice and running native code. We'll have our iserv that runs wasm code to do Template Haskell, which is a more principled approach.
- NCG!