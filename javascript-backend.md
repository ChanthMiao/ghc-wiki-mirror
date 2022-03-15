This page is a working document about adding a JavaScript backend to GHC.

This feature has been implemented in [GHCJS](https://github.com/ghcjs/ghcjs) and already has users. GHCJS relies on a GHC fork and our proposal is to integrate GHCJS into GHC.

The tracking ticket is: <https://gitlab.haskell.org/ghc/ghc/-/issues/21078>

Old doc: https://www.haskell.org/haskell-symposium/2013/ghcjs.pdf

## FAQ

### Which timeframe?

Initially we were targeting 9.4 but it may be more realistic to target 9.6.

### Which process?

Merging upstream whatever can be useful/tested independently of the rest.

Otherwise we maintain two branches `wip/javascript-backend`, and `wip/js-staging`. `wip/javascript-backend` is the branch into which MRs are merged until we feel that the entire story is well-enough developed to be confidently merged upstream. `wip/js-staging` is real wip code that once finalized is merged into `wip/javascript-backend`. 

To summarize:
- `wip/javascript-backend`: reviewed code, regularly rebased onto master
- `wip/js-staging`: wip code, only rebased or changed at stable points, once we are happy merge into `wip/javascript-backend` 

### How does the JS backend interact with the on-going plans to make GHC runtime retargetable? Is the current plan for JS to merge before runtime retargetability, therefore requiring the user to build a new compiler to use the backend?

There is no interaction. Currently even with GHCJS we need two compilers (one for the JS target and one for the host) so having a single multi-target compiler isn't mandatory.

In the long term, having the ability to select the backend without rebuilding the compiler would probably improve the user experience though.

### Is the expectation that the JS backend passes the entirety of the testsuite; if not, will it be a separate testsuite way?

It should be a separate testsuite way as there are some features that are not expected to pass (e.g. support for `compact`).

### Is the JS RTS going to be merged into the tree?

Yes. Not necessarily in the `rts/` directory (perhaps `rts-js` ?) Currently GHCJS loads its JS RTS from its collection of JS files ("shims") we don't want to upstream this feature. Instead we would like to make the JS RTS a proper package/unit.

### Is the RTS going to support the full feature surface supported by GHC's C RTS?

Probably not (e.g. no plan for `compact` support afaik). Do we have a list of GHC's C RTS features that we could use to indicate supported features?

### How are "ways" interpreted in the context of Javascript? For instance, "dyn" makes little sense in this context. Depending upon the RTS implementation, the same might be said of `-debug`, `-threaded`, and `-eventlog` (cf #20741)

GHCJS also has different RTS "ways" (e.g. to enable some assertions). Additionally we could imagine generating different code for different targets (NodeJS, browsers, etc.).

Currently GHCJS just ignore "ways" (e.g. it returns the same code for both objects with `-dynamic-too`). Until #20741 is properly fixed, we could check command-line flags when the JS backend is enabled to disallow `-dynamic`, `-dynamic-too`, etc.

As we have workarounds, we don't plan to work on #20741 until we have a usable JS backend. But we might work on it after it's done. Any help or feedback on #20741 is welcome.

### What are we going to do about GHC's many native-code-centric flags (e.g. -fPIC, -shared, -msse2). Will these be errors if used when targetting Javascript?

There are currently ignored by backends that don't use them. We could probably detect and report their use during command-line flags validation but it is out of the scope of the JS backend implementation.

### Are there plans to support the Ticky-Ticky profiler? Cost-centre profiling?

I don't see why we wouldn't support them (cost centre profiling has been supported in GHCJS before, but a few pieces are broken, ticky-ticky profiling hasn't been supported yet)

In addition, we could also support JS specific debugging to harness browser's debugging tools.

### Is there a design for Javascript FFI? Have we considered whether this should go through the GHC Proposals Process given that it is essentially adding new syntax?

This point needs to be discussed, especially with people working on a WebAssembly backend for GHC as ideally they should both use the same FFI interface.

Perhaps worth a ghc-proposal.

TODO: document current GHCJS and Asterius interfaces

### What is the plan for handling the various C bits in base/text/bytestring/etc.? Specifically, has there been any motion on discussions with upstreams regarding upstreaming the Javascript implementations?

We plan to propose patches for these libraries. We can use CPP to condition JS specific code to `HOST_OS=ghcjs` and similarly in `.cabal` files.

### #21078 lists "one-shot JS code generator" as one of its tasks. I suspect I am misunderstanding but my first interpretation of this phrase suggests that `--make` mode will not be supported.

We do plan to support `--make` mode. It's just that one task to get there is first to support compiling a single module into a single `.o` without any linking step, which we have summarized as "one-shot".

### Will GHC need to be aware of the system's Javascript interpreter (e.g. know the path to nodejs)? Presumably yes as we will at very least need to know how to start `iserv`.

At first it doesn't need to because users (or cabal/stack) can use `-pgmi` and `-opti` command line flags to setup the external interpreter to use.

In the longer term, it would probably be better for the external interpreter to be configured per target in a settings file. Probably something to discuss in the context of [#19877](https://gitlab.haskell.org/ghc/ghc/-/issues/19877).

### What needs to happen to put proper JS backend support in Cabal? Is upstream aware of this plan? Presumably `c-sources` support will require emscripten support in both Cabal and GHC?

This needs to be given some thoughts.

Currently Cabal doesn't support cross-compilation. We need it to support two toolchains: one for the host (plugins, Setup.hs), one for the target. It also needs to use the correct platform to resolve conditionals in `.cabal` files.

Need input from Luite about c-sources support with emscripten. We should also ensure that whatever solution we implement can also handle WebAssembly C compiler.

### Template Haskell support

The current Template Haskell support in the standalone GHCJS has two issues

    * It uses a non-standard Template Haskell runnner, GHC gained its own `iserv` external interpreter in the meantime
    * The GHCJS TH runner links JavaScript in the compiler, then sends it to the Template Haskell runner (`thrunner.js`) as a message. This occasionally runs into size limitations in node.js

A good solution fixes both problems. It has to integrate with iserv, but we still have some room for decisions:

    * We could have a full bytecode interpreter running in JavaScript, running most of the code in bytecode.
    * We could reuse most of the `iserv` infrastructure, but run compiled JS code (like `-fobject-code`). We'd have to fix the linker to get around the size issue.