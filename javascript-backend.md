This page is a working document about adding a JavaScript backend to GHC.

This feature has been implemented in [GHCJS](https://github.com/ghcjs/ghcjs) and already has users. GHCJS relies on a GHC fork and our proposal is to integrate GHCJS into GHC.

- tracking ticket: <https://gitlab.haskell.org/ghc/ghc/-/issues/21078>
- target release: 9.6
- process: development happens in [js-staging](https://gitlab.haskell.org/ghc/ghc/-/tree/wip/js-staging) branch (regularly rebased on `master`). We hope to get it merged when the following conditions are met:
  1. it can produce a working HelloWorld example
  2. the branch is clean enough (documentation, etc.)
  3. it passes reviews

## Build Requirements
The following have to be on your `$PATH`.
- emscripten version 3.14 or better
- llvm 15, or an llvm with a series of specific patches, see the FAQ for errors to expect if you don't have these patches.

## Build Instructions
The JS backend uses Hadrian. The build is similar to vanilla GHC, just slower and with some changes to the configure step.

### Boot and configure
Run, configure should take longer than normal because its being run through `emscripten`:
```
[nix-shell:~/programming/ghc]$ ./boot && emconfigure ./configure --target=js-unknown-ghcjs
```

Notice that `emconfigure` wraps the `configure` script _and_ its arguments. This was not the case in early versions of the JS backend (we didn't use `emconfigure` at all). But after a lot of pain we agreed it was the most portable way to set emscripten binaries for autoconf. A successful configure should point to `emscripten` wrapped binaries for `ar` `nm` `clang` and friends, like so:
```
----------------------------------------------------------------------
Configure completed successfully.

   Building GHC version  : 9.5.20220819
          Git commit id  : 08c3c4783c72d3173d79ccda2ac282e2d3e04e34

   Build platform        : x86_64-unknown-linux
   Host platform         : x86_64-unknown-linux
   Target platform       : js-unknown-ghcjs

   Bootstrapping using   : /nix/store/4bkmkc7c98m4qyszsshnw9iclzzmdn4n-ghc-9.2.3-with-packages/bin/ghc
      which is version   : 9.2.3
      with threaded RTS? : YES

   Using (for bootstrapping) : /nix/store/yzs8390walgk2rwl6i5li2g672hdn0kv-gcc-wrapper-11.3.0/bin/cc
   Using clang               : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
      which is version       : 15.0.0
      linker options         : 
   Building a cross compiler : YES
   Unregisterised            : NO
   TablesNextToCode          : YES
   Build GMP in tree         : NO
   hs-cpp       : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
   hs-cpp-flags : -E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs
   ar           : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emar
   ld           : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
   nm           : /nix/store/0dp0bfg9sncg7bjy389zwyg2gskknm6b-emscripten-llvm-3.1.15/bin/llvm-nm
   objdump      : /nix/store/zgvxnf9047rdd8g8kq2zxxm9k6kfqf8b-binutils-2.38/bin/objdump
   ranlib       : /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emranlib
   otool        : otool
   install_name_tool : install_name_tool
   windres      : 
   dllwrap      : 
   genlib       : 
   Happy        : /nix/store/ijdmyaj6i6hgx5ll0lxxgcm9b0xn8nma-happy-1.20.0/bin/happy (1.20.0)
   Alex         : /nix/store/qzgm2m7p7xc0fnyj4vy3jcmz8pvbg9p7-alex-3.2.6/bin/alex (3.2.6)
   sphinx-build : /nix/store/27dk5i52465a4azjr2dqmrhyc0m4lpf2-python3.9-sphinx-4.5.0/bin/sphinx-build
   xelatex      : /nix/store/8jc2258h4nqzqjy303zzkssd3ip675pf-texlive-combined-2021/bin/xelatex
   makeinfo     : /run/current-system/sw/bin/makeinfo
   git          : /nix/store/vsr2cn15h7cbwd5vqsam2ab2jzwfbyf9-git-2.36.0/bin/git
   cabal-install : /nix/store/cjmd2qv1b5pdw4lxh1aw4xwwy4ibnb2p-cabal-install-3.6.2.0/bin/cabal

   Using LLVM tools
      clang : clang
      llc   : llc
      opt   : opt

   HsColour was not found; documentation will not contain source links

   Tools to build Sphinx HTML documentation available: YES
   Tools to build Sphinx PDF documentation available: YES
   Tools to build Sphinx INFO documentation available: YES
----------------------------------------------------------------------
```

You can also double check this output in `ghc/hadrian/cfg/system.config`:
```
# This file is processed by the configure script.
# See hadrian/src/UserSettings.hs for user-defined settings.
#===========================================================

# Paths to builders:
#===================

alex           = /nix/store/qzgm2m7p7xc0fnyj4vy3jcmz8pvbg9p7-alex-3.2.6/bin/alex
ar             = /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emar
autoreconf     = /nix/store/yfnhm2b6plv48i8sgl64sd148b48hcly-autoconf-2.71/bin/autoreconf
cc             = /nix/store/p894nlicv53firllwgrfxfi51jzckh5l-emscripten-3.1.15/bin/emcc
...
```

Notice that each of the binaries very obviously point to the `emscripten` wrapped versions. We're now ready to start building.

### Build the compiler
Build using Hadrian, no surprises here:
```
[nix-shell:~/programming/ghc]$ hadrian/build -j12 --flavour=quick-js --bignum=native --docs=none
```

That should take around 30 minutes to complete. `hsc2hs` should be very slow (we're sorry) because its running `emcc` several times. Similarly, expect the `CCS.hs` module itself to take minutes (again, we're sorry! I (Jeff) promise we have a ticket for it). Once that's done you need to export the path to the JS shims and `jsbits` in the base library. This will be the case until we finish patching the `js-sources` field in cabal. So run:
```
[nix-shell:~/programming/ghc]$ export JS_RTS_PATH=~/programming/ghc/js/

[nix-shell:~/programming/ghc]$ export JS_BASE_PATH=/home/doyougnu/programming/ghc/libraries/base/jsbits/
```
These environment variables are checked by the JS backend linker. The linker will error out if they are not set properly. Now you should have a working Haskell to JavaScript compiler. Good luck!


## FAQ

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

## Building Issues and how to fix them

### Configure fails with: sub-word sized atomic operations not available

#### The error output
The error looks like this:
```
[nix-shell:~/programming/haskell/ghc]$ ./boot && ./configure --target=js-unknown-ghcjs
...
checking version of gcc... checking version of gcc... 15.0.0
15.0.0
checking whether CC supports -no-pie... no
checking whether C compiler supports __atomic_ builtins... yes
checking whether -latomic is needed for sub-word-sized atomic operations... failed
configure: error: sub-word-sized atomic operations are not available.
```

#### The cause and fix
If you got this error you are most likely using `nix` and `ghc.nix`. The error is caused by `emscripten` trying to write to the `emscripten_cache`, but it cannot write because the `nix store` is read-only. 

The fix is hacky, we need to provide the `emscripten_cache` to emscripten so that it is mutable. Similarly, setting `EM_CACHE` to the cache in the nix store does not work. The workaround, in nix, is to copy the cache, make it writable and export `EM_CACHE` to point to the cache. This is done in the following lines in `ghc.nix`:

```
# we have to copy from the nixpkgs because we cannot unlink the symlink and copy
# similarly exporting the cache to the nix store fails during configuration
# for some reason
cp -Lr ${emscripten}/share/emscripten/cache .emscripten_cache
chmod u+rwX -R .emscripten_cache
export EM_CACHE=.emscripten_cache
```
See [this](https://discourse.nixos.org/t/emscripten-tries-to-write-to-nix/15263) post for more.

### JS Backend fails during stage1 with error: symbol memset missing .functype

#### The error output
This will fail during stage1 with:
```
/run/user/1729/ghc3035129_0/ghc_1.s:528:2: error:
     error: symbol memset missing .functype
            call    memset
            ^
    |
528 |         call    ...
```

#### The cause and fix
This error is a bug in `llvm` which `emscripten` uses to generate JS through wasm (via `wasm2js`). The fix is to use a more recent version of `llvm` or a patched version, and a more recent version of `emscripten` (at least `3.1.x`). Whichever you choose the llvm you use must include [this](https://github.com/llvm/llvm-project/commit/2368f18eb305ae9d5a4f2110c048e5daf5007992) patch, and that llvm must be the llvm that `emscripten` uses. See also [this](https://github.com/llvm/llvm-project/issues/53712) issue. Thus far this has only been problematic for nix users. If that is you please see [this](https://github.com/NixOS/nixpkgs/pull/182840) PR on nixpkgs. Other distros, such as Ubuntu have worked out of the box due to a patched llvm included in their package manager.

## References

- Haskell Symposium 2013 paper: [demo proposal](https://www.haskell.org/haskell-symposium/2013/ghcjs.pdf)
- HIW2015: [slides](https://wiki.haskell.org/wikiupload/9/95/HIW15-Stegeman-The_State_of_the_GHCJS.pdf)
- State of WebGHC 2019 (compared with GHCJS): https://webghc.github.io/2019/01/18/state-of-webghc-january-2019.html