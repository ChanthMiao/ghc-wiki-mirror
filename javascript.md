# Compiling Haskell to JavaScript

JavaScript platforms have become the de facto way to distribute portable programs: almost every system has an up-to-date JavaScript VM installed: a Web browser. The “JavaScript problem” is that we don’t want to use JavaScript – a programming language with many deficiencies – but we have to in order to use JavaScript platforms. This document is about one solution to this problem, namely compiling a better language (Haskell) to target JavaScript platforms.

Some deficiencies of JavaScript platforms have been recognized and addressed by adding JavaScript features (e.g. support for tail recursion optimisation), by recognizing specific patterns in JS codes (cf asm.js), or by extending JavaScript platforms with a different language (WebAssembly). In addition, some JavaScript platforms (e.g. NodeJS) supports native extensions, hence we could imagine extensions dedicated to supporting some aspects of JavaScript codes generated from Haskell.

Using JavaScript extensions may offer different trade-offs (e.g. performance vs portability). As we don’t believe there will be a definite answer for every use-case, we will try to keep this document up to date with developments of JavaScript platforms and with their induced trade offs for a Haskell compiler targeting them.

# Approaches

## GHCJS / JS backend

**Target: JavaScript**.  A JavaScript backend for GHC was pioneered in the GHCJS project. It provided its own RTS implemented in JavaScript.

- RTS: provides its own JS implementation of a RTS
- Maximal portability: full control of the generated JavaScript code, hence may avoid more recent JS features
- Native JS backend effort: https://gitlab.haskell.org/ghc/ghc/-/wikis/JavaScript-backend
- GHCJS project: https://github.com/ghcjs/ghcjs

## wasm32-wasi-ghc

**Target: Web Assembly**. Use C to WebAssembly toolchain to compile native RTS into wasm. Add support for a new `wasi` platform to the RTS in addition to `posix` and `win32`.

- RTS: compile GHC's existing C RTS including the garbage collector, written in C, to WebAssembly.  Wasm doesn't yet provide a GC, so we pretty much have to follow this approach.
- C to WebAssembly toolchain: wasi-sdk
- https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-backend 

## Asterius

A WebAssembly backend for GHC was pioneered in the Asterius project.
The RTS was implemented in JavaScript, conforming certain run-time
conventions (e.g. nursery allocation)

- https://github.com/tweag/asterius 
- RTS: custom JS implementation
- Toolchain: use https://github.com/WebAssembly/binaryen to generate WebAssembly from Cmm

Now abandoned in faviour of wasm32-wasi-ghc

## WebGHC (Via LLVM approach)
Similar to Via C approach but replace C with LLVM IR (GHC has an LLVM backend).

Was worked on in 2017 SoC (WebGHC): https://webghc.github.io/2017/08/10/hsocwrapup.html
Blocked on missing syscall implementation.

- RTS: reuse native RTS. Blocked on syscall implementation.

Now apparently abandoned.

# Compiling Haskell code (lazy graph reduction)

GHC’s main task is to generate code that implements lazy graph reduction for Haskell programs. GHC has been designed to produce efficient code on native platforms. The challenge here is to determine what needs to change to target:

- JavaScript: a managed platform which has its own garbage collector
- WebAssembly: stack based very constrained abstract machine

Sadly there is no comprehensive documentation of GHC’s design to refer to. Hence we can’t just mention what a JavaScript backend has to do differently, but also what the other backends  (native code generator, interpreter, etc.) do.

Efficient lazy graph reduction is only one aspect of compiling Haskell codes. Most of the challenges come from the “Awkward squad” support (IO, concurrency, exceptions, FFI imports and exports), support for GHC’s features violating abstraction layers (weak references, stable pointers, explicit call to the GC, etc.), compile time code execution (Template Haskell, annotations, compiler plugins), support for interacting with C codes compiled into JavaScript too!

## Eval/apply for JS
https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/eval-apply-jfp.pdf 
Do we need to generate “stg_app_*” for JS targets? We could probably use `typeof` to detect if an object is a closure or an unboxed value.

## Tail calls

Tail calls are commonly used in GHC’s generated code for native platforms. Evaluating a thunk is efficiently done by jumping to its entry code. That is one of the reasons it is fast.

Initially JavaScript didn’t support tail calls. Since ES6 it should support it in theory. However some JS platforms disabled it because it makes debugging more difficult (see https://github.com/nodejs/CTC/issues/3, https://github.com/tc39/proposal-ptc-syntax/issues/23).

Similarly for WebAssembly, there is a proposal to add support for tail calls (https://github.com/webassembly/tail-call) which isn’t well supported. See https://leaningtech.com/extreme-webassembly-2-the-sad-state-of-webassembly-tail-calls/ for the status in 2020 as well as links to issues that are often still open in 2022.

A slower alternative to tail calls is to use trampolines: instead of being (tail) called, the function to call is returned to its caller which will just call it… The top-level function is a loop that calls returned functions. This was the solution implemented in Asterius. GHCJS and the GHC via-C backend also use trampolines.

# Awkward squad and more

## Template Haskell

Ghcjs pioneered the external interpreter (iserv): run JS code in NodeJS

Alternatives: fat interfaces + stg/bytecode interpreter

## Plugins

Can’t load plugins in cross-compiler (need to link with host RTS, e.g. for GHC’s global vars)
GHC not multi-target, even just for loading plugins for a different target (the compiler host), lest compiling plugins

## Setup.hs

Cabal’s Setup.hs must be run on the host
Two approaches:
- Compile to JS and run with nodejs
- Compile to host native (need different toolchain than for the target)

## Garbage collection

Reusing JavaScript’s garbage collector (e.g. ghcjs) or not (e.g. Asterius)?

Approaches that reuse the native RTS benefit from the native GC implementations (non-moving, staged, etc.).
- Do they make sense for a JS/WebAssembly target?
- Is block memory allocation sensible?

## Concurrency

- Support for light threads and concurrency primops
- STM

## C/Cmm sources

Many Haskell packages come with C source files. Via C and Via LLVM approaches should support these (compiling them into JS/Wasm). Other approaches need something else:
- Replacing C sources with JS sources
  - GHCJS’ “shims” library of JS codes
  - Make Cabal embed js-sources instead of c-source depending on the target
-  Compiling C sources into JS (reusing Via C or Via LLVM approach)
  - Approach impedance mismatch: different toolchains, not necessarily the same heap representation

## Interaction with JavaScript/WebAssembly

- FFI imports / exports
- FFI exports as JavaScript “promises” (cf Asterius)
- Asynchronous RTS API: cf https://mail.haskell.org/pipermail/ghc-devs/2021-December/020459.html 

# Alternative approaches to “The JavaScript problem”

- Make JS tend towards better languages: TypeScript
- Specific languages: PureScript, Elm, CoffeeScript
- DSLs: JMacro
