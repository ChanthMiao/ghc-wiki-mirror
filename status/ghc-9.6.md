# Ambitions and major features for the 9.6 compiler series

This article is a high-level exposé of our ambitions of the GHC 9.6 series.

_(It's not a fixed list of feature, rather a wish list to ourselves that would inspire our mid-term ambitions and hopefully inspire the readers that are outside of our development community.)_

## JavaScript backend

GHCJS is Haskell-to-JavaScript compiler started 12 years ago that relies on a GHC fork to reuse its frontend down to STG IR. Maintaining a fork of GHC is a lot of work for its developers and it makes GHCJS more difficult to use than stock GHC for end users. Hence the plan to realize a JavaScript backend directly into GHC 9.6 by adapting GHCJS' code (which is still using GHC 8.10.7 at the time of writing).

The advantages of doing this are:
- (users) no need to wait for GHCJS to be updated to benefit from the features of the latest GHC release
- Installation and use of GHCJS become easier
- Expect more stability, new features, performance improvement, etc.

## WebAssembly backend

The WebAssembly backend is the heir to the Asterius project, and is targeting the `wasm32-wasi` platform. 

- Haskell code compiled with GHC will interoperate with JavaScript, but JavaScript will be needed only when Haskell code contains `foreign import/export javascript`. If a Haskell program does not use foreign JavaScript functions, it can run on a WebAssembly engine that does not support JavaScript.
- It will be possible to build an application using a mix of Haskell and JavaScript, with JavaScript in charge.
- Haskell code will be able to call an asynchronous JavaScript function.

See [WebAssembly backend](https://gitlab.haskell.org/ghc/ghc/-/wikis/WebAssembly-backend) for details.