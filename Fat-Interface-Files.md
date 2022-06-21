A fat interface file is a file which stores the core program at the end of simplification. The file can be read and deserialised so that compilation can resume from this point and the core can be translated into STG and hence Bytecode.

Motivations:
- Faster GHCi startup time when using bytecode interpreter. You no longer have to typecheck and simplify every module every time you start GHCi. This also affects Haskell Language Server.
- If you can interpret everything (even package modules) makes TH evaluation much more resilient to ABI changes.
- Third party tooling might like to have access to simplified core
- if we distribute these files for all dependencies then you can generate code which passes types at runtime (as needed for a good implementation of typed template haskell)

Related issues: https://gitlab.haskell.org/ghc/ghc/-/issues/21067, https://gitlab.haskell.org/ghc/ghc/-/issues/21700, https://gitlab.haskell.org/ghc/ghc/-/issues/10871

This has been implemented in https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7502

An implementation based on this was implemented in Haskell Language Server in order to improve startup times
on existing GHCs: https://github.com/haskell/haskell-language-server/pull/2813


# Open questions

## Separate file?

Should it be a separate file, or combined with the ModIface/existing `.hi` files?

## Simplified or unsimplified

Tooling might want unsimplified core. Maybe it should be configurable?

## Serialize bytecode instead?

We can get many of the benefits by serializing bytecode, however this has not been implemented or tested. Serializing core is straightforward as it is already part of the normal operation of the compiler

It is also not as rich of a format for third party tooling to consume.

# HLS Implementation

The HLS implementation uses the term "core file" instead and writes core after simplification to a seperate
.core file, which is then used to resume compilation and generate bytecode.

This implementation is backwards compatible up to GHC 8.6.

There were a number of complications in the HLS implementation that should be fixed when this is
implemented in GHC:

1. Implicit bindings are not tidies, so serializing them leads to silent failures. The solution was to strip out the implicit bindings and regenerate them from the `ModIface` upon de-serialization.
2. Certain unfoldings like `OtherCon ...` and the `OccInfo` of Id's isn't preserved by the current serialization mechanism
3. Non External names were tricky to serialize, care must be taken to serialize these properly.

