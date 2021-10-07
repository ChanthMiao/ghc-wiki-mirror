The documentation and implementation of Backpack both deserve attention, as outlined in #17525. In its current state, the Backpack ecosystem presents a few difficulties:

- the lack of user-facing documentation means that users find it difficult to get started using Backpack, slowing adoption,
- the lack of a detailed specification means that GHC maintainers don't know how to resolve problems that arise from interactions with Backpack.

This page aims to give an overview of the current (as of October 2021) state of the documentation, specification and implementation of Backpack. The goal is to help GHC developers orient themselves around the feature, in order to better understand which aspects to prioritise.


# User experience

## User's guide

The user's guide does not mention Backpack at all. We should add an overview, and provide examples that covers the range of functionality that Backpack offers.

## Errors

Currently, the errors reported by Backpack are often quite confusing:

1. The errors are often overly-verbose, and include extraneous details like internal module hashes which are not helpful for users. SLD TODO: give specific examples.
2. It is easy to make a mistake that doesn't cause Backpack to throw a relevant error, but fail shortly after saying that an interface file is missing. SLD TODO: give specific examples.


# Developer experience

## Maintenance

The implementation of Backpack suffers from a lack of modularity, as many functions throughout the compiler contain ad-hoc special cases to handle Backpack, often quite inscrutably. MP TODO: add some examples.

In modules which deal specifically with Backpack, one often sees the reverse. For example, in `GHC.Tc.Utils.Backpack`, concerns about typechecking and loading of interfaces are interwoven in a very complex way. Some of this is perhaps inevitable given the nature of Backpack, but because of this it is quite difficult to maintain/modify.

## Testing: the .bkp file format

The `.bkp` file format is used internally in GHC for testing Backpack; it permits a more concise syntax in which different units are defined in a single file, as opposed to needing separate files and an associated `.cabal` file.

However, this file format is not specified or documented anywhere. Moreover, it doesn't support some important developer functionality such as `-ddump-tc-trace` (#20396).