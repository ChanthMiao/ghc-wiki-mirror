# GHC targets: sketch of ideas

## Big picture: Compile-time cross-compilation

GHC currently supports cross-compilation, but only for one target at a
time, which must be configured when GHC is built.  This limitation has
been an annoyance at least since Apple migrated its architecture from
Power to x86: a developer needed two complete installations (one to
generate Power code for current hardware, and one to generate code for
the x86 emulator).  Requiring two simultaneous installations of GHC is
not a good model.

A better model for development is a single installation that enables a
developer to choose a target platform at compile time (for example, by
a command-line option passed to `ghc`[^cabal]). This model has been
used by other compilers, but implementing it will require substantial
engineering effort.

[^cabal]: Eventually we'd like to make Cabal capable of managing
multiple targets, but that's a big hill we don't need to climb yet.

Under a single-installation model, a developer's experience might look
like this:

  - A developer installs GHC for their preferred host platform,
    perhaps via `ghcup`.  For concreteness, let's say the host
    platform is AMD64.

  - To cross-develop for, say, ARM, the developer installs

      * Platform-specific tools that handle cross-assembly and linking
        for ARM, e.g., GNU binutils and an ARM C library[^cross-toolchain]

      * A package containing an instance of GHC's run-time system
        compiled for ARM

How this model might be supported is discussed below.

[^cross-toolchain]: Distributions like Debian and NixOS make this easy.


## Targets

The new model would be organized around *targets*.  A target might be
characterized by the following properties:

  - A target has both internal and external components.
    The internal component notably knows how to generate code for a
    platform, including foreign calls.
    The external components include the assembler and linker needed to
    build relocatable object files and executable binaries (or the
    equivalent) for the same platform.
    Notably, *GHC's run-time system is just another external
    component*, like an assembler or linker.

  - Internal components are part of GHC's source code.  Every
    installation of a GHC release ships with all targets that are
    known to GHC as of that release.

  - External components can be identified by information on disk.
    Such information might take the form of an association list, like
    the current `settings` file.  External components can be added or
    changed *after* GHC is installed.  (For an idea about mechanism,
    see below.)

For any supported target, GHC should be capable of building its
run-time system in a standalone package for that target.  Presumably
this package contains a collection of `libHSrts*.a` and `libHSrts*.so`
files, one for each (supported) way.  It will be useful to be able to
build and ship such a package without a bundled compiler.

To help with external components, automatic-configuration logic can be
built into GHC.  Whenever internal components are present, GHC can
look for the corresponding external components.  This might mean
moving some autoconf things to Haskell.  But it means for example that
if user runs `ghc -target arm64`, GHC can respond "I don't have all
the components I need to target arm64; do you want me to look for
them?"

This model can support an "unzippable" GHC---that is, one that can
be unpacked and used in place, without a configure script.
An unzippable GHC would ship with only one target, which would be
the same platform as the host.  But more could be added after the fact.
  

## Questions

I have these questions:

  - To make this picture happen, what part of Hadrian should I focus on?
    
  - Is there any reason to limit a standalone RTS build to the host
    platform?  I expect not.  That is, if I'm running on AMD64,
    I should nevertheless be able to build an ARM package for the
    run-time system yes?  (Provided I have a suitable toolchain installed.)

  - What sort of linker fu is used to be sure that generated code is
    linked with the correct version and way (and possibly target)?
    "ABI hashes"?  How do they work?

  - Would it be possible or useful to build and ship a compiler
    without any run-time system?


## Bread crumbs worth following

  - The [cross-compilation road map](roadmap)

  - [!5965](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5965)
    simplifies configuration of GHC by pulling headers from the `rts`
    package.  This change will help isolate the RTS from autoconf?

  - #19896 suggests making the `ghc` package *reinstallable* like a
    normal Hackage package.

  - #17957 carries much water toward making GHC multi-target

  - !6803 gives the RTS its own `configure` script.

  - [Ben's notes from meeting of 25 October 2021](https://edit.smart-cactus.org/5KTMmI22R3-_onn3oqhkZg#)

  - #11470, support changing cross-compiler target at [compile] time

#### Footnotes
