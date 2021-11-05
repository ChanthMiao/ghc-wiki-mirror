# Cross-compilation via GHC targets

## Why Cross-Compilation Matters

Cross-compilation serves users who want to compile Haskell code on one
platform but run it on another.  The need for cross-compilation is
driven primarily by the availability of cheap single-board computers
such as Raspberry Pi and by the rise of mobile computing (e.g.,
Android and iOS).

  - On a single-board computer like Raspberry Pi, native compilation
    is conceivable in principle, provided you have a machine with
    enough memory.  But memory is a real limitation, and compared with
    a desktop computer or build server, a single-board computer is
    wildly underpowered.  An acceptable development experience demands a
    beefy machine equipped with massive RAM and a fast solid-state
    drive.

  - On Android, it might be possible to attempt native compilation,
    but the same performance considerations apply.  And because
    Android was not designed for this use case, the attempt would
    likely be annoying and painful.

  - On Apple's iPhones, iPads, and Apple TVs, native compilation is
    virtually impossible---the toolchain provided by Apple supports
    cross-compilation only.

  - On the relatively new WebAssembly platform, the same constraints
    apply: the toolchain supports cross-compilation only.

  - In the AWS cloud, developers may want to save money by compiling
    for AWS's Graviton (ARM) machines, even if their host platform is x86.

Who cares about cross-compilation?

  - Obsidian Systems is very invested in Haskell on mobile devices.

  - IOG (formerly IOHK) have been cross-compiling from Linux to
    Windows, as Linux provides a better CI experience.

  - IOG are working on blockchain with a Proof of Stake model.  This
    model demands relatively few computing resources (a strength), and
    IOG are interested in running it on single-board computers and
    other low-energy platforms.

  - Cardano smart contracts will include client code that runs off the
    blockchain, on client machines.  To enable users to participate
    independent of the computing, that client code needs to be
    cross-platform.  At present, Plutus (Cardano’s smart contract
    system) uses GHCJS to compile client code to Javascript, but the
    aim is to move to WebAssembly.


## New model: Select cross-compiler at compile time


At present, GHC supports cross-compilation awkwardly.  Any given `ghc`
binary understands only one target at a time, which must be configured
when `ghc` is built ("compiler-configuration time").  This limitation
has been an annoyance at least since Apple migrated its architecture
from Power to x86: a developer needed two complete installations (one
to generate Power code for current hardware, and one to generate code
for the x86 emulator).  Requiring two simultaneous installations of
GHC is not a good model.

A better model for development is a single installation that enables a
developer to choose a target platform at compile time (for example, by
a command-line option passed to `ghc`[^cabal]).   This ability might
require some extra installation, but the installation should fit in
nicely with the way GHC already supports development on a single
platform.

[^cabal]: Eventually we'd like to make Cabal capable of managing
multiple targets, but that's a big hill we don't need to climb yet.


GHC's single-platform support requires four elements:

  - A `ghc` compiler
  - A GHC run-time system
  - A platform-specific assembler, linker, and C libraries, often
    provided by "binutils"
  - Haskell libraries

The `ghc` compiler and run-time system are provided by a
*distribution* or *installer* like `ghcup`, the binutils are provided
by an underlying OS distribution, and the Haskell libraries are
provided by a mix of `ghcup` (base libraries) and `ghc-pkg` (other
libraries).  Libraries are often orchestrated by Cabal.

We would like cross-platform support to be similar:

  - Any `ghc` compiler would include logic for every supported target
    platform.

  - A run-time system compiled for a target platform might be
    installed by `ghcup`.  Eventually a run-time system might be
    installed just as an ordinary package.

  - Assembler, linker, and C libraries for the target platform would
    be provided by a binutils configured for the desired target,
    usually also available from an OS distribution

  - Haskell libraries might be installed as a package or might need to
    be cross-compiled and installed using `ghc` and `ghc-pkg`.

Under a single-installation model, a developer's experience would be
simple: they would start with their usual GHC, and any time they
wanted to add a new target, they would just install additional
packages, then continue.  For example,

  - A developer installs GHC for their preferred host platform,
    perhaps via `ghcup`.  For concreteness, let's say the host
    platform is AMD64.

  - To cross-develop for ARM, the developer installs

      * Platform-specific tools that handle cross-assembly and linking
        for ARM, e.g., GNU binutils and an ARM C library[^cross-toolchain]

      * A package containing an instance of GHC's run-time system
        compiled for ARM (this package might include base libraries)

How this model might be supported is discussed below.

[^cross-toolchain]: Distributions like Debian and NixOS make this easy.



## Targets

A new model could be organized around *targets*.  A target might be
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
    installation of a GHC release ships with the internal components
    of every target that is known to GHC (as of that release).

  - External components can be identified by information on disk.
    Such information might take the form of an association list, like
    the current `settings` file.  External components can be added or
    changed *after* GHC is installed.  (For an idea about mechanism,
    see below.)

If we want to stretch the idea of "component," we can also consider a
collection of compiled Haskell libraries (or perhaps a package
database) to be an external component.

For any supported target, GHC should be capable of building its
run-time system in a standalone package for that target.  Presumably
this package contains a collection of `libHSrts*.a` and `libHSrts*.so`
files, one for each (supported) way.  Such a package should be able to
be built and shipped without a bundled compiler.

To help with external components, automatic-configuration logic can be
built into GHC.  Whenever internal components are present, GHC can
search for the corresponding external components.  For example, if a
user runs `ghc -target aarch64`, GHC can respond "I don't have all the
components I need to target aarch64; do you want me to look for them?"
Implementing the
search might require moving some autoconf things to Haskell.  
(This model could even be used for the host platform, so that a `ghc`
could ship without *any* run-time system or libraries---they would
instead _all_ be build on demand.)

This model can support an "unzippable" GHC---that is, one that can be
unpacked and used in place, without an additional
configuration/installation step.  An unzippable GHC would ship with
only one target, which would be the same platform as the host.  But
more could be added after the fact.

## Template Haskell

Template Haskell raises tiresome questions about cross-compilation.
For example, what if conditional compilation makes our Haskell source
code different on the host (machine where `ghc` runs) and the target
(machine where the generated code runs)?   Splices seem like they
should run on the host, but it's not so simple.   Some references
appear below.

Another vexing question: if splices run on the host, is it necessary
to compile *all* code for both host and target?  And if not, how does
one tell which code must be compiled for the host?  (Perhaps the
coward's way out is to go ahead and compile it all---make things work
and let someone else make them fast.)

## Questions

I have these questions:

  - To make this picture happen, what part of Hadrian should I focus on?
    
  - Is there any reason to limit a standalone RTS build to the host
    platform?  I expect not.  That is, if I'm running on AMD64,
    I should nevertheless be able to build an ARM package for the
    run-time system, yes?  (Provided I have a suitable toolchain installed.)

  - What sort of linker fu is used to be sure that generated code is
    linked with the correct version and way (and possibly target)?
    "ABI hashes"?  How do they work?

  - Should the host's run-time system get special treatment, or could
    we go ahead and (under the hood) build and ship the compiler
    without any run-time system?


## Bread crumbs worth following

### Cross-compilation

  - [Moritz Angermann's series of blog posts](https://log.zw3rk.com/),
    which address both cross-compilation and Template Haskell

  - The [cross-compilation road map](./roadmap)

  - [!5965](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5965)
    simplifies configuration of GHC by pulling headers from the `rts`
    package.  This change will help isolate the RTS from autoconf?

  - #19896 suggests making the `ghc` package *reinstallable* like a
    normal Hackage package.

  - #17957 carries much water toward making GHC multi-target

  - !6803 gives the RTS its own `configure` script.

  - [Ben Gamari's notes from a meeting of 25 October 2021](https://edit.smart-cactus.org/5KTMmI22R3-_onn3oqhkZg#)

  - #11470, support changing cross-compiler target at ~~run~~ compile time

### Template Haskell

  - A series of three blog posts from Moritz, of which the first is at
    https://log.zw3rk.com/posts/2017-05-23-template-haskell/ 

  - [GHC Proposal
    412](https://github.com/ghc-proposals/ghc-proposals/pull/412)
    identifies some issues around making imports specific to a TH splice.

  - [GHC Proposal
    243](https://github.com/ghc-proposals/ghc-proposals/pull/243)
    introduces some ideas of staging

  - Matthew Flatt's 2002 ICFP paper, [Composable and Compilable
    Macros: You Want it
    When?](https://www.cs.utah.edu/plt/publications/macromod.pdf)
    appears to be the definitive treatment of staging for macros.
    I quote:

    > For macros to serve as reliable compiler extensions, the
    > programming model must clearly separate the compile-time and
    > run-time phases of all code at all times. The phases may be
    > interleaved for interactive evaluation, but compiling new code
    > must not affect the execution of previously compiled
    > code. Similarly, the amount of interleaving should not matter:
    > code should execute the same if it is compiled all in advance,
    > if it is compiled with interleaved execution, or if half the
    > code is compiled today and the rest is compiled on a different
    > machine tomorrow. Finally, when a complete application is
    > compiled, the programming environment should be able to strip
    > all compile-time code from the final deliverable.

    Flatt's work suggests that module granularity is too coarse a
    granularity with which to distinguish compile-time code (splices)
    from run-time code.


----

#### Footnotes
