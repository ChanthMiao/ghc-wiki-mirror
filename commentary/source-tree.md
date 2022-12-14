# Layout of important files and directories


This page summarises the overall file and directory structure of GHC. We include both source files and generated files; the latter are always identified "build-tree only".


Everything starts with the main GHC repository (see [Building/GettingTheSources](building/getting-the-sources)). The build system calls that directory `$(TOP)`. All the paths below are relative to `$(TOP)`.

## Files in `$(TOP)`

- **`packages`**

  Despite the name "package", this file contains the master list of the \*repositories\* that make up GHC. It is parsed by `./boot`.

- **`validate`**

  Run `validate` (a shell script) before committing (see [TestingPatches](testing-patches)). The script is documented in the file itself.

- **Documentation files**

  `README.md`, `ANNOUNCE`, `HACKING.md`, `LICENSE`

- **GNU autoconf machinery**

  `aclocal.m4`, `config.guess`, `config.sub`, `configure.ac`, `install-sh`, `mk/config.mk.in`, `settings.in`

- **`Makefile`**

  The top-level `Makefile`: see [GHC Build System Architecture](building/architecture). GHC requires
[GNU make](http://www.gnu.org/software/make/).

- **Make system files**

  `ghc.mk`, `MAKEHELP`

## `libraries/`


The `libraries/` directory contains all the packages that GHC needs to build. It has one sub-directory for each package repository (e.g. `base`, `haskell98`, `random`). Usually each such repository builds just one package, but there is more than one in `dph`.


GHC's libraries are described in more detail on the [libraries page](commentary/libraries).

## `compiler/`, `docs/`, `ghc/`


These directories contain the main GHC compiler and documentation.
The `compiler/` directory contains the ghc package, which is linked
into an executable in the `ghc/` directory.


There is [documentation of the intended module dependency structure](commentary/module-structure) of the `compiler/` directory.

- **`compiler/ghc.cabal.in`**: the Cabal file for GHC is generated from this. If you add a module to GHC's source code, you must add it in the `ghc.cabal.in` file too, else you'll get link errors.


The following directories appear only in the build tree:

- **`compiler/stage1`**: generated files for the stage1 build of GHC. There are a handful of files (`ghc_boot_platform.h` etc), and a directory `compiler/stage1/build/` that contains all the `.o` and `.hi` files for the compiler.
- **`compiler/stage2`**: similarly stage2.


You can't run a binary from here: look in the `inplace/` directory below for that.

## `rts/`


Sources for the runtime system; see [Commentary/SourceTree/Rts](commentary/source-tree/rts).

## `includes/`


Header files for the runtime system; see [Commentary/SourceTree/Includes](commentary/source-tree/includes).

## `utils/`, `libffi/`


The `utils` directory contains support utilities that GHC uses.


These utils may be built with the bootstrapping compiler, for use during the build, or with the stage1 or stage2 compiler, for installing. Some of them are built with both; we can't install the utils built with the bootstrapping compiler as they may use different versions of C libraries. The reason we use sometimes stage2 rather than stage1 is that some utils, e.g. haddock, need the GHC API package.

- **`utils/ghc-cabal`** is a little program we use for building the libraries. It's similar to cabal-install, but without the dependencies on `http` etc.
- **`utils/count_lines`** is a program that counts the number of source-code lines in GHC's code-base. It distinguishes comments from non-comments.

## `driver/`


This contains some simple wrapper programs and scripts, for example the `ghci` wrapper that invokes the `ghc` binary with the `--interactive` flag.  These wrappers tend to be executable programs on Windows and scripts on Unix systems.

## `ghc-tarballs/` (Windows only)


This contains some tarball files (binary packages) that GHC relies upon. Used for easier development / deployment on windows.

## `testsuite/`, `nofib/`


The `testsuite/` and `nofib/` directories contain apparatus for testing GHC.

- [Building/RunningTests](building/running-tests)
- [Building/RunningNoFib](building/running-nofib)

## `mk/`, `rules/`


The `mk/` and `rules/` directories contains all the build system Makefile boilerplate; see [GHC Build System Architecture](building/architecture).  Some particular files are interesting:

- **`mk/build.mk`**: contains Makefile settings that control your build. Details [here](building/using).  The file `mk/build.mk.sample` contains a starting point that you can copy to `mk/build.mk` if you want.
- **`mk/are-validating.mk`**: this file records the fact that you are doing [validation](testing-patches), by containing the single line `Validating=YES`.  That in turn means the build system gets its settings from `mk/validate-settings.mk` instead of from `mk/build.mk`.  Remove the file to stop validating.
- **`mk/validate.mk`**: just like `build.mk`, but applies when validating.  Use this file to override the default settings for validation, which are in `mk/validate-settings.mk`.

## `distrib/`


Miscellaneous files for building distributions.

## Stuff that appears only in a build tree

### `inplace/`


The `inplace/` directory is where we "install" stage1 and stage2 compilers, and other utility programs, when they are built, to be used when building other things in the build tree.  The layout is exactly the same as that of an installed GHC on the host platform.

- **`inplace/bin/`**: executables, including 

  - `ghc-stage1`
  - `ghc-stage2`
  - `ghc-pkg`
  - `hasktags`
  - `hsc2hs`
  - `haddock`
  - `count_lines`
  - `compareSizes`

- **`inplace/lib/`**: supporting libraries for the executables.

### `.../dist*/`


In many directories, `dist*` subdirectories appear. These are where Cabal, and the build system makefiles, put all of the files generated while building.  Some particularly interesting files are:

- **`docs/users_guide/users_guide/index.html`**: the HTML for the user manual
- **`libraries/LIB/dist-install/doc/html/LIB`**: contains the Haddock'd documentation for library *LIB*
