# Hadrian

Hadrian is GHC's new build system, written in Haskell and based on the [Shake](http://hackage.haskell.org/package/shake) library. It lives under the `hadrian/` directory in GHC's source tree.

Hadrian is a replacement for the Make build system, and therefore replaces `make` commands in the GHC developer's workflow. You can still run `./boot` and `./configure` as usual (see [here](https://gitlab.haskell.org/ghc/ghc/wikis/building/using) for more).

A more detailed introduction to Hadrian with pointers to detailed documentation about its various aspects is available in [`hadrian/README.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/README.md) in GHC's source tree.

## Prerequisites

Just like with the Make build system, you first need to [get the sources](https://gitlab.haskell.org/ghc/ghc/wikis/building/getting-the-sources) and [prepare your environment](https://gitlab.haskell.org/ghc/ghc/wikis/building/preparation).

## First build

To build a complete stage 2 compiler with the default build flavour:

```
$ ./boot && ./configure
$ hadrian/build -j
```

Depending on your system setup (typically on Mac), the above will fail due to missing GMP libraries. In that case you need to run `./configure --with-intree-gmp`.

On Windows:

``` sh
$ ./boot && ./configure --enable-tarballs-autodownload
$ hadrian/build.bat -j
```

**Important**: we will refer to the right `hadrian/build.*` script for your system as just `build` from now on.

All build artifacts are put under `_build`, which we refer to as the build root. You can change it using the `--build-root=some/path` or `-osome/path` command line flags. The build command will build stage 1 and stage 2 (which are essentially different flavours of ghc). Stage 2 is the final build, which is placed in the `_build/stage1` directory because all things built by stage 1 are in the stage 1 directory. The stage 2 ghc compiler at `_build/stage1/bin/ghc` is ready to be used right away. Its package database is in `_build/stage1/lib/package.conf.d/`

The resulting GHC is built in the default flavour. You can use the `--flavour=<flavour name>` flag to build with any other flavour, such as `devel2`, `perf`, `prof`, `quick`, `quickest` to name just a few; see [`hadrian/doc/flavours.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/flavours.md) for more details about each flavour. To learn about defining your own flavours or customizing existing ones, see [`hadrian/doc/user-settings.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/user-settings.md).

## A few essential commands

``` sh
build test --only=abcd # build and run test 'abcd'
build binary-dist      # generate a binary distribution, building everything required
build docs             # generate documentation
```

The `./validate` script uses Hadrian by default and can be used to build GHC, produce and install a bindist and run the testsuite against it in one simple command. You can read more about this in [Validating patches](https://gitlab.haskell.org/ghc/ghc/-/wikis/testing-patches).

## Subsequent builds and tests

To rebuild GHC after pulling upstream changes, you may need to update the submodules via `git submodule update --init --recursive`. Alternatively you can configure git to do this for you automatically: `git config --local submodule.recurse true`.

To speed up subsequent builds, you will often want to add this option to `build`:

  - `--freeze1` (avoid rebuilding the stage 1 compiler) documentation tests

A good compromise between a fast compiler which passes most the testsuite and fast rebuilds is building with the following options.

```
 ./hadrian/build --flavour=default+no_profiled_libs+omit_pragmas --freeze1 -j
```

To speed up testing of your modified GHC, you may want to add these options to `test`:

  - `--docs=none` (disable documentation tests)
  - `--skip-perf` (disable performance tests)



## Going further

- You can see a summary of Hadrian's features in [its README](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/README.md).
- A cheatsheet for all GHC developers who are used to the Make build system can be found under [`hadrian/doc/make.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/make.md).
- The make build system's `mk/build.mk` file for customizing GHC builds is replaced by Hadrian's user settings mechanism, documented at [`hadrian/doc/user-settings.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/user-settings.md).
- `hadrian/ghci` loads all of GHC's code in ghci; it only typechecks the modules though, so it is not possible to run the functions from the REPL. This is useful for generating tag files, with the usual `:ctags` and `:etags` commands.
- `build test` lets you run the testsuite; see [`hadrian/doc/testsuite.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/testsuite.md) for detailed explanations about the `test` rule and the options it supports.
- Hadrian supports `build docs`, `build source-dist`, `build binary-dist`, `build stage<N>:lib:<library name>`, `build stage<N>:exe:<executable name>`, `build clean`, `build nofib` and more. See `build --help` for a more detailed listing, as well as all the aforementioned documents for detailed explanations about the build rules and command line options supported by Hadrian.

## Making efficient use of time

Below are general tips for making efficient use of your time while working on GHC from [Ben](mailto:ben@well-typed.com>:

  * Be careful about pulling/rebase. I generally pick a base commit to build
    off of and rebase fairly sparingly: Having to stop what I'm doing
    to wait for full rebuild is an easy way to lose momentum.

  * Avoid switching branches; I generally have a GHC tree per on-going
    project.

  * Similarly, be careful about doing things that would incur
    significant amounts of rebuilding. This includes:

     * After modifying, e.g., `compiler/ghc.cabal.in` (e.g. to add a new
       module to GHC), modify `compiler/ghc.cabal` manually instead of
       rerunning `configure`.

  * Take advantage of Hadrian's `--freeze1` flag

  * Use `hadrian/ghci` to typecheck changes

  * Use the stage1 compiler instead of stage2 to smoke-test changes when
    possible. (specifically, using the script generated by Hadrian's
    `_build/ghc-stage1` target)

  * Use the right build flavour for the task at hand: If I don't need a
    performant compiler and am confident that I can get by without
    testsuite validation, use `quick`. Otherwise, plan ahead for what you
    need.

  * Run the fraction of the testsuite that is relevant to your change.
    Hadrian's `--test-way` and `--only` flags are your friends.

  * Take advantage of CI. At the moment we have a fair amount of CI
    capacity. If you think that your change is close to working, you can
    open an MR and start a build locally. If it fails, iterate on just the
    failing testcases locally.

  * Task-level parallelism. Admittedly, this is harder when you are
    working as a hobby, but I often have two or three projects on-going
    at one time. While one tree is building I try to make progress on
    another.

## Hadrian.settings

As described in [`hadrian/doc/user-settings.md`](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/user-settings.md) the configuration of the build can be modified via `_build/hadrian.settings`. Here are a few useful snippets to accomplish common tasks:

Most of these are now available as **flavour transformers** but still listed here as examples.

### Enabling DWARF debug symbols (`+debug_info`)

```
stage1.*.ghc.hs.opts += -g3
stage1.*.cabal.configure.opts += --disable-library-stripping --disable-executable-stripping
```

### Building the RTS with debug symbols

Typically the `-debug` RTS is built with debug symbols enabled. However, it also includes 
```
stage1.rts.ghc.c.opts += -optc-ggdb
```

### Building a ticky-enabled stage2 compiler (`+ticky_ghc)`

Here we also dump STG syntax since this is necessary to interpret the profile:
```
stage1.*.ghc.hs.opts += -ddump-to-file -ddump-simpl -ddump-stg-final
stage1.*.ghc.hs.opts += -ticky -ticky-allocd
stage1.ghc-bin.ghc.link.opts += -ticky
```

## Shell aliases

Typing `hadrian/build -j8` is a pain to remember and to write, even when you can scroll your command history. Therefore I found it useful to define a bunch of shell aliases. Here's an excerpt from my home-manager config:

```
      hb = "hadrian/build -j$(($(ncpus) +1))";
      hbq = "hb --flavour=quick";
      hbqs = "hbq --skip='//*.mk' --skip='stage1:lib:rts'";
      hbqf = "hbqs --freeze1";
      hbv = "hb --flavour=validate --build-root=_validate";
      hbvs = "hbv --skip='//*.mk' --skip='stage1:lib:rts'";
      hbvf = "hbvs --freeze1";
      hbt = "mkdir -p _ticky; [ -e _ticky/hadrian.settings ] || echo 'stage1.*.ghc.hs.opts += -ticky\\nstage1.ghc-bin.ghc.link.o
pts += -ticky' > _ticky/hadrian.settings; hb --flavour=validate --build-root=_ticky";
      hbts = "hbt --skip='//*.mk' --skip='stage1:lib:rts'";
      hbtf = "hbts --freeze1";
```

Note how this even allows for a Ticky-enabled build through creation of a `hadrian.settings` file (see also the previous section). The mnemonics are:

- `hb..` for `hadrian/build`
- `..{q,v,t,...}.` for quick, validate and ticky (similarly prof and devel2) flavours
- `...{s,f,}` for a build where we only want to rebuild the stage1 compiler (takes ~10s instead of 30s on my machine), where we only want to rebuild the stage2 compiler without recompiling stage1 or the RTS, or a complete rebuild.
