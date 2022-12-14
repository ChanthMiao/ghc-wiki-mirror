# Loading GHC into GHCi

With one command you can load GHC into GHCi (assuming a configured source tree: `./boot && ./configure`):

```
./hadrian/ghci
```

`ghcid` should also work without any further modifications.

If you run into unexpected error messages, you may need to remove and rebuild the `.hadrian_ghci` directory.

:warning: If your `~/.ghci` configuration file enables extensions like TemplateHaskell or OverloadedStrings, they will interfere with the loading, and **will** produce compilation errors. In order to skip `~/.ghci`, start GHCi with

```
./hadrian/ghci -ignore-dot-ghci
```

## Old GHC in GHCi script

This section is mostly out of date but retained here until the current iteration of the ghci script supports running `:main` (see https://gitlab.haskell.org/ghc/ghc/-/issues/16672)

This page says how you can load GHC into GHCi for more iterative development. Csongor Kiss was the first person to record this feat. The scaffolding has been added to `master` since Aug 2018, and if you have to use an older working copy, refer to the guide at the bottom ([Old GHC source tree](https://gitlab.haskell.org/trac/ghc/wiki/Building/InGhci#OldGHCsourcetree)).


After building GHC, just do the following in the repository root:

```wiki
./utils/ghc-in-ghci/run.sh
```


Notes:

- This works best on systems with a decent amount of RAM.  The GHCi process uses about 3.5GB initially, and the usage sometimes increases during development.  So, if you have less than 8GB of RAM, loading GHC into GHCi probably will not aid development efficiency.

- Adding `-j4` might speed up the process. Increasing the number will utilize more cores.

- The script will also run `:load Main`, to load GHC's main module. After that, running `main` will run an inner GHCi, because there is a default `:set args --interactive ...`. To override this, use `:set args ...` or `:main ...`.

- If you don't want to wait for `:load Main`, since you want to load some other module, then you can use `Ctrl+C` to cancel the initial load.

### ghcid


There is a `.ghcid` file which allows use of Neil Mitchell's handy [ghcid](https://github.com/ndmitchell/ghcid) tool directly by just running `ghcid`. This tool drives GHCi to automatically reload when files change.  

## Old GHC source tree

1. Put this .ghci file in compiler/

```wiki
:set -ibackpack
:set -ibasicTypes
:set -icmm
:set -icodeGen
:set -icoreSyn
:set -ideSugar
:set -ighci
:set -ihsSyn
:set -iiface
:set -illvmGen
:set -imain
:set -inativeGen
:set -iparser
:set -iprelude
:set -iprofiling
:set -irename
:set -isimplCore
:set -isimplStg
:set -ispecialise
:set -istgSyn
:set -istranal
:set -itypecheck
:set -itypes
:set -iutils
:set -ivectorise
:set -I../compiler
:set -I../compiler/stage2
:set -I../compiler/stage2/build
:set -i../compiler/stage2/build
:set -I../includes
:set -I../includes/dist-derivedconstants/header
:set -package=ghc-boot-th
:set -fobject-code
:set -DSTAGE=2
:set -XNoImplicitPrelude
:load GHC
```

1. Run `../inplace/bin/ghc-stage2 --interactive -odir tmp -hidir tmp -j<n> +RTS -A128m` where \<n\> is the number of cores on your machine


from inside compiler/


It may take a while and require a little bit of memory but in the end
all 500 or so modules will be loaded.


It can also be used with `ghcid`.

```wiki
ghcid -c "../inplace/bin/ghc-stage2 --interactive -odir tmp -hidir tmp -j4 +RTS-A128m"
```