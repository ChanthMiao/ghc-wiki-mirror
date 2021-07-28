If you are debugging GHC's performance and need a workload for comparing multiple GHC builds, the `Cabal` library is a standard choice. Here's the command to build it:
```
_build/stage1/bin/ghc -hide-package Cabal -ilibraries/Cabal/Cabal/src libraries/Cabal/Cabal/Setup.hs -hidir tmp -odir tmp +RTS -s -RTS -XHaskell2010 -fforce-recomp -O0
```

See also [`build-cabal.sh`](https://gitlab.haskell.org/bgamari/ghc-utils/-/blob/master/build-cabal.sh) for a convenient wrapper script.