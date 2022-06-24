# Hadrian FAQ


## Make the stage-1 compiler (only)

```
./hadrian/build stage1:exe:ghc-bin
```

## Make the stage-2 compiler after changing (say) one module.

In theory this requires rebuilding stage 1, and building all libraries from scratch (since the compiler might change what code it spits out).  But I want to say "just build me that stage-2 binary; do NOT rebuild stage 1 and all the libraries".

```
./hadrian/build stage2:exe:ghc-bin --freeze1
```

# Compile one specific base-library module, or one specific ghc-stage2 module, on its own.

This happens, say, when there is a Lint failure in the libraries or stage2.  I want to compile that particular module, adding -dcore-lint, or -dverbose-core2core.  Today I scroll back through my build log, snip the command line that invoked GHC, add -dcore-lint and run that command.

```
./hadrian/build stage2:exe:ghc-bin -VV
```

Then you can scroll back through the build log and copy the command like before.

## Do not recompile the RTS every time

Add `--skip=_build/stage1/rts/**` to your command line.

# Testsuite

## Run one test (make TEST=T23232)

```
./hadrian/build test --only="T23232"
```

## Run one test and accept the new output (make TEST=T23234 accept)

```
./hadrian/build test --only="T23234" --test-accept
```

## Run one test with the stage-1 compiler (less common) (make stage=1 TEST=T3343)

```
./hadrian/build test --only="T23234" --test-compiler=stage1
```


## Compile the test with the command line used by the test framework.

```
./hadrian/build test --only="T23234" -k -V
```

Then copy the command line used to run the test in order change directory
into the right folder and run the failing test.


# Nofib

See [documentation here](https://gitlab.haskell.org/ghc/nofib/-/blob/master/shake/README.mkd)

## Build all of nofib

```
cd nofib
cabal v2-run -- nofib-run --compiler=/path/to/test-ghc --output=test
```

## Build just one nofib test
```
 cabal v2-run -- nofib-run --compiler=/path/to/test-ghc --output=test
```
