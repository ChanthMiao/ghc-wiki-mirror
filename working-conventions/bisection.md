# Bisection to find bad commits


Say you have a program that fails to build or a test suite that fails at some point between
GHC 8.2.2 and 8.4.1. It can often be useful to know which commit introduced the regression. [Bisection](https://en.wikipedia.org/wiki/Bisection_%28software_engineering%29) is an efficient way of determining this, requiring at most `log_2(N)` commits to find the culprit among `N` commits assuming GHC and your test case builds at each commit tried in the search which in practice may not be the case. 

We can do [manual](#manual bisection) or [scripted](#scripted bisection) bisection. This approach to finding a problem commit is especially appealing as git provides convenient support of binary search with [git bisect](https://www.kernel.org/pub/software/scm/git/docs/git-bisect.html). 

## Preparation

### Version Range

Try out released GHC versions to find out the version range between the pass and fail of your test case.

The tool [ghcup](https://www.haskell.org/ghcup/) can be used to install and switch between system GHC
versions. Once a version has been installed with this tool it can be set as the system GHC or a named
version can be used by setting the [with-compiler](https://cabal.readthedocs.io/en/3.4/cabal-project.html?highlight=with-compiler#cfg-field-with-compiler) option:

```
# command line option
> cabal v2-build --with-compiler=ghc-8.2.2

# cabal.project file field
with-compiler: ghc-8.2.2
```

Stack users can switch [resolver](https://docs.haskellstack.org/en/stable/yaml_configuration/#resolver)
to do the same thing:

```
# command line option
> stack --resolver lts-11.22

# stack.yaml file field
resolver: lts-11.22
```

Stack's resolver specifies both stackage package set and GHC version. The GHC version
can be set independently of resolver with the [compiler](https://docs.haskellstack.org/en/stable/yaml_configuration/#compiler) option.

## Manual Bisection

Manual bisection allows for intervention at each step in the search to workaround problems in the build of GHC or your test case. Also if we see that a commit is trivial we can exclude it with skip without having to wait for anything to build.

Let's say something has gone awry between GHC 8.2.2 and 8.4.1. We can do a manual bisection where git
picks the commit to try and we manually report back to git on how the test case went.

```
> git bisect start
> git bisect good ghc-8.2.2-release
> git bisect bad ghc-8.4.1-release
Bisecting: a merge base must be tested
warning: unable to rmdir 'hadrian': Directory not empty
warning: unable to rmdir 'libraries/mtl': Directory not empty
warning: unable to rmdir 'libraries/parsec': Directory not empty
warning: unable to rmdir 'libraries/text': Directory not empty
[2b5b9dc69e5d0af20b6e7be31638c2e3a1bb765f] Fix typo in base changelog
# Do your testing (see below) then report back to git whether it's good or bad.
> git bisect bad
```

For each step in the search, build GHC and run your test case. Here's a real world example, finding when exactly a typechecker plugin stopped working with GHC, [uom-plugin#43](https://github.com/adamgundry/uom-plugin/issues/43):

```
ghc> rm -r _build
ghc> git submodule update --init
ghc> ./boot
ghc> ./configure
ghc> ./hadrian/build -j4 --docs=none binary-dist-dir
ghc> cd ../uom-plugin
uom-plugin> cabal new-build uom-plugin:units --enable-tests
  --with-compiler=/Users/pdejoux/dev/src/haskell/ghc/_build/stage1/bin/ghc
# The test case is can we build the units test-suite.
# Let's say this time it was good so we'll report that back to git bisect.
uom-plugin> cd ../ghc
ghc> git bisect good
```

If for some reason you want to get the commit hashes between the initial good and bad versions:

```
> git show-ref -s ghc-8.2.2-release
aa3ffbdaffb1cdfc08720ebd3a8d3663ee3293f9
> git show-ref -s ghc-8.4.1-release
985d8979a02fe297d0ccf121d3207983b8de3661
> git log aa3ffb..985d89 --format=format:"%H"
f31c40efdc918bc9da8a325327ba5a472bd6ea9e
6540b7113aea04ef7feb3b849861fd4be7c38f1f
c760ae373d47a16170dab0b9ed6f1680a75d4263
...
```

## Scripted Bisection

Coupling a reliable test case and the script below (with appropriate modifications) turns the task of bisection into a relatively painless exercise.


**Note:** Bisecting revisions before the switch to submodules (i.e. more than a couple of months prior to the GHC 7.10.1 release) is quite difficult and is generally not worth the effort. The script below probably won't work in this regime.


Download the script below and edit it to reflect your test-case then begin the bisection:

```
> git bisect start
> git bisect good ghc-8.2.2-release   # we know the test case worked here
> git bisect bad ghc-8.4.1-release    # but it fails here
> git bisect run ./ghc-bisect.sh
```


This will run the script for each point in the bisection, skipping commits which are unbuildable. Hopefully this will end with a message informing you of the first bad commit. A log of this procedure will be place in `$logs`: `$logs/all` gives a nice high-level overview and the remaining files record each step of the bisection.



By default the script will clean the tree for every commit. While this is likely to give correct results, it performs a number of potentially redundant rebuilds. The process can be made faster by setting `ALWAYS_CLEAN=0`, which will only clean the tree when a commit fails to build.


## ghc-bisect.sh


```bash
#!/usr/bin/env bash

logs=/tmp/bisect-logs
hadrian_opts="--flavour=devel2 -j4"
ghc=`pwd`/_build/stage1/bin/ghc

mkdir -p $logs
rev=$(git rev-parse HEAD)

# Bisection step return codes
function skip_commit() { exit 125; }
function commit_good() { exit 0; }
function commit_bad() { exit 1; }
function stop_bisection() { exit 255; }

function log() {
    echo "$@" | tee -a $logs/all
}

function do_it() {
    step=$1
    shift
    log "Commit $rev: $step = $@"
    $@ 2>&1 | tee  $logs/$rev-$step.log
    ret="${PIPESTATUS[0]}"
    log "Commit $rev: $step = $ret"
    return $ret
}

function build_ghc() {
    do_it submodules git submodule update || skip_commit
    do_it clean rm -rf _build || log "clean failed"
    do_it boot ./boot && ./configure
    do_it ghc1 ./hadrian/build --docs=none binary-dist-dir $hadrian_opts || skip_commit
    fi
}

# This is the actual testcase
# Note that this particular case depended upon the `cabal`
# library, which is checked out in $tree
function run_test() {
    tree=$HOME/trees/cabal
    cd $tree
    #do_it "clean-test" rm -R dist-newstyle
    do_it "build-test" $ghc T22048.hs -O -fomit-interface-pragmas -fforce-recomp
    do_it "run-test" $ghc --show-iface T22048.hi | grep "_rule"
    # The test has succeeded if the rule fired
    if [ "x$?" = "x0" ]; then
        log "Commit $rev: failed"
        commit_bad
    else
        log "Commit $rev: passed"
        commit_good
    fi
}

if [ -z "$@" ]; then
    build_ghc
    run_test
else
    $@
fi
```

## Gotchas

### HTTPS

With an HTTPS clone, it is going to get tiring quickly to reenter credentials when
prompted given the number of submodules GHC has. Better to use SSH for the clone:

```diff
-- git clone https://gitlab.haskell.org/ghc/ghc.git
++ git clone git@gitlab.haskell.org:ghc/ghc.git
```

### Forks

If you're working off a fork of GHC then submodules are not going to work with the
script. To use the bisect script, clone of from `gitlab.haskell.org` instead:

```
> git bisect run ./bisect.sh                                                      
running ./bisect.sh
Commit 2b5b9dc69e5d0af20b6e7be31638c2e3a1bb765f: submodules = git submodule update
Cloning into '/Users/.../ghc/.arc-linters/arcanist-external-json-linter'...
remote:
remote: ========================================================================
remote:
remote: The project you were looking for could not be found.
remote:
remote: ========================================================================
remote:
fatal: Could not read from remote repository.

Please make sure you have the correct access rights and the repository exists.
fatal: clone of
'git@gitlab.haskell.org:philderbeast/arcanist-external-json-linter.git' into
submodule path '/Users/.../ghc/.arc-linters/arcanist-external-json-linter' failed
Failed to clone '.arc-linters/arcanist-external-json-linter'. Retry scheduled
```

### Unhappy Happy

Errors with the stage1 parser like the following can be worked around by
downgrading to `happy-1.19.9` or older, see #18571 for more on this.

```
compiler/stage1/build/Parser.hs:1445:48: error:
    Not in scope: type variable ???a???
     |
1445 | newtype HappyWrap205 = HappyWrap205 (([Located a],Bool))
     |
```

Another relevant happy issue (in GHC 9.0) is #18620.


### Base Library Changes

If a library maintained alongside GHC changes in ways your test case or its dependencies doesn't expect then you may not be able to build your test case.

Compiling with the 8.3 branch of GHC and the base library, I got an error building hashable, a dependency of my test case:

```
Data/Hashable/Class.hs:107:32: error:
    Module
    ???Type.Reflection.Unsafe???
    does not export
    ???typeRepFingerprint???
    |
107 | import Type.Reflection.Unsafe (typeRepFingerprint)
    |                                ^^^^^^^^^^^^^^^^^^
cabal: Failed to build hashable-1.3.0.0 (which is required by test:units from uom-plugin-0.3.0.1).
```

Here's the change needed to get `./hashable/Data.Hashable/Class.hs` compiling again:

```diff
-- import Type.Reflection.Unsafe (typeRepFingerprint)
++ import Type.Reflection (typeRepFingerprint)
```

Before??making that edit, I got a local version of hashable. Either git or cabal can be used to do this:

```
# Adding hashable as a git submodule gets its source.
uom-plugin> git submodule add https://github.com/tibbe/hashable.git

# We could command cabal to get the source for the version of hashable we depend on.
uom-plugin> cabal get hashable
Unpacking to hashable-1.3.0.0/
```

### Pre-8.2


If you are on Linux and see errors of the form,

```wiki
/usr/bin/ld: -r and -pie may not be used together
collect2: error: ld returned 1 exit status
```


You are seeing #12759 and need to cherry-pick d421a7e2.
