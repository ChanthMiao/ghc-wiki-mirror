# Running the Testsuite


This section gives information on how to use the testsuite. Note that the testsuite driver requires Python 3. For hadrian trees the testsuite is invoked by hadrian.

```
./hadrian/build test
```

NOTE: If you are using a flavour that is not tested by CI then there is no guarantee that the testsuite will pass. You use other flavours at your own risk!


## Commonly used options


You can run the testsuite in parallel to speed it up:

```wiki
$ ./hadrian/build test -j4
```


By default the testsuite uses the stage2 compiler. If you want to use another stage then:

```wiki
$ ./hadrian/build test --test-compiler=stage1
```


To run the testsuite against a different GHC, say ghc-5.04 (this assumes the name specified corresponds to an executable on your path):

```wiki
$ ./hadrian/build test --test-compiler=$(which ghc-5.04)
```


To run an individual test or tests (eg. tc054):

```wiki
$ ./hadrian/build test --only="tc054")
```

To run only performance tests:

```wiki
$ ./hadrian/build test --only-perf
```

See [performance tests](building/running-tests/performance-tests) for more details on performance tests. To run everything but the performance tests:

```wiki
$ ./hadrian/build test --skip-perf
```

To only run a subset of tests you can point hadrian to one (or more) subdirectories to query for tests to run:

```
$ ./hadrian/build test --test-root-dirs=testsuite/tests/driver
```


To run several tests, you just space separate them:

```wiki
$ ./hadrian/build test --only="tc053 tc054"
```

To run the tests one particular way only (eg. GHCi):

```wiki
$ ./hadrian/build test --test-way=ghci
```


The testsuite ways are defined in `testsuite/config/ghc`.


To add specific options to the compiler:

```wiki
$ EXTRA_HC_OPTS='+RTS -K32M -RTS' ./hadrian/build test
```


By default tests clean up after themselves. If you want to keep the temporary files that tests generate, you can run:


```wiki
$ ./hadrian/build test -k
```

Using in conjunction with `-V` to increase the verbosity, you can inspect the command used to run the test which can be useful to reproduce directly without complications of the testsuite driver.

```wiki
$ ./hadrian/build test -k -V
```

## Running the testsuite inplace

By default each test is run in a new temporary directory to isolate each test from previous runs of the compiler. Sometimes it is desirable to run the tests in-place, so the compilation artifacts are placed next to the test files. This is quite an expert thing to do, so be careful.

```
cp testsuite/ghc/config _build/test-config
# Set config.local= True
vim _build/test-config
./hadrian/build test --test-config-file=_build/test-config
```

## Speed settings

The `--test-speed` option controls some settings about which tests are run and in what configurations:


This table shows the differences between these three speed settings.

<table><tr><th> --test-speed </th>
<th> how many tests </th>
<th> how many ways </th>
<th> used by whom 
</th></tr>
<tr><th> fast   </th>
<th> some </th>
<th> 1   </th>
<th> Typically unused
</th></tr>
<tr><th> normal (default)   </th>
<th> all  </th>
<th> 1   </th>
<th> CI (slow takes too long) 
</th></tr>
<tr><th> slow   </th>
<th> all  </th>
<th> all </th>
<th> Nightly (slow is ok) 
</th></tr></table>


See also `Note [validate and testsuite speed]` in the toplevel `Makefile`.

## Problems running the testsuite


If the testsuite fails mysteriously, make sure that the timeout utility is working properly. This Haskell utility is compiled with the stage 1 compiler and invoked by the python driver, which does not print a nice error report if the utility fails. This can happen if, for example, the compiler produces bogus binaries. A workaround is to compile timeout with a stable ghc.

### Why are my tests being skipped?


There are a few possible explanations for a test being skipped run:

- The test is a performance test yet the compiler was built with `-DDEBUG` (perhaps the tree was built with `./validate --slow` or with `--flavour=devel2`?)
- There was an error evaluating a `.T` file; see the output from the testsuite driver's initialization phase for hints.

## Running tests in the same environments as CI

It is fairly straightforward to locally reproduce the exact results that one gets when running the testsuite in one of the Linux environments that we test in CI, using Docker. There is a registry running at `registry.gitlab.haskell.org` which hosts and serves all the Linux Docker images that we use in CI. The sources of those images can be found in the [`ghc/ci-images`](https://gitlab.haskell.org/ghc/ci-images) repository.

It is helpful to use the `ghc-docker.sh` script for setting up your docker image. - https://gitlab.haskell.org/ghc/ghc/-/snippets/4471

You may first have to login to the docker registry using your gitlab credentials:

```
docker login registry.gitlab.haskell.org
```

Let's take a concrete example: the `PartialDownsweep` test is failing in the `validate-x86_64-linux-deb8-hadrian` CI job. By looking at the `.gitlab-ci.yml`'s `DOCKER_REV` entry for the job's commit, you can find the `ci-images` commit that the CI job used. For our example, we will use the following commit: `ac65f31dcffb09cd7ca7aaa70f447fcbb19f427f`. You can now the `ghc-docker.sh` script that pulls the image (if needed) and drops you into a corresponding shell:

``` sh
$ ./ghc-docker.sh registry.gitlab.haskell.org/ghc/ci-images/x86_64-linux-deb8:ac65f31dcffb09cd7ca7aaa70f447fcbb19f427f
```
One in the shell you can run the commands from the CI configuration:

```
.gitlab/ci.sh setup
.gitlab/ci.sh configure
.gitlab/ci.sh build_hadrian
.gitlab/ci.sh test_hadrian
etc
```

Windows and OS X jobs on the other hand do not run through Docker and you therefore need access to real machines running those systems in order to reproduce any problem that you observe in the corresponding CI jobs.
