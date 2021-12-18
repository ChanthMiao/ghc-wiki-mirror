# Updating test cases when the output changes


If the output of a test has changed, but the new output is still
correct, you can automatically update the sample output to match the
new output like so:

```wiki
$ make accept TEST=<test-name>
```


where \<test-name\> is the name of the test. If you are using [hadrian](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/hadrian) the invocation is similar:

```wiki
hadrian/build test --test-accept --only='<multiple-test-names>'
```

If you want to update a whole directory of tests to accept the
new output, simply call `make accept` in that directory:

```wiki
$ cd array/should_run
$ make accept
```

For hadrian, you can use the option `--test-root-dirs` to run all tests in a given directory, for example:

```wiki
# in ghc root directory
# accept output of all tests in testsuite/tests/count-deps
hadrian/build test --test-accept --test-root-dirs='testsuite/tests/count-deps'
```

`make accept` does not change the expected output of tests that are
marked as `expect_broken` or `expect_broken_for`. 


It is also possible to accept test changes only for a specific `platform` or `os`.

```wiki
$ make accept TEST=<test-name> [OS=YES | PLATFORM=YES]
```


However please avoid this as much as possible and if applicable use a custom formatter
to format the output in a platform neutral way. See e.g. [normalise_errmsg_fun](building/running-tests/adding)

## Performance tests


Some of the performance tests in `perf/` check for peak memory usage.  Peak memory usage is obtained when there is a major GC, so the peak value can be vulnerable to the timing of major GC.  If `peak_megabytes` appears to increase, try running with `RTS -h -i0.01` before and after. That makes major GC happen more often, and gives a much more accurate result.
