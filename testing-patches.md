# Validating Patches

When you [contribute a patch](/Contributing-a-Patch) to GHC, it has to be validated before we push it to the master branch.

First, [add a new test](building/running-tests/adding), and [run it](building/running-tests/running).

## How to validate patches

There are two ways to validate your patch.

### Travis

Travis CI is a free-for-open-source continuous integration service. When your patch is not quite ready yet for review on Phabricator, you can let Travis validate it in the privacy of your own Github fork.

- Fork the GHC repository on [Github](https://github.com/ghc/ghc).
- Sign up for [Travis CI](https://travis-ci.org/).
- During the signup process, flick your ghc repository fork switch on (it will be clear when you sign up).
- Push your changes to your Github fork (any branch will do).
- You (and only you) will get an email if your patch failed to build.
- If you make a fork of any of the submodule repositories (i.e. [ghc/packages-filepath](https://github.com/ghc/packages-filepath)), Travis will use it instead of the original. This is feature that Phabricator doesn't provide at the moment.
- Settings are in the file `.travis.yml`, though you shouldn't have to change anything. See the [Travis wiki page](travis) for details.

### Locally

The following commands will make a clean build of GHC and its libraries using Hadrian, package them up in a binary distribution that is then installed, and run the testsuite against the installed bindist.

- Get a repository containing the latest HEAD, the patches you want to push, and no other patches or unrecorded changes.
- (Optional) Create a git worktree and cd there, so that your future non-validation builds are not affected and don't need to run from scratch:

  ```plaintext
  ~/ghc $ git worktree add ../ghc-validate -b validation-branch
  ~/ghc $ cd ../ghc-validate
  ~/ghc-validate $ git submodule update --init
  ~/ghc-validate $
  ```
- In the root directory of the tree, run

  ```plaintext
  ./validate
  ```
- Depending on the nature of the changes, more testing might be sensible. e.g. if possible, build system changes should be tested on Linux, Mac OS X and Windows machines. Look at the full documentation for the [test suite](building/running-tests).

validate runs `hadrian -j$THREADS`, where by default `THREADS` is the number of cpus your computer has +1. You can set the environment variable `THREADS` to override this. For a sequential build (and also test run) you would for example use

```plaintext
THREADS=1 ./validate
```

If you hit a hadrian bug or want to specifically use the make build system to build and package up GHC for some other reason, you can pass `--legacy` to the `validate` script.

## More details on the validate script

### Flags

In order to save time while debugging problems revealed by validate, the validate script understands several flags. These flags, and others understood by `validate` are documented in the `validate` script itself.
| Validate Flag | Description |
|---------------|-------------|
| `--no-clean` | does **not** clean the tree before a run, so it continues with a previous build |
| `--testsuite-only` | does not build the tree, only runs the testsuite. This is useful if the problems `validate` finds are only due to the testsuite falling out of sync with the code |
| `--build-only` |  only builds the tree |
| `--hpc` | builds stage2 with -fhpc, and checks how much of the compiler the test suite covers |
| `--fast` | omits building a binary and does not build the 'extra' packages (i.e. does not set `BUILD_EXTRA_PKGS=YES`, see [Building/Using](/ghc/ghc/-/wikis/building/using#build-configuration)) |
| `--slow` | builds the stage2  compiler with `-DDEBUG`and runs more tests (except those that call `compiler_stats_num_fields`, those are skipped when debugging is on) |
| `--quiet` | shows less output (by default all shell commands are shown so you can copy/paste and rerun them on failures) |
| `--legacy` | builds the tree and runs the tests through the legacy make-based build system |
| `--stack` | uses Stack to build Hadrian and to provide the bootstrap compiler |
| `--ignore-perf-all ` | uses Hadrian's user settings to ask the "runtests" driver to ignore all changes in performance test measurements |
| `--ignore-perf-incrs ` | uses Hadrian's user settings to ask the "runtests" driver to ignore increases in performance test measurements |
| `--ignore-perf-decrs ` | uses Hadrian's user settings to ask the "runtests" driver to ignore decreases in performance test measurements |
| `--debug` | Uses inbuilt Hadrian(/Make) flag to generate information for debugging |
| `--help` | shows all the above flags and notes (detailed below) |

Extra notes:

1. **<span dir="">Note on BINDIST variable:</span>**<span dir="">  which details on how the BINDIST variable value can indicate if a binary</span> <span dir=""> distribution of GHC built using Hadrian/Make is to be installed.</span>
2. **Note on validate vs GitLab CI setup**: on how the validate script is distinct from the set of validation settings that are run via GitLab.

### Make mode (`--legacy`) configuration

`validate` usually starts by `make maintainer-clean` to make sure that everything builds from scratch. Furthermore, it ignores the build settings you have put in `mk/build.mk`, and instead uses those in `mk/flavours/validate.mk`.

You may want to validate a different configuration, e.g. with `GhcLibWays = p`. To do that, create a new file `mk/validate.mk` and put those settings in there. Settings in `mk/validate.mk` override those from `mk/flavours/validate.mk` (which in turn override those from `mk/config.mk`).

After you run `validate --legacy` your tree will continue to use the same settings. The way to get back to using your own `build.mk` is to run `make distclean`. Less brutally, simply remove the file `mk/are-validating.mk`.

### Validate has failing tests without any local patches; what do I do?

The best thing to do is to fix them! This will help make the world a better place, and gain you the admiration and thanks of your colleagues.

Fixing them could mean one of two things: Fix a bug in GHC (or the libraries) that the test is reporting, or fixing a broken test to not report a failure when nothing is actually going wrong.

#### I tried to fix them, but I got stuck

If you can't fix them yourself, then first file a ticket for the problem so that it doesn't get forgotten about. First run the testsuite for just that test (e.g. `make fast TEST=thefailingtest` in `testsuite` directory), and include the full testsuite output in the ticket description. If you found any information out that might be useful to someone later fixing the bug, add that too. Also, add the name of the test in the `Test Case` field.

Then add `expect_broken(123)` to the test options for that test, where `123` is the number of the ticket you just opened. If the problem only applies in certain circumstances, then only mark it broken for those circumstances, e.g. `when(opsys('mingw32'), expect_broken(123))`.