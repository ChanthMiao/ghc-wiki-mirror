# Redesigned GHC Warnings


GHC currently uses a somewhat unsatisfying warning CLI:

```wiki
-W      (enable normal warnings)
-w      (disable all warnings)
-Wall   (enable almost all warnings)
-Werror (make warnings fatal)
-Wwarn  (make warnings non-fatal)

-Wduplicate-exports
-Widentities
-Wmissing-signatures
-Wunused-binds
...
```


By reusing the GCC CLI convention for warning-flags, we can make GHC's CLI a bit more intuitive to people used to GCC (& Clang's) CLI.

## Changes to be implemented, and timing

**Already implemented in GHC 8.0:**

- (#11218) Keep the current `-f(no-)warn-$WARNTYPE` flags as hidden flag aliases for newly introduced -W(no-)$WARNTYPE\` flags more in line with GCC's conventions, e.g.

  - `-Worphans` instead of `fwarn-orphans`
  - `-Wno-missing-methods` instead of `-fno-warn-missing-methods`

  This is already done in GHC 8.0.

- (#11429) Make unrecognised `-W` flags a warning (`-Wunrecognised-warning-flags`) rather than an error. 

- (#11370) Remove `warn-redundant-constraints` from the default constraint set and the `-Wall` constraint set

- (#11451) Split off `-Wunused-foralls` and `-Wunused-type-patterns` from `-Wunused-matches`. Make `-Wall` imply `-Wunused-foralls` and `-Wunused-type-patterns`, but *not* imply `-Wunused-type-patterns`

**~~Proposed~~also implemented for GHC 8.0:**

- ([phab:D1850](https://phabricator.haskell.org/D1850)) Introduce some new warning sets, e.g.

  - Define set `-Wstandard` (modulo bikeshed, maybe `-Wdefault`?) to denote the set of warnings on by default, together with its negation `-Wno-standard`
  - Define set `-Weverything` (c.f. clang's [-Weverything](http://clang.llvm.org/docs/UsersManual.html#diagnostics-enable-everything) as precedent) to comprise really \*all\* warnings (together with its negation `-Wno-everything` for symmetry, which is a synonym for `-w`)
  - Define set `-Wextra` (modulo bikeshed, maybe `-Wnormal`?) as synonym for `-W`, together with its negation `-Wno-extra`
  - (#11000) Define set `-Wcompat` to denote all warnings about future compatility GHC *currently* knows about (like e.g. `-Wcompat-amp`, `-Wcompat-mfp`, `-Wcompat-mrp`)  In addition, have `ghc` provide a way to dump the current warning-sets (in a format that's parseable by humans and machines)

- (#10752) When emitting warnings/errors, show which warning flag was responsible,
  e.g.

  ```wiki
  foo.hs:1:1: Warning:  [-Wmissing-signatures]
    Top-level binding with no type signature: main :: IO ()
  ```

  making it easier to silence specific warnings via e.g. `-Wno-missing-signatures`


**Anytime someone is motivated** 


- (#11219) Introduce variant of `-Werror` (c.f. GCC's `-Werror=*`) which allows to specify the individual warnings to be promoted to errors, e.g.

  - `-Wall -Werror=orphans` would only promote `-Worphans` warnings into errors
  - `-Wall -Werror -Wno-error=missing-methods` would promote all warnings *except* `-Wmissing-methods` into errors

-  (this is subsumed by `-Weverything` now)~~Introduce `-Wpedantic`, which turns on warnings that `-Wall` doesn't turn on (e.g., `-Wredundant-constraints` and `-Wunused-type-patterns`)~~

## Intended usage of warnings


Here is the proposed warning framework:

- Intended meanings

  - `-Wdefault`: the ones you get by default. This indicate things that are almost certainly wrong.
  - `-Wall`: indicate suspicious code
  - `-Wcompat`: indicate code that in future releases may trigger errors or `-Wdefault` warnings.  Subset of `-Wall`.

- With no flags, GHC implements the "default warnings". You can get these by saying `-Wdefault` (yet to be implemented).

- When GHC implements a new warning it is put into the `-Weverything` set.  In the next release, that warning may move into the `-Wall` set or the `-Wdefault` set.  So libraray authors may want to future-proof their libraries by compiling with `-Weverything`.

- A flag should only move into `-Wall` or `-Wdefault` if there is a reasonable way for a library author to change their source code to avoid the warning. Experimental, unpredicatable, or compiler-writer-guidance warnings should not be in `-Wall`.

- Libraries on Hackage should not be distributed with `-Werror`, so that any warnings do not abort compilation.  Authors may want to use `-Wall` to help encourage other contributors to improve the library to avoid the warning; but `-Wdefault` is more typical usage.

### To `-Wcompat` ??? `-Wall`, or not? (#11494)


See [Design/Warnings/Wcompat](design/warnings/wcompat).
