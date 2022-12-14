## Splitting base


In a [thread on glasglow-haskell-users](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023764.html) in February some ideas about splitting base in smaller components were floating around. This wiki page tries to assemble ideas on how to re-group the modules.


This has been discussed before, e.g. in [2008](http://www.haskell.org/pipermail/libraries/2008-August/010543.html).

### Goals


Structural changes to the base package can be attempted towards the following goals:

#### (G1) To allow changes to internals without forcing a version-bump on ‘base’, on which every package depends


SPJ: But that goal needs a bit of unpacking. Suppose we divided base into six, base1, base2, base3, etc, but each was a vertical silo and every other package depended on all six.  Then nothing would be gained; bumping any of them would cause a ripple of bumps down the line.

#### (G2) To allow packages to be explicit about what they need


A library that does not use the IO monad could communicate that just by not depending on some base-io package. Similar with the Foreign Function Interface or unsafe operations.

#### (G3) To allow alternative implementations/targets


A Haskell-to-Javascript compiler will not support File IO, or maybe not even IO at all. It would be desirable such an implementation has a chance to at least provide a complete and API compatible base-pure package, and that one can hence reasonably assume that packages and libraries depending only on `base-pure` will indeed work without modification. This might be subsumed by fulfilling (G2).

#### (G4) Allow well-engineered libraries to be used in what is currently base


At the moment we cannot use libraries like `containers`, `bytestring`, `unix` and `Win32` in code that is in
the `base` package.  (Why?  Because those libraries in turn depend on `base` and we don't allow mutual recursion between packages.)  If we split `base` up, we could use these libraries, and perhaps others, in at least parts of what is currently `base`.   Some examples:

- We would like to be able to use the Text and ByteString types in the I/O layer. For example, we'd like to have:

  ```wiki
  module System.IO where

  read :: Handle -> Int -> IO ByteString
  write :: Handle -> ByteString -> IO ()
  ```

  but since `System.IO` is defined in base it cannot depend on e.g. bytestring and thus we cannot write these functions. At the moment we have to use `String` for all I/O which is both slow, due to its cache-inefficient nature, and incorrect, as `String` is not a representation of a sequence of bytes (but rather a sequence of Unicode code points).

- The I/O manager currently has a copy of IntMap inside its implementation because base cannot use containers. Why?  Because `containers` depends on `base`, so `base` can't depend on `containers`.  Splitting base would let us get rid of this code duplication. For example:

  - `base-pure` doesn't need `containers`
  - `containers` depends on `base-pure`
  - `base-io` depends on `containers`

- In #7427, we would like to use functions from the `unix` package in `base:System.Environment`

#### (G5) Installable base


Right now, if a package depends on a specific version of base, there's no way to compile it with GHC that provides a different version of base.


After the split, hopefully, many subpackages of base will lose their «magic» status and become installable via cabal.

#### (G6) Split base into as FEW packages as possible, consistent with meeting the other goals


Other things being equal, we should split `base` into as few packages as necessary to meet other goals. Johan points out, a split now could paint us into a corner later, so we should not gratuitously split things up.

### Approaches

#### (A) Large base, re-exporting API packages


Here we would keep one large `base` package, as now, with a number of wrapper packages that selectively expose stable sub-APIs.


Meets goals (G1), (G2), (G3)


Advantages:

- Cheap: little or no changes to the actual code in base
- Easier to define the APIs as desired, i.e. focused and stable, without worrying about implementation-imposed cycles
- No need to include internal modules in the API packages
- Alternative compilers/targets can provide these APIs with totally independent implementations

#### (B) Actual base split


Here we genuinely split the code in `base` into sub-packages.


Meets goals (G4), (G5), I think (G3) 


Could meet goals (G1), (G2), though shim packages might still be needed.


Advantages:

- Quite a bit of work
- Narrows implementation choices, because packages can't be mutually recursive. (i.e. forces `IOError`-less `error`)
- Hence further development may be easier ([according to Ian](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023818.html))
- Some base-foo package can use other libraries like containers in their implementation (IntMap issue)
- More appropriate types like ByteString and Text can be used in, say, base-io-file
- Alternative compilers/targets may only have to reimplement some of the base-\* packages.
- Possibly fewer modules in “magic” packages that cannot be installed via cabal.

### Handling Prelude


If there is no longer one base package that all package depend on, the question where the Prelude will be arises. This is issue is mostly independent of the choice of approaches to splitting base.


Some options to handling Prelude include:

- (P1) Prelude stays in base, packages wanting to use the shim packages exclusively have to use `{-# LANGUAGE NoImplicitPrelude #-}` everywhere and import stuff explicitly. base-pure would probably provide its subset of the Prelude in Prelude.Pure, to be imported explicitly.
- (P2) Prelude goes to the first shim package that has everything required for Haskell98 (probably something like base-io-file). Packages that want to only use base-io have no Prelude to use (see (P1)).
- (P3) Prelude goes to a separate shim package, base-prelude. Packages that want to only use base-io have no Prelude to use (see (P1)). Allows packages to mix the shim-packages easily with other, non-standard Prelude providing packages, e.g. classy-prelude.
- (P4) Multiple shim packages have a Prelude module, providing the part of the Prelude that belongs to that shim package.. Good for those who depend on base-pure, but those who require both base-pure and base-io have now an ambiguous import.
- (P5) Separate packages base-io-prelude and base-pure-prelude providing Prelude’s containing stuff of base-\* (+ dependencies). Packages can pull in precisely the Prelude they want, but yet more packages.


None of these look particularly appealing. Here some ideas to make it more convenient for the programmer that require changes to GHC and how it treats packages:

- (I1) It automatically imports _all_ visible Prelude modules. So base-pure provides the pure Prelude and base-io adds the IO functions.
- (I2) Same, but a bit more explicit: We extend the package configuration by a "prelude-modules" field. Every module listed in that field of every visible package is treated as a Prelude and automatically imported (unless `{-# LANGUAGE NoImplicitPreldue #-}` is active.)
- (I3) Ambiguous module imports do not cause an error if, among the modules, there is one that is a superset of all others, i.e. reexports all other modules.


Joachim's preference: I find (P4)+(I1) quite elegant and appealing.


SPJ’s gut feeling prefers (P2): The minority who do not want to depend on enough base-X packages to get the Haskell-98 Prelude should use NoImplicitPrelude (since indeed you don't depend on enough to get the H98 Prelude) and import what they want explicitly.

### Non-Obvious interdependencies


This is a list of interdependencies between seemingly unrelated parts that need to be taken into consideration:

- class Monad mentions `String`, hence pulling Char
- class Monad mentions `error` and `Data.Int` requires `throw DivideByZero`, hence pulling in exceptions
- Exceptions pull in `Typeable`
- `Typeable` pulls in `GHC.Fingerprint`
- GHC.Fingerprint pulls in `Foreign` and `IO` (but could be replaced by a pure implementation)
- The Monad instance of `IO` calls `failIO`, which creates an `IOException`, which has fields for handles and devices, and hence pulls in some `Foreign` stuff and some file-related `IO`, preventing the creation of a clean base-io package. There exists a [somewhat backwards compatible work-around](http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023796.html).

Note that since the `MonadFail` class was created, this is no longer a problem. Horray!

### Other issues

- Some names of base are hardcoded in GHC and hence cannot be moved to a different package name without changes in GHC. This includes:

  - The `Num` constraint on polymorphic literals. Can be avoided by writing `fromIntegral 0` instead of `0`.
  - Similar, the `[x..y]` syntax generates a `base:GHC.Enum.Enum` constraint, `RebindableSyntax` does not help (GHC bug?)
  - `StablePtr`, as used in `GHC.Stable`
  - `Typeable`, `Show` when used in `deriving`. Can probably be avoided by hand-writing instances. `Read` can probably move completely out.
  - `error` has its type wired in GHC when in package base; This is used in a hack in [GHC/Err.hs-boot](https://github.com/ghc/packages-base/blob/master/GHC/Err.lhs-boot). Work-around: Import `GHC.Types` in `GHC/Err.lhs-boot`
  - The `Monad` constraint on do-notation expects the definition to live in base. `RebindableSyntax` helps, but requires to define a local `ifThenElse` function.
- The ST Monad can (and should) be provided independently of IO, but currently functions like `unsafeIOToST` are provided in the `Control.Monad.ST` namespace.


  


### First attempt


Joachim has started a first attempt to pull stuff out of the bottom of base. See [https://github.com/nomeata/packages-base/blob/base-split/README.md](https://github.com/nomeata/packages-base/blob/base-split/README.md) for an overview of progress and a description of changes. Use `git clone git://github.com/nomeata/packages-base.git; git checkout base-split` to experiment. This \*does\* try to split out as many packages as possible, just to see what is possible.
