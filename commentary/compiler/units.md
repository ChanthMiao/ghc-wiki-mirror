This is a work in progress by @hsyl20, stay tuned.

GHC's handling of units (a.k.a. GHC's "packages" but see the terminology below) has been under heavy lifting for some time to accommodate new needs (e.g. IDE support) and to fix long standing bugs (e.g. support for compiler plugins in cross-compiling GHC). This page summarizes the current plan.

## Terminology

Terminology related to units can be very confusing especially because it has changed over time, for example when Backpack has been implemented or when Cabal started to support several library components in the same package. We strive to use the following terminology from now on. Note that many command-line flags, documents and codes have not been updated to reflect this yet...

* **Package**: a Cabal package

* [Package] **component**: packages may contain several components (libraries, executables, testsuite). The most interesting one for GHC are libraries because they can be reused by other components.

* **Unit**: a compiled library component. Contains interfaces and object codes for the library modules (and even Haskell code for Backpack indefinite units).

* **Unit key**: units may be compiled in different ways. There are assigned (by Cabal) a unit key (usually composed of the package name, the package version, the component name, and a hash of the compiling options and dependencies unit keys)

* **Unit id**: another unit identifier used to declare unique symbols. Usually unit id = unit key, except for some boot packages which must use fixed symbol names (cf -this-unit-id flag). E.g. `base` package can have `unit id = base` and `unit key = base-4.16.0`.

* **Unit database**: a set of installed units. Often referred to as a “package database”. GHC doesn’t deal with units directly but with unit databases. However it can only read them, while ghc-pkg program can read/write them (it depends on Cabal library while GHC doesn’t).

* **Indefinite** unit: a unit which depends on an indefinite unit or with some module holes. Module holes can be filled by other modules fulfilling the module hole signature by instantiating the indefinite unit. Module holes can be instantiated with other module holes in which case the resulting module hole inherits of their signatures merged into one.

## Entities overview

![units.svg](uploads/3fea59379c5f34afd8a5e5c76ec04759/units.svg)

