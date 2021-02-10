This is a work in progress by @hsyl20, stay tuned.

GHC's handling of units (a.k.a. GHC's "packages" but see the terminology below) has been under heavy lifting for some time to accommodate new needs (e.g. IDE support) and to fix long standing bugs (e.g. support for compiler plugins in cross-compiling GHC). This page summarizes the current plan.

## Background

### Unit terminology

Terminology related to units can be very confusing especially because it has changed over time, for example when Backpack has been implemented or when Cabal started to support several library components in the same package. We strive to use the following terminology from now on. Note that many command-line flags, documents and codes have not been updated to reflect this yet...

* **Package**: a Cabal package

* [Package] **component**: packages may contain several components (libraries, executables, testsuite). The most interesting ones for GHC are libraries because they can be reused by other components.

* **Unit**: a compiled library component. Contains interfaces and object codes for the library modules (and even Haskell code for Backpack indefinite units).

* **Unit key**: units may be compiled in different ways. There are assigned (by Cabal) a unit key (usually composed of the package name, the package version, the component name, and a hash of the compiling options and dependencies unit keys)

* **Unit id**: another unit identifier used to declare unique symbols. Usually unit id = unit key, except for some boot packages which must use fixed symbol names (cf -this-unit-id flag). E.g. `base` package can have `unit id = base` and `unit key = base-4.16.0`.

* **Unit database**: a set of installed units. Often referred to as a “package database”. GHC doesn’t deal with units directly but with unit databases. However it can only read them, while ghc-pkg program can read/write them (it depends on Cabal library while GHC doesn’t).

* *Unit info*: each unit in the unit database has associated unit info. It contains everything needed for GHC to reuse a unit: foreign library dependencies, dependencies on other units, available modules, etc.

* **Indefinite** unit: a unit which depends on an indefinite unit or with some module holes. Module holes can be filled with other modules by *instantiating* the indefinite unit. Module holes have a *signature* (a kind of interface, think `.hs-boot` files) and modules used to fill a module hole have to be compatible with its signature. Module holes can be instantiated with other module holes in which case the resulting module hole inherits of their signatures merged into one.

### Internal unit terminology

This is the terminology for the entities only used inside the compiler.

* *Home unit*: a/the unit for which GHC is currently type-checking/compiling some module(s). One of our goals is to add support for more than one home unit at the same time (e.g. to load projects consisting of several units into GHCi or in an IDE).

* *External unit*: a unit installed in a unit database and used as a dependency or loaded into GHCi.

* *Unit state*: consolidated virtual database of units. We want to split it into an external unit database and external unit view to help supporting plugins in cross-compilers and multiple home-units (home units share the external unit database but not necessarily the external unit view).

* *ExternalPackageState (EPS)*: a cache for things read from external units.

* *HomePackageTable (HPT)*: cache for home unit’s already compiled modules.


### Entities overview

![units.svg](uploads/548be9e2eb9cd383f7de0a20a3deb7a5/units.svg)


## Roadmap

### Generic

- [ ] Split UnitState in two parts: ExternalUnitDB and ExternalUnitView

### Multiple home-unit support

- [ ] Allow users to provide UnitInfo for home units. Especially foreign libraries and other stuff needed to link/load the unit.
- [ ] Associate the HPT (interface cache for HomeUnit) to each HomeUnit

### Compiler plugins in cross-compilers

- [ ] Add command-line flags to configure the unit database stack for plugins
- [ ] Make the HomeUnit optional in interface loading API: the environment for plugins won't have a home unit in cross-compilers
- [ ] Add support for plugins in the home unit by using two home units