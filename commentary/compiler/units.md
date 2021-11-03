GHC's handling of units (a.k.a. GHC's "packages" but see the terminology below) has been under heavy lifting for some time to accommodate new needs (e.g. IDE support) and to fix long standing bugs (e.g. support for compiler plugins in cross-compiling GHC). This page summarizes the current plan.

## Background

### Unit terminology

Terminology related to units can be very confusing especially because it has changed over time, for example when Backpack has been implemented or when Cabal started to support several library components in the same package. We strive to use the following terminology from now on. Note that many command-line flags, documents and codes have not been updated to reflect this yet...

* **Package**: a Cabal package

* [Package] **component**: packages may contain several components (libraries, executables, testsuite). The most interesting ones for GHC are libraries because they can be reused by other components.

* **Unit**: a compiled library component. Contains interfaces and object codes for the library modules.

* **Unit key**: units may be compiled in different ways. There are assigned (by Cabal) a unit key (usually composed of the package name, the package version, the component name, and a hash of the compiling options and dependencies unit keys)

* **Unit id**: another unit identifier used to declare unique symbols. Usually unit id = unit key, except for some boot packages which must use fixed symbol names. E.g. `base` package can have `unit id = base` and `unit key = base-4.16.0`. The unit-id is given to GHC via `-this-unit-id` command-line flag.

* **Unit database**: a set of installed units. Often referred to as a “package database”. GHC doesn’t deal with units directly but with unit databases. However it can only read them, while ghc-pkg program can read/write them (it depends on Cabal library while GHC doesn’t).

* **Unit info**: each unit in the unit database has associated unit info. It contains everything needed for GHC to reuse a unit: foreign library dependencies, dependencies on other units, available modules, etc.

* **Indefinite** unit: a unit which depends on an indefinite unit or with some module holes. Module holes can be filled with other modules by *instantiating* the indefinite unit. Module holes have a *signature* (a kind of interface, think `.hs-boot` files) and modules used to fill a module hole have to be compatible with its signature. Module holes can be instantiated with other module holes in which case the resulting module hole inherits of their signatures merged into one.

### Internal unit terminology

This is the terminology for the entities only used inside the compiler.

* **Home unit**: a/the unit for which GHC is currently type-checking/compiling some module(s). The home-unit unit-id is given to GHC with `-this-unit-id` command-line flag.

* **External unit**: a unit installed in a unit database and used as a dependency or loaded into GHCi.

* **Unit state**: consolidated virtual database of units.

* **ExternalPackageState (EPS)**: a cache for things read from external units.

* **HomePackageTable (HPT)**: cache for home-unit’s modules already compiled.


## Work in progress

### Goals

* G0: generic unit code enhancement/documentation/simplification
* G1: add support for plugins in cross-compiling GHCs #14335
* G2: add support for more multiple home-units in a session (e.g. to load several components in GHCi)

### Current implementation overview

* Unit related command-line flags are used to build a UnitConfig describing external unit dbs, their visibility, etc.

* UnitConfig is used to build a UnitState: I've added this intermediate UnitConfig between DynFlags and UnitState to make it easier to create a UnitState not directly defined from DynFlags (e.g. for G1).

* UnitState provides the map from ModuleName to Module, the wiring-map (i.e. which units are base, ghc-prim, rts, etc.), and some other stuff (preload units to link, etc.)

* Since Backpack, the HomeUnit isn't a simple UnitId: it can refer to an indefinite unit to instantiate with modules from other units ("instantiations"). It is created after the UnitState as it needs the wiring-map (instantiations can be wired-in units).

* But to create the UnitState we need to know if we are type-checking an indefinite HomeUnit because in this case we can instantiate external units on the fly (they don't really need to exist on disk). This mess is only described in http://blog.ezyang.com/2016/08/optimizing-incremental-compilation/ afaik.

* UnitEnv contains UnitState, HomeUnit, their respective caches (EPS/HPT), and some other stuff (e.g. namever to compute library names).

### Tasks

1. [G0] UnitEnv is not self-sufficient: DynFlags are queried to detect one-shot mode here and there and to bypass the HPT in this case. We should make the HPT optional in UnitEnv instead. One-shot would then imply `ue_hpt = Nothing`.

1. [G1] Support optional HomeUnit in interface loading API. The UnitEnv for plugins won't have a home unit in cross-compilers. `ue_home_unit` has already type `Maybe HomeUnit` but we need to ensure that setting it to `Nothing` doesn't panic in our use cases (i.e. remove uses of `unsafeGetHomeUnit`).

1. [G2] Support a graph of HomeUnit in UnitEnv. Each HomeUnit would need its own view of external units (see ExternalUnitView below) and of other home-units.

1. [G0] UnitKey vs UnitId: UnitKeys are unit identifier in unit DBs and UnitIds are unit identifier everywhere else. Currently we convert from UnitKey to UnitId at an arbitrary point and the wiring-map is defined as `Map UnitId UnitId`. We should fix this and define it as `Map UnitId UnitKey`. It's not as trivial as it looks because we mix up UnitId/UnitKey quite freely. Fixing this would make the code safer.

1. [G0,G1,G2] Split UnitState in two parts: ExternalUnitDB and ExternalUnitView:
    - ExternalUnitDB is a cache of what is on disk: it can be shared for different use cases (e.g. by different HomeUnits)
    - ExternalUnitView is a view of the ExternalUnitDB: it handles module visibility, unit thinning and module renaming, Safe Haskell overriding flags ("distrust all"), etc. 

1. [G0,G1] Split plugin state from UnitState. Currently the UnitState contains two ModuleName provider maps: one for plugins and one for target code. We should have one ExternalUnitView for each instead. For now they could be based on the same ExternalUnitDB, but for our G1 goal we need to support using two different ExternalUnitDB.

1. [G0,G2] Remove state from DynFlags: dynamicToo handling uses an IORef in DynFlags. But it's purely a driver thing and it doesn't belong there.

1. [G1] CmdLine: Add command-line flags to configure unit databases for plugins.

1. [G2] CmdLine: Allow users to provide UnitInfo for home units. Especially foreign libraries and other stuff needed to link/load the unit.

1. [G1] Add `--target=self` support. This should target GHC host platform, ways, boot libraries used to build GHC (not the target ones) and use the internal interpreter. This would be used to build compiler plugins.

1. [G1] Support plugins in home unit. GHC currently supports plugins in the home unit. It's quite wrong because these plugins are built against the target code environment, not the plugin one. So it should only be allowed in `--target=self` mode. In this case we would have a single ExternalUnitDB, two ExternalUnitView (to still support plugin package flags as currently) and allow fetching plugins from the home-unit. If we are not in `--target=self` mode, just disallow plugins in the home-unit for now. Perhaps in the future we could have two home-units: one for plugins and another for target code.


### Future entities overview

![units.svg](uploads/548be9e2eb9cd383f7de0a20a3deb7a5/units.svg)