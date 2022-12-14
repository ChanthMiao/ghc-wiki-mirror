## Current Status


It is possible to install multiple instances of the same package version with my forks of cabal and ghc. Quite a few problems remain.


See also [Commentary/Packages/MultiInstances](commentary/packages/multi-instances)

### Unique Install Location


When specifying the install location there is a new variable $unique available. It is resolved to a random number by cabal-install during configuring. The default libsubdir for cabal-install should be "$packageid-$unique" for example "mtl-2.1.2-1222131559". Cabal the library does not understand $unique so multiple instances of the same package version installed via "runhaskell Setup.hs install" are still problematic.

### ghc-pkg


ghc-pkg never removes registered packages when registering a new one. Even if a new package with the same `InstalledPackageId` as an existing package is registered. Or if a new package that points to the same install directory is registered. `ghc-pkg` should probably check this and issue a warning.

### Adhoc dependency resolution


A new field `timestamp` was added to `InstalledPackageInfo`. It is set by Cabal the library when registering a package. It is used by Cabal the library, GHC and cabal-install to choose between different instances of the same package version.

### Detect whether an overwrite happens and warn about it


Currently cabal-install still warns about dangerous reinstalls and requires `--force-reinstalls` when it is sure a reinstall would happen. The correct behaviour here would be to detect if a reinstall causes overwriting (because of a version of ghc-pkg that does this) and warn only in this case. In this implementation reinstalls are not dangerous anymore.

### Communicate the `InstalledPackageId` back to cabal-install


An `InstallPlan` contains installed packages as well as packages to be installed and dependencies between those. We want to specify all of these dependencies with an `InstalledPackageId`. Unfortunately the `InstalledPackageId` is determined after installation and therefore not available for not yet installed packages. After installation it would have to be somehow communicated back to cabal-install. The current workaround is to only specify those packages that were previously installed with an `InstalledPackageId` and trust on Cabal picking the instance that was most recently (during execution of this install plan) installed for the other ones.

### Garbage Collection


A garbage collection should offer the removal of a certain package specified by `InstalledPackageId`, the removal of broken packages and the removal of probably unnecessary packages. A package is unnecessary if all packages that depend on it are unnecessary (this includes the case that no package depends on it) and it is not the most recently installed instance for its version. All of this should be accompanied by a lot of "are you sure" questioning.

### About Shadowing


GHC has the concept of shadowing. It was introduced as far as i understand (correct me please) because when combining the global and the user package databases you could end up with two instances of the same package version. The instance in the user database was supposed to shadow the one in the global database. Now that there are multiple instances of the same package version even in one package database this concepts needs to be rethought. This is non-trivial because flags asking for a package version as well as flags requiring a certain instance need to be taken into account.

### About Unique Identifier


Currently a big random number is created by cabal-install during configuration and passed to Cabal to be appended to the `InstalledPackageId` before registering. The reason is that the `InstalledPackageId` still contains the ABI hash which is only known after compilation. I personally would like the `InstalledPackageId` to be the name of the package, the version and a big random number. This could be determined before compilation, used as the `libsubdir` and baked into `package_Paths.hs`. Since it would be determined by cabal-install it would also make communicating the InstalledPackageId back to cabal-install after an installation unnecessary. The problem is that the `InstalledPackageId` would not be deterministic anymore.

## Original Plan


Cabal and GHC do not support multiple instances of the same package version installed at the same time. If a second instance of a package version is installed it is overwritten on the file system as well as in the `PackageDB`. This causes packages that depended upon the overwritten instance to break. The idea is to never overwrite an installed package. As already discussed in [https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Packages/MultiInstances](https://gitlab.haskell.org/trac/ghc/wiki/Commentary/Packages/MultiInstances) the following changes need to be made:

- Cabal should install packages to a location that does not just depend on name and version,
- `ghc-pkg` should always add instances to the `PackageDB` and never overwrite them,
- `ghc --make`, `ghci`, and the configure phase of Cabal should select suitable instances according to some rule of thumb (similar to the current resolution technique),
- we want to be able to make more fine-grained distinctions between package instances than currently possible, for example by distinguishing different build flavours or "ways" (profiling, etc.)
- `cabal-install` should still find an `InstallPlan`, and still avoid unnecessarily rebuilding packages whenever it makes sense
- some form of garbage collection should be offered to have a chance to reduce the amount of installed packages

## Hashes and identifiers


There are three identifiers:

- `XXXX`: the identifier appended to the installation directory so that installed packages do not clash with each other
- `YYYY`: the `InstalledPackageId`, which is an identifier used to uniquely identify a package in the package database.
- `ZZZZ`: the ABI hash derived by GHC after compiling the package


The current situation:

- `XXXX`: is empty, which is bad (two instances of a package install in the same place)
- `YYYY`: is currently equal to `ZZZZ`, which is bad because we need to make more distinctions:

  - we need to distinguish between two packages that have identical ABIs but different behaviour (e.g. a bug was fixed)
  - we need to distinguish between two instances of a package that are compiled against different dependencies, or with different options, or compiled in a different way (profiling, dynamic)  


Some notes:

- `XXXX` must be decided *before* we begin compiling, because we have to generate the `Paths_P.hs` file that is compiled along with the package, whereas `ZZZZ` is only available *after* we have compiled the package.
- `ZZZZ` is not uniquely determined by the compilation inputs (see #4012), although in the future we hope it will be
- It is desirable that when two packages have identical `YYYY` values, then they are compatible, even if they were built on separate systems.  Note that this is not guaranteed even if `YYYY` is a deterministic function of the compilation inputs, because `ZZZZ` is non-deterministic (previous point).  Hence `YYYY` must be dependent on `ZZZZ`. 
- It is desirable that `YYYY` be as deterministic as possible, i.e. we would rather not use a GUID, but `YYYY` should be determined by the compilation inputs and `ZZZZ`.  We know that `ZZZZ` is currently not deterministic, but in the future it will be, and at that point `YYYY` will become deterministic too, in the meantime `YYYY` should be no less deterministic than `ZZZZ`.


Our proposal:

- We define a new *Cabal Hash* that hashes the compilation inputs (the `LocalBuildInfo` and the contents of the source files)
- `XXXX` is a GUID.

  - Why not use the *Cabal Hash*?  We could, but then there could conceivably be a clash. (Andres - please expand this point, I have forgotten the full rationale).
- `YYYY` is the combination of the *Cabal Hash* and `ZZZZ` (concatenated)
- `ZZZZ` is recorded in the package database as a new field `abi-hash`.

  - When two packages have identical `ZZZZ`s then they are interface-compatible, and the user might in the future want to change a particular dependency to use a different package but the same `ZZZZ`.  We do not want to make this change automatically, because even when two packages have identical `ZZZZ`s, they may have different behaviour (e.g. bugfixes).

## Install location of installed Cabal packages


Currently the library part of packages is installed to `$prefix/lib/$pkgid/$compiler`. For example the `GLUT` package of version 2.3.0.0 when compiled with GHC 7.4.1 when installed globally lands in `/usr/local/lib/GLUT-2.3.0.0/ghc-7.4.1/`. This is the default path. It is completely customizable by the user. In order to allow multiple instances of this package to coexist we need to change the install location to a path that is unique for each instance. Several ways to accomplish this have been discussed:

### Hash


Use a hash to uniquely identify package instances and make the hash part of both the InstalledPackageId and the installation path.


The ABI hash currently being used by GHC is not suitable for unique identification of a package, because it is nondeterministic and not necessarily unique. In contrast, the proposed Cabal hash should be based on all the information needed to build a package.


This approach requires that we know the hash prior to building the package, because 
there is a data directory (per default under $prefix/share/$pkgid/) that is baked into Paths_foo.hs in preparation of the build process.

### Unique number


Use a unique number as part of the installation path.


A unique number could be the number of packages installed, or the number of instances of this package version already installed, or a random number. It is important that the numbers are guaranteed to be unique system-wide, so the counter-based approaches are somewhat tricky.


The advantage over using a hash is that this approach should be very simple to implement. On the other hand, identifying installed packages (see below) could possibly become more difficult, and migrating packages to other systems is only possible if the chance of collisions is reasonably low (for example, if random numbers are being used).

1. The unique number is also part of the installed package id.

1. We can use another unique identifier (for example, a Cabal hash) to identify installed packages. In this case, that identifier would be allowed to depend on the output of a package build.

## `ghc-pkg`

`ghc-pkg` currently identifies each package by means of an `InstalledPackageId`. At the moment, this id has to be unique per package DB and is thereby limiting the amount of package instances that can be installed in a single package DB at one point in time.


In the future, we want the `InstalledPackageId` to still uniquely identify installed packages, but in addition to be unique among all package instances that could possibly be installed on a system. There's still the option that one InstalledPackageId occurs in several package DBs at the same time, but in this case, the associated packages should really be completely interchangeable. \[If we want to be strict about this, we'd have to include the ABI hash in the `InstalledPackageId`.\]


Even though, as discussed above, the ABI hash is not suitable for use as the `InstalledPackageId` given these changed requirements, we will need to keep the ABI hash as an essential piece of information for ghc itself.

`ghc-pkg` is responsible for storing all information we have about installed packages. Depending on design decisions about the solver and the Cabal hash, further information may be required in `ghc-pkg`'s description format (see below).


The following fields will be added to the description format:


A field *Way* of type `[String]`. It tracks the way in which the package was compiled. It is a subset of `{v,d,p}`. "v" means vanilla, "d" means dynamic linking and "p" means profiling. Other ways may be added later.


A `timestamp` of the time when the package was installed (or built?). It is used by GHC and Cabal to put a preference on the latest package of a certain version.


A currently empty but extensible set of fields starting with "x-cabal-...". `ghc-pkg` ignores them when parsing. During the resolution phase `cabal-install` might use them to decide compatibility between packages.


A field abi-hash that contains the ABI hash because it is no longer stored implicitly as part of the `InstalledPackageId`.

## Simplistic dependency resolution


The best tool for determining suitable package instances to use as build inputs is `cabal-install`. However, in practice there will be many situations where users will probably not have the full `cabal-install` functionality available:

1. invoking GHCi from the command line,
1. invoking GHC directly from the command line,
1. invoking the configure phase of Cabal (without using `cabal-install`).


In these cases, we have to come up with a suitable selection of package instances, and the only info we have available are the package DBs plus potential command line flags. Cabal will additionally take into account the local constraints of the package it is being invoked for, whereas GHC will only consider command-line flags, but not modules it has been invoked with.


Currently if GHC is invoked by the user it does some adhoc form of dependency resolution. The most common case of this is using ghci. If there are multiple instances of the same package in the `PackageDBStack` the policy used to select a single one prefers DBs higher in the stack. It then prefers packages with a higher version. Once we allow package instances with the same version within a single package DB, we need to refine the algorithm. Options are:

- pick a random / unspecified instances
- use the time of installation
- user-specified priorities
- use the order in the `PackageDB`
- look at the transitive closure of dependencies and their versions
- build a complex solver into GHC


Picking a random version is a last resort. A combination of installation time and priorities seems rather feasible. It makes conflicts unlikely, and allows to persistently change the priorities of installed packages. Using the order in the package DB is difficult if directories are being used as DBs. Looking at the transitive closure of dependencies makes it hard to define a total ordering of package instances. Adding a complex solver is unattractive unless we find a way to reuse `cabal-install`'s functionality within GHC, but probably we do not want to tie the two projects together in this way.

## Build flavours


Once we distinguish several package instances with the same version, we have a design decision how precise we want that distinction to be.


The minimal approach would be to just take the transitive dependencies into account. However, we might also want to include additional information about builds such as Cabal flag settings, compiler options, profiling, documentation, build tool versions, external (OS) dependencies, and more.


These differences have to be tracked. The two options we discuss are to store information in the `ghc-pkg` format, or to incorporate them in a Cabal hash (which is then stored). Both options can be combined.

### The Cabal hash


\[A few notes about where to find suitable information in the source code:\]


A build configuration consists of the following:


The Cabal hashes of all the package instances that are actually used for compilation. This is the environment. It is available in the `installedPkgs` field of `LocalBuildInfo` which is available in every step after configuration. It can also be extracted from an `InstallPlan` after dependency resolution.


The compiler, its version and its arguments and the tools and their version and their arguments. Available from LocalBuildInfo also. More specifically: `compiler`, `withPrograms`, `withVanillaLib`, `withProfLib`, `withSharedLib`, `withDynExe`, `withProfExe`, `withOptimization`, `withGHCiLib`, `splitObjs`, `stripExes`. And a lot more. \[Like what?\]


The source code. This is necessary because if the source code changes the result of compilation changes. For released packages I would assume that the version number uniquely identifies the source code. A hash of the source code should be available from hackage to avoid downloading the source code. For an unreleased package we need to find all the source files that are needed for building it. Including non-haskell source files. One way is to ask a source tarball to be built as if the package was released and then hash all the sources included in that.


OS dependencies are not taken into account because i think it would be very hard.

### Released and Unreleased packages


If we cabal install a package that is released on hackage we call this a **clean install**. If we cabal install an unreleased package we call this a **dirty install**. Clean installs are mainly used to bring a package into scope for ghci and to install applications. While they can be used to satisfy dependencies this is discouraged. For released packages the set of source files needed for compilation is known. For unreleased packages this is currently not the case.

## Dependency resolution in cabal-install


There are two general options for communicating knowledge about build flavors to the solver:

1. **the direct way**: i.e., all info is available to ghc-pkg and can be communicated back to Cabal and therefore the solver can figure out if a particular package is suitable to use or not, in advance;

1. **the agnostic way**: this is based on the idea that the solver at first doesn't consider installed packages at all. It'll just do resolution on the source packages available. Then, taking all build parameters into account, Cabal hashes will be computed, which can then be compared to hashes of installed packages.


Reusing installed packages instead of rebuilding them is then an optimization of the install plan.


The agnostic way does not require `ghc-pkg` to be directly aware of all the build parameters, as long as the hash computation is robust


The options are to support either both by putting all info into `InstalledPackageInfo` or to support only the second option by just putting a hash into `InstalledPackageInfo`. The disadvantage of supporting both is that `InstalledPackageInfo` would have to change more often. This could be fixed by explicitly making the `InstalledPackageInfo` format extensible in a backwards-compatible way.


The advantages of having all info available, independently of the solver algorithm, are that the info might be useful for other tools and user feedback. 


Possible disadvantages of the agnostic approach could be that is is a rather significant change and can probably not be supported in a similar way for other Haskell implementation. Also, in the direct approach, we could in principle allow more complex compatibility rules, such as allowing non-profiling libraries to depend on profiling libraries.


Also, even if we go for the agnostic approach, we still have to be able to handle packages such as base or ghc-prim which are in general not even available in source form.


On the other hand, the agnostic approach might lead to more predictable and reproducible solver results across many different systems.

## Garbage Collection


The proposed changes will likely lead to a dramatic increase of the number of installed package instances on most systems. This is particularly relevant for package developers who will conduct lots of dirty builds that lead to new instances being installed all the time.


It should therefore be possible to have a garbage collection to remove unneeded packages. However, it is not possible for Cabal to see all potential reverse dependencies of a package, so automatic garbage collection would be extremely unsafe.


Options are to either offer an interactive process where packages that look unused are suggested for removal, or to integrate with a sandbox mechanism. If, for example, dirty builds are usually installed into a separate package DB, that package DB could just be removed completely by a user from time to time.


The garbage collection functionality is part of cabal-install not of ghc-pkg. As a first approximation gc does not remove files only unregisters packages from the `PackageDB`.

## Currently open design decisions

### `InstalledPackageId` and install path


Options for uniquely identifying `InstalledPackageId`:

- Cabal hash only
- Cabal + ABI hash (truly unique)
- random number


Options for identifying install path:

- Cabal hash
- random number


ABI hash cannot be in install path because it's only available after build.

### Handling of dirty builds


How should hash computation work for dirty builds?

- Use a random number even if we otherwise use hashes
- Hash the complete build directory
- Attempt to make a clean (sdist-like) copy or linked copy of the sources and hash and build from that.
- Use the Cabal file to determine the files that would end up in an sdist and hash those directly without copying.


The third option has the advantage(?) that the build is more guaranteed to use only  files actually mentioned in the Cabal file.

### Build flavours


To what degree should we distinguish package instances?

- Only package versions transitively
- Ways and Cabal flags
- Everything Haskell-specific info that we can query
- Even non-Haskell-specific inputs such as OS dependencies

### `InstalledPackageInfo` and solver algorithm


Options for `InstalledPackageInfo`:

- Only add Cabal hash.
- Add (nearly) all information, but in an extensible format.
- Add all information in a way that `ghc-pkg` itself can use it.


\[These aren't necessarily mutually exclusive.\]


Options for the solver:

- Direct (see above): requires a certain amount of info in the `InstalledPackageInfo`.

- Agnostic (except for builtin packages): could be done with only the Cabal hash in `InstalledPackageInfo`.

### Simplistic dependency resolution


Options (in order of preference):

- use the time of installation
- user-specified priorities
- pick a random / unspecified instances
- (build a complex solver into GHC)


A combination of the first two seems possible and useful.

## Related topics


In the following, we discuss some other issues which are related to the multi-instance problem, but not necessarily directly relevant in order to produce an implementation.

### Separating storage and selection of packages


Currently the two concepts of storing package instances (cabal store) and selecting package instances for building (environment) are conflated into a `PackageDB`. Sandboxes are used as a workaround to create multiple different environments. But they also create multiple places to store installed packages. The disadvantages of this are disk usage, compilation time and one might lose the overview. Also if the multi-instance restriction is not lifted sandboxes will eventually suffer from the same unintended breakage of packages as non-sandboxed `PackageDB`s.
There should be a separation between the set of all installed packages called the cabal store and a subset of these called an environment. While the cabal store can contain multiple instances of the same package version an environment needs to be consistent. An environment is consistent if for every package version it contains only one instance of that package version.

### First class environments


It would be nice if we had some explicit notion of an environment.

## Questions to remember


Should the cabal version be part of the hash?


Does the hash contain characters conflicting under windows?


What about builtin packages like ghc-prim, base, rts and so on?


Inplace Registration?


Who has assumptions about the directory layout of installed packages?


Executables?


Haddock?


Installation Planner?


Custom Builds and BuildHooks?


Other Compilers, backwards compatibility?


What is ComponentLocalBuildInfo for?
