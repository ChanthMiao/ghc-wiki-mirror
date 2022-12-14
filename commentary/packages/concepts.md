Package keys, installed package IDs, ABI hashes, package names and
versions, Nix-style hashes, ... there's so many different identifiers,
what do they all mean?  I think the biggest source of confusion (for
myself included) is keeping straight not only what these terms mean,
but also what people want them to mean in the future, and what we
*actually* care about.  So I want to help clarify this a bit, by
clearly separating the *problem you are trying to solve* from *how you are solving the problem*.


The content here overlaps with [wiki:Commentary/Packages](commentary/packages) but is looking at the latest iteration of the multi-instances and Backpack work.


See also `Note [The identifier lexicon]` in `compiler/basicTypes/Module.hs`.


Some relevant tickets and pages: #10622 [wiki:Commentary/Compiler/Units](commentary/compiler/units)

## What problems do we need to solve?


When we come up with identification schemes for packages, we are trying to solve a few problems:

- **\[SYMBOL\]**

  What symbol names should we put in the binary? (e.g., the `foozm0zi1` in `foozm0zi1_A_DZCF_closure`)

- **\[ABI\]**

  When can I swap out one compiled package with another WITHOUT recompiling, i.e. what is the ABI of the package? Equal ABIs implies equal symbols, though not vice versa. ABI is usually computed after compilation is complete.

  - ABI can serve as correctness condition: if we link against a specific ABI, we can be sure that anything with an equivalent ABI won't cause our package to segfault.
  - ABI can also serve as an indirection: we linked against an ABI, anything that is compatible can be hotswapped in without compilation. In practice, this capability is rarely used by users because it's quite hard to compile a package multiple times with the same ABI, because (1) compilation is nondeterministic, and (2) even if no types change, a change in implementation can cause a different exported unfolding, which is ABI relevant.

- **\[SOURCE\]**

  What is the unit of distribution? In other words, when a maintainer uploads an sdist to Hackage, how do you identify that source tarball?

  - On Hackage, a package name plus version uniquely identifies an sdist.  This is enforced by community standards; in a local development environment, this may not hold since devs will edit code without updating the version number. Call this \[WEAK SOURCE\].
  - Alternately, a cryptographic hash of the source code uniquely identifies the stream of bytes.  This is enforced by math. Call this \[STRONG SOURCE\].

- **\[LIBRARY\]**

  When you build a library, you get an `libfoo.so` file. What identifies an OS level library?

- **\[NIX\]**

  What is the full set of source which I can use to reproduceably build a build product?

  - In today's Cabal, you could approximate this by taking \[WEAK SOURCE\] of a package, as well as all of its transitive dependencies. Call this \[WEAK NIX\].
  - The Nix approach is to ensure deterministic builds by taking the hash of the source \[STRONG SOURCE\] and also recursively including the \[NIX\] of each direct dependency. Call this \[STRONG NIX\].
  - Note that \[ABI\] does NOT imply \[NIX\]; a package might be binary compatible but do something different, and in a Nix model they should be recorded differently.

- **\[TYPES\]**

  When are two types the same?  If there are from differing packages, they are obviously different; if they are from the same package, they might still be different if the dependencies were different in each case.

  - Types show up in error message, so this is a USER VISIBLE notion.  Many people have (cogently) argued that this should be AS SIMPLE as possible, because there's nothing worse
  than being told that Data.ByteString.ByteString is not
  equal to Data.ByteString.ByteString (because they were from
  different packages.)

</td></tr></table>

## Current mechanisms


Today, we have a lot of different MECHANISMS for identifying these:

- **Package Name**

  Something like "lens"

- **Package Version**

  Something like "0.1.2"

- **(Source) Package ID**

  Package name plus version.  With Hackage today, this identifies a unit of distribution: given a package ID you can download a source tarball \[SOURCE\] of a package (but not build it). Pre-GHC 7.10, the package ID was used for library identification, symbols and type-checking (\[LIBRARY\], \[SYMBOL\] and \[TYPES\]), but this is no longer the case.

- **Installed Package ID**

  Package name, package version, and the output of ghc --abi-hash.  This is currently used to uniquely identify a built package, although technically it only identifies \[ABI\].

- **Package Key (new in 7.10)**

  Hash of package name, package version, the package keys of all
textual dependencies the package included, and in Backpack
a mapping from hole name to module by package key.
  In GHC 7.10 this is used for library identification, symbols and type-checking (\[LIBRARY\], \[SYMBOL\] and \[TYPES\]).  Because it includes package keys of textual dependencies, it also distinguishes between different dependency resolutions, ala \[WEAK NIX\].

## New concepts for Backpack


First, we have to take the concept of an InstalledPackageId and make it more precise, having it identity components rather than packages.

- **Component ID**

  The package name, the package version, the name of the component (blank in the case of the default library component), and the hash of source code sdist tarball, selected Cabal flags (not the command line flags), GHC flags, hashes of direct dependencies of the component (the `build-depends` of the library in the Cabal file).


Then in Backpack we have these concepts:

- **Indefinite/definite unit**

  An indefinite unit is a single unit which hasn't been instantiated; a definite unit is one that has an instantiation of its holes.  Units without holes are both definite and indefinite (they can be used for both contexts).

- **Indefinite unit record (in "logical" indefinite unit database)**

  An indefinite unit record is the most general result of type-checking a unit without any of its holes instantiated.  It consists of the types of the modules in the unit (ModIfaces) as well as the source code of the unit (so that it can be recompiled into a definite unit). Indefinite unit records can be installed in the "indefinite unit database."

- **Definite unit record (previously installed package record, in the definite unit database, previously the installed package database)**

  A definite unit record is a fully-instantiated unit with its associated library. It consists of the types and objects of the compiled unit; they also contain metadata for their associated package.  Definite unit records can be installed in the "definite unit database" (previously known as the "installed package database.")


To handle these, we need some new identifiers:

- **Unit Id (previously named Package Key)**

  For Backpack units, the unit ID is the component ID plus a mapping from holes to modules (unit key plus module name). For non-Backpack units, the unit ID is equivalent to the component source hash (the hole mapping is empty). These serve the role of \[SYMBOL, LIBRARY, TYPES\]. (Partially definite unit keys can occur on-the-fly during type checking.) When all of the requirements are filled (so there is no occurrence of HOLE), the unit key serves as the primary key for the installed unit database. (We might call this an "installed unit ID" in this context) The unit ID "HOLE" is a distinguished unit ID, which is for the "hole package", representing modules which are not yet implemented (there is not actually a unit named hole, it's just a notational convention).

- **Module**

  A unit ID plus a module name.

## Features


There are a number of enhancements proposed for how Cabal handles packages, which have often been conflated together. I want to clearly separate them out here:

- **Non-destructive installs**

  If I have package foo-0.2 compiled against bar-0.1, and a different build compiled against bar-0.2, I should be able to put them in the same installed package database.  THIS IS HIGH PRIORITY.

- **Views**

  If I have package foo compiled against bar-0.1, and baz compiled against bar-0.2, these two packages aren't usable together (modulo private dependencies, see below).  Views are a UI paradigm making it easier for users to work in a universe where foo is available, or a universe where baz is available, but not both simultaneously. Cabal sandboxes are views but without a shared installed package database.  This is lower priority, because if you use cabal-install to get a coherent dependency set, you'll never see both foo and baz at the same time; the primary benefit of this is to assist with direct use of GHC/GHCi, however, it is generally believed that non-destructive installs will make it difficult to use GHC/GHCi by itself.

- **Private dependencies**

  If I have a package foo-0.2 which depends on a library bar-0.1, but not in any externally visible way, it should be allowed for a client to separately use bar-0.2. This is LOW priority; amusingly, in 7.10, this is already supported by GHC, but not by Cabal.

- **Hot swappable libraries**

  If I install a library and it's assigned ABI hash 123abc, and then I install a number of libraries that depend on it, hot swappable library means that I can replace that installed library with another version with the same ABI hash, and everything will keep working. This feature is accidentally supported by GHC today, but no one uses it (because ABIs are not stable enough); we are willing to break this mode of use to support other features.

## Constraints


For an implementer, it is best if each problem is solved separately.  However, Simon has argued strongly it is best if we REDUCE the amount of package naming concepts. You can see this in pre-7.10 GHC, where the package ID (package name + version) was used fulfill many functions: linker symbols, type identity as well as being a unit of distribution.


So the way I want to go about arguing for the necessity of a given identifier is by showing that it is IMPOSSIBLE (by the intended functions) for a single identifier to serve both roles. Here are the main constraints:

- \[SYMBOL\] and \[STRONG NIX\]/\[STRONG SOURCE\] don't play nicely together.  If you modify your source code, a \[STRONG NIX/SOURCE\] identifier must change; if this means \[SYMBOL\] changes too, you will have to recompile everything. However, you can work around this problem by using fake identifiers during development to avoid recompilation, recompiling with the correct NIX identifier when it's finally time to install.

- \[SOURCE\] and \[TYPES\] are incompatible under non-destructive installs and private dependencies. With private dependencies (which GHC supports!), I may link against the multiple instances of the same source but compiled against different dependencies; we MUST NOT consider these types to be the same. Note: GHC used to use package ID for both of these; so coherence was guaranteed by requiring destructive installs.

- \[NIX\] and \[TYPES\] are incompatible under Backpack.  In Backpack, a library author may distribute a package with the explicit intent that it may be used in the same client multiple times with different instantiations of its holes; these types must be kept distinct.