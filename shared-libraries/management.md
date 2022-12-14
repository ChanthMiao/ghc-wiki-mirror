# Shared Libraries: distribution and build-system issues


This page is for discussing and documenting our strategy for

- How shared libraries are found
- How the build system works
- How distributions (of GHC and programs built by GHC) work
- Issues that affect Cabal

## Goals/Scenarios


First of all, we take it as a given that a normal GHC installation will be a good citizen on its host platform: shared libraries will go in the standard locations, and we'll use the system's normal method for finding them at link time and runtime.  Windows is an exception: there is no standard location for installing shared libraries on Windows.


So that we can support having multiple versions of GHC installed, shared libraries will have the GHC version number embedded, e.g. `libHSnetwork-1.1-ghc6.6.1.so`.


Here is what else we'd like to do:

1. Support installing GHC outside of the standard location (e.g. in a home directory), and build
  binaries using that installation.  Multiple such installations should be supported.
1. We need to build a distribution that supports choosing the install location at install time, for
  use in (1).
1. Binaries that are built as part of the GHC build (e.g. stage2/ghc-inplace) need to run from
  the build tree.
1. Cabal needs to build libraries that can be installed in the system location or elsewhere.

# Proposed strategies

## 1. Static linking


(1,2) Installations of GHC that are not in the standard locations use static linking and come with static libraries only.


(3) stage2/ghc-inplace is linked statically.


(4) Cabal packages installed outside the system locations are static only.


This is attractive, but there are some drawbacks:

- we still need to build a distribution that uses shared libs.  Presumably we have to build both
  shared and static libs then.
- the testsuite needs to build binaries against the shared libs for testing, without installing GHC.
- we want the GHC binary in a shared-library installation to be dynamically linked, not statically linked.
- if there are some static-only libraries on the system, then all packages must have static versions,
  because dynamic linking is all-or-nothing in GHC.
- This approach doesn't address Windows

## 2. Dynamic linking


The first plan was this:

[http://www.haskell.org/pipermail/glasgow-haskell-users/2007-June/012740.html](http://www.haskell.org/pipermail/glasgow-haskell-users/2007-June/012740.html)


It has since been pointed out that `LD_LIBRARY_PATH` overrides `-rpath` on some platforms (see below).  This might cause some difficulties (or not?).


Assuming we can fix the locations of shared libraries at link time (eg. with -rpath), then:

1. Installations of GHC outside the system default location hardwire the locations of shared libraries
  into the binaries they build.  (hence such binaries cannot be distributed; this is a drawback)
1. Binaries in the distribution must not have rpaths.  We should use wrapper scripts that set
  `LD_LIBRARY_PATH` instead.
1. Binaries in the build tree need `LD_LIBRARY_PATH` wrappers.
1. A Cabal package may install a shared library outside the standard location, but when linking to
  it we must do the equivalent of adding -rpath to point to its location.


ToDo: Windows?

## 3. libtool


libtool hides the building of shared and static libraries and executables behind a single simple command-line interface.  It hides the details of how to build executables against uninstalled shared libraries, and how to install those executables, on multiple platforms.


When building an object file for a library, libtool builds both the PIC and non-PIC versions.


When building a library, libtool builds both the shared and static version, and remembers where the shared version will be installed later (you have to supply this path when building the library).


When building an executable against shared libraries, libtool creates an executable ready for installation (in `.libs`): this either has no paths embedded (if the shared libs are to be installed in system locations), or with appropriate `-rpath` settings pointing to the locations that the shared libs are to be installed. `libtool` also creates a script for running the program in-place.  The script relinks the executable against uninstalled shared libraries (using `-rpath` on Linux) on demand, caches the resulting executable in `.libs`.

# Platform support for locating shared libraries


The following analysis is mostly from Reilly Hayes on the cvs-ghc mailing list.

## On Linux


An ELF executable can have an embedded list of paths to search for dynamic libraries (the DT_RPATH entry).  This can be set by using -rpath with ld.  DT_RPATH is deprecated.  This list applies to all shared libraries used by the executable (it is not per shared library).  There is no default value placed in the DT_RPATH entry.  You must use -rpath to set it.


There is a new entry, DT_RUNPATH.  DT_RUNPATH works similarly to DT_RPATH.  However, when it is set, DT_RPATH is ignored.  DT_RUNPATH is also set using -rpath, but you must use the --enable-new-dtags switch as well.  


When looking for a shared library, the dynamic linker(ld.so) checks the paths listed in DT_RPATH (unless DT_RUNPATH Is set) , the paths listed in the environment variable LD_LIBRARY_PATH, the paths listed in DT_RUNPATH, the libraries listed in /etc/ld.so.cache, and finally /usr/lib and /lib.  It checks in that order and takes the first library found.  At least on my linux box, LD_LIBRARY_PATH does NOT override the paths in DT_RPATH even though the documentation implies that it does.   LD_LIBRARY_PATH does override DT_RUNPATH.


You CAN override the search path embedded using DT_RPATH by using the LD_PRELOAD environment variable.  This variable contains a \*whitespace-separated\* list of libraries (not directories to search) to load prior to the search process.  The listed libraries are loaded whether or not they are needed to resolve a dependency in the executable.


Finally, an ELF shared library can also have a DT_RPATH entry.  This only impacts the search for shared libraries that are dependencies of the shared library and not the executable.  As with the DT_RPATH entry in an ELF executable, this is not overridden by LD_LIBRARY_PATH but can be overridden using LD_PRELOAD as above.  

## On Mac OS X


A Mach-O executable can embed the full path name for each shared library (as well as rules for acceptable substitutes).  This is called the "install name" for the library and it is included by default when building an executable.  The install name for the library is NOT based on where the static linker (ld) found the library when the executable was built.  The static linker (ld) extracts the install name from the shared library when building the executable.  The install name of the shared library is set when building the shared library.  When you build a shared library you should know where the library is going to be installed so that the install name is set correctly.


When looking for shared libraries, the dynamic linker (dyld) first scans the directories in DYLD_LIBRARY_PATH, then checks the location in the install name (which is per library), and finally checks the standard locations.


DYLD_LIBRARY_PATH successfully overrides the path embedded in the executable.


Caveat 1: LD_LIBRARY_PATH has no runtime impact, but it does impact where the static linker looks for share libraries.  It looks first in the directories specified using -L, the directories in LD_LIBRARY_PATH, and finally in /lib, /usr/lib, & /usr/local/lib.  This is particularly confusing  because many configure scripts seem to ignore LD_LIBRARY_PATH and you can get inconsistent results from configure and gcc/ld on whether a library is present.


Caveat 2: Mac OS X has a set of compiler/linker switches for dealing with Frameworks (packages of shared libraries and include files).  These are installed outside the typical \*nix directory structure.  These switches act like -I (to gcc) and -L (to ld).  If you end up totally confused about where to find something, read up on this.  The OpenGL and OpenAL headers and libraries are in Frameworks, for example.

## On Windows


ToDo: link to the MSDN page about how DLLs are found, and the details about manifests.


Windows has a few ways of locating DLLs. Unfortunately none are perfect.

1. Find them in the same dir as the .exe

>
>
> This means you have to copy all the required dlls into the dir with the .exe. That's just about OK for a `cabal install` command but clearly not ok for `ghc --make`.
>
>

1. Find them on the system (Windows/System dir)


  


>
>
> This is the old "DLL hell" approach and is clearly out.
>
>

1. Stick them on the $PATH

>
>
> This just about works but is very fragile. You could imagine that there was one central place where cabal installed all libs and that we tried to ensure that location is on the $PATH. This is exactly the approach that the Gtk2Hs installer on Windows takes. As that example demonstrates however this solution is very fragile: it's easy to end up with other libs getting in the way and misc other issues.
>
>

1. Use local assemblies

>
>
> This is the modern variation of option 1. The dynamic linker in Win XP and later has this new system of assemblies. An assembly is a bundle of one or more DLLs that live together in a dir. An .exe can have an xml manifest baked into it that specifies dependent assemblies (by GUID). Local assemblies mean that the assembly dirs are subdirs of where the .exe lives, so instead of dumping all the dll files into the same dir as the .exe, you put them in subdirs, one per assembly. This is obviously better but they do still have to be local to where the .exe lives. You can't link to assemblies that live in arbitrary locations in the file system.
>
>

1. Use global assemblies

>
>
> This is very nearly perfect except for one massive problem which makes it no good as a general solution. In addition to local assemblies, the dynamic linker looks for assemblies in a global location. Then the .exe xml manifest specifies the assembly GUID and it just gets looked up in the global "SxS" assembly area. This is great. The only problem is you have to be administrator to install assemblies in the global "SxS" location. There is no per-user or non-privileged location. So while this would be ok for global ghc installs, we still have to support installing ghc and other Haskell packages for non-root users.
>
>

## Conclusions


For Mac: The -rpath switch is not available on Mac OS X because it is superfluous.  The default behavior of embedding a location for each individual shared library is at least as good.  Cabal (and the GHC build process) should use their knowledge of the ultimate install location to set the install name when shared libraries are built.  In-place compilation can override this with DYLD_LIBRARY_PATH


For Linux: On linux, we should be sure to use the --enable-new-dtags switch if we use -rpath.  Otherwise we risk having paths that can't be overridden by LD_LIBRARY_PATH.


For Windows: the classic model of `ghc --make hello.hs; hello.exe` really only works with static linking. With a build model with an explicit install/deploy phase like `cabal install` or a Windows installer then we can use a mixture of global and local assemblies. One thing worth investigating is whether Windows new support for symlinks can be used to allow local assemblies to actually live in fixed locations.
