# Shared Libraries


This page provides an introduction to shared libraries in general and specifically how they are supported and implemented in GHC.


More detailed topics:

- [SharedLibraries/Management](shared-libraries/management): how we organise and manage shared libs
- [SharedLibraries/PlatformSupport](shared-libraries/platform-support): status of shared lib support on various platforms
- [Commentary/PositionIndependentCode](commentary/position-independent-code): how `ghc -fPIC` works

## What shared libs are

[Shared libraries](http://en.wikipedia.org/wiki/Shared_libraries) (sometimes called dynamic libraries) are an alternative way of organising pre-compiled code compared to traditional static libraries. The key difference is that with shared libs, linking programs against library functions takes place when the program is run rather than when the program is built and installed.


All modern operating systems use shared libs. For system libraries they have a particular advantage. They allow the library to be upgraded separately from the programs that use the libs. However this requires preserving an ABI. They also allow a single copy of code to be shared in memory between several programs that use it. For common system libraries this can be a significant saving.

## The three major shared libs systems


There are three systems in common use:

- **ELF** ([Executable and Linkable Format](http://en.wikipedia.org/wiki/Executable_and_Linkable_Format)) is used on all modern Unix systems (except MacOS X), in particular it is used on Linux, Solaris and the BSDs.
- **PE** ([Portable Executable](http://en.wikipedia.org/wiki/Portable_Executable)) format is used on Windows.
- **Mach-O** ([Mach object](http://en.wikipedia.org/wiki/Mach-O)) is the format used on Mac OS X.


On each system, the same format is used for executables, shared libraries and intermediate object files. Each system uses their own file extension for shared libraries:

<table><tr><th> System </th>
<th> executable extension </th>
<th> shared library extension 
</th></tr>
<tr><th> ELF    </th>
<th> (no extension)       </th>
<th> <tt>.so</tt>     
</th></tr>
<tr><th> PE     </th>
<th> <tt>.exe</tt>               </th>
<th> <tt>.dll</tt>    
</th></tr>
<tr><th> Mach-O </th>
<th> (no extension)       </th>
<th> <tt>.dylib</tt>  
</th></tr></table>



Unfortunately, while static linking is relatively uncomplicated and similar between systems, shared libraries are implemented rather differently between different operating systems and pose somewhat of a management headache.

## Background reading


An excellent technical introduction to ELF shared libraries is [How To Write Shared Libraries](http://people.redhat.com/drepper/dsohowto.pdf) by Ulrich Drepper (author of glibc).


ELF "visibility" reading list:

- [http://gcc.gnu.org/wiki/Visibility](http://gcc.gnu.org/wiki/Visibility)
- [http://gcc.gnu.org/onlinedocs/gcc-4.3.3/gcc/Code-Gen-Options.html](http://gcc.gnu.org/onlinedocs/gcc-4.3.3/gcc/Code-Gen-Options.html), see `-fvisibility` flag
- [http://gcc.gnu.org/onlinedocs/gcc-4.3.3/gcc/Function-Attributes.html](http://gcc.gnu.org/onlinedocs/gcc-4.3.3/gcc/Function-Attributes.html), see `visibility` attribute


PE format introduction:

- [Part 1](http://msdn.microsoft.com/en-us/magazine/cc301805.aspx)
- [Part 2](http://msdn.microsoft.com/en-us/magazine/cc301808.aspx)


In particular these describe how dll import and export works.

## Why we care about shared libraries


There are several reasons we care.


The greatest advantage is that it enables us to make plugins for other programs. There are loads of examples of this, think of plugins for things like vim, gimp, postgres, apache. On Windows if you want to make a COM or .NET component then it usually has to be as a shared library (a .dll file).


Similar to plugins, shared libraries have become a common way of composing large systems. Each shared library can be written in a different language. Compared to static libraries, shared libraries are typically more self-contained. The ability to produce nice self-contained shared libraries from Haskell code would simplify the integration of Haskell code into larger existing systems.


A somewhat superficial reason is that it makes your ???Hello World??? program much smaller because it doesn???t have to include a complete copy of the runtime system and half of the base library. It???s true that in most circumstances disk space is cheap, but if you???ve got some corporate shared storage that???s replicated and meticulously backed-up and if each of your 100 ???small??? Haskell plugins is actually 10MB big, then the disk space does not look quite so cheap.


Using shared libraries also makes things a bit easier for Haskell applications that want to do dynamic code loading. For example GHCi itself currently has to load two copies of the base package, the one that is statically linked with and another copy that it loads dynamically. With shared libraries it would just end up with another reference to the same copy of the single shared base library.


Shared libs also completely eliminates the need for the ???split objs??? hack that GHC uses to reduce the size of statically linked programs. This should make our link times a bit quicker.


Note that we have not mentioned the two major advantages that shared libraries were originally developed for, namely saving memory at runtime (when several programs use the same lib) and making it possible to upgrade libraries without touching the programs that use them. These advantages are more significant in core operating system libraries. C code can be made to follow a stable ABI where as historically this has not been a priority in Haskell implementations (though this may change). Similarly, there are not too many systems yet where having multiple copies of the RTS and base libraries in memory at once is a significant problem. Again, this may change if people choose to target memory-constrained systems.

## TODO


More stuff to explain:

- Position independent code, what it is, why we need it on some systems
- relationship between ghc flags -dynamic, -shared and -fPIC
- difference between C and ghc in compiling for shared libs
- peculiarities of ELF and PE
