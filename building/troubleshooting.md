
 


# Troubleshooting the GHC build


Here we keep track of failures that can occur when building GHC, with solutions.


We don't expect anyone to read this page from beginning to end.  The only way you get here is by searching, so remember when adding a new entry the most important thing to do is to **include the error message verbatim**, so searches will find it.  If a build failure is caused by a bug in GHC or the build system, please link to the ticket number so we can tell when it's safe to remove the entry and keep this page from getting too crufty.

## Segmentation fault and "strange closure type" panics


If the build fails with a `segmentation fault (core dumped)` or a `strange closure type` panic from GHC, but the error goes away or occurs in a different place when you restart the build, then the problem is most likely with your hardware.  Please run a [memtest](http://www.memtest.org/) before going any further.

## Permission denied errors on Windows that go away when the build is restarted


A common cause of "permission denied" or "Access denied" errors during a build on Windows is having a realtime virus scanner.  If you have a virus scanner turned on, try excluding the GHC build directory from realtime scanning if your virus scanner supports it, or otherwise disable the scanner while building GHC.  See [Building/Preparation/Windows](building/preparation/windows).

## Make has restarted itself 3 times; is there a makefile bug?


If you see this when building:

```wiki
ghc.mk:96: *** Make has restarted itself 3 times; is there a makefile bug?.  Stop.
```


then it could mean you have introduced a build system bug, causing an infinite loop.


This can also happen (although we don't know precisely why) if you modify something in a built tree, and then re-run `make`. In this case the error is just overly conservative, and restarting is the right workaround.


It can also happen if you are building the sources on FreeBSD in a really fast environment, e.g. on a multi-core Xeon with multiple parallel threads (`make -j`) or a memory-backed file system (`mfs`, `tmpfs`) (see #7592). It is because precision of file timestamps is not fine-grained enough by default (due to the common VFS layer).  You can change this granularity by adjusting the value of the `vfs.timestamp_precision` sysctl(3) variable (`sudo sysctl -w vfs.timestamp_precision=1`).


If you encounter this without touching any files after typing 'make', then it's probably a bug in the build system. The `make -d` output will be useful in tracking it down, but depending on when it happens there might be a lot of it!

## libraries/ghc-prim/GHC/PrimopWrappers.hs:48:18: Not in scope: \`GHC.Prim.gcdInt\#'


If you get this message when the build system runs Haddock

```wiki
libraries/ghc-prim/GHC/PrimopWrappers.hs:48:18:
    Not in scope: `GHC.Prim.gcdInt#'
```


it's probably because you have a file `PrimopWrappers.hs` in the source-code directory `libraries/ghc-prim/GHC/`.  It's a derived file, and is now generated into `libraries/ghc-prim/dist-install/build/GHC/`.  Just remove the offending files (probably `PrimopWrappers.hs` and `Prim.hs`) from the source directory and try again.

## tar: unable to record current working directory: No such file or directory


On MSYS I got this, during the construction of a binary distribution:

```wiki
...
cd bindistprep && "/usr/bin/tar" cf - ghc-6.13.20091020 | bzip2 -c > ../bindistprep/ghc-6.13.20091020-i386-unknown-mingw32.tar.bz2
/usr/bin/tar: unable to record current working directory: No such file or directory
```


It turns out that, at least on my Windows XP machine, the `tar` in the MSYS bundle for 1.0.10 (namely `tar` version 1.19.90) simply fails when creating a tar archive for a directory.  For example:

```wiki
sh-3.1$ tar cf foo.tar mk
tar: unable to record current working directory: No such file or directory
sh-3.1$ tar --version
tar (GNU tar) 1.19.90
```


I fixed this by downloading an up-to-date `tar`, from [http://sourceforge.net/projects/mingw/files/](http://sourceforge.net/projects/mingw/files/).  I put this `tar.exe` in `c:/msys/1.0/bin`, overwriting the old `tar.exe`.  This works:

```wiki
sh-3.1$ tar cf foo.tar mk
tar: unable to record current working directory: No such file or directory
sh-3.1$ tar --version
tar (GNU tar) 1.22
```


Simon and/or Ian plan to investigate; is MSYS 1.0.10 really so broken?

## wget: missing URL


On MSYS I got this:

```wiki
bash$ git pullall
     ....snip...
== Syncing tarballs
== running  wget http://darcs.haskell.org/ghc-tarballs/binutils-2.19.1-mingw32-
bin.tar.gz -O mingw/binutils-2.19.1-mingw32-bin.tar.gz
wget: missing URL
Usage: wget [OPTION]... [URL]...
```


This happened to me with an old version of the shell (say "sh --version").  I think perhaps the path-mangling is different.  With the MSYS recommended [here](building/preparation/windows), all is well.  The shell there is version 3.1.0(1).

## configure: error: C++ preprocessor "/lib/cpp" fails sanity check


On Windows, if you see this error when building GHC:

```wiki
checking how to run the C++ preprocessor... /lib/cpp
configure: error: C++ preprocessor "/lib/cpp" fails sanity check
See `config.log' for more details.
make[1]: *** [libffi/stamp.ffi.configure] Error 1
```


then you probably have the Microsoft C compiler in your `PATH`, and this has confused the `configure` script for `libffi`.  Try editing your `PATH` so that it no longer points to the MS compilers and tools.

## haddock: internal Haddock or GHC error: Data.Binary.getWord8: end of file


If you see this

```wiki
Documentation created: stage2/doc/html/ghc/index.html
cd libraries && sh gen_contents_index --inplace
haddock: internal Haddock or GHC error: Data.Binary.getWord8: end of file
make[1]: *** [libraries/index.html] Error 1
make: *** [all] Error 2
```


it's probably because you have an old library in your tree with incomprehensible `.haddock` files.  Try

```wiki
rm libraries/*/*/doc/*/*/*.haddock
```

### ar: Bad file number

**Fixed in 6.12.1**.  See #3201.  Workaround: add `SplitObjs=NO` to `mk/build.mk`.

### chr: bad argument


Occasionally you see this:

```wiki
Compile failed (status 256) errors were:
ghc-stage2.exe: panic! (the 'impossible' happened)
  (GHC version 6.11.20090722 for i386-unknown-mingw32):
	Prelude.chr: bad argument: 50331648
```


This is almost certainly because you have stale interface files (`Foo.hi`) lying around, and GHC's binary interface-file format has changed.  Try `make distclean` and rebuild.  If you still get it, use `find` to find `*.hi` and remove them.

### All binaries crash (Windows only)


On Windows you may see this

```wiki
"inplace/bin/hsc2hs.exe" --cc=gcc --ld=gcc --cflag=-g --cflag=-O2
--cflag=-D__GLASGOW_HASKELL__=611 '--cflag=-O' '--cflag=-g'
'--cflag=-O2' '--cflag=-Ilibraries/hpc/.'
'--cflag=-isystemc:/ghc/ghc-6.10.3\directory-1.0.0.3\include'
'--cflag=-isystemc:/ghc/ghc-6.10.3\old-time-1.0.0.2\include'
'--cflag=-isystemc:/ghc/ghc-6.10.3\Win32-2.2.0.0\include'
'--cflag=-isystemc:/ghc/ghc-6.10.3\bytestring-0.9.1.4\include'
'--cflag=-isystemc:/ghc/ghc-6.10.3\base-4.1.0.0\include'
'--cflag=-isystemc:/ghc/ghc-6.10.3/include'
'--cflag=-isystemPAPI_INCLUDE_DIR'
'--lflag=-Lc:/ghc/ghc-6.10.3\directory-1.0.0.3'
'--lflag=-Lc:/ghc/ghc-6.10.3\old-time-1.0.0.2'
'--lflag=-Lc:/ghc/ghc-6.10.3\old-locale-1.0.0.1'
'--lflag=-Lc:/ghc/ghc-6.10.3\filepath-1.1.0.2'
'--lflag=-Lc:/ghc/ghc-6.10.3\containers-0.2.0.1'
'--lflag=-Lc:/ghc/ghc-6.10.3\array-0.2.0.0'
'--lflag=-Lc:/ghc/ghc-6.10.3\syb-0.1.0.1'
'--lflag=-Lc:/ghc/ghc-6.10.3\Win32-2.2.0.0'
'--lflag=-Lc:/ghc/ghc-6.10.3\bytestring-0.9.1.4'
'--lflag=-Lc:/ghc/ghc-6.10.3\base-4.1.0.0'
'--lflag=-Lc:/ghc/ghc-6.10.3\integer-0.1.0.1'
'--lflag=-Lc:/ghc/ghc-6.10.3\ghc-prim-0.1.0.0'
'--lflag=-Lc:/ghc/ghc-6.10.3' '--lflag=-Lc:/ghc/ghc-6.10.3/gcc-lib'
'--lflag=-luser32' '--lflag=-lgdi32' '--lflag=-lwinmm'
'--lflag=-lkernel32' '--lflag=-ladvapi32' '--lflag=-lwsock32'
'--lflag=-lmsvcrt' '--lflag=-lkernel32' '--lflag=-luser32'
'--lflag=-lshell32' '--lflag=-lm' '--lflag=-lffi' '--lflag=-lgmp'
'--lflag=-lwsock32' libraries/hpc/./Trace/Hpc/Reflect.hsc -o
libraries/hpc/dist-boot/build/Trace/Hpc/Reflect.hs

running libraries/hpc/dist-boot/build/Trace/Hpc/Reflect_hsc_make.exe failed
command was: libraries/hpc/dist-boot/build/Trace/Hpc/Reflect_hsc_make.exe  
    >libraries/hpc/dist-boot/build/Trace/Hpc/Reflect.hs
make[1]: *** [libraries/hpc/dist-boot/build/Trace/Hpc/Reflect.hs] Error 1
make: *** [all] Error 2
```


A dialog pops up: ???Reflect_hsc_make.exe has stopped working???, with the buttons ???Debug??? and ???Close program???.


This signals an obscure problem whose source is still unknown: 
if GHC links in certain Windows libraries, `kernel32` and `msvcrt`, the resulting program crashes.  
See [Sigbjorn's email](http://www.haskell.org/pipermail/glasgow-haskell-bugs/2009-April/018643.html).  We wish we knew why!


We've worked around this in GHC 6.10.4 (and later) so that the problem shouldn't arise if you use that to build GHC with.  But if you have an earlier GHC on your machine you can still work around it as follows.  These two commands will fix up the base and Win32 packages respectively to remove the offending libraries from `extra-libraries` and add a suitable `extra-ghci-libraries`:

```wiki
ghc-pkg describe 'base-4*' | sed 's/msvcrt//;s/kernel32//;s/^extra-ghci-libraries:/extra-ghci-libraries: wsock32 user32 shell32 kernel32 msvcrt/' | ghc-pkg update -
ghc-pkg describe Win32 | sed 's/kernel32//' | ghc-pkg update -
```

### Using autoconf by mistake


If you used `autoconf` instead of `perl boot`, you'll get an error when you run `./configure`:

```wiki
...lots of stuff...
creating mk/config.h
mk/config.h is unchanged
configuring in ghc
running /bin/sh ./configure  --cache-file=.././config.cache --srcdir=.
./configure: ./configure: No such file or directory
configure: error: ./configure failed for ghc
```

### Cannot create configure

`autoreconf` (which gets run by `perl boot`) seems to create the file `configure` read-only.  So if you need to run `perl boot` again (which I sometimes do for safety's sake), you get

```wiki
/usr/bin/autoconf: cannot create configure: permission denied
```


Solution: delete `configure` first.

### Argument list too long


You may find this towards the end of compiling the base library:

```wiki
c:\ghc\ghc-6.6.1\bin\ar.exe: creating libHSbase.a
xargs: c:/ghc/ghc-6.6.1/bin/ar: Argument list too long
make[2]: *** [libHSbase.a] Error 126
make[2]: *** Deleting file `libHSbase.a'
Failed making all in base: 1
make[1]: *** [all] Error 1
make[1]: Leaving directory `/cygdrive/c/GHC6.6.1/ghc-6.6.1/libraries'
make: *** [stage1] Error 2
```


Sadly the argument list has a limited length in Windows.  This may be fixable
somehow (Windows expertise welcomed here), but what we do is to set

```wiki
SplitObjs = NO
```


in `build.mk`.  That stops the splitting-up of object files, and dramatically reduces
the number of object files involved.  Link times are also improved.  (Binary size increases
though.)


Also, you can arrange for the (huge) list of files to be processed iteratively, rather all at once, and that would probably be a principal solution. `xargs` feeds the file names to the appropriate command (e.g. `ar`). In `$(GHC_TOP)/mk/target.mk` find the place where it is called and add this switch

```wiki
xargs -n NNN
```


where NNN is the number of arguments processed at a time. It should be small enough to be less than the limit and large enough for the whole thing not to be too slow.


Note, that it's not good to edit `target.mk` in general.

### Space in TMPDIR


One difficulty that comes up from time to time is running out of space
in `TMPDIR`.  (It is impossible for the configuration stuff to
compensate for the vagaries of different sysadmin approaches to temp
space.)


The quickest way around it is `setenv TMPDIR /usr/tmp` or
even `setenv TMPDIR .` (or the equivalent incantation with your shell
of choice).


The best way around it is to say

```wiki
export TMPDIR=<dir>
```


in your `build.mk` file.  Then GHC and the other
tools will use the appropriate directory in all cases.

### Warning "warning: assignment from incompatible pointer type"


You may occasionally see a warning from the C compiler when compiling some
Haskell code, eg. "warning: assignment from
incompatible pointer type".  These are usually harmless, but it's a good idea to
report it on the mailing list so that we can fix it.

### Warning "ar: filename `GlaIOMonad__1_2s.o` truncated to `GlaIOMonad_`"


Similarly, `ar`chiving warning messages like the following are not a problem:

```wiki
ar: filename GlaIOMonad__1_2s.o truncated to GlaIOMonad_
ar: filename GlaIOMonad__2_2s.o truncated to GlaIOMonad_
...
```

### Cpp variations


GHC's sources go through `cpp` before being compiled, and `cpp` varies
a bit from one Unix to another.  One particular gotcha is macro calls
like this:

```wiki
SLIT("Hello, world")
```


Some `cpp`s treat the comma inside the string as separating two macro
arguments, so you get

```wiki
:731: macro `SLIT' used with too many (2) args
```


Alas, `cpp` doesn't tell you the offending file!
Workaround: don't put weird things in string args to `cpp` macros.

### Cabal/Distribution/Compat/FilePath.hs: No such file or directory


You may see this:

```wiki
Distribution/Compat/FilePath.hs:2: 
  error: Cabal/Distribution/Compat/FilePath.hs: No such file or directory
make[1]: *** [depend] Error 1
make: *** [stage1] Error 1
```

**Possible Solution**::
Be sure you have run `git pullall (which runs git submodule update --init)` to get all necessary packages. Don't forget to run `./boot` again after you pull in new packages.

### xargs: /usr/bin/ar: terminated by signal 11


You may see this when compiling libraries:

```wiki
(echo Control/Concurrent_stub.o System/CPUTime_hsc.o System/Time_hsc.o ;
/usr/bin/find Control/Applicative_split Control/Arrow_split
Control/Concurrent_split Control/Concurrent/Chan_split 
   ...long mess...
Text/PrettyPrint/HughesPJ_split Text/Printf_split Text/Read_split
Text/Read/Lex_split Text/Show_split Text/Show/Functions_split -name '*.o'
-print) | xargs /usr/bin/ar q libHSbase.a
/usr/bin/ar: creating libHSbase.a
xargs: /usr/bin/ar: terminated by signal 11
make[2]: *** [libHSbase.a] Error 125
make[2]: *** Deleting file `libHSbase.a'
make[1]: *** [all] Error 1
```


What is happening is that the ghc build system is linking thousands and
thousands of tiny .o files into `libHSbase.a`. GNU `ar` isn't optimised for
this use-case and it takes far more memory than it really needs to. So
what happens is that ar takes \>500Mb of memory and your virtual
machine / virtual server probably isn't configured with that much memory
and so the linux kernel OOM killer terminates the ar process.


To make this worse, since there are so many .o files, it takes several
invocations of ar to link them all. On each invocation `ar` is building
the symbol index (-q is ignored) and this is what takes the most time
and memory. It's a good deal quicker to use a custom program (100 lines
of Haskell) to build `libHSbase.a` and then use `ranlib` just once to build
the symbol index.


\[Duncan Coutts\] I submitted a patch to gnu `binutils` to make ar take less memory when
linking 1000's of files so it now only takes around 100Mb rather than
500Mb when linking `libHSbase.a`. That patch is included in version 2.17 I
think (in other words most systems don't have it yet).


What you can do in the mean time is either configure your virtual
machine with more memory or turn off the split-objs feature when you
configure ghc. Just add `SplitObjs=NO` to your `mk/build.mk` file (which
may not exist to start with). (The Gentoo ebuild does this
automatically)

### Crippled `ld`


It turns out that on both Cygwin and MSYS, the `ld` has a
limit of 32kbytes on its command line.  Especially when using split object
files, the make system can emit calls to `ld` with thousands
of files on it.  Then you may see something like this:

```wiki

(cd Graphics/Rendering/OpenGL/GL/QueryUtils_split && /mingw/bin/ld -r -x -o ../QueryUtils.o *.o)
/bin/sh: /mingw/bin/ld: Invalid argument

```


The solution is either to switch off object file splitting (set
`SplitObjs` to `NO` in your
`build.mk`),
or to make the module smaller.

### `CYGWIN` environment variable in MSYS


When using MSYS, check that the `CYGWIN` environment variable is *not* set.  It's a bad bug
that MSYS is affected by this, but if you have CYGWIN set to "ntsec ntea", which is right for Cygwin, it
causes the MSYS `ssh` to bogusly fail complaining that your `.ssh/identity`
file has too-liberal permissinos. 


ToDo: what's the error message for this?

### Forgetting to install `automake`


If you get a message like this:

```wiki
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.
Can't locate object method "path" via package "Autom4te::Request" (perhaps you forgot to load "Autom4te::Request"?) at /usr/bin/autom4te line 81.
autoreconf: /usr/bin/autoconf failed with exit status: 1
```


then you have probably not got `automake` installed (or at least findable).

### Vista installer detection


Vista has a "feature" called "installer detection" which tries to elevate permissinos for executables named things like `Setup` and `Install`.  There are lots of programs called `Setup` in a GHC build, and if you see permission-denied errors relating to programs called `Setup` you may need to disable installer detection.  Go to `Start -> All Programs -> Accessories > Run` and enter `secpol.msc`.  Then under `Security Settings -> Local Policies -> Security Options`,  disable `UAC: Detect application installations and prompt for elevation`.  Then reboot.


We added a workaround for install-detection in GHC 6.8.1 (see #1271), so if you're using that version or later you shouldn't encounter this issue.

### Cygwin: failure to use native path to `gcc` when configuring


It's *very important* that you specify a 
native Windows path for `gcc`, not a Cygwin path, because GHC (which
uses this path to invoke `gcc`) is a Windows program and won't
understand a Cygwin path.  For example, you want to say something like `--with-gcc=c:/mingw/bin/gcc.exe` and *not* `--with-gcc=/cygdrive/c/mingw/bin/gcc.exe` or `--with-gcc=/mingw/bin/gcc.exe`.  If you get this wrong, the failure might come with no error message whatsoever.  GHC simply fails silently when first invoked, 
typically leaving you with this:


```wiki
make[4]: Leaving directory `/cygdrive/e/ghc-stage1/ghc/rts/gmp'
../../ghc/compiler/ghc-inplace -optc-mno-cygwin -optc-O 
-optc-Wall -optc-W -optc-Wstrict-prototypes -optc-Wmissing-prototypes 
-optc-Wmissing-declarations -optc-Winline -optc-Waggregate-return 
-optc-Wbad-function-cast -optc-Wcast-align -optc-I../includes 
-optc-I. -optc-Iparallel -optc-DCOMPILING_RTS 
-optc-fomit-frame-pointer -O2 -static 
-package-name rts -O -dcore-lint -c Adjustor.c -o Adjustor.o
make[2]: *** [Adjustor.o] Error 1
make[1]: *** [all] Error 1
make[1]: Leaving directory `/cygdrive/e/ghc-stage1/ghc'
make: *** [all] Error 1
```

### Ubuntu: `dash` vs `bash`


In Ubuntu 6.10 the default system shell `/bin/sh` was changed to `dash` (The Debian Almquist Shell) instead of `bash`, see [DashAsBinSh](http://wiki.ubuntu.com/DashAsBinSh). This has been reported to break the GHC build. Until the GHC scripts are updated, the easiest way to fix this problem is to (as `root`) change the `/bin/sh` link back to `/bin/bash`. There should be minimal effect on the rest of the system, bar a small speed penalty for script heavy processes due to `bash` slowness.

### c:\\msys\\1.0\\bin\\make.exe: **\* couldn't commit memory for cygwin heap, Win32 error 0**


This error occurs when using the new build system with MSYS on Windows.  It is a temporary error; just type `make` again to continue the build.  Hopefully this is a bug in MSYS that will be fixed at some point.

### /usr/bin/patch: Permission denied


This happened to me with MSYS on Windows Server 2003.  Exact cause unknown, but it seems that Windows Server 2003 was upset by the `patch.exe.manifest` supplied with MSYS alongside `patch.exe` to work around the installer-detection nonsense on Vista.  Workaround: remove `/usr/bin/patch.exe.manifest`.

## Relocation error when linking the RTS shared library


If you use a system with an older GCC (4.1.2 in my case), you may run into the following error:

```wiki
/usr/bin/ld: rts/dist/build/RtsStartup.dyn_o: relocation R_X86_64_PC32 against `StgRun' can not be used when making a shared object; 
recompile with -fPIC
/usr/bin/ld: final link failed: Bad value
collect2: ld returned 1 exit status
make[1]: *** [rts/dist/build/libHSrts-ghc6.13.20100816.so] Error 1
make[1]: *** Waiting for unfinished jobs....
make: *** [all] Error 2
```


The Scientific Linux system I was building on had the following software installed:
kernel 2.6.18-128.1.1.el5
gcc version 4.1.2 20080704 (Red Hat 4.1.2-44)
GNU ld version 2.17.50.0.6-9.el5 20061020 (that's the binutils version afaik)
Bootstrapping GHC was version 6.12.1.


As far as I could tell all .dyn_o files had been built to allow relocation, but StgCRun.c contains some inline assembler code that specifically targets the x86 and x86_64. Upon removal and using the generic version (the part that's protected by the \#ifdef USE_MINIINTERPRETER), the error shifted to another file, so the problem did not seem to be with that specific symbol that was referenced in RtsStartup.c 


There was another GCC on the system however (4.3.3). Using that version did allow GHC to build. 

### MacOS gmp compiled --with-pic


If you've installed gmp from source on your Mac OS machine, you may see an error like this:

```wiki
  ld: illegal text-relocation to ___gmp_binvert_limb_table in /usr/local/lib/libgmp.a(mp_minv_tab.o) from ___gmpn_divexact_1 in /usr/local/lib/libgmp.a(dive_1.o) for architecture x86_64
  collect2: ld returned 1 exit status
```


The problem is described [on this page](https://github.com/mxcl/homebrew/issues/12946), a quick work-around is to install gmp with homebrew, i.e. `brew install gmp; brew link gmp`.
