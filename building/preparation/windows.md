# Building GHC on Windows


This page documents the instructions for setting up a Windows build using [MSYS2](http://msys2.org), which contains [ MinGW-w64 compilers](http://mingw-w64.org/) and the MSYS2 shell utilities.  This guide should get you running in \~5 minutes, modulo download speeds.

## I. Setting up MSYS2

### Method A: Directly

#### Installation


Download and run the latest MSYS2 installer: either [64-bit (x86-64)](http://repo.msys2.org/distrib/x86_64/) or [ 32-bit (i686)](http://repo.msys2.org/distrib/i686/).



From the [MSYS2 installation instructions](https://github.com/msys2/msys2/wiki/MSYS2-installation):


>
>
> After installing or extracting MSYS2 you should start MSYS2 by executing **msys2_shell.cmd**. (if you did not use an installer and this is first time running of MSYS2 after unpacking, then at this point it will create the files and settings necessary for it to function properly. After this initial run you **MUST** restart MSYS2 so that the settings are correct)
>
>


<sub>The result of attempting to create a 32-bit build of GHC on a 64-bit machine has not been documented yet. Building 32-bit GHC on a 32-bit version of Windows works, of course.</sub>


Launch the MinGW shell using the shortcuts added to Start Menu: *MinGW-w64 Win32 Shell* or *MinGW-w64 Win64 Shell*.

#### Note


Do **not** use the *MSYS2 Shell* shortcut.  *MSYS2 Shell* is for building applications that utilize an additional POSIX compatibility layer akin to Cygwin, while the *MinGW-w64* shells are for building native Windows applications.  The latter is the correct environment for building GHC.  For details on the distinction between the two, read the [introduction to MSYS2](https://github.com/msys2/msys2/wiki/MSYS2-introduction).  An easy way to check that you are running the right shell is to check the output of `echo $MSYSTEM`: it should show either `MINGW32` or `MINGW64`.  You can also tell by examining the `$PATH`.


After starting the shell, make sure `/mingw64/bin` (or `/mingw32/bin` depending on the arch you're building for) is the first thing on `$PATH`. If using Bash, `echo "export PATH=/mingw<bitness>/bin:\$PATH" >>~/.bash_profile` can be run to append your profile. Replace `<bitness>` with either `64` or `32` depending on platform.

#### Note


This is **required** to ensure that the mingw-w64 variant of tools get priority over the msys versions.

### Method B: Via Stack


This method has been reported to be unreliable, so use this at your own risk. The recommended method we use ourselves is "Method A" described above.


It is possible to set up MSYS indirectly through Stack:

1. [download and install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/#windows).

1. Run `stack setup` to get GHC and MSYS2, then `stack install alex happy` to get the tools.  (This means step IV can be skipped.)


To start a MinGW shell, run `stack exec --no-ghc-package-path mintty` from Command Prompt.  The `--no-ghc-package-path` flag is necessary to avoid complaints from Cabal.

### Method C: Hadrian Build Via Stack

"GHC supports **two build systems**: Hadrian (recommended) and Make." And this method is dedicated to `Hadrian` build. Below script navigates you directly to Step 5, which means after running the script, it is ready to run hadrian build, `hadrian/build-stack.bat --flavour=quickest -j`. If you'd like to learn more about `hadrian`, here is [hadrian's wiki page](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/hadrian). If you encountered any problem, join the discussion of #21695.  
 
```
cd c:\project
git clone --recursive https://gitlab.haskell.org/ghc/ghc.git
cd ghc
curl -sSL https://get.haskellstack.org/ | sh -s - -f
stack exec --stack-yaml hadrian/stack.yaml -- pacman -Syu --noconfirm
stack exec --stack-yaml hadrian/stack.yaml  -- pacman -S autoconf automake-wrapper make patch python mintty tar --noconfirm
stack build --stack-yaml hadrian/stack.yaml  alex happy
stack --stack-yaml hadrian/stack.yaml build --only-dependencies
stack --stack-yaml hadrian/stack.yaml exec -- bash -c ./boot
stack --stack-yaml hadrian/stack.yaml exec -- bash -c "./configure --enable-tarballs-autodownload"
```


## II. Upgrading packages in MSYS2


To manage packages, MSYS2 uses pacman, the venerable ArchLinux package manager.


Before installing system dependencies required for building GHC, the packages should be upgraded.  If pacman is 5.0.1.6403 or newer, one simply needs to run

```
pacman -Syuu
```


You may need to retry a few times if SourceForge times out.  On the other hand, if your pacman is older, refer to the [MSYS2 installation instructions, section III](https://github.com/msys2/msys2/wiki/MSYS2-installation#iii-updating-packages).

## III. Installing packages & tools


Now we can install GHC's dependencies as follows:

```
pacman -S --needed git tar bsdtar binutils autoconf make xz zstd \
    curl automake python python3 p7zip patch ca-certificates \
    mingw-w64-$(uname -m)-gcc \
    mingw-w64-$(uname -m)-python3 \
    mingw-w64-$(uname -m)-python3-sphinx \
    mingw-w64-$(uname -m)-tools-git
```

## IV. Installing the host GHC and tools (Alex and Happy)

<sub>This step is not necessary if you used Method B of Step I.</sub>


A host GHC binary is required for bootstrapping. In order to keep different architectures separate, download and install a prebuilt GHC into `/mingw64` or `/mingw32`:



Run


```
arch=x86_64 # or i386
bitness=64 # or 32
curl -L https://downloads.haskell.org/~ghc/8.10.2/ghc-8.10.2-${arch}-unknown-mingw32.tar.xz | tar -xJ -C /mingw${bitness} --strip-components=1
```


#### Note


`--strip-components=1` places everything within the archive's `ghc-8.10.2` folder directly into the target directory.


Building GHC requires [Alex](http://www.haskell.org/alex/) and [ Happy](http://www.haskell.org/happy/).  They can be installed using `cabal-install`. We will also put them in `/usr/local/bin`, which is by default included in `PATH` in MSYS.

For newer Cabal 3.* using v2-style install
```
mkdir -p /usr/local/bin
curl -L https://downloads.haskell.org/cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-${arch}-unknown-mingw32.zip | bsdtar -xzf- -C /usr/local/bin
cabal update
cabal install -j --installdir=/usr/local/bin --install-method=copy alex-3.2.6 happy hscolour
```

For older Cabal 2.* using v1-style install
```
mkdir -p /usr/local/bin
curl -L https://downloads.haskell.org/cabal/cabal-install-2.4.1.0/cabal-install-2.4.1.0-${arch}-unknown-mingw32.zip | bsdtar -xzf- -C /usr/local/bin
cabal update
cabal install -j --prefix=/usr/local alex-3.2.5 happy hscolour
```

hscolour is optional for building documentation with source links.


## V. Build!


You should now be able to build GHC using the instructions in [QuickStart](building/quick-start)!

## Troubleshooting

### Configure says "Unknown OS msys"


Check that you have

```wiki
export MSYSTEM=MINGW64
```


in your bash profile

### pacman failed to commit transaction


If `pacman` fails with error message like this:

```wiki
error: failed to commit transaction (conflicting files)
mingw-w64-x86_64-libiconv: /mingw64 exists in filesystem
```


then try re-running the `pacman` command with the `--force` option (see [MSYS2 bug \#31](https://github.com/msys2/msys2.github.io/issues/31)).

### Sphinx missing the requests package as dependency


If you encounter an error while running make with Haddock enabled:

```wiki
pkg_resources.DistributionNotFound: The 'requests' distribution was not found and is required by Sphinx
```


Try installing the package manually. For example by using pip:

```wiki
pacman -S mingw-w64-x86_64-python3-pip
pip3 install requests
```

### Error setting certificate verify locations


If you encounter this error while running `git clone`:

```wiki
error: error setting certificate verify locations:
  CAfile: /usr/ssl/certs/ca-bundle.crt
  CApath: none
```


then try reinstalling `ca-certificates` via `pacman -S ca-certificates`.

### Cabal-1.22.0.0 crashes on Windows Server 2008 R2


The pre-packaged cabal-1.22.0.0 crashes on Windows Server 2008 R2 during `cabal update` due to [this bug](https://github.com/haskell/cabal/issues/2331).  If so, try using a newer version such as 1.24.0.0:

```
mkdir -p /usr/local/bin &&
pacman -S unzip &&
curl -LO https://www.haskell.org/cabal/release/cabal-install-1.24.0.0-rc1/cabal-install-1.24.0.0-rc1-x86_64-unknown-mingw32.zip &&
unzip cabal-install-1.24.0.0-rc1-x86_64-unknown-mingw32.zip -d /usr/local/bin &&
cabal update &&
cabal install -j --prefix=/usr/local alex happy
```

### Build problems


MSYS2 is known to be glitchy in some situations. If you see errors related to `fork()`, try closing and reopening the shell; see also [msys2 issue \#74](http://sourceforge.net/p/msys2/tickets/74/). Also there have been issues with the build process segfaulting. The reason is not known (we're looking into it). If that happens, simply rerunning `make` will continue the build process.


Alternatively, to run a pristine build and tests (takes a while):

```
./validate
```

**NOTE**: You may see an error like `make 7628 child_info_fork::abort: ... make: fork: Resource temporarily unavailable` when running `make`. To fix this, go to the root of your MSYS dir and run `autorebase.bat`; see [http://sourceforge.net/projects/mingw/files/MSYS/Extension/rebase/rebase-4.0.1_1-1/](http://sourceforge.net/projects/mingw/files/MSYS/Extension/rebase/rebase-4.0.1_1-1/) and again [ http://sourceforge.net/p/msys2/tickets/74/](http://sourceforge.net/p/msys2/tickets/74/). Alternatively, run `shutdown //r`.

**NOTE**: If the build seems super slow (takes more than 1 hour to complete), check your virus scanner and whitelist C:/msys64.

### Segmentation fault when using parallel make


Running parallel make (e.g., `make -j5`) is faster, but appears to cause segfaults during the build sometimes. The reasons are not clear yet.

## Other documentation


Other documentation for Windows includes:

- [MinGW/MSYS/Cygwin](building/platforms/windows) information for people new to using UNIX tools on Windows.
- [Setting up a SSH Daemon](building/windows/sshd) on CygWin/MinGW and let's you treat Windows as yet another remote SSH session.
- [Using MSYS1](building/preparation/windows/msys1) to build GHC (not recommended any more)
- [Using Cygwin](building/windows/cygwin) to build GHC. (no longer supported)
- [Using SSH](building/windows/ssh) on Windows.
- [Guidance on how to use Haskell on Windows](http://www.haskell.org/haskellwiki/Windows)
