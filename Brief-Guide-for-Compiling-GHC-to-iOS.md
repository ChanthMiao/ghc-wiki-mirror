# Building GHC for iOS ARM64

Basically, GHC have a Haskell app available in App Store, but as I have a jailbroken iOS device, I wanted to use GHC itself rather than iOS Apps on my machine. I have been attempted many times but all goes failure till now.

I would like to show the basic process of how I've compiled GHC 9.2.1 with full featured stage2 compiler to work on any jailbroken iOS with version 12 or up.

Tested on iPhone XR with iOS 14.3 (Darwin 20.2.0), [unc0ver](https://unc0ver.dev) jailbreak.

## Target Device

Only tested on iPhones with A12 Chips or up, _theoretically_ working on A7 or up, arm64 or arm64e. We are targeting arm64 with LSE (Large System Extension) enabled. `-march=armv8-a+lse -mcpu=apple-a7`

## Build Device

Compiling GHC was really time spending and GHC needs an existing GHC toolchain to bootstrap, so don't attempt to build GHC on your tiny iOS devices.

I'm using my laptop with Apple M1 chip (MacBook Pro 13-inch, M1, 2020) with 8GB ram.

I believe many of you guys looking for cross compiling GHC codes to iOS have been guided to [this page](https://gitlab.haskell.org/ghc/ghc/-/wikis/building/cross-compiling/iOS), don't follow this if you are living after 2022, I was trapped with this tutorial. Now, just follow my setup.

For most compilation steps, I was just simply following how Homebrew Formulae does. I will mark which part you should notice in this article.

#### Build Tools

python 3.10.0

LLVM 12.0.0 with Polly enabled for `llc` and `opt` (Apple's fork, or if you don't want to build by your self, Homebrew/MacPorts LLVM12 is okay)

GNU Make 4.3

GNU Autoconf 2.71

GNU automake 1.16.5

Xcode CLT if macOS (We are going to set up cross toolchains using `xcrun`)

cctools-port if non-macOS, a [prebuilt toolchain](https://github.com/sbingner/llvm-project/releases/tag/v10.0.0-2) is available for x86_64 linux host, or check [osxcross](https://github.com/tpoechtrager/osxcross)

#### Required Dependencies

Note: I'm using latest version of following libs, but older should be okay

libffi 3.4.2

GMP 6.2.1 (for bignum support)

LLVM 12.0.0 (same as above, but built for iOS)

## Preparing Toolchain

On macOS with Xcode CLT installed, we have a program called `xcrun` to help us do some sort of cross compiling configurations. When calling xcrun, it will then fetch Xcode Toolchain instance under `/Library/Developer/CommandlineTools` or `/Applications/Xcode.app/Contents/Developer`. For example, if we wants to call a `clang` which targeting iOS, we can do

```
$ xcrun --sdk iphoneos clang hello.c -o hello
```

Then clang will choose correct target specifications and compile source code to platform `iphoneos`.

We knows a thing called "target triplet", those machine-vendor-operatingsystem combinations that helps compilation progresses to determine which compiler we should use for targeting a specific platform. But something different between GNU autotools and LLVM on how they treat triples:

1. `arm64-*` was not recognized by GNU autoconf, but accepted by LLVM.
2. `*-darwin` was recognized as general Darwin based systems by GNU autoconf, but for LLVM it just means macOS. For LLVM to compile to iOS, os name should be `ios` but not `darwin`.

As we are configuring GHC by Autoconf/automake, we have to follow GNU host triple to make it recognizes ours toolchain, so the toolchain should be prefixed as `aarch64-apple-darwin20.1.0` if targeting iOS 14.2 (See <https://www.theiphonewiki.com/wiki/Kernel#iOS.2FiPadOS>)

We can simply create wrappers to toolchain binaries by executing following command:

```
$ sudo for tools in `ls /Library/Developer/CommandlineTools/usr/bin/`; do /usr/bin/printf "#\!/bin/bash\nIPHONEOS_DEPLOYMENT_TARGET=14.2\nexec xcrun --sdk iphoneos $tools \${@}" > /usr/local/bin/aarch64-apple-darwin20.1.0-${tools}; chmod +x /usr/local/bin/aarch64-apple-darwin20.1.0-${tools}; done
```

Then, delete `cc`, `c++`, `gcc`, `g++`, `clang`, `clang++`,with prefix `aarch64-apple-darwin20.1.0` under the path (`/usr/local/bin` or somewhere else if you like) where we created symlinks. and create a new file named `aarch64-apple-darwin20.1.0-cc` with bash scripts

```bash
#!/bin/bash
SDKROOT=$(xcrun --sdk iphoneos --show-sdk-path)
if [ ! ${ALT_CLANG} ]; then
  ALT_CLANG="clang"
fi
ORIG_ARG="$@"
POSITIONAL_ARGS=()
# This step for filter out unexpected options
while [[ $# -gt 0 ]]; do
  case $1 in
    -mmacosx-version-min=*|-no-pie)
      shift
      ;;
    --target=arm64-apple-darwin*)
      POSITIONAL_ARGS+=("--target=arm64-apple-ios")
      shift
      ;;
    -target)
      if [[ $2 != arm64-apple-darwin* ]]; then
        POSITIONAL_ARGS+=("$1")
        POSITIONAL_ARGS+=("$2")
      fi
      shift
      shift
      ;;
    -isysroot)
      if [[ $2 != *MacOS* ]]; then
        POSITIONAL_ARGS+=("$1")
        POSITIONAL_ARGS+=("$2")
      fi
      shift
      shift
      ;;
    *)
      POSITIONAL_ARGS+=("$1")
      shift
      ;;
  esac
done

if [ ${APPEND_C_FLAGS} ]; then
  for flags in "${APPEND_C_FLAGS}"; do
    POSITIONAL_ARGS+=("$flags")
  done
fi

set -- "${POSITIONAL_ARGS[@]}"
"exec" "env" "IPHONEOS_DEPLOYMENT_TARGET=14.2" "xcrun" "--sdk" "iphoneos" "$ALT_CLANG" "-B" "/usr/local/bin/ios" "-mios-version-min=12.0" "${POSITIONAL_ARGS[@]}" "-isysroot" "$SDKROOT"
```

Same step for `aarch64-apple-darwin20.1.0-c++`, but replacing `clang` with `clang++`, `ALT_CLANG` with `ALT_CLANGXX`, `APPEND_C_FLAGS` with `APPEND_CXX_FLAGS`.

For `aarch64-apple-darwin20.1.0-ld`, use following scripts.

```bash
#!/bin/bash
SDKROOT=$(xcrun --sdk iphoneos --show-sdk-path)
if [ ! ${ALT_LINKER} ]; then
  ALT_LINKER="ld"
fi
ORIG_ARG="$@"
POSITIONAL_ARGS=()
while [[ $# -gt 0 ]]; do
  case $1 in
    -r)
      POSITIONAL_ARGS+=("$1")
      shift
      ;;
    -syslibroot)
      HAVE_SYSLIBROOT="1"
      POSITIONAL_ARGS+=("$1")
      if [[ $2 != *MacOS* ]] && [[ $2 != "/" ]]; then
        POSITIONAL_ARGS+=("$2")
      else
        POSITIONAL_ARGS+=("$SDKROOT")
      fi
      shift
      shift
      ;;
    -platform_version)
      POSITIONAL_ARGS+=("$1")
      if [[ $2 != "ios" ]]; then
        POSITIONAL_ARGS+=("ios")
        POSITIONAL_ARGS+=("14.2") # Target version
        POSITIONAL_ARGS+=("14.5") # SDK version
      else
        POSITIONAL_ARGS+=("$2")
        POSITIONAL_ARGS+=("$3")
        POSITIONAL_ARGS+=("$4")
      fi
      shift
      shift
      shift
      shift
      ;;
    -macos_version_min)
      shift
      shift
      ;;
    *)
      POSITIONAL_ARGS+=("$1")
      shift
      ;;
  esac
done

if [ ${APPEND_LD_FLAGS} ]; then
  for flags in "${APPEND_LD_FLAGS}"; do
    POSITIONAL_ARGS+=("$flags")
  done
fi
if [ ! ${HAVE_SYSLIBROOT} ]; then
  POSITIONAL_ARGS+=("-syslibroot")
  POSITIONAL_ARGS+=("$SDKROOT")
fi

set -- "${POSITIONAL_ARGS[@]}"
"exec" "env" "IPHONEOS_DEPLOYMENT_TARGET=14.2" "xcrun" "--sdk" "iphoneos" ${ALT_LINKER} "${POSITIONAL_ARGS[@]}"
```

After creating those wrapper scripts, we still have to make complier always choose our's wrapper ld as default linker, since LLVM Clang cannot specify a alternative linker like GCC, we shall use `-B /usr/local/bin/ios` which we uses in clang wrappers to set an alternative dependency search path.

Now `mkdir /usr/local/bin/ios` , you can change the directory if you like, but remember also make changes to your wrapper scripts. Create symlink for our ld wrapper like `ln -s /usr/local/bin/aarch64-apple-darwin20.1.0-ld /usr/local/bin/ios/ld`.

We should have every thing setup except `llc` and `opt` which required by GHC but not present in Xcode CLT. Apple didn't provided us Polly based LLVM tools in their toolchain, that's why we still need an external LLVM toolchain to build GHC.

Download LLVM 12 from your package manager(distro), or compile by yourself, I wrote a [terrible Homebrew formulae](https://gist.github.com/Torrekie/ce430183d06274c0b3854bd2d517447b) that compiles Apple's LLVM 12, if you decide to use mine, remember `brew install llvm-apple@12 -s -v -d` for dealing with some simple errors.

Once you installed LLVM 12 from Homebrew, you can create `aarch64-apple-darwin20.1.0-llc` like that

```bash
#!/bin/bash
IPHONEOS_DEVELOPMENT_TARGET=12.0
"exec" "xcrun" "--sdk" "iphoneos" "<your_llc_path>" "-mtriple=arm64-apple-ios" "--aarch64-neon-syntax=apple" "$@"
```

Replace "<your_llc_path>" with the path of llc executable you have installed with LLVM, same for creating `aarch64-apple-darwin20.1.0-opt`.

Also create symlinks under /usr/local/bin/ios

```shell
$ ln -s /usr/local/bin/aarch64-apple-darwin20.1.0-llc /usr/local/bin/ios/llc
$ ln -s /usr/local/bin/aarch64-apple-darwin20.1.0-opt /usr/local/bin/ios/opt
```

Now we should have iOS cross toolchain set up, the final step is to get prebuilt GHC binaries, you can install from your package managers or manually download from [<span dir="">https://downloads.haskell.org</span>](https://downloads.haskell.org)

Toolchain should be okay now, we are going to actually compile GHC next step, make sure power is connected before proceeding.

## Building GHC

Before we actually building GHC to iOS, dependencies should be resolved first.

#### Compile libffi and gmp to iOS

libffi:

```bash
# Download libffi source
$ wget https://github.com/libffi/libffi/releases/download/v3.4.2/libffi-3.4.2.tar.gz
# Unpack source tarball
$ tar xf libffi-3.4.2.tar.gz
$ cd libffi-3.4.2
# Configure with cross toolchain we've created
$ ./configure --prefix=/usr --build=`bash config.guess` --host=aarch64-apple-darwin20.1.0
# After configuration, we can proceed with make, and then install
# Create a directory under ~/ios-sysroot for storing compiled binaries
$ make install DESTDIR=${HOME}/ios-sysroot
```

gmp:

```bash
# Download gmp source
$ wget https://gmplib.org/download/gmp/gmp-6.2.1.tar.xz
# Unpack source tarball
$ tar xf gmp-6.2.1.tar.xz
$ cd gmp-6.2.1
# Configure like libffi, but with additional args
$ ./configure --prefix=/usr --build=`bash config.guess` --host=aarch64-apple-darwin20.1.0 --disable-shared --enable-cxx --with-pic 
# Make
$ make
# Install to ~/ios-sysroot
$ make install DESTDIR=${HOME}/ios-sysroot
```

As Homebrew's maintainer said, we have to avoid libgmp being dynamically linked to GHC, so we are not installing gmp dynamic libraries to \~/ios-sysroot.

#### Optional dependencies

I haven't really get into these packages, but it would be nice to enable all optional features

Alex: https://www.haskell.org/alex

Happy: https://github.com/simonmar/happy

HsColour: http://hackage.haskell.org/package/hscolour

### Now, Compile GHC

Download and change working directory to there, then copy `mk/build.mk.sample` to `mk/build.mk`. We are going to perform a full build with LLVM backend, set
```
# As above but build GHC using the LLVM backend
BuildFlavour = perf-llvm
```
and if you don't want haddock documentations, turn `HADDOCK_DOCS` off.
```
# Enable pretty hyperlinked sources
HADDOCK_DOCS = NO
```
Remember to edit llvm-targets, GHC 9.2.1's iOS configuration was too old.

Replace
```
,("aarch64-apple-ios", ("e-m:o-i64:64-i128:128-n32:64-S128", "apple-a7", "+fp-armv8 +neon +crypto +zcm +zcz +sha2 +aes"))
```
With
```
,("arm64-apple-ios", ("e-m:o-i64:64-i128:128-n32:64-S128", "apple-a9", "+v8.3a +fp-armv8 +neon +crc +crypto +fullfp16 +ras +lse +rdm +rcpc +zcm +zcz +sha2 +aes"))
,("aarch64-apple-ios", ("e-m:o-i64:64-i128:128-n32:64-S128", "apple-a9", "+v8.3a +fp-armv8 +neon +crc +crypto +fullfp16 +ras +lse +rdm +rcpc +zcm +zcz +sha2 +aes"))
```
You can replace `apple-a9` with your chip (But llvm@12 don't support `apple-m1`, the latest apple `mcpu` supported are `apple-a13` and `apple-latest`), or just `generic` instead.

While configuring GHC with cross toolchains, it seems not working quite properly if we don't force a `PATH`, I use GNU Coreutils for some basic commands, but on macOS we should normally use shipped BSD style commands. To make sure everything working as expected, only put necessary paths into `PATH`.
```shell
# Set ALT_CLANG and ALT_CLANGXX to LLVM 12 one rather than the one in Xcode CLT
# Remember change the path to your actual installed location
$ export ALT_CLANG=/opt/homebrew/Cellar/llvm-apple@12/5.5/bin/clang
$ export ALT_CLANGXX=/opt/homebrew/Cellar/llvm-apple@12/5.5/bin/clang++
# GHC uses llc to generate assembly codes, then compile it with clang, and LSE feature needs to be enabled.
$ export APPEND_C_FLAGS="-march=armv8-a+lse"
# You might want to add a cross sysroot to search paths
$ export APPEND_C_FLAGS+="-I${HOME}/ios-sysroot/usr/include"
$ export APPEND_CXX_FLAGS=$APPEND_C_FLAGS
$ export APPLEND_LD_FLAGS="-L${HOME}/ios-sysroot/usr/lib"
# By using these environment variables, we can avoid reconfiguring GHC if some flags needs to be modified
# Make sure PATH only contains system defaults, bin path for native GHC and LLVM
$ export PATH=/usr/bin:/usr/local/bin:/usr/sbin:/bin:/sbin:/opt/homebrew/Cellar/ghc/8.10.7_1/bin:/opt/homebrew/Cellar/llvm-apple@12/5.5/bin
# Now configure GHC
# Arguments explanation:
#   --target=aarch64-apple-darwin20.1.0 : Targeting compiler to aarch64-apple-darwin,
#                                       ; Normally we only use --build= and --host=
#                                       ; But building a compiler will require target.
#   --enable-numa=no : Darwin does not support NUMA
#   --with-gmp-includes : Specify GMP header search path
#   --with-gmp-libraries : Specify GMP library search path
#   --with-system-libffi : Using system libffi instead of bundled one
#   --with-ffi-includes : Specify libffi header search path
#   --with-ffi-libraries : Specify libffi library search path
#   --disable-dtrace : Disabling DTrace since we don't have DTrace support on iOS yet
#
$ ./configure --prefix=/usr --target=aarch64-apple-darwin20.1.0 --enable-numa=no --with-gmp-includes=$HOME/ios-sysroot/usr/include --with-gmp-libraries=$HOME/ios-sysroot/usr/lib --with-system-libffi --with-ffi-includes=$HOME/ios-sysroot/usr/include --with-ffi-libraries=$HOME/ios-sysroot/usr/lib
# Make sure llc and opt were configured with our aarch64-apple-darwin20.1.0 prefix, then we can run make, this takes a long process, go grab a cup of coffee and your iPad
# Don't open too many threads, your tiny computer can't afford this
$ make -j2
# After all builds complete, run make install
$ make install DESTDIR=`pwd`/ghc-ios
```

Now we have a pretty stage2 GHC toolchain under `ghc-ios/`, we still need to do some modifications to installed settings

### Fix settings for built GHC
Open `ghc-ios/usr/lib/ghc-9.2.1/settings`, then replace these configs depending on your condition:
```
,("C compiler command", "cc")
,("C compiler flags", "--target=arm64-apple-ios -mios-version-min=14.2 ")
,("C++ compiler flags", "--target=arm64-apple-ios -mios-version-min=14.2 ")
,("C compiler link flags", "--target=arm64-apple-ios -mios-version-min=14.2 ")
,("C compiler supports -no-pie", "NO")
,("Haskell CPP command", "cc")
,("ld command", "ld")
,("ld flags", "")
,("ranlib command", "ranlib")
,("otool command", "otool")
,("install_name_tool command", "install_name_tool")
,("touch command", "touch")
,("cross compiling", "NO")
,("target platform string", "aarch64-apple-darwin")
,("LLVM target", "arm64-apple-ios")
,("LLVM llc command", "/usr/libexec/ghc/bin/llc-ios")
,("LLVM opt command", "/usr/libexec/ghc/bin/opt-ios")
,("LLVM clang command", "/usr/libexec/ghc/bin/clang-ios")
```

You might noticed that we are pointing LLVM llc/opt/clang to `/usr/libexec/ghc/bin`, the reason we are doing that is, while GHC calling `clang` (not 'C compiler' but 'LLVM clang') as assembler, no target specifications are passed to it and no GHC flags can change its behavior till I wrote this article. For `llc` and `opt`, sometimes users might not have LLVM with Polly enabled on their device (e.g. LLVM 10 in Elucubratus), we also need wrapper scripts for them.

I have compiled and installed a LLVM 12 to my device, so set `LLVM_ROOT` to my LLVM prefix `/usr/lib/llvm-12`, you might want to edit these scripts for your environment.

Create this directory and add our wrapper scripts
```shell
$ mkdir -p ghc-ios/usr/libexec/ghc/bin
```
create `ghc-ios/usr/libexec/ghc/bin/llc-ios` and `ghc-ios/usr/libexec/ghc/bin/opt-ios`
```bash
#!/bin/bash
LLVM_CMD="llc" # change this to "opt" and save as `opt-ios'
LLVM_ROOT="/usr/lib/llvm-12"

if [[ $(${LLVM_CMD} -h &> /dev/null; echo $?) == 127 ]]; then
  if [[ -d ${LLVM_ROOT} ]] && [[ $(${LLVM_ROOT}/bin/${LLVM_CMD} -h &> /dev/null; echo $?) != 127 ]]; then
    LLVM_CMD="${LLVM_ROOT}/bin/${LLVM_CMD}"
  else
    LLVM_CMD="false"
  fi
fi

${LLVM_CMD} "-mtriple=arm64-apple-ios" "--aarch64-neon-syntax=apple" "$@"
```
Then clang wrapper `ghc-ios/usr/libexec/ghc/bin/clang-ios`:
```bash
#!/bin/bash
"clang" "-march=armv8-a+lse" "-target" "arm64-apple-ios14.2" "-mios-version-min=14.2" ${@}
```

### Modify Auto Generated conf files
Since conf files were auto generated from our configuration, unexpected search paths will be added to some of them, causing `ghc-pkg` cannot perform package recache after installed on iOS. Do `grep "ios-sysroot/usr" -rl ghc-ios/usr/lib/ghc-9.2.1/package.conf.d` to find all files containing `*ios-sysroot/usr*` that we've set for searching libffi and gmp during config. Open the files found with `.conf` suffix, delete all lines containing "ios-sysroot" and save.

Previously generated `package.cache` and `package.cache.lock` needs to be deleted.
```
$ rm ghc-ios/usr/lib/ghc-9.2.1/package.conf.d/package.cache{.lock}
```

### Change @rpaths to Absolute Paths
iOS dyld does not behaving quite friendly with relative paths, on macOS, if `/usr/lib/libsomething.dylib` not exist, dyld will automatically try `/usr/local/lib/libsomething.dylib` and `@rpath/libsomething.dylib`. But due to some code signing magics, dyld on iOS will met "codesign invalid" errors while attempting to load guessed library location. So we are going to make everything absolute.

Create a script named `replace-install-name.sh` with following content:
```
#!/bin/bash
# Terrible script for batch replacing install names
GHC_IOS="ghc-ios"
FILE_CMD="/opt/homebrew/opt/file-formula/bin/file"
change() {
for i in $(find ${GHC_IOS} -type f -name "*.dylib" -executable); do
  for j in $(for k in $(find ${GHC_IOS}/ -type f -executable); do ${FILE_CMD} -i $k | grep "application/x-mach-binary; charset=binary" | sed "s/: application\/x-mach-binary; charset=binary//g"; done); do
    install_name_tool -change @rpath/$(basename $i) $(echo $i | sed "s/${GHC_IOS}//g") $j
  done
done
}

id() {
for i in $(find ${GHC_IOS} -type f -name "*.dylib" -executable); do
  install_name_tool -id $(echo $i | sed "s/${GHC_IOS}//g") $i
done
}

change
id
```
Then run `bash replace-install-name.sh`, wait for it finishes.

### Codesign
Create a XML file named `ghc.xml`, then fill with these entitlements.
```
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
	<key>com.apple.private.syspolicy.execution-policy-bypass</key>
	<true/>
	<key>platform-application</key>
	<true/>
	<key>com.apple.private.security.no-container</key>
	<true/>
	<key>com.apple.private.skip-library-validation</key>
	<true/>
	<key>com.apple.security.network.client</key>
	<true/>
	<key>com.apple.security.network.server</key>
	<true/>
	<key>com.apple.system-task-ports</key>
	<true/>
	<key>run-unsigned-code</key>
	<true/>
	<key>task_for_pid-allow</key>
	<true/>
	<key>get-task-allow</key>
	<true/>
	<key>dynamic-codesigning</key>
	<true/>
	<key>com.apple.security.exception.files.absolute-path.read-write</key>
	<array>
		<string>/</string>
	</array>
	<key>com.apple.security.temporary-exception.files.absolute-path.read-write</key>
	<array>
		<string>/</string>
	</array>
</dict>
</plist>
```
Not sure if all entitlements were needed, but it just works.

Then codesign all Mach-O files.
```
# If no `codesign', use `ldid' instead
## ldid -Sghc.xml $j
$ for j in $(for i in `find ghc-ios/ -type f -executable`; do file -i $i | grep "application/x-mach-binary; charset=binary" | sed "s/: application\/x-mach-binary; charset=binary//g"; done); do sudo codesign -f -s - --entitlements ghc.xml $j; done
```

### Packaging
On jailbroken iOS we use APT/dpkg as default package manager, we are going to pack it to deb for installation.

Create DEBIAN directory under ghc-ios
```
$ mkdir ghc-ios/DEBIAN
```
Create file `ghc-ios/DEBIAN/control` with content, and modify it for your setup
```
Package: ghc
Name: Glasgow Haskell Compiler
Version: 9.2.1
Architecture: iphoneos-arm
Depends: libffi, llvm-12, bash
Maintainer: Torrekie <me@torrekie.dev>
Section: Haskell
Tag: purpose::console, role::developer
Homepage: https://haskell.org/ghc/
Description: Glorious Glasgow Haskell Compilation System
```
Next, create a `postinst` script for recache GHC packages after the installation.
```bash
#!/bin/bash
/usr/bin/ghc-pkg recache
```
Finally, let's switch to `fakeroot` and perform the final step for packaging.
```
# Use fakeroot to start a bash session
$ fakeroot bash
bash-5.1# whoami
root
# Pack all contents in ghc-ios/
bash-5.1# dpkg -b ghc-ios/ .
dpkg-deb: building package 'ghc' in './ghc_9.2.1_iphoneos-arm.deb'.
# Exit fakeroot
bash-5.1# exit
```

## Meeting Exceptions
During the process of attempting to port GHC to iOS, I have met lots lots of problems:

1. #21049 memory allocation failed (requested 274878955520 bytes)
    - When I started to write this article, Developers suggests me try to reconfigure with `—-disable-large-address-space`, I believe this is a proper solution but not yet tested by me.
    - I modified `rts/posix/OSMem.c`, changed how `my_mmap()` allocates memory on iOS, it also worked but considered unstable.
2. runghc: /usr/bin/ghc-9.2.1: executeFile: permission denied (Operation not permitted)
    - iOS handles `argv` quite differ than how they do it on macOS, causing args with unexpected format being passed to common *nix softwares. (See https://github.com/ProcursusTeam/Procursus/issues/229)
    - For solving this issue, recompile GHC with [libiosexec](https://github.com/ProcursusTeam/libiosexec), open `libraries/unix/System/Posix/Process.hsc` and change the externs to symbols defined in libiosexec
```haskell
-- Switch exec* implementations to libiosexec
-- e.g. "execv" => "ie_execv"
foreign import ccall unsafe "ie_execvp"                                                                                                                                                           
  c_execvp :: CString -> Ptr CString -> IO CInt                                                                                                                                                   
                                                                                                                                                                                                  
foreign import ccall unsafe "ie_execv"                                                                                                                                                            
  c_execv :: CString -> Ptr CString -> IO CInt                                                                                                                                                    
                                                                                                                                                                                                  
foreign import ccall unsafe "ie_execve"                                                                                                                                                           
  c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt
```

### Notes
You might want to use libffi and gmp that provided by your distro, not really needs to compile a brand new one by your self.