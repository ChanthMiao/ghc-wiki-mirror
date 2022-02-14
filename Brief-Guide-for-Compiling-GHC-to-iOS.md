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

We can simply create symlinks to toolchain binaries by executing following command:

```
$ sudo for tools in `ls /Library/Developer/CommandlineTools/usr/bin/`; do /usr/bin/printf "#\!/bin/bash\nIPHONEOS_DEPLOYMENT_TARGET=14.2\nexec xcrun --sdk iphoneos $tools \${@}" > /usr/local/bin/aarch64-apple-darwin20.1.0-${tools}; done
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