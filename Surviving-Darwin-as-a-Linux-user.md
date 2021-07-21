Darwin has quite a few quirks which may catch a Linux user by surprise. Good luck!

# Collecting core dumps

In principle core dumps are supposed to end up in `/cores`, although to this day I have yet to see this actually happen.

# Rosetta 2 and multi-architecture support

Rosetta 2 is Apple's x86-64 to AArch64 dynamic translation engine and was introduced in Big Sur. Many of the system utilities and toolchain executables take advantage of MachO's multi-architecture object support which allows a single executable to contain multiple "slices". The operating system appears to use undocumented heuristics to decide whether to run the AArch64 or x86-64 slice when starting a new process. In particular, it would seem that the x86-64 slice is loaded if *any* of the parents of the spawning process are emulated.

This gives rise to somewhat surprising behavior in the case of toolchain executables like `clang`: using an x86-64 `cabal-install` executable to build a package with an AArch64 GHC will result in assembler errors due to `clang` trying to compile GHC's assembler as x86-64 code. For this reason, we take pains to always pass a `--target` flag to `clang`.

Additionally, it seems that trying to run `lldb` on an AArch64 executable will fail unless one explicitly sets the architecture (`arch -arm64e lldb ...`)

# lldb over ssh

One must run `sudo dscl . append /Groups/_developer GroupMembership $username` on a user before that user can attach to a process in `lldb` from a non-interactive (e.g. SSH) session.

# Debug information

Darwin has a [rather non-traditional](https://stackoverflow.com/questions/10044697/where-how-does-apples-gcc-store-dwarf-inside-an-executable) means of  storing debug information in executables.

Unfortunately, it is rather tricky to arrange that symbols for dynamic objects are picked up by debugging tools. Consequently, users needing symbols are probably best advised to rather use a static build (e.g. using Hadrian's `+no_dynamic_ghc` flavour transformer).