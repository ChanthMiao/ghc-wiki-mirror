# Remote GHCi


Patches to implement this: [Phab:D1562](https://phabricator.haskell.org/D1562), [ Phab:D1747](https://phabricator.haskell.org/D1747), [ Phab:D1748](https://phabricator.haskell.org/D1748)


Remote GHCi was first available in GHC 8.0.1.  Implementation notes: [Commentary/Compiler/ExternalInterpreter](commentary/compiler/external-interpreter)


This is a design page for implementing GHCi and Template Haskell by running the interpreted code in a separate process from GHC itself.  

## Motivation


There are several reasons for wanting to do this:

- It **decouples the compiler from the interpreted code**, meaning that they can run on separate runtimes.  This means that the interpreted code can run profiled while the compiler can be unprofiled, for example.   

- We can have **stack traces in interpreted code** (#11047), without the compiler itself having to be profiled.

- It separates the decision about whether to use the dynamic linker for interpreted code from whether GHC itself must be dynamically linked.  We could **go back to a statically-linked GHC, which should be faster.**

- We could **use dynamic linking in GHCi on Windows**, which was previously blocked because GHC itself is too big to make into a dynamic library. (whether this is a good idea or not is debatable, but at least it's technically possible now)

- When compiling Template Haskell code with `-prof`, we wouldn't have to build the code the normal way first, or use `-osuf`.  The way this is done currently is inherently unsafe, because we use the profiled `.hi` files with the unprofiled object files, and hope that the two are in sync.  This annoyance (and associated code and build-system nonsense) just goes away with Remote GHCi.

- When compiling Template Haskell code we no longer need to force `-dynamic-too` if GHC is dynamically-linked.

- Various annoying stuff in the base package to ensure that we get one instance of certain pieces of state when there are multiple base packages loaded goes away.  The `Global` stuff in the RTS goes away.

- The interpreted code could be running on an **entirely separate platform**, architecture, or hardware.  GHCJS does this to run TH code and GHCi, for example.

- We can have **multiple instances of a GHC Session** on the GHC API, each running TH and/or interpreted code.  Right now this is not possible because the linker's state is global.

- The virtual CWD in `InteractiveEval` is not necessary, because the GHCi process has its own CWD.

- We could use TH and limited GHCi without any dynamic linker support at all. (#12218)

## Disadvantages

- We can't directly share data between interpreted and compiled code, everything must be marshaled via `Binary`.
- `dynCompileExpr` doesn't work any more.

## Implementation


See [Commentary/Compiler/ExternalInterpreter](commentary/compiler/external-interpreter)

## Template Haskell in stage1


If the stage1 compiler supported Template Haskell, then we could use it in GHC itself or the libraries that GHC depends on.  


However, this is still difficult even running the interpreted code in an external process.  The problem is that the server is compiled with the stage 0 compiler, using the stage0 RTS and linked against the stage0 libraries, meanwhile it would be loading the stage1 libs.  If we, say, added a new primop and used that in the stage1 base package, it would fail to link against the stage0 RTS which doesn't have the primop.  So this isn't necessarily going to just work, unless we have a sufficiently compatible compiler to bootstrap from.


So we shouldn't rush to use TH in GHC or the libraries, because it would constrain the stage0 compiler to be a much more recent version than we currently require.
