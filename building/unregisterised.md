# Unregisterised builds


Normally GHC will try to do a so-called registerised build, where it uses various architecture and OS specific knowledge to get more efficient code. However, on an architecture where this information has not been created, or has not been kept up-to-date, it is necessary to do an unregisterised build, which uses just plain old portable C.


To do an unregisterised build, use the `--enable-unregisterised` flag when configuring.


GHC will automatically do an unregisterised build on platforms that it knows don't currently have registerised support. See the variable `UnregisterisedDefault` in [configure.ac](https://gitlab.haskell.org/ghc/ghc/tree/master/configure.ac).


The various part of GHC work with registerised and unregisterised as follows:

- The native code generator requires a registerised build.
- Object splitting requires a registerised build.
- The C backend requires an unregisterised build.
- The LLVM backend requires a registerised build.
