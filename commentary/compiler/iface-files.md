# Interface files


An **interface file** supports separate compilation by recording the information gained by compiling `M.hs` in its interface file `M.hi`.  Morally speaking, the interface file `M.hi` is part of the object file `M.o`; it's like a super symbol-table for `M.o`.


Interface files are kept in binary, GHC-specific format.  The format of these files changes with each GHC release, but not with patch-level releases.  The contents of the interface file is, however, completely independent of the back end you are using (`-fviaC`, `-fasm`, `-fcmm` etc).


Although interface files are kept in binary format, you can print them in human-readable form using the command:

```wiki
  ghc --show-iface M.hi
```


This textual format is not particularly designed for machine parsing.  Doing so might be possible, but if you want to read GHC interface files you are almost certainly better off using the [GHC API](commentary/compiler/api) to do so. If you are wondering how some particular language feature is represented in the interface file, this command is really useful! Cross-reference its output with the `Outputable` instance defined in [compiler/GHC/Iface/Load.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Iface/Load.hs)


Here are some of the things stored in an interface file `M.hi`

- The version of GHC used to compile the module, as well as the compilation way and other knick-knacks
- A list of what `M` exports.
- The types of exported functions, definition of exported types, and so on.
- Version information, used to drive the [recompilation checker](commentary/compiler/recompilation-avoidance).
- The strictness, arity, and unfolding of exported functions.  This is crucial for cross-module optimisation; but it is only included when you compile with `-O`.
- Imports and usages from those imports. This information is crucial for [recompilation avoidance](commentary/compiler/recompilation-avoidance).


The contents of an interface file is the result of serialising the **`IfaceSyn`** family of data types.  The data types are in [compiler/GHC/Iface/Syntax.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Iface/Syntax.hs) and [compiler/GHC/Iface/Type.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Iface/Type.hs); the binary serialisation code is in [compiler/GHC/Iface/Binary.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Iface/Binary.hs). The definition of a module interface is the **`ModIface`** data type in [compiler/GHC/Driver/Types.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Types.hs).


Details of some of the types involved in GHC's representation of Modules and Interface files can be found [here](commentary/compiler/module-types).

## When is an interface file loaded?


The act of loading an interface file can cause various parts of the compiler to behave differently; for instance, a type class instance will only be used if the interface file which defines it was loaded.  Additionally, GHC tries to avoid loading interface files if it can avoid it, since every loaded interface file requires going to the file system and parsing the result.


The big situations when we load an interface file:

- When you import it (either explicitly using an `import`, or implicitly, e.g. through `-fimplicit-import-qualified` in GHCi; `loadSrcInterface`)
- When we need to get the type for an identifier (`loadInterface` in `importDecl`)
- When it is listed as an orphan of an imported module (`loadModuleInterfaces "Loading orphan modules"`)


We also load interface files in some more obscure situations:

- When it is used as the backing implementation of a signature (`loadSysInterface` in `tcRnSignature`)
- When we look up its family instances (`loadSysInterface` in `getFamInsts`)
- When its information or safety (`getModuleInterface` in `hscGetSafe`)
- When an identifier is explicitly used (including a use from Template Haskell), we load the interface to check if the identifier is deprecated (`loadInterfaceForName` in `warnIfDeprecated`/`loadInterfaceforName` in `rn_bracket`)
- Recompilation checking (`needInterface` in `checkModUsage`)
- When we need the fixity for an identifier (`loadInterfaceForName` in `lookupFixityRn`)
- When we reify a module for Template Haskell (`loadInterfaceForModule` in `reifyModule`)
- When we use a wired-in type constructor, since otherwise the interface file would not be loaded because the compiler already has the type for the identifier. (`Loading instances for wired-in things`)
- When `-XParallelArrays` or `-fvectorise` are specified for DPH (`loadModule` in `initDs`)
- When we load a plugin (`DynamicLoading`)
- To check consistency against the `hi-boot` of a module
- To check the old interface file for recompilation avoidance
