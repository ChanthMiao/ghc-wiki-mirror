# Parsing of command line arguments


GHC's many flavours of command line flags make the code interpreting them rather involved. The following provides a brief overview of the processing of these options. Since the addition of the interactive front-end to GHC, there are two kinds of flags: static and dynamic. Static flags can only be set once on the command line. They remain the same throughout the whole GHC session (so for example you cannot change them within GHCi using `:set` or with `OPTIONS_GHC` pragma in the source code). Dynamic flags are the opposite: they can be changed in GHCi sessions using `:set` command or `OPTIONS_GHC` pragma in the source code. There are few static flags and it is likely that in the future there will be even less. Thus, you won't see many static flag references in the source code, but you will see a lot of functions that use dynamic flags.


Command line flags are described by Flag data type defined in [compiler/GHC/Driver/CmdLine.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/CmdLine.hs):

```haskell
data Flag m = Flag
    {   flagName    :: String,   -- Flag, without the leading "-"
        flagOptKind :: OptKind m -- What to do if we see it
    }
```


    
This file contains functions that actually parse the command line parameters. 

## Static flags


Static flags are managed by functions in [compiler/main/StaticFlags.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/main/StaticFlags.hs).


Function `parseStaticFlags ::` is an entry point for parsing static flags. It is called by the `main :: IO ()` function of GHC in [ghc/Main.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/ghc/Main.hs). Two global IORefs are used to parse static flags: `v_opt_C_ready` and `v_opt_C`. These are defined using `GLOBAL_VAR` macro from [compiler/HsVersions.h](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/HsVersions.h). First IORef is a flag that checks whether the static flags are parsed at the right time. Initialized to `False`, it is set to `True` after the parsing is done. `v_opt_C` is a `[String]` used to store parsed flags (see `addOpt` and `removeOpt` functions). 


In [compiler/main/StaticFlags.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/main/StaticFlags.hs), `flagsStatic :: [Flag IO]` defines a list of static flags and what actions should be taken when these flags are encountered (see `Flag` data type above). It also contains some helper functions to check whether particular flags have been set. Functions `staticFlags :: [String]` and `packed_staticFlags :: [FastString]` return a list of parsed command line static flags, provided that parsing has been done (checking the value of `v_opt_C_ready`).

## Dynamic flags


They are managed by functions in [compiler/GHC/Driver/Session.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/Session.hs) file. Looking from the top you will find data types used to described enabled dynamic flags: `DumpFlag`, `GeneralFlag`, `WarningFlag`, `Language`, `SafeHaskellMode`, `ExtensionFlag` and finally `DynFlags`. Function `defaultDynFlags :: Settings -> DynFlags` initializes some of the flags to default values. Available dynamic flags and their respective actions are defined by `dynamic_flags :: [Flag (CmdLineP DynFlags)]`. Also, `fWarningFlags :: [FlagSpec WarningFlag]`, `fFlags :: [FlagSpec GeneralFlag]`, `xFlags :: [FlagSpec ExtensionFlag]` and a few more smaller functions define even more flags needed for example for language extensions, warnings and other things. These flags are descibred by the data type `FlagSpec f`:

```haskell
type FlagSpec flag
   = ( String   -- Flag in string form
     , flag     -- Flag in internal form
     , TurnOnFlag -> DynP ())    -- Extra action to run when the flag is found
                                 -- Typically, emit a warning or error
```


Flags described by `FlagSpec` can be reversed, e.g. flags that start with `-f` prefix are reversed by using `-fno-` prefix instead.
