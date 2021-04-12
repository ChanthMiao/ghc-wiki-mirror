[[_TOC_]]

**Overall progress (estimated)**: ![65%](https://progress-bar.dev/65)
<details><summary> :gear: <b>Progress Breakdown</b> (click the arrow to expand!)</summary>

As this strand of work touches a lot of modules, doing everything as a single gargantuan MR seems highly impractical. Rather, we are considering breaking things down into atomic chunks which could be reviewed in isolation. A sketch of the plan might be the following:

- [X] Split `GHC.Driver.Env` into the former and a new 
  `GHC.Driver.Env.Types` module to avoid `.boot` modules later on.
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4551

- [X] Rename types inside `GHC.Parser.Errors` to give them a `Ps` prefix. 
  This is because when we will have a lot of different `Warning` and 
  `Error` types it will be better to be explicit in the code to understand 
  at glance "which is which".
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4555

- [X] Untangle the error reporting functions from the `DynFlag` even 
  further. This can be done by getting rid of the `errShortString` field 
  from the `ErrMsg` record, which allows us to drop an extra `DynFlag` 
  argument from a lots of functions (e.g. `mkErr`) and give us more 
  flexibility on where to place certain error-related utility functions;
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4574

- [X] Clean-up the error hierarchy by introducing a proper data 
  type for `Messages` (instead of a type alias to a tuple), parameterised
  by an error type `e`. Initially everything can be instantiated with `e = ErrDoc`
  to not change too many things at once, and later use proper domain-specific types
  (e.g. parser diagnostic messages, typecheck messages, etc);
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4728

- [x] (Optional, but desirable) Get rid of `ErrDoc`, `MsgDoc` and `WarnMsg` to
  reduce the cognitive overload when dealing with error types;
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4747

- [X] Split the old Severity into `MessageClass` and `Severity`, introduce
  `DiagnosticReason` and `DiagnosticMessage`.
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5116

- [X] Correctly give the right `Severity` "at birth", without spurious reclassifications
  (i.e. demotions/promotions etc) for diagnostic messages;
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5172

- [X] Remove redundant checks when emitting some warnings (Design A);
  **Implemented**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5207

- [ ] Introduce proper diagnostic types for the different phases of 
  the compilation pipeline (i.e. `TcRnMessage`, `PsMessage` etc). Initially these
  can also contain a selection of all the GHC-emitted messages, and "filled" later, to minimise
  breakages. Introduce also an umbrella `GhcMessage` type which will be used in the
  driver, at the top level, to report diagnostics. At this stage we won't yet
  make use of any of the new types;
  **Waiting CI/review**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5509

- [ ] Extend the parser error types to adhere to the new error-messages
  architecture, and port the codebase to use these new format of errors.

- [ ] Convert the `TcRn` error types to adhere to the new error-messages
  architecture. We will also try to make use of the new `Suggestion` API
  when reporting suggestions to users.

- [ ] Convert the `Ds` error types to adhere to the new error-messages
  architecture;

- [ ] Convert the `Driver` error types to adhere to the new error-messages
  architecture.
</details>


# Short summary / Intro

Following the high-level plan posted in [ghc proposal#306](https://github.com/ghc-proposals/ghc-proposals/pull/306), we have been planning on switching our error representation away from mere documents. The end goal is to have subsystem-specific error data types, where each constructor of such a data type will represent one error that this subsystem can throw. A global `GhcMessage` sum type would also be provided, at the driver level, so as to be able to represent any error thrown by any subsystem and provide a trivial mechanism for API users to deal with error values (namely: just hand them a bag of `GhcMessage` values).
The relevant ticket is #18516.

# Background: the current error infrastructure

<details><summary>Current error infrastructure (click the arrow to expand!)</summary>

We currently have:

``` haskell
type Messages        = (WarningMessages, ErrorMessages)
type WarningMessages = Bag WarnMsg
type ErrorMessages   = Bag ErrMsg

type WarnMsg = ErrMsg
data ErrMsg = ErrMsg {
    errMsgSpan        :: SrcSpan,
    errMsgContext     :: PrintUnqualified,
    errMsgDoc         :: ErrDoc,
    -- | This has the same text as errDocImportant . errMsgDoc.
    errMsgShortString :: String,
    errMsgSeverity    :: Severity,
    errMsgReason      :: WarnReason
    }

data ErrDoc = ErrDoc {
    -- | Primary error msg.
    errDocImportant     :: [MsgDoc],
    -- | Context e.g. \"In the second argument of ...\".
    errDocContext       :: [MsgDoc],
    -- | Supplementary information, e.g. \"Relevant bindings include ...\".
    errDocSupplementary :: [MsgDoc]
    }

type WarnMsg = ErrMsg

data Severity
  = SevOutput
  | SevFatal
  | SevInteractive

  | SevDump
    -- ^ Log message intended for compiler developers
    -- No file/line/column stuff

  | SevInfo
    -- ^ Log messages intended for end users.
    -- No file/line/column stuff.

  | SevWarning
  | SevError
    -- ^ SevWarning and SevError are used for warnings and errors
    --   o The message has a file/line/column heading,
    --     plus "warning:" or "error:",
    --     added by mkLocMessags
    --   o Output is intended for end users

data WarnReason
  = NoReason
  -- | Warning was enabled with the flag
  | Reason !WarningFlag
  -- | Warning was made an error because of -Werror or -Werror=WarningFlag
  | ErrReason !(Maybe WarningFlag)
```

</details>

# New API

Let's start by making `Messages` and `MsgEnvelope` polymorphic over `e`, which is the particular message they now carry:

```haskell

newtype Messages e = Messages (Bag (MsgEnvelope e))

data MsgEnvelope e = MsgEnvelope
   { errMsgSpan        :: SrcSpan
   , errMsgContext     :: PrintUnqualified
   , errMsgDiagnostic  :: e
   , errMsgSeverity    :: Severity
   } deriving Functor

-- Text organized into bullets.
newtype DecoratedSDoc = Decorated { unDecorated :: [SDoc] }

class Diagnostic a 
  ... -- more on this later

-- .. operations on messages
```

This allows us to move away from `SDoc` and simply instantiate `e` with a structured message, specific a particular compilation phase (parsing, tcRn, ...)


# The envelope contents (i.e. the diagnostics)

We can get each subsystem to define their own message (diagnostic) types. At the beginning, to smooth out the integration, they can even be very simple wrappers around `DecoratedSDoc`s:


``` haskell
-- somewhere in the parser
data PsMessage = PsUknownMessage DecoratedSDoc

-- in GHC.Tc.Monad/Types?
data TcRnMessage = TcRnUnknownMessage DecoratedSDoc

-- somewhere under GHC.Driver
data GhcMessage where
  -- | A message from the parsing phase.
  GhcPsMessage      :: PsMessage -> GhcMessage
  -- | A message from typecheck/renaming phase.
  GhcTcRnMessage    :: TcRnMessage -> GhcMessage
  -- | A message from the desugaring (HsToCore) phase.
  GhcDsMessage      :: DsMessage -> GhcMessage
  -- | A message from the driver.
  GhcDriverMessage  :: DriverMessage -> GhcMessage
  -- | An \"escape\" hatch which can be used when we don't know the source of the message or
  -- if the message is not one of the typed ones.
  GhcUnknownMessage :: forall a. (Diagnostic a, Typeable a) => a -> GhcMessage

```

With those types in place, we could begin instantiating `e` to the relevant type for all use sites of the error infrastructure. The parser could would deal with `Messages PsMessage` values, the renamer and typechecker would produce `Message TcRnMessage` values, and so on.

Finally, we could start turning concrete errors into dedicated constructors of `PsMessage`/`TcRnMessage`. Starting slowly with simple [`not in scope` errors](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Rename/Unbound.hs#L64) and the likes, before converting over [the entire typechecking error infrastructure](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Errors.hs) and more.  For example:
```
data TcRnMessage = TcRnUnknownMessage DecoratedSDoc
  | OutOfScopeErr RdrName
  | ...
```
The idea is to have one data constructor per error, so that a IDE using the GHC API would not have to parse strings to understand the errors.

This might involve systematically retaining a bit more information (context lines for the typechecker, for instance) and therefore might give rise to some more generic error infrastructure constructs. This page will be updated to incorporate such details once they are figured out.

At the "top level", in the driver, where we call the different subsystems to process Haskell modules, we would end up accumulating and reporting `GhcMessage` values. The goal is to have the GHC program emit the exact same diagnostics as it does today, but affect the API in such a way that GHC API users would at this point get a chance to work with the algebraic error descriptions, making inspection and custom treatment a whole lot easier to implement. We could perhaps even demonstrate it at this point by implementing a little "demo" of sorts for this new way to consume errors.

# New API key points

API design explanation/considerations:

* `Messages` is now a `newtype` so it can be expressed in terms of an opaque interface, and
  it's parameterised over an abstract payload `e`. It's just a bag of **envelopes**;

* The `ErrMsg` is renamed `MsgEnvelope` and has to be intended as an envelope
  that carries information about the _content_ it stores inside. We can call such
  envelope content the **diagnostic**;

* The old `Severity` type is split into two types, `MessageClass` and `Severity`. The former
  is the _class_ of the message (is this a debug message? A dump one? An information log
  emitted by GHC somewhere?) whereas the `Severity` is the severity of the **diagnostic**. This
  split **prevents** the construction of **impossible states**, like creating a `MsgEnvelope`
  which `Severity` is `MCDump`, for example.

* The **diagnostic** is the _envelope content_ of a `MsgEnvelope`, and it characterises the
  particular provenance of the envelope (is this a parser error? Is this a TcRn warning?). For
  example, `MsgEnvelope PsMessage` is an envelope which was created during GHC parsing phase,
  and represents a parsing diagnostic (either an error or a warning);

* A `MsgEnvelope` has a `Severity`, which type reflects the fluid relationship between
  warnings and errors. The `Messages` type simply collects facts about the GHC running program:
  peeking into the individual envelope tells us:
     `a)` If this is an error or a warning;
     `b)` What does this `MsgEnvelope` carries inside its `errMsgDiagnostic`;

* A `DecoratedSDoc` is a newtype that allows us to collect a list of `SDoc` from various
  printing functions and place a bullet between each of them, so that they render nicely.

* Every warning or error arises for a *reason*.
  * Most warnings are controlled by specific `-Wblah-blah` flags; we call these *reasons* for the warning.
  * Some warnings are not associated with a flag. We currently say these have "no reason" for arising, but really, we mean that there is no flag.
  * Errors are not currently controlled by flags. But this is a small lie, as a flag like `-fdefer-type-errors` really is meant to change an error into a warning. By making a *reason* for the error that connects it to a flag, we might imagine controlling errors by flags, just like we do warnings. (Some errors will always be fatal.)
* Separate from a reason, each diagnostic has a *severity*: will reporting the diagnostic always halt the compiler, or can we continue?
* Relating a reason with a severity requires the use of the `DynFlags`, because the `DynFlags` stores the presence/absence of the `-Wblah-blah` flags.
* Not just any `DynFlags` will do when converting a *reason* to a *severity*: we must use the `DynFlags` in action when the diagnostic was generated. This is important because, for example, `deriving` code might give rise to a warning that we wish to suppress, even with `-Werror` enabled. This is accomplished by locally changing the `DynFlags` while processing code generated by `deriving`. If we use the outer, top-level `DynFlags` to get a severity from a reason, we'll get this wrong. Conclusion: the severity must be chosen when the error message is first created or soon thereafter. (Current design: soon thereafter.)
* When we have informative datatypes describing errors (like `TcRnMessage`), each constructor will be associated with precisely one reason.
* Not all messages are diagnostics, but non-diagnostic error messages do not have associated reasons or flags. Real-world example from GHC:

```hs
displayLintResults :: ...
displayLintResults logger dflags display_warnings pp_what pp_pgm (warns, errs)
  | not (isEmptyBag errs)
  = do { putLogMsg logger dflags Err.MCDump noSrcSpan
           $ withPprStyle defaultDumpStyle
           (vcat [ lint_banner "errors" pp_what, Err.pprMessageBag errs
                 , text "*** Offending Program ***"
                 , pp_pgm
                 , text "*** End of Offense ***" ])
       ; Err.ghcExit logger dflags 1 }

{-
  This is just an informational message, it doesn't have a `DiagnosticReason` or a flag: it's 
  just GHC telling us something as a statement, so it's logged with `MCDump`. Previously to this 
  design things like `Dump`,`Info`,`Fatal`, etc were a `Severity`, which meant we could construct 
  a GHC warning or error with `Severity = Dump` for example, which would have been nonsense.
-}
```

* Messages sometimes have bullet points in them.

A lesser (but non-negligible) concern:
* If we classify severity at the birth of an error message, we get error message regressions with `-Werror`: GHC on occasion checks if any error messages have been emitted. If `-Werror` warnings have already been classified as fatal diagnostics, than the check for error messages will return `True`, prematurely aborting compilation. Really, we want functions like `checkIfErrs` to look for always-fatal messages, not just happens-to-be-fatal messages, but perhaps that would be easier in a separate patch.

When we consider a `MsgEnvelope`, then, we do not want the envelope to describe the reason for the message, as doing so would be redundant: the payload of a `MsgEnvelope` (something that might be a `TcRnMessage`) implicitly stores a reason. It also must be possible to extract a reason and severity from a `MsgEnvelope` in order to correctly format the error message; this must be done without consulting the `DynFlags` (which may be the wrong `DynFlags` for severity-classification, if they have changed).

Putting this all together, this situation suggests the following temporary design:

```hs
data Severity 
 = SevWarning 
 | SevError

data DiagnosticReason 
  = WarningWithoutFlag 
  | WarningWithFlag !WarningFlag 
  | ErrorWithoutFlag

-- | Computes a severity from a reason in the absence of DynFlags. This will likely
-- be wrong in the presence of -Werror.
defaultReasonSeverity :: DiagReason -> Severity
defaultReasonSeverity WarningWithoutFlag   = SevWarning
defaultReasonSeverity (WarningWithFlag {}) = SevWarning
defaultReasonSeverity ErrorWithoutFlag     = SevError

data MsgEnvelope a = MsgEnvelope   -- assume `Diagnostic a` holds
  { errMsgSpan       :: SrcSpan
  , errMsgContext    :: PrintUnqualified
  , errMsgDiagnostic :: a
  , errMsgSeverity   :: Severity
  } deriving Functor

-- See "New API key points" for the rationale on why we introduced this.
-- The class of the message (Is this a dump message? An info message? A diagnostic?)
data MessageClass
  = MCOutput
  | ...    -- several other non-diagnostic message classifications
  | MCDiagnostic Severity DiagnosticReason

-- | A 'MessageClass' for always-fatal errors.
mcDiagnosticError :: MessageClass
mcDiagnosticError = MCDiagnostic SevError ErrorWithoutFlag

-- | Make a 'MessageClass' for a given 'DiagnosticReason', without consulting the 'DynFlags'.
-- This will not respect -Werror or warning suppression and so is probably wrong
-- for any warning.
mkMCDiagnostic :: DiagnosticReason -> MessageClass
mkMCDiagnostic reason = MCDiagnostic (defaultReasonSeverity reason) reason

-- Text organized into bullets.
newtype DecoratedSDoc = Decorated { unDecorated :: [SDoc] }

data DiagnosticMessage = DiagnosticMessage
  { diagMessage :: !DecoratedSDoc
  , diagReason  :: !DiagnosticReason
  }

type WarnMsg = MsgEnvelope DiagnosticMessage  -- INVARIANT: errMsgSeverity == SevWarning
type ErrMsg  = MsgEnvelope DiagnosticMessage  -- INVARIANT: errMsgSeverity == SevError

class Diagnostic a where
  diagnosticMessage :: a -> DecoratedSDoc
  diagnosticReason  :: a -> DiagnosticReason

instance Diagnostic DiagnosticMessage where
  diagnosticMessage = diagMessage
  diagnosticReason  = diagReason

mkMsgEnvelope :: Diagnostic err => SrcSpan -> PrintUnqualified -> err -> MsgEnvelope err
mkMsgEnvelope locn print_unqual err
  = MsgEnvelope { errMsgSpan = locn
                , errMsgContext = print_unqual
                , errMsgDiagnostic = err
                , errMsgSeverity = defaultReasonSeverity (diagnosticReason err) -- wrong, but will be fixed in printOrThrowWarnings
                }
 
-- in the type-checker
data TcRnMessage
  = TcRnUnknownMessage DiagnosticMessage
  | TcRnNameShadowing ...
  | TcRnBadTelescope ...
  | ...

pprTcRnMessage :: TcRnMessage -> DecoratedSDoc
pprTcRnMessage = ...

instance Diagnostic TcRnMessage where
  diagnosticMessage = pprTcRnMessage
  diagnosticReason  = \case
    TcRnUnknownMessage msg -> diagReason msg
    TcRnNameShadowing {}   -> WarningWithFlag Opt_WNameShadowing
    TcRnBadTelescope {}    -> ErrorWithoutFlag
    ...

-- in GHC.Driver.Errors
printOrThrowWarnings = ...
  where
    is_warn_msg_fatal :: Diagnostic err => DynFlags -> WarnMsg -> Bool
    is_warn_msg_fatal dflags (MsgEnvelope { errMsgSeverity = SevError }) = panic "but this should be a warning"
    is_warn_msg_fatal dflags (MsgEnvelope { errMsgDiagnostic = d }) = case diagReason d of
      WarningWithoutFlag   -> gopt Opt_WarnIsError dflags
      WarningWithFlag flag -> wopt_fatal flag dflags
      ErrorWithoutFlag     -> panic "error classed as warning"

    promote_warning_as_error :: WarnMsg -> ErrMsg
    promote_warning_as_error msg = msg { errMsgSeverity = SevError }
```

A few details to highlight:
* `DiagnosticReason` and `Severity` are independent. A `DiagnosticReason` is a fundamental property of a message, while `Severity` also takes `DynFlags` into account.
* While a `Severity` is chosen at birth, it can be modified by `printOrThrowWarnings`. This is the temporary part of the design; ideally, we would get the severity correct at birth. In this "at birth" design, `mkMsgEnvelope` would also take a `DynFlags`.
* There is a representable impossible state: `MCDiagnostic SevWarning ErrorWithoutFlag`. This one seems hard to get rid of without resorting to fancy types. (Really, we want the severity to be greater than or equal to the default severity.)
* There is no duplication between the `MsgEnvelope` and its payload.
* `MCDiagnostic` carries enough information to be used in much the same way it is used in !5024.
* Right now, all code that generates warnings first checks to see whether the warning flag is enabled. With this proposed design, it seems straightforward to extend `Severity` with `SevIgnore` that instructs the logger to simply do nothing on a message.
* This design points out that `defaultReasonSeverity` is naughty, as it bypasses `DynFlags`. I bet there are real bugs in GHC around this, but they are hard to trigger: the warnings that bypass the normal mechanism arise in Core Lint, Stg Lint, the optimizer, and in two situations dealing with dynamic linking. (I searched for uses of `MCDiagnostic` with a hard-coded warning reason.) The warning text does not appear in the testsuite, so these code paths are completely untested. This proposed design does not fix these bugs, but it makes them more obvious by forcing the use of a function that is labeled as wrong.

# Eventual design

The design described above is a temporary waypoint. In particular, it includes `defaultReasonSeverity :: DiagReason -> Severity`, which cannot be implemented correctly, because determining the `Severity` of a `DiagReason` requires consulting the `DynFlags`. A more correct design would look like this:

```hs
diagReasonSeverity :: DynFlags -> DiagReason -> Severity
diagReasonSeverity dflags (WarningWithFlag wflag) | wopt_fatal wflag dflags     = SevError
                                                  | otherwise                   = SevWarning
diagReasonSeverity dflags WarningWithoutFlag      | gopt Opt_WarnIsError dflags = SevError
                                                  | otherwise                   = SevWarning
diagReasonSeverity _      ErrorWithoutFlag                                      = SevError

mkMsgEnvelope :: Diagnostic err => DynFlags -> SrcSpan -> PrintUnqualified -> err -> MsgEnvelope err
mkMsgEnvelope dflag slocn print_unqual err
  = MsgEnvelope { errMsgSpan = locn
                , errMsgContext = print_unqual
                , errMsgDiagnostic = err
                , errMsgSeverity = diagReasonSeverity dflags (diagnosticReason err)
                }

-- printOrThrowWarnings would no longer reclassify
```

This eventual design classifies diagnostics as error or warnings *at birth*, using the `DynFlags` in effect at the time of birth. These `DynFlags` will be correct. We cannot do this today because of interaction with `checkIfErrs` and friends (search within this page to find more discussion), but we will hopefully be able to do this soon.

Once this is done, we should hopefully be able to rid ourselves of `defaultReasonSeverity`. Exception: `defaultReasonSeverity` is correct for errors. So we might keep around some `errReasonSeverity :: DiagReason -> Severity` which always returns `SevError` after confirming that its argument is `ErrorWithoutFlag`, panicking otherwise. It's not clear whether this is a good design or a bad design.

# Suppressed warnings

This section contains thoughts on improvements to the design to support more uniform suppression of warnings that the
user does not want. Note that this *builds on* the design above. It does *not* suggest a new alternative to the design above.

**EDIT: Design A triumphed. Expand the arrow to read the original write-up.**

<details><summary> Original write up </summary> 

## Current design

Today, all code that creates a warning should check whether that warning is suppressed. Here is one representative example:

```hs
; warn_mono <- woptM Opt_WarnMonomorphism
       ; when (case infer_mode of { ApplyMR -> warn_mono; _ -> False}) $
         diagnosticTc (WarningWithFlag Opt_WarnMonomorphism)
                      (constrained_tvs `intersectsVarSet` tyCoVarsOfTypes taus)
                      mr_msg
```

This code produces a warning that the monomorphism restriction has affect types. Note that it first checks whether `Opt_WarnMonomorphism` is enabled. Then, (maybe) it creates a warning with `WarningWithFlag Opt_WarnMonomorphism`. This is strage, because the code must interact with `Opt_WarnMonomorphism` *twice*: once to see whether the warnings should be generated at all, and again to mark the flag for the warning, which other code uses to determine whether the warning should be fatal (i.e. because of `-Werror`). This is error prone (every code that generates a warning must remember to make an extra check) and duplicative.

## Proposed Design A

One possibility is to introduce a new `Severity`:

```hs
data Severity = SevIgnore | SevWarning | SevError

diagReasonSeverity :: DynFlags -> DiagReason -> Severity
diagReasonSeverity dflags (WarningWithFlag wflag) | not (wopt wflag dflags)     = SevIgnore
                                                  | wopt_fatal wflag dflags     = SevError
                                                  | otherwise                   = SevWarning
diagReasonSeverity dflags WarningWithoutFlag      | gopt Opt_WarnIsError dflags = SevError
                                                  | otherwise                   = SevWarning
diagReasonSeverity _      ErrorWithoutFlag                                      = SevError
```

`SevIgnore`d diagnostics are never printed. This is nice and uniform with the other severities and would likely be very easy to implement. However, it's a bit disappointing in that we would continue to carry around lots of `SevIgnore`d warning messages, just to let them be garbage-collected at the end of execution.

## Proposed Design B

Another possibility is to keep `data Severity = SevWarning | SevError` (as described above) but make the creation of `MsgEnvelope`s partial. To wit:

```hs
diagReasonSeverity :: DynFlags -> DiagReason -> Maybe Severity
diagReasonSeverity dflags (WarningWithFlag wflag) | not (wopt wflag dflags)     = Nothing
                                                  | wopt_fatal wflag dflags     = Just SevError
                                                  | otherwise                   = Just SevWarning
diagReasonSeverity dflags WarningWithoutFlag      | gopt Opt_WarnIsError dflags = Just SevError
                                                  | otherwise                   = Just SevWarning
diagReasonSeverity _      ErrorWithoutFlag                                      = Just SevError


mkMsgEnvelope :: Diagnostic err => DynFlags -> SrcSpan -> PrintUnqualified -> err -> Maybe (MsgEnvelope err)
mkMsgEnvelope dflags locn print_unqual err
  = do sev <- diagReasonSeverity dflags (diagnosticReason err)
       return $ MsgEnvelope { errMsgSpan = locn
                            , errMsgContext = print_unqual
                            , errMsgDiagnostic = err
                            , errMsgSeverity = sev
                            }
```

This appears to be more efficient (we more eagerly drop any ignored messages) but it requires all callers of `mkMsgEnvelope` to deal with the possibility of failure. A quick review of usage sites suggests this won't be hard (the result is often just put into a bag, and if `mkMsgEnvelope` returns `Nothing`, it's very easy to put nothing into a bag), but this design is definitely more invasive than Design A.

## Knock-on simplifications

One we execute either Design A or Design B, we can then remove the many places throughout GHC that check for a warning flag before emitting a warning. We must be slightly careful, though: it might be computationally intensive even to know whether to produce a warning, and it's foolish to work hard only to ignore the result. (Maybe sometimes laziness would save us, but not if any monadic work is involved, of course.) This computationally-intensive case is likely the exception, and so most places that might give rise to a warning can just skip the check entirely, leaving it to the error-message infrastructure above to toss out unwanted warnings.

A good way to approach this is simply to search for any occurrence of `Opt_Warn` in the code and remove extra conditionals.

## Conclusion

I (Richard) recommend design B, because I'm worried about the performance implications of Design A. Though, it might be worthwhile to implement A first (should be very very easy), then remove the conditionals, and then measure performance before going with the more-involved B.
</details>