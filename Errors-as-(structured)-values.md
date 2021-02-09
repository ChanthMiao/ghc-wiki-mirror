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

# New design

This is the new API:

```haskell

newtype Messages e = Messages (Bag (MsgEnvelope e))

data MsgEnvelope e = MsgEnvelope
   { errMsgSpan        :: SrcSpan
   , errMsgContext     :: PrintUnqualified
   , errMsgDiagnostic  :: e
   , errMsgSeverity    :: Severity
   } deriving Functor

data MessageClass
  = MCOutput
  | MCFatal
  | MCInteractive
  | MCDump
  | MCInfo
  | MCDiagnostic Severity
  deriving (Eq, Show)

data Severity
  = SevWarning !WarnReason -- ^ Born as a warning or demoted from an error.
  | SevError   !ErrReason  -- ^ Born as an error  or promoted from a warning (e.g. -Werror)
  deriving (Eq, Show)

class RenderableDiagnostic a where
  renderDiagnostic :: a -> DecoratedSDoc

newtype DecoratedSDoc = Decorated { unDecorated :: [SDoc] }

-- .. operations on messages
```

API design explanation/considerations:

* `Messages` is now a `newtype` so it can be expressed in terms of an opaque interface, and
  it's parameterised over an abstract payload `e`. It's just a bag of **envelopes**;

* The `ErrMsg` is renamed `MsgEnvelope` and has to be intended as an envelope
  that carries information about the _content_ it stores inside. We can call such
  envelope content the **diagnostic**;

* The old `Severity` type is split into two types, `MessageClass` and `Severity`. The former
  is the _class_ of the message (is this a debug message? A dump one? An information log
  emitted by GHC somewhere?) whereas the `Severity` is the severity of the **diagnostic**;

* The **diagnostic** is the _envelope content_ of a `MsgEnvelope`, and it characterises the
  particular provenance of the envelope (is this a parser error? Is this a TcRn warning?). For
  example, `MsgEnvelope PsMessage` is an envelope which was created during GHC parsing phase;

* A `MsgEnvelope` has a `Severity`, which type reflects the fluid relationship between
  warnings and errors. The `Messages` type simply collects facts about the GHC running program:
  peeking into the invividual envelope tells us:
     `a)` If this is an error or a warning;
     `b)` What does this `MsgEnvelope` carries inside its `errMsgDiagnostic`;

* There is a **fluid** relationship between warnings and errors. A warning can be turned
  into a fatal error and an error can be relaxed for example by deferring type errors. We
  can distinguish between warnings and errors by looking at each `MsgEnvelope`'s `Severity`;

* We should try to give a `MsgEnvelope` the right `Severity` "at birth", without doing dodgy
  demotions or promotions later in the codebase, at it makes much harder to track down the
  precise semantic of the diagnostic, or even trying to reconstruct the original "provenance"
  (was this a warning now turned into an error? If yes, when?);

* We render back the messages into an `SDoc` via the `RenderableMessage` type class. We
  use `renderDiagnostic` to turn the structured message into something that can be printed
  on screen by GHC when it needs to report facts about the compiled program (errors, warnings
  etc);

* A `DecoratedSDoc` is a newtype that allows us to collect a list of `SDoc` from various
  printing functions and place a bullet between each of them, so that they render nicely.

# The envelope contents (i.e. the diagnostics)

We can get each subsystem to define their own error types. At the beginning, to smooth out the integration, they can even be very simple wrappers around `DecoratedSDoc`s:


``` haskell
-- somewhere in the parser
data PsMessage = PsMessage DecoratedSDoc

-- in GHC.Tc.Monad/Types?
data TcRnMessage = TcRnMessage DecoratedSDoc

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
  GhcUnknownMessage :: forall a. (RenderableDiagnostic a, Typeable a) => a -> GhcMessage

```

With those types in place, we could begin instantiating `e` to the relevant type for all use sites of the error infrastructure. The parser could would deal with `Messages PsMessage` values, the renamer and typechecker would produce `Message TcRnMessage` values, and so on.

Finally, we could start turning concrete errors into dedicated constructors of `PsMessage`/`TcRnMessage`. Starting slowly with simple [`not in scope` errors](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Rename/Unbound.hs#L64) and the likes, before converting over [the entire typechecking error infrastructure](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Tc/Errors.hs) and more.  For example:
```
data TcRnMessage = TcRnMessage DecoratedSDoc
  | OutOfScopeErr RdrName
  | ...
```
The idea is to have one data constructor per error, so that a IDE using the GHC API would not have to parse strings to understand the errors.

This might involve systematically retaining a bit more information (context lines for the typechecker, for instance) and therefore might give rise to some more generic error infrastructure constructs. This page will be updated to incorporate such details once they are figured out.

At the "top level", in the driver, where we call the different subsystems to process Haskell modules, we would end up accumulating and reporting `GhcMessage` values. The goal is to have the GHC program emit the exact same error as it does today, but affect the API in such a way that GHC API users would at this point get a chance to work with the algebraic error descriptions, making inspection and custom treatment a whole lot easier to implement. We could perhaps even demonstrate it at this point by implementing a little "demo" of sorts for this new way to consume errors.


# Implementation plan

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

- [ ] Split the old Severity into `MessageClass` and `Severity`, and try to construct
  messages with their `Severity` set "at birth", without spurious reclassifications
  (i.e. demotions/promotions etc);

- [ ] Introduce proper diagnostic types for the different phases of 
  the compilation pipeline (i.e. `TcRnMessage`, `PsMessage` etc). Initially these
  can also contain a selection of all the GHC-emitted messages, and "filled" later, to minimise
  breakages. Introduce also an umbrella `GhcMessage` type which will be used in the
  driver, at the top level, to report diagnostics. At this stage we won't yet
  make use of any of the new types;
  **Waiting CI/review**: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4798

- [ ] Extend the parser error types to adhere to the new error-messages
  architecture, and port the codebase to use these new format of errors.

- [ ] Convert the `TcRn` error types to adhere to the new error-messages
  architecture. We will also try to make use of the new `Suggestion` API
  when reporting suggestions to users.

- [ ] Convert the `Ds` error types to adhere to the new error-messages
  architecture;

- [ ] Convert the `Driver` error types to adhere to the new error-messages
  architecture.