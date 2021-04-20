# In-tree exact-printing annotations

This section describes a new (in 2021) design for tracking enough information in order 
to replicate the original source from a parsed AST.

**This is currently just a stub, written by Richard E. as a place to fill out by Alan Z. and others.**
Sections delimited by "RAE" are questions that should be answered in the final version of this design.
Once such a question is answered, please delete the question.

## What are "API annotations" and "exact-printing"?

Taking an AST tree and converting it into a string that looks like what the user originally wrote
is called *exact-printing*. An exact-printed program includes the original spacing and all comments.

We use the term *Exact-Printing Annotations* (EPAs) to refer to the extra bits of information included in the AST
only for the purposes of exact-printing.  For example:
* An if-then-else node in the syntax tree will include the precise location of the `if`, `then`, and `else` keywords
* An list-comprehension node such as `[ f x | x <- xs, x > 3 ]` will include the locations of the `[`, `|`, and `]` punctuation; its statements will include the location of the comma separators.
* Every node will contain the locations and content of "nearby" comments (see XXX for what "nearby" means)
In total, every visible part of the original program will be represented somewhere in the AST.

By definition, exact-printing annotations are used only for exact-printing; they are not otherwise consulted during compilation.

## Goals

The reason to include exact-printing annotations is to perform
exact-printing, so that a tool can render a program exactly as the
user wrote it, comments and all, from just the AST.

This becomes particularly useful because it should be possible to
preserve much of this layout information across refactorings of the
AST.  For example, a tool might want to float out a local definition
from a `where` clause to become a top-level definition. It should be
possible to do this without disrupting the user's stylistic choices
and comments.

## Mechanism

Having the original locations of everything in the original source
code is necessary for exact-printing the original, but is not
sufficient for exact-printing an AST that has been manipulated as part
of a refactoring.

The key to enabling this is to realise that exact-printing is about
calculating the amount of space *between* things, and printing that
space when reproducing the source. The EPAs give us the original
locations, so we can easily calculate spacing from these.

The exact-printing model works on the basis that if we can position
the "top-left" of a given AST element somewhere on the page, we can
use the stored original locations to reproduce the original relative
spacing between the elements, and so print it with the "original"
spacing, just in a different location.

So if we float out the local definition referred to above, we will use
its original location as the "top-left" when printing out its
definition, but need to have a way to capture that the "top-left" is
now in a different place, from the perspective of the top-level AST.

The location of this "top-left" position is stored in an **Anchor**.

```haskell
data Anchor = Anchor { anchor :: RealSrcSpan
                     , anchor_op :: AnchorOperation }
data AnchorOperation = UnchangedAnchor
                     | MovedAnchor DeltaPos
data DeltaPos
  = SameLine { deltaColumn :: !Int }
  | DifferentLine
      { deltaLine   :: !Int, deltaColumn :: !Int }
```

An **Anchor** is in fact used in two different ways

* Firstly, it provides the reference point for the anchored item
  relative to its superior context.

  So for the code fragment

     `where x = 1`

  The **Anchor** belonging to `x = 1` would be used to calculate the
  spacing from the `where` keyword to where the local definition starts.

* Secondly, it serves as a reference point for the parts *inside* `x =
  1`.  So the distance to the `=` will be based on it.

The **DeltaPos** is what is actually used for the final exact printing
step. It captures the spacing from the current print position on the
page, to the position required for the thing about to be printed.
This is either on the same line in which case is is simply the number
of spaces to emit, or it is some number of lines down, with a given
column offset.  The exact printing algorithm keeps track of the column
offset pertaining to the anchor position, so the `deltaColumn` is the
additional spaces to add in this case.  The details are presented
below in **TBD**.

The **anchor_op** is used to facilitate moving an entire AST subtree
into a new location.  If it is not moved, the normal case, it will be
`UnchangedAnchor`.

If it has been moved, the actual span captured in the `anchor` field
will no longer be relevant for spacing relative to its context, and
the spacing is provided directly by the `DeltaPos` in the
`MovedAnchor` variant.  But its original `anchor` location
remains unchanged, as it is used as a reference for the elements
inside the local definition when printing them.

This allows us to painlessly build up new ASTs based on fragments from
anywhere, and we only need to worry about spacing where we actually
fit the new part in.

## Limitations

The exact printing approach described here is fine-tuned to the
`GhcPs` AST, and is not expected to work for the `GhcRn` or `GhcTc`
variants.

This is because there is information loss as the AST is transformed by
the renamer and typechecker, meaning it cannot be supported without
additional engineering work to track the lost information.

In practical usage, the main requirement is to be able to look up the
`Name` and less often the `Id` equivalent of a given `RdrName` in the
`GhcPs` AST.  This is facilitated by ensuring that every `RdrName` in
the `GhcPs` occurs with its own `SrcSpan`, which can be used to tie it
up to its matching version in the `GhcRn` AST, and hence ascertain the
corresponding `Name`.

## General approach

The general approach is to store extra information in the
Trees-That-Grow extension points to each constructor. This extra
information includes locations of any keywords used in a construct.
For example, in a `HsLet`, we need to store the location of the `let`
and the `in`.

The general pattern is this:

The base AST is captured in the `Language.Haskell.Syntax` hierarchy.

```haskell
module Language.Haskell.Syntax.Expr where
  -- The client-independent syntax tree
  data HsExpr p = ...
    | HsLet       (XLet p)           -- The extension field
                  (HsLocalBinds p)
                  (LHsExpr  p)
```

The usage of the TTG extension field (`XLet`) in this example is
specified in the `GHC.Hs` hierarchy.


```haskell
module GHC.Hs.Expr where
  -- The GHC specific stuff about HsExpr
  type instance XLet GhcPs = EpAnn AnnsLet
  type instance XLet GhcRn = ...
  type instance XLet GHcTc = ...

  data AnnsLet = AnnsLet { alLet :: EpaLocation, alIn :: EpaLocation }
```

The `AnnsLet` structure is specific to the `HsLet` constructor, but
`EpAnn` is a general purpose structure to manage the exact print
annotations for a given AST element.

```haskell
module GHC.Parser.Annotation where
  -- Shared data types relating to exact print annotations
data EpAnn ann
  = EpAnn { entry   :: Anchor
          , anns     :: ann
          , comments :: EpAnnComments
          }
  | EpAnnNotUsed
```

Here you can see

* Every extension field uses `EpAnn'` to store stuff that every node
  has in common: an `Anchor` and comments.
* The `AnnsLet` data type records the locations of the `let` and `in`
  keywords for `HsLet`.  There is one such data type for each
  constructor.

* **Comments**.  Because we must exact-print with comments intact, we
  track all comments. Non-toplevel comments are associated with the
  innermost enclosing AST node -- that is, the one whose `SrcSpan` is
  smallest, yet includes the comment.  Top level ones are associated
  by the parser with the immediately following top level
  declaration. Details of the `EpAnnComments` structure and usage are
  provided below.

***AZ stopping now 2021-04-20*** will carry on tomorrow.

## Data structures

`AnnKeywordId`: This is a simple enumeration of all keywords in Haskell, including alphanumeric
keywords (such as `let` or `data`), alphanumeric pseudo-keywords (such as `family` or `qualified`),
symbolic keywords (such as `->` or `;`), and symbolic pseudo-keywords (such as `-<` and `!`).

---------------------------

```haskell
data EpaComment = EpaComment { ac_tok :: EpaCommentTok
                             , ac_prior_tok :: RealSrcSpan
                             }

data EpaCommentTok =
  -- Documentation annotations
    EpaDocCommentNext  String     -- ^ something beginning '-- |'
  | EpaDocCommentPrev  String     -- ^ something beginning '-- ^'
  | EpaDocCommentNamed String     -- ^ something beginning '-- $'
  | EpaDocSection      Int String -- ^ a section heading
  | EpaDocOptions      String     -- ^ doc options (prune, ignore-exports, etc)
  | EpaLineComment     String     -- ^ comment starting by "--"
  | EpaBlockComment    String     -- ^ comment in {- -}
  | EpaEofComment                 -- ^ empty comment, capturing
                                  -- location of EOF

-- | When we are parsing we add comments that belong a particular AST
-- element, and print them together with the element, interleaving
-- them into the output stream.  But when editin the AST, to move
-- fragments around, it is useful to be able to first separate the
-- comments into those occuring before the AST element and those
-- following it.  The 'EpaCommentsBalanced' constructor is used to do
-- this. The GHC parser will only insert the 'EpaComments' form.
data EpAnnComments = EpaComments
                        { priorComments :: ![LEpaComment] }
                    | EpaCommentsBalanced
                        { priorComments :: ![LEpaComment]
                        , followingComments :: ![LEpaComment] }
        deriving (Data, Eq)

type LEpaComment = GenLocated Anchor EpaComment
```

This stores a comment, differentiating between the different comment styles.
**RAE** what's up with `AnnEofComment`? **End RAE**
**AZ** : `AnnEofComment` is used to keep track of the actual end of the file, so that if there are blank lines at the end we can reproduce them when printing.

**RAE** Why do we need `ac_prior_tok`? The comment is not helpful: *everything* here is about exact-printing. **End RAE**
**AZ**: We need to calculate a `DeltaPos` between every piece of output when printing.  It is not always clear what the spacing is before a comment, so the lexer now emits the prior token location as well with a comment, so we can calculate this.

**RAE** Why does `LAnnotationComment` get an `Anchor` not a `SrcSpan`? **End RAE**
**AZ** Everything that can be moved (which is everything) gets an `Anchor`.

----------------------------------

```hs
-- | Captures an annotation, storing the @'AnnKeywordId'@ and its
-- location.  The parser only ever inserts @'EpaLocation'@ fields with a
-- RealSrcSpan being the original location of the annotation in the
-- source file.
-- The @'EpaLocation'@ can also store a delta position if the AST has been
-- modified and needs to be pretty printed again.
-- The usual way an 'AddApiAnn' is created is using the 'mj' ("make
-- jump") function, and then it can be inserted into the appropriate
-- annotation.
data AddApiAnn = AddApiAnn AnnKeywordId EpaLocation
```

-------------------------------------

```hs
-- | The anchor for an @'AnnKeywordId'@. The Parser inserts the @'AR'@
-- variant, giving the exact location of the original item in the
-- parsed source.  This can be replace by the @'AD'@ version, to
-- provide a position for the item relative to the end of the previous
-- item in the source.  This is useful when editing an AST prior to
-- exact printing the changed one.
data EpaLocation = AR RealSrcSpan
               | AD DeltaPos

-- | Relative position, line then column.  If 'deltaLine' is zero then
-- 'deltaColumn' gives the number of spaces between the end of the
-- preceding output element and the start of the one this is attached
-- to, on the same line.  If 'deltaLine' is > 0, then it is the number
-- of lines to advance, and 'deltaColumn' is the start column on the
-- new line.
data DeltaPos =
  DP
    { deltaLine   :: !Int,
      deltaColumn :: !Int
    } deriving (Show,Eq,Ord,Data)

-- | An 'Anchor' records the base location for the start of the
-- syntactic element holding the annotations, and is used as the point
-- of reference for calculating delta positions for contained
-- annotations.  If an AST element is moved or deleted, the original
-- location is also tracked, for printing the source without gaps.
data Anchor = Anchor        { anchor :: RealSrcSpan
                                 -- ^ Base location for the start of
                                 -- the syntactic element holding
                                 -- the annotations.
                            , anchor_op :: AnchorOperation }
        deriving (Data, Eq, Show)

-- | If tools modify the parsed source, the 'MovedAnchor' variant can
-- directly provide the spacing for this item relative to the previous
-- one when printing. This allows AST fragments with a particular
-- anchor to be freely moved, without worrying about recalculating the
-- appropriate anchor span.
data AnchorOperation = UnchangedAnchor
                     | MovedAnchor DeltaPos
        deriving (Data, Eq, Show)
```

**RAE** As I commented on the MR, I find `AR` and `AD` shorter than necessary, and I think `DeltaPos` would be better with two constructors. **End RAE**
**RAE** Why do we need both of these types? How are they different? **End RAE**
**AZ**: I am not *sure* that we do. I do know that as explained in **Mechanism** above we need the anchor to have a `RealSrcSpan` and sometimes a `DeltaPos`.  An `EpaLocation` only needs to provide the one or the other. But perhaps we can come up with a way of harmonising this.

----------------------------------------

```hs
data ApiAnn' ann
  = ApiAnn { entry   :: Anchor
           -- ^ Base location for the start of the syntactic element
           -- holding the annotations.
           , anns     :: ann -- ^ Annotations added by the Parser
           , comments :: ApiAnnComments
              -- ^ Comments enclosed in the SrcSpan of the element
              -- this `ApiAnn'` is attached to
           }
  | ApiAnnNotUsed -- ^ No Annotation for generated code,
                  -- e.g. from TH, deriving, etc.

-- | This type is the most direct mapping of the previous API
-- Annotations model. It captures the containing `SrcSpan' in its
-- `entry` `Anchor`, has a list of `AddApiAnn` as before, and keeps
-- track of the comments associated with the anchor.
type ApiAnn = ApiAnn' [AddApiAnn]
```

This is the heart of this design: an `ApiAnn'` stores the `Anchor` for an AST node, along with any contained comments.
In addition, the `anns` field stores the locations for any keywords (like `let` and `in`) associated with an AST
node. **RAE** Some AST nodes (e.g. `HsLet`) get custom data structures for `ann`. Some (e.g. `LazyPat`) get `[AddApiAnn]`. Why
the difference? What's the guiding principle? **End RAE**
**AZ**: The intention is that each gets a custom structure, but this was a big task and I was focusing on making sure the overall approach actually works. Going forward, @int_index has proposed a slightly different mechanism that may make this moot in time.
**RAE** I don't think it's helpful having a reference to the previous model; that will get stale quickly. **End RAE**

-------------------------------------------------

```hs
-- | The 'SrcSpanAnn\'' type wraps a normal 'SrcSpan', together with
-- an extra annotation type. This is mapped to a specific `GenLocated`
-- usage in the AST through the `XRec` and `Anno` type families.
data SrcSpanAnn' a = SrcSpanAnn { ann :: a, locA :: SrcSpan }
        deriving (Data, Eq)

-- | We mostly use 'SrcSpanAnn\'' with an 'ApiAnn\''
type SrcAnn ann = SrcSpanAnn' (ApiAnn' ann)

type LocatedA = GenLocated SrcSpanAnnA
type LocatedN = GenLocated SrcSpanAnnN

type LocatedL = GenLocated SrcSpanAnnL
type LocatedP = GenLocated SrcSpanAnnP
type LocatedC = GenLocated SrcSpanAnnC

type SrcSpanAnnA = SrcAnn AnnListItem
type SrcSpanAnnN = SrcAnn NameAnn

type SrcSpanAnnL = SrcAnn AnnList
type SrcSpanAnnP = SrcAnn AnnPragma
type SrcSpanAnnC = SrcAnn AnnContext

-- | General representation of a 'GenLocated' type carrying a
-- parameterised annotation type.
type LocatedAn an = GenLocated (SrcAnn an)
```

**RAE** I'm completely lost here, even after reading Note [XRec and Anno in the AST]. I need some examples 
of concrete user-written syntax that would necessitate this design. **End RAE**

---------------------------------------------------------

```hs
-- | Captures the location of punctuation occuring between items,
-- normally in a list.  It is captured as a trailing annotation.
data TrailingAnn
  = AddSemiAnn EpaLocation    -- ^ Trailing ';'
  | AddCommaAnn EpaLocation   -- ^ Trailing ','
  | AddVbarAnn EpaLocation    -- ^ Trailing '|'
  | AddRarrowAnn EpaLocation  -- ^ Trailing '->'
  | AddRarrowAnnU EpaLocation -- ^ Trailing '->', unicode variant

-- | Annotation for items appearing in a list. They can have one or
-- more trailing punctuations items, such as commas or semicolons.
data AnnListItem
  = AnnListItem {
      lann_trailing  :: [TrailingAnn]
      }
```

**RAE** What is "trailing" about this bit? **End RAE**
**AZ** when we have things in a list requiring punctuation, the punctuation goes here. so in
```hs
let { x = 1; y = 2 } in x+y
```
the `;` would be captured in a `TrailingAnn`.
Similarly, in
```hs
data Foo = A | B | C
```
the `A` and `B` constructors would have `|` in a `TrailingAnn`.  If we moved the constructor elsewhere, we sould discard the trailing annotations, or add new ones in the new location.

**AZ**: stopping now, will carry on tomorrow (now 2021-03-15).

---------------------------------

**RAE** There follows a number of data structures that look like they are engineered for specific use cases (e.g. lists, contexts). Why are they in Annotation.hs instead of closer to their usage sites?

---------------------------------

```hs
-- | API Annotations for a 'RdrName'.  There are many kinds of
-- adornment that can be attached to a given 'RdrName'. This type
-- captures them, as detailed on the individual constructors.
data NameAnn
  -- | Used for a name with an adornment, so '`foo`', '(bar)'
  = NameAnn {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_name      :: EpaLocation,
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @(,,,)@, or @(#,,,#)#
  | NameAnnCommas {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_commas    :: [EpaLocation],
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @()@, @(##)@, @[]@
  | NameAnnOnly {
      nann_adornment :: NameAdornment,
      nann_open      :: EpaLocation,
      nann_close     :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for @->@, as an identifier
  | NameAnnRArrow {
      nann_name      :: EpaLocation,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used for an item with a leading @'@. The annotation for
  -- unquoted item is stored in 'nann_quoted'.
  | NameAnnQuote {
      nann_quote     :: EpaLocation,
      nann_quoted    :: SrcSpanAnnN,
      nann_trailing  :: [TrailingAnn]
      }
  -- | Used when adding a 'TrailingAnn' to an existing 'LocatedN'
  -- which has no Api Annotation (via the 'ApiAnnNotUsed' constructor.
  | NameAnnTrailing {
      nann_trailing  :: [TrailingAnn]
      }

-- | A 'NameAnn' can capture the locations of surrounding adornments,
-- such as parens or backquotes. This data type identifies what
-- particular pair are being used.
data NameAdornment
  = NameParens -- ^ '(' ')'
  | NameParensHash -- ^ '(#' '#)'
  | NameBackquotes -- ^ '`'
  | NameSquare -- ^ '[' ']'
```

**RAE**
* What is `(bar)` in the example of `NameAnn`? Did you mean `(+)`?
* What if a name doesn't have an open/close component? What do those fields of `NameAnn` contain?
* What is `nann_trailing` doing here?
* Why is `->` special? And isn't it spelled `(->)` when used as an identifier?
* `NameAdornment` always seems too big. That is, I don't think there's an occurrence that could use all four of its constructors.
** End RAE **

# This is a decription of the API Annotations introduced with GHC 7.10 RC2


The hsSyn AST does not directly capture the locations of certain keywords and
punctuation, such as 'let', 'in', 'do', etc.


These locations are required by any tools wanting to parse a haskell
file, transform the AST in some way, and then regenerate the original
layout for the unchaged parts.


Rather than pollute the AST with information irrelevant to the actual
compilation process, these locations are captured in the lexer /
parser and returned as a separate `ApiAnns` structure in the
`ParsedSource`.


This keeps both the keyword location and the original comments with their
locations.


```
type ApiAnns = ( Map.Map ApiAnnKey [SrcSpan]
               , Map.Map SrcSpan [Located AnnotationComment])

type ApiAnnKey = (SrcSpan,AnnKeywordId)
```


Each AST element with annotations has an entries in this Map. The key comprises the SrcSpan of the original element and the AnnKeywordId of the stored annotation. The value is a list of SrcSpans where that particular keyword appears. This is a list to cater for e.g. ";;;;". Top level elements are captured against 'nullSrcSpan'.



So for the source file "examples/Test.hs" having contents


```
-- |Comment not in a SrcSpan
foo x = -- Compute foo
  let a = 4 -- using a
  in a + x
```


The returned ApiAnns are


```
([((examples/Test.hs:(2,1)-(4,10), AnnEqual), [examples/Test.hs:2:7]),
  ((examples/Test.hs:(2,1)-(4,10), AnnFunId), [examples/Test.hs:2:1-3]),
  ((examples/Test.hs:(2,1)-(4,10), AnnSemi),  [examples/Test.hs:5:1]),
  ((examples/Test.hs:(3,3)-(4,10), AnnIn),    [examples/Test.hs:4:3-4]),
  ((examples/Test.hs:(3,3)-(4,10), AnnLet),   [examples/Test.hs:3:3-5]),
  ((examples/Test.hs:3:7-11,       AnnEqual), [examples/Test.hs:3:9]),
  ((examples/Test.hs:3:7-11,       AnnFunId), [examples/Test.hs:3:7]),
  ((examples/Test.hs:4:6-10,       AnnVal),   [examples/Test.hs:4:8]),
  ((<no location info>, AnnEofPos),           [examples/Test.hs:5:1])],

 [(examples/Test.hs:(2,1)-(4,10),
   [AnnLineComment "-- Compute foo"]),
  (examples/Test.hs:(3,3)-(4,10), [AnnLineComment "-- using a"]),
  (<no location info>,
   [AnnLineComment "-- |Comment not in a SrcSpan"])])
```


This allows the annotation to be retrieved by


```
-- | Retrieve a list of annotation 'SrcSpan's based on the 'SrcSpan'
-- of the annotated AST element, and the known type of the annotation.
getAnnotation :: ApiAnns -> SrcSpan -> AnnKeywordId -> [SrcSpan]
getAnnotation (anns,_) span ann
   = case Map.lookup (span,ann) anns of
       Nothing -> []
       Just ss -> ss
```

### Annotation structures



Each annotation is simply a `SrcSpan`.


```
-- | Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
data Ann = AnnAs
         | AnnAt
         | AnnBang
         | AnnBy
         | AnnCase
         | AnnClass
         | AnnClose -- ^ } or ] or ) or #) etc
         | AnnColon
         | AnnColon2
         ..
```


Points to note:

1. `AnnOpen` / `AnnClose` capture all bracketed structures.

1. Where a value is being captured via e.g. `getINTEGER` the annotation index is called `AnnVal`.

### Capturing in the parser



The annotations are captured in the lexer / parser by extending `PState` to include a field


```
data PState = PState {
       ...
        annotations :: [(ApiAnnKey,SrcSpan)]
       }
```


The lexer exposes a helper function to add an annotation


```
addAnnotation :: SrcSpan -> Ann -> SrcSpan -> P ()
addAnnotation l a v = P $ \s -> POk s {
  annotations = ((AK l a), v) : annotations s
  } ()
```


The parser also has some helper functions of the form


```
type MaybeAnn = Maybe (SrcSpan -> P ())

gl = getLoc
gj x = Just (gl x)

aa :: Located a -> (Ann,Located c) -> P (Located a)
aa a@(L l _) (b,s) = addAnnotation l b (gl s) >> return a

ams :: Located a -> [MaybeAnn] -> P (Located a)
ams a@(L l _) bs = (mapM_ (\a -> a l) $ catMaybes bs) >> return a
```


This allows the annotations to be added in the parser productions as follows


```
ctypedoc :: { LHsType RdrName }
        : 'forall' tv_bndrs '.' ctypedoc {% hintExplicitForall (getLoc $1) >>
                                            ams (LL $ mkExplicitHsForAllTy $2 (noLoc []) $4)
                                                [mj AnnForall $1,mj AnnDot $3] }
        | context '=>' ctypedoc         {% ams (LL $ mkQualifiedHsForAllTy   $1 $3)
                                               [mj AnnDarrow $2] }
        | ipvar '::' type               {% ams (LL (HsIParamTy (unLoc $1) $3))
                                               [mj AnnDcolon $2] }
        | typedoc                       { $1 }
```

### Parse result


```
data HsParsedModule = HsParsedModule {
    hpm_module    :: Located (HsModule RdrName),
    hpm_src_files :: [FilePath],
       -- ^ extra source files (e.g. from #includes).  The lexer collects
       -- these from '# <file> <line>' pragmas, which the C preprocessor
       -- leaves behind.  These files and their timestamps are stored in
       -- the .hi file, so that we can force recompilation if any of
       -- them change (#3589)
    hpm_annotations :: ApiAnns
  }

-- | The result of successful parsing.
data ParsedModule =
  ParsedModule { pm_mod_summary   :: ModSummary
               , pm_parsed_source :: ParsedSource
               , pm_extra_src_files :: [FilePath]
               , pm_annotations :: ApiAnns }
```

## Notes / Shortcomings



Currently the annotations are only guaranteed to apply to the `ParsedSource`, the renaming process flattens some of the location hooks for


```
data HsType name
  ...
  | HsRecTy [Located [ConDeclField name]] -- Only in data type declarations
  ...

-- and

    HsDataDefn { 
    ...
                 dd_cons   :: [Located [LConDecl name]],
                     -- ^ Data constructors
   ...
```

# Comments


Comments are returned as annotations, if enabled by the existing `Opt_KeepRawTokenStream` flag.


There is an additional structure to the `PState` for comments indexed by `SrcSpan`, and also a queue of pending comments.


The basic mechanism is for `lexToken` in `Lexer.x` to accumulate the comments in the queue as parsing progressed, and when the helper function `addComments` is called with a `SrcSpan` argument it moves any comments in the queue that fit within the `SrcSpan` into the comment annotation structure indexed by the given `SrcSpan`.


These are then available for retrieval at the and, as with the existing annotations.

## Notes on comments

1. In practical terms, the usage of comments by tools requires them to be attached in some way to the logical units to which they apply. A case in point is comments preceding a function. In general, this is a hard problem, which is attacked via heuristics in `hindent` and `HaRe`.  This comment allocation would aim for a mechanistic allocation to the lowest enclosing `SrcSpan`, as a start.

1. It is possible to put the lexer into `Opt_Haddock` mode, a flag which is currently unset when `Opt_KeepRawTokenStream` is enabled. If these were made inclusive, it would be possible to explicitly tag the comments as being of type haddock, so that at a future date the annotations could possibly be used directly by haddock, rather than the very complicated parsing rules currently in place.
