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

## Annotations in TTG Extension Points (Plan A)

As of May 2021, the mechanism implemented in master stores EPA annotation in TTG extension points, which we call Plan A. The alternative is to record the necessary information in the AST directly (rather than in extension points), see Plan B below. To avoid confusion, all subsections below are marked as either (A) or (B).

### Mechanism (A)

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

When performing exact-printing, the spacing between all elements is
first converted to a series of **DeltaPos**, and printing occurs based
on these delta positions.

The **DeltaPos** captures the spacing from the current print position
on the page to the position required for the thing about to be
printed.  This is either on the same line in which case is is simply
the number of spaces to emit, or it is some number of lines down, with
a given column offset.  The exact printing algorithm keeps track of
the column offset pertaining to the *current* anchor position, so the
`deltaColumn` is the additional spaces to add in this case.  The
details are presented below in **TBD**.

The **anchor_op** is used to facilitate moving an entire AST subtree
into a new location.  If it is not moved, the normal case, it will be
`UnchangedAnchor`.

If it has been moved, the actual span captured in the `anchor` field
will no longer be relevant for spacing relative to its parent context,
and the spacing is provided directly by the `DeltaPos` in the
`MovedAnchor` variant.  But its original `anchor` location remains
unchanged, as it is used as a reference for the elements inside the
local definition when printing them.

This allows us to painlessly build up new ASTs based on fragments from
anywhere, and we only need to worry about spacing where we actually
fit the new part in.

### Limitations (A)

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

Note: it may be possible to retain some of the original ordering of
declarations by using `AnnSortKey` fields in the `GhcRn` and `GhcTc`
ASTs.

### General approach (A)

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


### Data structures (A)

`AnnKeywordId`: This is a simple enumeration of all keywords in Haskell, including alphanumeric
keywords (such as `let` or `data`), alphanumeric pseudo-keywords (such as `family` or `qualified`),
symbolic keywords (such as `->` or `;`), and symbolic pseudo-keywords (such as `-<` and `!`).

`EpaComment` : Keeps an original comment together with the RealSrcSpan
of the token preceding it, for calculating the spacing when exact printing it.

`EpAnnComments` : Keeps a list of comments associated with a specific
AST element. Initially this just keeps all comments, but functions
exist in the exact printing library to split this into ones that occur
before and after the AST element, and to move them between elements,
prior to modifying the AST.  This facilitates keeping comments
attached to the corrent AST element if the element is moved.  See
`balanceComments` and `balanceCommentsList` in the check-exact
Transform module.

`AddEpAnn` : a container structure tying together an `AnnKeywordId`
with its corresponding `EpaLocation`.

An `EpaLocation` is used by the parser to store the original
`RealSrcSpan` belonging to the keyword identified by the
`AnnKeywordId` in an `AddEpAnn`.  If tools are used to modify an AST,
the `EpaLocation` can alternatively store a `DeltaPos` directly.

### Anchor and EpaLocation (A)

At first blush there seems to be overlap between the `Anchor` and
`EpaLocation` types, and one of them could be redundant.

```haskell
data EpaLocation = EpaSpan RealSrcSpan
                 | EpaDelta DeltaPos

data Anchor = Anchor { anchor :: RealSrcSpan, anchor_op :: AnchorOperation }
data AnchorOperation = UnchangedAnchor
                     | MovedAnchor DeltaPos
```

The difference is that `EpaLocation` is only used for calculating a
`DeltaPos` for a given item in an AST element.  As such it needs only
provide the original `RealSrcSpan` for calculating it from the prior
position, or the `Deltapos` directly.

An `Anchor` is used both to calculate the `DeltaPos` of the AST
fragment when printing its containing element, and also as a basis for
calculating the elements within it.  So the original `RealSrcSpan` is
always required for an `Anchor`, and it optional for the
`EpaLocation`.

**AZ** once the dust settles, I must check in the exact print
algorithm that this is in fact necessary. It is used this way at
present, but perhaps we can make things work with `EpaLocation` only.

**AZ** 2021-04-27 up to here.

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


## Token information in the syntax tree (Plan B)

[#19623](https://gitlab.haskell.org/ghc/ghc/-/issues/19623) proposes a more direct encoding of the information necessary for exact printing.

The idea is to introduce a new data type, `HsToken`, defined as follows:

```
type LHsToken tok p = XRec p (HsToken tok)

data HsToken (tok :: Symbol) = HsTok
```

Then we record token information directly in the syntax tree:

```diff
  data HsExpr p
    = ...
      ...
    | HsLet       (XLet p)
+                 (XRec p (LHsToken "let"))
                  (LHsLocalBinds p)
+                 (XRec p (LHsToken "in"))
                  (LHsExpr  p)
```

One might argue that this makes our AST less abstract, so it’s actually a concrete syntax tree. But we already retain certain information uncharacteristic of a proper AST, such as parentheses (with `HsPar`), so adding token information seems also appropriate.

### Comparison between A and B

* Plan A, using TTG extension fields:

  ```
  -- In the shared code base, Language.Haskell.Syntax.Expr
     data HsExpr p = HsLet (XLet p) (HsLocalBinds p) (HsExpr p) | ...

  -- In the GHC-specific code base, GHC.Hs.Expr
     type instance XLet (GHC p) = LetKeywords
     data LetKeywords = LK { lkLet :: AnnAnchor, lkIn :: AnnAnchor }
  ```

* Plan B, directly storing info in the syntax tree:

  ```
  -- In the shared code base, Language.Haskell.Syntax.Expr
     data HsExpr p = HsLet (XLet p)
                           (LHsToken "let" p) (HsLocalBinds p)
                           (LHsToken "in" p)  (HsExpr p)
                   | ...
  ```

  Here the "let" parameter to LHsToken is entirely phantom, present mostly for documentation reasons.

Let's see the pros and cons (Simon’s analysis):

* The big change is that the tokens become part of the (client-independent) syntax tree, in 
  Language.Haskell.Syntax.

* With Plan A, clients can completely ignore all the exact-print
  stuff.  With Plan B they have to handle those fields, if only to
  pass them on.

* With Plan B we can do exact-print in an almost GHC-free way,
  provided only that we can extract enough info from the `XRec` thing.
  With Plan A, exact-print is deeply tied to GHC.

* Plan B has many, many fewer data types.  And interspersing the
  tokens in the "right" place in the record (e.g. "let" then binds
  then "in" then expression) is very perspicuous, much more so than a
  `lkLet` field in `LetKeywords`.

* Many GHC passes use those extension fields.  With Plan A GHC will need
  to pair up that pass-specific info with the LetKeywords stuff.  Possible,
  but pretty tiresome.

### Dealing with UnicodeSyntax (B)

How will this approach deal with tokens that have unicode variants? Define `HsUniToken` as follows:

```
data HsUniToken (tok :: Symbol) (utok :: Symbol) = HsUTok IsUnicodeSyntax
```

and then e.g. `HsUTok u :: HsUniToken "->" "→"`, where `u` is either `UnicodeSyntax` or `NormalSyntax`.





# (Old) This is a description of the API Annotations introduced with GHC 7.10 RC2

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
