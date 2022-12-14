
The completed feature is described at [ApiAnnotations](api-annotations). This page now captures the discussion leading up to the implementation.

# This is a proposal / discussion page for adding annotations to the AST, for ticket #9628


Right now the locations of syntactic markers such as `do`/`let`/`where`/`in`/`of` in the source are discarded from the AST, although they are retained in the rich token stream.


The `haskell-src-exts` package deals with this by means of using the [\`SrcSpanInfo\`](http://hackage.haskell.org/package/haskell-src-exts-1.15.0.1/docs/Language-Haskell-Exts-SrcLoc.html#t:SrcSpanInfo) data type which contains the SrcSpan as per the current GHC Located type but also has a list of `SrcSpan`s for the  syntactic markers, depending on the particular AST fragment being annotated.


The motivation for this change is then

1. Simplify the roundtripping and modification of source by explicitly capturing the missing location information for the syntactic markers.

1. Allow simple round-tripping of code via a GHC variant of [hindent](https://hackage.haskell.org/package/hindent)

1. Allow tools such as a future [hlint](https://hackage.haskell.org/package/hlint) and [ HaRe](https://hackage.haskell.org/package/HaRe) to make changes to the AST and then return the source unchanged except for the intended changes.

# Current design

## Current implementation


Note: This implementation has been split apart


The first part covers the AST changes only [D426](https://phabricator.haskell.org/D426)


The second covers the addition of annotations [D438](https://phabricator.haskell.org/D438).


Also, [D412](https://phabricator.haskell.org/D412) adds the original source text to literals that are converted during parsing.


The original proof of concept was [D297](https://phabricator.haskell.org/D297)

### Theory of operation.


The HsSyn AST does not capture the locations of certain keywords and
punctuation, such as 'let', 'in', 'do', etc.


These locations are required by any tools wanting to parse a haskell
file, transform the AST in some way, and then regenerate the original
layout for the unchaged parts.


Rather than pollute the AST with information irrelevant to the actual
compilation process, these locations are captured in the lexer /
parser and returned as a separate structure ApiAnns structure in the
ParsedSource.


Each AST element that needs an annotation has an entry in this Map,
which as a key comprising the SrcSpan of the original element and the
TyeRep of the stored annotation, if it were wrapped in a Just.



This allows code using the annotation to access this as follows


```
processHsLet :: ApiAnns -> LHsExpr -> CustomReturnType
processHsLet anns (L l (HsExpr localBinds expr)) = r
  where
    Just letPos = getAnnotation anns l AnnLet
    Just inPos  = getAnnotation anns l AnnIn
    ...
```


Key data structures


```
type ApiAnns = Map.Map ApiAnnKey SrcSpan

data ApiAnnKey = AK SrcSpan Ann
                  deriving (Eq,Ord,Show)
```


This allows the annotation to be retrieved by


```
-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotation :: ApiAnns -> SrcSpan -> Ann -> Maybe SrcSpan
getAnnotation anns span ann = Map.lookup (AK span ann) anns
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

1. In the few places where the same annotation type can repeat, multiple indices are provided, hence `AnnColon` and `AnnColon2`.

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

### Implications


This approach has minimal implications on the rest of GHC, except that some AST elements will require to be `Located` to enable the annotation to be looked up.


Also, initial `./validate` tests show that haddock complains of increased memory usage, due to the extra information being captured in the AST. If this becomes a major problem a flag could be introduced when invoking the parser as to whether to actually capture the annotations or not.

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

# Possible Extension for Comments


A possible extension to the above annotations would be to add comments as annotations, if enabled by the existing `Opt_KeepRawTokenStream` flag.


This would add an additional structure to the `PState` for comments indexed by `SrcSpan`, and also a queue of pending comments.


The basic mechanism would be to modify `lexToken` in `Lexer.x` to accumulate the comments in the queue as parsing progressed, and when the helper function `addComments` is called with a `SrcSpan` argument it moves any comments in the queue that fit within the `SrcSpan` into the comment annotation structure indexed by the given `SrcSpan`.


These are then available for retrieval at the and, as with the existing annotations.

## Notes on comments

1. In practical terms, the usage of comments by tools requires them to be attached in some way to the logical units to which they apply. A case in point is comments preceding a function. In general, this is a hard problem, which is attacked via heuristics in `hindent` and `HaRe`.  This comment allocation would aim for a mechanistic allocation to the lowest enclosing `SrcSpan`, as a start.

1. It is possible to put the lexer into `Opt_Haddock` mode, a flag which is currently unset when `Opt_KeepRawTokenStream` is enabled. If these were made inclusive, it would be possible to explicitly tag the comments as being of type haddock, so that at a future date the annotations could possibly be used directly by haddock, rather than the very complicated parsing rules currently in place.


 


# Early design discussion


## Richard Eisenberg response



For what it's worth, my thought is not to use SrcSpanInfo (which, to me, is the wrong way to slice the abstraction) but instead to add SrcSpan fields to the relevant nodes. For example:


```

  | HsDo        SrcSpan              -- of the word "do"
                BlockSrcSpans
                (HsStmtContext Name) -- The parameterisation is unimportant
                                     -- because in this context we never use
                                     -- the PatGuard or ParStmt variant
                [ExprLStmt id]       -- "do":one or more stmts
                PostTcType           -- Type of the whole expression

...

data BlockSrcSpans = LayoutBlock Int  -- the parameter is the indentation level
                                 ...  -- stuff to track the appearance of any semicolons
                   | BracesBlock ...  -- stuff to track the braces and semicolons
```


The way I understand it, the SrcSpanInfo proposal means that we would have lots of empty SrcSpanInfos, no? Most interior nodes don't need one, I think.



Popping up a level, I do support the idea of including this info in the AST.


## SPJ response to concern re extra noise in AST


>
>
> I thiink the key question is whether it is acceptable to sprinkle this kind of information throughout the AST. For someone interested in source-to-source conversions (like me) this is great, others may find it intrusive.
>
>


It???s probably not too bad if you use record syntax; thus


```
  | HsDo  { hsdo_do_loc :: SrcSpan              -- of the word "do"
          , hsdo_blocks :: BlockSrcSpans
          , hsdo_ctxt   :: HsStmtContext Name
          , hsdo_stmts  :: [ExprLStmt id]
          , hsdo_type    :: PostTcType }

```

## Other issues


The AST is initially created by the parser, and then changed through the renamer and type checker.


From a source to source conversion perspective the `ParsedSource` is closest to the original source code, as it respects the original linear ordering of the declarations, which are each wrapped in an appropriate constructor from `HsDecl`.


The `RenamedSource` gathers all the like declarations together, and strips out the `HsDecl`, as well as re-ordering binds to appear in dependency order.


The `TypeCheckedSource` further changes the `RenamedSource` to replace the original type information with the calculated types.


So manipulations need to happen at the `ParsedSource` level, but with the ability to query information from the `RenamedSource` or `TypecheckedSource` as required.



At the moment HaRe manages this by building up a token tree indexed by SrcSpan with tokens at the leaves, constructed from the `ParsedSource`, and then indexes into it for changes based on the `RenamedSource`.  The token tree is fiddly and brittle, so it would be better to keep this information directy in the AST.


## Abortive annotation parameter attempt



[D246](https://phabricator.haskell.org/D246) captures an attempt to work through a type parameter. This exploded in complexity, and was abandoned.


## SPJ alternative suggestion


>
>
> Another way to tackle this would be to ensure that syntax tree nodes have a "node-key" (a bit like their source location) that clients could use in a finite map, to map node-key to values of their choice.
>
>


An initial investigation shows some complexity blow up too.  The same effect can be achieved with a virtual node key.

## AZ Virtual node key proposal


Instead of physically placing a "node-key" in each AST Node, a virtual
node key can be generated from any `GenLocated SrcSpan e` comprising a
combination of the `SrcSpan` value and a unique identifier from the
constructor for `e`, perhaps using its `TypeRep`, since the entire AST
derives Typeable.


To further reduce the intrusiveness, a base Annotation type can be
defined that captures the location of noise tokens for each AST
constructor. This can then be emitted from the parser, if the
appropriate flag is set to enable it.



So


```

    data ApiAnnKey = AK SrcSpan TypeRep

    mkApiAnnKey :: (Located e) -> ApiAnnKey
    mkApiAnnKey = ...

    data Ann =
      ....
      | AnnHsLet    SrcSpan -- of the word "let"
                    SrcSpan -- of the word "in"

      | AnnHsDo     SrcSpan -- of the word "do"
```


And then in the parser


```

        | 'let' binds 'in' exp   { mkAnnHsLet $1 $3 (LL $ HsLet (unLoc $2) $4) }
```


The helper is


```

    mkAnnHsLet :: Located a -> Located b -> LHsExpr RdrName -> P (LHsExpr RdrName)
    mkAnnHsLet (L l_let _) (L l_in _) e = do
      addAnnotation (mkAnnKey e) (AnnHsLet l_let l_in)
      return e;
```


The Parse Monad would have to accumulate the annotations to be
returned at the end, if called with the appropriate flag.


There will be some boilerplate in getting the annotations and helper
functions defined, but it will not pollute the rest.


This technique can also potentially be backported to support older GHC
versions via a modification to ghc-parser\[1\].

[https://hackage.haskell.org/package/ghc-parser](https://hackage.haskell.org/package/ghc-parser)

## Neil Mitchell Response


I was getting a bit lost between the idea and the implementation. Let
me try rephrasing the idea in my own words.


The goal: Capture inner source spans in AST syntax nodes. At the
moment if ... then ... else ... captures the spans \[if \[...\] then
\[...\] else \[...\]\]. We want to capture the spans for each keyword as
well, so: \[{if} \[...\] {then} \[...\] {else} \[...\]\].


The proposal: Rather than add anything to the AST, have a separate
mapping `(SrcSpan,AstCtor)` to `[SrcSpan]`. So you give in the `SrcSpan`
from the `IfThenElse` node, and some token for the `IfThenElse`
constructor, and get back a list of `IfThenElse` for the particular
keyword.


I like the proposal because it adds nothing inside the AST, and
requires no fresh invariants of the AST. I dislike it because the
contents of that separate mapping are highly tied up with the AST, and
easy to get out of sync. I think it's the right choice for three
reasons, 1) it is easier to try out and doesn't break the AST, so we
have more scope for changing our minds later; 2) the same technique is
able to represent things other than `SrcSpan` without introducing a
polymorphic src span; 3) the people who pay the complexity are the
people who use it, which is relatively few people.


That said, as a tweak to the API, rather than a single data type for
all annotations, you could have:


```
data AnnIfThenElse = AnnIfThenElse {posIf, posThen, posElse :: SrcSpan}
data AnnDo = AnnDo {posDo :: SrcSpan}
```


Then you could just have an opaque `Map (SrcSpan, TypeRep) Dynamic`,
with the invariant that the `TypeRep` in the key matches the `Dynamic`.
Then you can have: 


```
getAnnotation :: Typeable a => Annotations -> SrcSpan -> Maybe a
```


I think it simplifies some of the TypeRep trickery
you are engaging in with `mkAnnKey`.


There was some further email between AZ and NDM (teaching AZ some basics) resulting in the following



This allows code using the annotation to access this as follows


```
processHsLet :: ApiAnns -> LHsExpr -> CustomReturnType
processHsLet anns (L l (HsExpr localBinds expr)) = r
  where
    Just ann = getAnnotation anns l :: Maybe AnnHsLet
    ...
```


Key data structures


```
type ApiAnns = Map.Map ApiAnnKey Value

data ApiAnnKey = AK SrcSpan TypeRep
                  deriving (Eq,Ord,Show)

mkApiAnnKey :: (Typeable a) => SrcSpan -> a -> ApiAnnKey
mkApiAnnKey l a = AK l (typeOf (Just a))

data Value = forall a . (Eq a, Show a, Typeable a, Outputable a) => Value a

newValue :: (Eq a, Show a, Typeable a, Outputable a) => a -> Value
newValue = Value

typeValue :: Value -> TypeRep
typeValue (Value x) = typeOf x

fromValue :: Typeable a => Value -> a
fromValue (Value x) = fromMaybe (error errMsg) $ res
  where
    res = cast x
    errMsg = "fromValue, bad cast from " ++ show (typeOf x)
                ++ " to " ++ show (typeOf res)

```


Note that the `Value` type is based on the one in [shake](https://github.com/ndmitchell/shake/blob/master/Development/Shake/Value.hs)



This allows the annotation to be retrieved by


```

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotation :: (Typeable a) => ApiAnns -> SrcSpan -> Maybe a
getAnnotation anns span = res
  where res = case  Map.lookup (AK span (typeOf res)) anns of
                       Nothing -> Nothing
                       Just d -> Just $ fromValue d
```

### Annotation structures


Each annotation is a separate data structure, named specifically for the constructor of the AST element being retrieved.


So if we have an AST element `L l ConstructorXXX` the corresponding annotation will be called `AnnConstructorXXX`.



An examples


```

-- TyClDecl
data AnnClassDecl = AnnClassDecl
        { aclassdecl_class   :: SrcSpan
        , aclassdecl_mwhere  :: Maybe SrcSpan
        , aclassdecl_mbraces :: Maybe (SrcSpan,SrcSpan) }
            deriving (Eq,Data,Typeable,Show)

```

## Open Questions (AZ)


I am currently working annotations into the parser, provided them as a separate structure at the end of the parse, indexed to the original by SrcSpan and AST element type.


The question I have is how to capture commas and semicolons in lists of items.


There are at least three ways of doing this

1. Make sure each of the items is Located, and add the possible comma location to the annotation structure for it.


This has the drawback that all instances of the AST item annotation have the possible comma location in them, and it does not cope with multiple separators where these are allowed.

1. Introduce a new hsSyn structure to explicitly capture comma-separated lists.


This is the current approach I am taking, modelled on the OrdList implementation, but with an extra constructor to capture the separator location.



Thus


```
data HsCommaList a
  = Empty
  | Cons a (HsCommaList a)
  | ExtraComma SrcSpan (HsCommaList a)
       -- ^ We need a SrcSpan for the annotation
  | Snoc (HsCommaList a) a
  | Two (HsCommaList a) -- Invariant: non-empty
        (HsCommaList a) -- Invariant: non-empty
```

1. Change the lists to be of type `[Either SrcSpan a]` to explicitly capture the comma locations in the list.

1. A fourth way is to add a list of SrcSpan to the annotation for the parent structure of the list, simply tracking the comma positions. This will make working with the annotations complicated though.


I am currently proceeding with option 2, but would appreciate some comment on whether this is the best approach to take. 


Option 2 will allow the AST to capture the extra commas in record constructors, as suggested by SPJ in the debate on that feature.


However, the structure is being misused in that `ExtraComma` is used to capture ALL commas, as well as semicolons in the `{ .. ; .. }` idiom.

## Update 2014-10-12



Based on further feedback from Neil Mitchell and SPJ, the basic annotation is now


```
type ApiAnns = Map.Map ApiAnnKey SrcSpan

data ApiAnnKey = AK SrcSpan Ann
                  deriving (Eq,Ord,Show)

-- ---------------------------------------------------------------------

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotation :: ApiAnns -> SrcSpan -> Ann -> Maybe SrcSpan
getAnnotation anns span ann = Map.lookup (AK span ann) anns

-- --------------------------------------------------------------------

-- | Note: in general the names of these are taken from the
-- corresponding token, unless otherwise noted
data Ann = AnnAs
         | AnnBang
         | AnnClass
         | AnnClose -- ^ } or ] or ) or #) etc
         | AnnComma
         | AnnDarrow
         | AnnData
         | AnnDcolon
         ....
```


This is a lot simpler than before.
