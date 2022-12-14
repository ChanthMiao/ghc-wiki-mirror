# Pattern Synonyms


Most language entities in Haskell can be named so that they can be abbreviated instead of written out in full.
This proposal provides the same power for patterns.  See the [implementation](pattern-synonyms/implementation) page for implementation details.

See the ~"PatternSynonyms" label.

## Motivating example


Here is a simple representation of types

```haskell
data Type = App String [Type]
```


Using this representations the arrow type looks like `App "->" [t1, t2]`.
Here are functions that collect all argument types of nested arrows and recognize the `Int` type:

```haskell
collectArgs :: Type -> [Type]
collectArgs (App "->" [t1, t2]) = t1 : collectArgs t2
collectArgs _ = []

isInt (App "Int" []) = True
isInt _ = False
```


Matching on `App` directly is both hard to read and error prone to write.


The proposal is to introduce a way to give patterns names:

```haskell
pattern Arrow t1 t2 = App "->" [t1, t2]
pattern Int = App "Int" []
```


And now we can write

```haskell
collectArgs :: Type -> [Type]
collectArgs (Arrow t1 t2) = t1 : collectArgs t2
collectArgs _ = []

isInt Int = True
isInt _ = False
```


Here is a second example from [pigworker on Reddit](http://www.reddit.com/r/haskell/comments/1kmods/patternsynonyms_ghc_trac/).
Your basic sums-of-products functors can be built from this kit.

```haskell
newtype K a        x  = K a
newtype I          x  = I x
newtype (:+:) f g  x  = Sum (Either (f x) (g x))
newtype (:*:) f g  x  = Prod (f x, g x)
```


and then you can make recursive datatypes via

```haskell
newtype Fix f = In (f (Fix f))
```


e.g.,

```haskell
type Tree = Fix (K () :+: (I :*: I))
```


and you can get useful generic operations cheaply because the functors in the kit are all `Traversable`, admit a partial zip operation, etc.


You can define friendly constructors for use in expressions

```haskell
leaf :: Tree
leaf = In (Sum (Left (K ())))
node :: Tree -> Tree -> Tree
node l r = In (Sum (Right (Prod (I l, I r))))
```


but any `Tree`-specific pattern matching code you write will be wide and obscure. Turning these definitions into pattern synonyms means you can have both readable type-specific programs and handy generics without marshalling your data between views.

## Uni-directional (pattern-only) synonyms



The simplest form of pattern synonyms is the one from the examples above.  The grammar rule is:



`pattern` *conid* *varid<sub>1</sub>* ... *varid<sub>n</sub>* `<-` *pat*



`pattern` *varid<sub>1</sub>* *consym* *varid<sub>2</sub>* `<-` *pat*


- Each of the variables on the left hand side must occur exactly once on the right hand side 
- Pattern synonyms are not allowed to be recursive.  Cf. type synonyms.

<table><tr><th>
There have been several proposals for the syntax of defining pattern-only synonyms:


- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt>~</tt> <i>pat</i>
- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt>:=</tt> <i>pat</i>
- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt>-></tt> <i>pat</i>
- <tt>pattern</tt> <i>conid</i> <i>varid<sub>1</sub></i> ... <i>varid<sub>n</sub></i> <tt><-</tt> <i>pat</i>

</th></tr></table>


Pattern synonyms can be exported and imported by prefixing the *conid* with the keyword `pattern`:

```haskell
module Foo (pattern Arrow) where ...
```


This is required because pattern synonyms are in the namespace of constructors, so it's perfectly valid to have

```haskell
data P = C
pattern P = 42
```


You may also give a type signature for a pattern, but as with most other type signatures in Haskell it is optional:



`pattern` *conid* `::` *type*



E.g.

```haskell
pattern Arrow :: Type -> Type -> Type
pattern Arrow t1 t2 <- App "->" [t1, t2]
```


Together with [ViewPatterns](view-patterns) we can now create patterns that look like regular patterns to match on existing (perhaps abstract) types in new ways:

```haskell
import qualified Data.Sequence as Seq

pattern Empty <- (Seq.viewl -> Seq.EmptyL)
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x)
```

## Simply-bidirectional pattern synonyms



In cases where *pat* is in the intersection of the grammars for patterns and expressions (i.e. is valid both as an expression and a pattern), the pattern synonym can be made bidirectional, and can be used in expression contexts as well. Bidirectional pattern synonyms have the following syntax:



`pattern` *conid* *varid<sub>1</sub>* ... *varid<sub>n</sub>* `=` *pat*



`pattern` *varid<sub>1</sub>* *consym* *varid<sub>2</sub>* `=` *pat*



For example, the following two pattern synonym definitions are rejected, because they are not bidirectional (but they would be valid as pattern-only synonyms)

```haskell
pattern ThirdElem x = _:_:x:_
pattern Snd y = (x, y)
```


since the right-hand side is not a closed expression of {*x*} and {*y*} respectively.


In contrast, the pattern synonyms for *Arrow* and *Int* above are bidirectional, so you can e.g. write:

```haskell
arrows :: [Type] -> Type -> Type
arrows = flip $ foldr Arrow
```

## Explicitly-bidirectional pattern synonyms


What if you want to use `Succ` in an expression:

```wiki
    pattern Succ n <- n1 | let n = n1 -1, n >= 0
```


It's clearly impossible since its expansion is a pattern that has no meaning as an expression.
Nevertheless, if we want to make what looks like a constructor for a type we will often want to use it in both patterns and expressions.
This is the rationale for the most complicated synonyms, the bidirectional ones.  They provide two expansions, one for patterns and one for expressions.



`pattern` *conid* *varid<sub>1</sub>* ... *varid<sub>n</sub>* `<-` *pat* `where` *cfunlhs* *rhs*



where *cfunlhs* is like *funlhs*, except that the functions symbol is a *conid* instead of a *varid*.


Example, using [ViewPatterns](view-patterns):

```haskell
pattern Succ n <- ((\x -> (x-1) <$ guard (x > 0)) -> Just n)
 where
  Succ n = n + 1
```


The first part as is before and describes the expansion of the synonym in patterns. The second part describes the expansion in expressions.

```haskell
fac (Succ n) = Succ n * fac n 
fac 0 = 1
```

## Associated pattern synonyms


Just like data types and type synonyms can be part of a class declaration, it would be possible to have pattern synonyms as well.


Example:

```haskell
class ListLike l where
  pattern Nil :: l a
  pattern Cons :: a -> l a -> l a
  isNil :: l a -> Bool
  isNil Nil = True
  isNil (Cons _ _) = False
  append :: l a -> l a -> l a

instance ListLike [] where
  pattern Nil = []
  pattern Cons x xs = x:xs
  append = (++)

headOf :: (ListLike l) => l a -> Maybe a
headOf Nil = Nothing
headOf (Cons x _) = Just x
```


One could go one step further and leave out the `pattern` keyword to obtain *associated constructors*, which are required to be bidirectional. The capitalized identifier would indicate that a pattern synonym is being defined. For complicated cases one could resort to the `where` syntax (shown above).

**TODO**: Syntax for associated pattern synonym declarations to discern between pattern-only and bidirectional pattern synonyms

## Static semantics


A unidirectional pattern synonym declaration has the form

```haskell
pattern P var1 var2 ... varN <- pat
```


The formal pattern synonym arguments `var1`, `var2`, ..., `varN` are brought
into scope by the pattern pat on the right-hand side. The declaration
brings the name `P` as a pattern synonym into the module-level scope.


The pattern synonym `P` is assigned a *pattern type* of the form

```haskell
pattern P :: CReq => CProv => t1 -> t2 -> ... -> tN -> t 
```


where `t1`, ..., `tN` are the types of the parameters `var1`, ..., `varN`, `t` is the simple type (with no context) of the thing getting matched, and `CReq` and `CProv` are type contexts.

`CProv` can be omitted if it is empty. If `CReq` is empty, but `CProv` is not, `()` is used. The following example shows cases:

```haskell
data Showable where
    MkShowable :: (Show a) => a -> Showable

-- Required context is empty, but provided context is not
pattern Sh :: () => (Show a) => a -> Showable
pattern Sh x <- MkShowable x

-- Provided context is empty
pattern One :: (Num a, Eq a) => a
pattern One <- 1
```


A pattern synonym can be used in a pattern if the
instatiated (monomorphic) type satisfies the constraints of
`CReq`. In this case, it extends the context available in the
right-hand side of the match with `CProv`, just like how an
existentially-typed data constructor can extend the context.


As with function and variable types, the pattern type signature can be inferred, or it can be explicitly written out on the program. 


Here's a more complex example. Let's look at the following definition:

```haskell
{-# LANGUAGE PatternSynonyms, GADTs, ViewPatterns #-}
module ShouldCompile where

data T a where
	MkT :: (Eq b) => a -> b -> T a

f :: (Show a) => a -> Bool

pattern P x <- MkT (f -> True) x
```


Here, the inferred type of `P` is

```haskell
pattern P :: (Show a) => (Eq b) => b -> T a
```


A bidirectional pattern synonym declaration has the form

```haskell
pattern P var1 var2 ... varN = pat
```


where both of the following are well-typed declarations:

```haskell
pattern P1 var1 var2 ... varN <- pat

P2 = \var1 var2 ... varN -> pat
```


In this case, the *pattern type* of `P` is simply the pattern type
of `P1`, and its *expression type* is the type of `P2`. The name `P`
is brought into the module-level scope both as a pattern synonym and
as an expression.

## Dynamic semantics


A pattern synonym occurance in a pattern is evaluated by first
matching against the pattern synonym itself, and then on the argument
patterns. For example, given the following definitions:

```haskell
pattern P x y <- [x, y]

f (P True True) = True
f _             = False

g [True, True] = True
g _            = False
```


the behaviour of `f` is the same as

```haskell
f [x, y] | True <- x, True <- y = True
f _                             = False
```


Because of this, the eagerness of `f` and `g` differ:

```haskell
*Main> f (False:undefined)
*** Exception: Prelude.undefined
*Main> g (False:undefined)
False
```


This is because we generate the matching function at the definition site. 

## Typed pattern synonyms


So far patterns only had *syntactic* meaning. In comparison [??mega](http://code.google.com/p/omega) has *typed* pattern synonyms, so they become first class values. For bidirectional pattern synonyms this seems to be the case

```haskell
data Nat = Z | S Nat deriving Show
pattern Ess p = S p
```


And it works:

```haskell
*Main> map S [Z, Z, S Z]
[S Z,S Z,S (S Z)]
*Main> map Ess [Z, Z, S Z]
[S Z,S Z,S (S Z)]
```

## Branching pattern-only synonyms

*N.B. this is a speculative suggestion!
*


Sometimes you want to match against several summands of an ADT simultaneously. E.g. in a data type of potentially unbounded natural numbers:

```haskell
data Nat = Zero | Succ Nat
type UNat = Maybe Nat -- Nothing meaning unbounded
```


Conceptually `Nothing` means *infinite*, so it makes sense to interpret it as a *successor* of something. We wish it to have a predecessor just like `Just (Succ Zero)`!


I suggest *branching pattern synonyms* for this purpose:

```haskell
pattern S pred <- pred@Nothing | pred@(Just a <- Just (Succ a))
pattern Z = Just Zero
```


Here `pred@(Just a <- Just (Succ a))` means that the pattern invocation `S pred` matches against `Just (Succ a)` and - if successful - binds `Just a` to `pred`.


This means we can syntactically address unbound naturals just like bounded ones:

```haskell
greetTimes :: UNat -> String -> IO ()
greetTimes Z _ = return ()
greetTimes (S rest) message = putStrLn message >> greetTimes rest message
```


As a nice collateral win this proposal handles `pattern Name name <- Person name workplace | Dog name vet` too.

## Record Pattern Synonyms


See [PatternSynonyms/RecordPatternSynonyms](pattern-synonyms/record-pattern-synonyms)

## Associating synonyms with types


See [PatternSynonyms/AssociatingSynonyms](pattern-synonyms/associating-synonyms)

## `COMPLETE` pragmas

See [PatternSynonyms/CompleteSigs](pattern-synonyms/complete-sigs)
