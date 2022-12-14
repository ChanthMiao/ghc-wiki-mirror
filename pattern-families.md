
This is a proposal (formerly named *pattern families*) for extending pattern synonyms ([PatternSynonyms](pattern-synonyms)) allowing patterns to depend on terms. The implementation is a straightforward desugaring into pattern synonyms and view patterns ([ViewPatterns](view-patterns)) so familiarity with those two extensions is recommended before reading the proposal.


The ticket associated with this design is #9671.



The simplest use case is checking whether a set contains a value:


```haskell
-- Normal Haskell
answer :: Set Int -> String
answer set = case member 42 set of
  True  -> "We know the answer"
  False -> "Never mind."

-- Using view patterns
answer :: Set Int -> String
answer (member 42 -> True)  = "We know the answer"
answer (member 42 -> False) = "Never mind."
```


With this extension we could define patterns that check for containment:


```haskell
pattern IsMember  val <- (member val -> True)
pattern NotMember val <- (member val -> False)

-- With extension
answer :: Set Int -> String
answer (IsMember  42) = "We know the answer"
answer (NotMember 42) = "Never mind."
```


This allows us to avoid pattern matching on the Boolean result of `member`. In the case of `IsMember` (and `NotMember`) the argument `val` flows into the view pattern as indicated by this figure:

![](pattern-families/member.png)



Let's consider a similar example with a `Map` where we want to look up a value based on some key:


```haskell
-- Normal Haskell
addressAlice :: Map Name Address -> Either String Address
addressAlice people = case lookup "Alice" people of
  Just address -> Right address
  Nothing      -> Left "Alice's address not found."
```


With the extension we define a pattern that only succeeds if `lookup` returns a value wrapped in `Just` which feeds that value back into the pattern:


```haskell
pattern Lookup     key val <- (lookup key -> Just val)
pattern LookupFail key     <- (lookup key -> Nothing)

-- With extension
addressAlice :: Map Name Address -> Either String Address
addressAlice (Lookup "Alice" address) = Right address
addressAlice _                        = Left "Alice's address not found."
```


where the key `"Alice"` is used in the view pattern expression and the resulting value is made available in the pattern main pattern:

![](pattern-families/lookup.png)



Now we don't have to unnecessarily give a name to an argument (like `people`) like with view patterns but unlike view patterns we don't need to mention to `Bool` and `Maybe` success result values of member and lookup. These kinds of patterns also nest arbitrarily without too much noise:


```haskell
foo :: Map Person (Map Receptacle (Set Items)) -> Status
foo = \case
  Lookup "Bob" (Lookup Pocket (IsMember  Keys)) -> HasKeys
  Lookup "Bob" (Lookup Pocket (NotMember Keys)) -> NoKeys
  Lookup "Bob" (LookupFail Pocket)              -> NoPocket
  LookupFail "Bob"                              -> Failure "No Bob"
```


Another simple example is the `Between` pattern that matches a particular range (a feature built into [Rust](http://doc.rust-lang.org/master/tutorial.html#pattern-matching)):


```haskell
import Data.Ix

pattern Between from to <- (inRange (from, to) -> True)

-- A teenager is between thirteen and nineteen.
-- `Between 13 19` would be `13..19` in Rust.
isTeen :: Age -> Bool
isTeen (Between 13 19) = True
isTeen _               = False
```


that gets transformed into:


```haskell
isTeen :: Age -> Bool
isTeen (inRange (13, 19) -> True) = True
isTeen _                          = False
```


`Between` will work on any indexable type (`Ix a => a`):


```haskell
generalCategory' :: Char -> GeneralCategory 
generalCategory' = \case
  Between '\x00' '\x16' -> Control
  Between 'a'    'z'    -> LowercaseLetter
  Between 'A'    'Z'    -> UppercaseLetter
  Between '0'    '9'    -> DecimalNumber
```

## Syntax



The syntax from [PatternSynonyms](pattern-synonyms) can be reused for simplicity:


```haskell
pattern Lookup key value <- (lookup key -> Just value)
```


here `value` is a normal variable as in [PatternSynonyms](pattern-synonyms) but `key` is some term the view pattern is indexed by: this can be inferred from it appearing in the [ViewPatterns](view-patterns) expression (`lookup key`) rather than in the pattern (`Just value`).



The function `fn`:


```haskell
fn :: Map String Int -> Int
fn (Lookup "five" n) = n
```

```wiki
ghci> fn do Map.fromList [("four", 4), ("five", 5)]
5
```


is the same as writing `fn (lookup "five" -> Just n) = n` using view patterns.

### Grammar



A simple grammar would then be


>
>
> `pattern` *conid* *varid<sub>1</sub>* .. *varid<sub>n</sub>* `<-` (*expr* `->` *pat*)
>
>


where each *varid<sub>i</sub>* must be free in either *expr* or *pat*.

## Dynamic semantics (Desugaring)



More concretely, a pattern definition has the form:


>
>
> `pattern` *conid* \[*evarid<sub>i</sub>*, *pvarid<sub>j</sub>*\] `<-` (*expr* `->` *pat*)
>
>


where \[*evarid<sub>i</sub>*, *pvarid<sub>j</sub>*\] denotes some interleaving of variables that appear in *expr* or *pat*. When matched against, all *evarid<sub>i</sub>* must be instantiated with expresions *expr<sub>i</sub>*:


>
>
> `fn` (*conid* \[*expr<sub>i</sub>*, *pvarid<sub>j</sub>*\]) = `result`
>
>


where *pvarid<sub>j</sub>* may appear in `result` as in usual patterns. This then gets translated into the following view pattern:


>
>
> `fn` (*expr*\[*evarid<sub>i</sub>* := *expr<sub>i</sub>*\] `->` `pat`) = `result`
>
>


For the simple example of the pattern family `Take` this (where 3 is *expr<sub>1</sub>* and `xs` is *pval<sub>1</sub>*):


```haskell
foo (Lookup "five" n) = n + n
```


would get translated to:


```haskell
foo (lookup "five" -> Just n) = n + n
```


which is the same as:


```haskell
foo ys = case lookup "five" n of
  Just n -> n + n
```

## Static semantics (Typing)


If *expr* in the view pattern is an expression of type *t* with free variables *evarid<sub>i</sub>* of type *t<sub>i</sub>* then an *expr<sub>i</sub>* used to instantiate the corresponding *evarid<sub>i</sub>* must have a type *u<sub>i</sub>* that unifies with *t<sub>i</sub>*, the final expression *expr* will have type *t*. Otherwise same typing and scoping rules as [ViewPatterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns#Semantics).

## Motivating examples

### Sets



Example from [ViewPatternsAlternative](view-patterns-alternative):


```haskell
module Set(Set, empty, insert, delete, has) where

newtype Set a = S [a]
  
has :: Eq a => a -> Set a -> Maybe (Set a)
has x (S xs) | x `elem` xs = Just (S (xs \\ [x]))
             | otherwise   = Nothing
```


Using patterns indexed by an element of `Set a`:


```haskell
pattern Has    x set <- (has x        -> Just set)
pattern HasNot x set <- (has x &&& id -> (Nothing, set))
```


One can write:


```haskell
delete :: Eq a => a -> Set a -> Set a
delete x (Has x set) = set
delete x set         = set

insert :: Eq a => a -> Set a -> Set a
insert x (HasNot x (S xs)) = S (x:xs)
insert x set               = set
```


Compare that to the [ViewPatternsAlternative](view-patterns-alternative) proposal:


```haskell
delete :: Eq a => a -> Set a -> Set a
delete x (r | Just s <- has r) = set
delete x set                   = set
  
insert :: Eq a => a -> Set a -> Set a
insert x (s | Just _ <- has x s) = set
insert x (S xs)                  = S (x:xs)
```


Where the user has to worry about matching on `Just`s. 

### Erlang-style parsing



Another example stolen from [ViewPatternsAlternative](view-patterns-alternative) where the benefits are more apparent. Given a parsing function:


```haskell
-- (bits n bs) parses n bits from the front of bs, returning
-- the n-bit Word, and the remainder of bs

bits :: Int -> ByteString -> Maybe (Word, ByteString)
```


and using the following pattern family:


```haskell
pattern Bits n val bs <- (bits n -> Just (val, bs))
```


one can write a pattern like this:


```haskell
parsePacket :: ByteString -> _
parsePacket (Bits 3 n (Bits n val bs)) = _
```


Where we can nest pattern families, more examples of nesting follow in the more advanced examples below. Compare that to the more verbose [ViewPatternsAlternative](view-patterns-alternative) version:


```haskell
parsePacket :: ByteString -> _
parsePacket (p1 |  Just (n, (p2 | Just (val, bs) <- bits n p2)) <- bits 3 p1) = _
```

### N+k patterns



Another one from [ViewPatternsAlternative](view-patterns-alternative) using the following view and pattern family:


```haskell
np :: Num a => a -> a -> Maybe a
np k n | k <= n    = Just (n-k)
       | otherwise = Nothing

pattern NP k n <- (np k -> Just n)
```


Used as follows:


```haskell
fib :: Num a -> a -> a
fib 0        = 1
fib 1        = 1
fib (NP 2 n) = fib (n + 1) + fib n
```


Compare [ViewPatternsAlternative](view-patterns-alternative) version:


```haskell
fib :: Num a -> a -> a
fib 0 = 1
fib 1 = 1
fib (n2 | let n = n2-2, n >= 0) = fib (n + 1) + fib n
```

### Type checking



From [Bidirectional Typing Rules: A Tutorial](http://itu.dk/people/drc/tutorials/bidirectional.pdf):


```haskell
inferType ctx (If t1 t2 t3) = case (inferType ctx t1, inferType ctx t2, inferType ctx t3) of
  (Just BoolT, Just ty2, Just ty3) -> 
    if ty2 = ty3 
    then Just ty2
    else Nothing
  _ -> Nothing
```


could be rewritten using pattern families as:


```haskell
-- Here ???Inf??? is a family indexed by ???ctx???
pattern Inf ctx ty <- (inferType ctx -> Just ty)

inferType ctx (If (Inf ctx BoolT) (Inf ctx ty1) (Inf ctx ty2))
   | ty1 == ty2 = Just ty1
inferType ctx If{} = Nothing
```


allowing the user to pattern match *directly* on the inferable types without manually checking for `Just`s ??? note the use of the previous argument `ctx` to index later. This could currently be written somewhat awkwardly using view patterns:


```haskell
inferType ctx (If (inferType ctx -> Just BoolT) (inferType ctx -> Just ty1) (inferType ctx -> Just ty2))
  | ty1 == ty2 = Just ty1
inferType ctx If{} = Nothing
```


which is longer and clunkier, especially since the user is forced to deal with `Just`s again.



Again one could use operators (`:??? = Inf`) in which case it the examples follow notation in type theory more closely:


```haskell
inferType ?? (If (?? :??? BoolT) (?? :??? ?????) (?? :??? ?????)) = ...
```

### More advanced examples: Regular expressions



Given a regular expression operator `(~=) :: String -> String -> Maybe [String]` we can define a pattern:


```haskell
pattern Match x regexp <- ((~= regexp) -> Just x)
```


where `regexp` is indexes the `Match` pattern family:


```haskell
firstWord (Match words "[a-zA-Z]+") = words
firstWord _                         = error "No words found"
```


or


```haskell
vowels (Match vwls "[aeiou]") = length vwls
```


As an operator:


```haskell
pattern x :~= regexp <- ((~= regexp) -> Just x)
```

### More advanced examples: Prism patterns

#### Matching a simple prism



Indexing patterns with prisms from [Control.Lens.Prism](http://hackage.haskell.org/package/lens-4.2/docs/Control-Lens-Prism.html):


```haskell
import Control.Lens.Prism

pattern Match prism a <- (preview prism -> Just a)
```


one can write


```haskell
bar :: Either c (Either a b) -> a
bar (Match (_Right._Left) a) = a
bar _                        = error "..."
```

#### More complicated prisms



Pattern families can be used to match nested data like JSON, ASTs or XML, here is an example of using it to match on [Data.Aeson.Lens](http://hackage.haskell.org/package/lens-4.2/docs/Data-Aeson-Lens.html):


```haskell
jsonBlob = "[{\"someObject\": {\"version\": [1,0,3]}}]"
    
-- val = Number 0.0
val = jsonBlob ^?! nth 0 . key "someObject" . key "version" . nth 1
```


Pattern families allow us to say we want to fetch the same value as `val` using patterns:


```haskell
foo (Match (nth 0) (Match (key "someObject") (Match (key "version") (Match (nth 1) a)))) = a
```


Which is terribly verbose, but can be improved by introducing:


```haskell
pattern Get i   a <- (preview (nth i)   -> Just a)
pattern Key str a <- (preview (key str) -> Just a)

baz (Get 0 (Key "someObject" (Key "version" (Get 1 a)))) = a
```


or  by writing it infix:


```haskell
baz (0 `Get` "someObject" `Key` "version" `Key` 1 `Get` a) = a
```


or by defining operators `:??? = Get i a` and `:??? = Key`:


```haskell
baz (a :??? "someObject" :??? "version" :??? 1 :??? a) = a
```
