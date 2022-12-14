# View patterns: lightweight views for Haskell


This page describes a rather lightweight proposal for adding views to 
Haskell Prime.  I'm thinking of prototyping the idea in GHC, so I'm looking
for feedback.


This page is open to editing by anyone.  (Chase the "Wiki notes" link in the sidebar to find out how.)

## The problem


We are keen on abstraction, but pattern matching is so convenient that
we break abstractions all the time.  It's our dirty little secret.
Looked at this way, object-oriented folk are much more obsessive 
about abstraction than we are: everything (including field access 
these days) is a method.


Views have, in one form or another, repeatedly been proposed as a
solution for this problem.   (See the end for a comparison with related work.)

---

## The lightweight view proposal

### Informally


The proposal introduces a new form of pattern, called a **view pattern**
Here are some function definitions using view patterns.
To read these definitions, imagine that `sing` is
a sort of constructor that matches singleton lists.

```haskell
  f :: [Int] -> Int
  f (sing -> n) = n+1	-- Equiv to: f [n] = ...
  f other     = 0

  g :: [Bool] -> Int
  g (sing -> True)  = 0		-- Equiv to: g [True] = ...
  g (sing -> False) = 1		-- Equiv to: g [False] = ...
  g other           = 2

  h :: [[Int]] -> Int	
  h (sing -> x : sing -> y : _) = x+y
			-- Equiv to: h ([x]:[y]:_) = ...
  h other = 0
```


So what is `sing`?  It is just an ordinary Haskell function that
returns a `Maybe` type:

```haskell
  sing :: [a] -> Maybe a
  sing [x]   = Just x
  sing other = Nothing
```


So `sing` simply identifies singleton lists, and returns the payload (that is,
the singleton element; otherwise it returns `Nothing`.
It is very important that **there is nothing special about `sing`**.  It is
not declared to be a view; it can be called as a normal Haskell function; the author
of `sing` might not have intended it to be used in pattern matching.  

### More formally


The only special stuff is in the pattern.  
The sole change is this: add a single new sort of pattern, of the 
form


>
>
> (*expr* `->` *pat*)
>
>


where *expr* is an arbitrary Haskell expression.   I'll call a pattern
of this form a **view pattern**. 



From a **scoping** point of view, the variables bound by the pattern (*expr* `->` *pat*)
are simply the variables bound by ``pat``.
Any variables in ``expr`` are bound occurrences.


The rule for **pattern-matching** is this:
To match a value *v* against a pattern *(expr -\> p)*, 

- Evaluate *(expr v)*
- If the result is *(`Just` w)*, match *w* against *p*
- If the result is `Nothing`, the match fails.


The **typing rule** is similarly simple.  
The expression *expr* must have type
*t1 `-> Maybe` t2*. Then the pattern *pat* must have type *t2*, and the
whole pattern (*expr* `->` *pat*) has type *t1*.


### Features


For the different features this proposal (and others) have, see [Features views can have](#Featuresviewscanhave).
The proposal

- has the value input feature
- has the implicit `Maybe` feature
- doesn't have the transparent ordinary patterns feature
- has the nesting feature

### Possible extension 1: multi-argument view patterns


It would be quite useful to allow more than one sub-pattern in a view
pattern.  To do this we'd need a `Maybe` data type that returns more than
one result, thus:

```haskell
  data Maybe2 a b   = Nothing2 | Just2 a b
  data Maybe3 a b c = Nothing3 | Just3 a b c
  	-- ..etc..., up to 8 perhaps (sigh)
```


With this in hand we can extend the views story to have multiple sub-patterns.
Example:

```haskell
  snoc :: [a] -> Maybe2 [a] a
  snoc [] = Nothing2
  snoc (x:xs) = case snoc xs of
		  Nothing2   -> Just2 [] x
		  Just2 ys y -> Just2 (x:ys) y

  last :: [Int] -> Int
  last (snoc -> xs x) = x
  last other = error "empty list"
```


It is tiresome that we need types `Maybe2`, `Maybe3` etc, but we already have 
that in Haskell; consider `zip3`, `zip4` and so on.
We could always get away without it, by sticking to unary view patterns and
using tuples, thus:

```haskell
  snoc :: [a] -> Maybe ([a], a)
  snoc [] = Nothing
  snoc (x:xs) = case snoc xs of
		  Nothing     -> Just ([], x)
		  Just (ys,y) -> Just (x:ys, y)

  last :: [Int] -> Int
  last (snoc -> (xs, x)) = x
  last other = error "empty list"
```


But the tuple looks a bit clumsy.


Under this proposal, the number of sub-patterns in the view pattern determines
which return type the view function should have.  E.g. in the pattern '(e -\> p1 p2 p3)',
'e' should return a `Maybe3`.


If n=0, then we want `Maybe0`, which is called `Bool`.  Thus

```haskell
  even :: Int -> Bool
  even n = n `div` 2 == 0

  f (even ->) = ...	-- Matches even numbers
  f other     = ...
```


Here `even` is used as a nullary view pattern, with no sub-patterns
following the `->`.


Another variation (call it "extension 1b"), which avoids the tiresome need to define new types, is this: supplying multiple sub-patterns in a view pattern is synonymous with tupling.  Thus `(f -> p1 p2)` would be synonymous with `(f -> (p1,p2))`.  Here the effect is purely syntactic, allowing you to omit parens and commas without confusion.  No new types.  The power-to-weight ratio is probably better for this alternative.

### Possible extension 2: the implicit `Maybe`


Thus far, the view function is required to return a `Maybe` type, with `Nothing` to indicate match
failure.  An alternative, presented in the Erwig paper on transformational patterns (see Related work below), 
this implicit matching is not performed; instead, the sub-pattern is matched against
whatever the view function returns.  So you'd have to write:

```haskell
f (snoc -> Just2 xs x) = ...
```


(Note the tiresome `Just2`.)


For more one the consequences of removing the implicit `Maybe`, see the [Implicit \`Maybe\` feature](#Implicit`Maybe`feature)


I can think of three alternatives:

- The `Maybe` stuff is built-in. This is the main proposal, because I think it is often exactly what you want.
- No built-in `Maybe` stuff.  Arguably this is more consistent with pattern-guards.
- Both are available, with different syntax.  For example 

  - *(expr `->` pat)* for the built-in `Maybe` story
  - *(expr `=>` pat)* with no built-in `Maybe`

### Concrete syntax


A disadvantage of the arrow syntax is that it looks a bit confusing
when it appears in a case expression:

```haskell
  last xs = case xs of
                (snoc -> x xs) -> x
```


(Also that "x xs" looks a bit like `x` applied to `xs`.)


Here are some other possible syntax choices I've considered:

```haskell
  f ($snoc x xs) = ...		-- Use prefix "$"
  g ($(bits 3) x bs) = ...	-- Extra parens for the value input feature

  f (%snoc x xs) = ...		-- Or use prefix "%" instead
  f (.snoc x xs) = ...		-- Or use prefix "." instead
  f (?snoc x xs) = ...		-- Or use prefix "?" instead

  f (snoc? x xs) = ...		-- Postfix "?"
  g ((bits 3)? x bs) = ...	-- With parens

  f (snoc | x xs) = ..		-- Use "|" instead of "->"
  g (bits 3 | b bs) = ...
```


Another possibility is to use a backward arrow, more like pattern guards:

```haskell
  f ((x,xs) <- snoc) = ...  -- More like pattern guards
```


But that messes up the left-to-right flow that is useful in some cases.
For example, compare these:

```haskell
  parsePacket1 (bits 3 -> n (bits n -> val bs)) = ...

  parsePacket2 (n (val bs <- bits n) <- bits 3) = ...
```


I also thought about infix view patterns, where the view function
appears between its (pattern) arguments, but I could not think of a
nice syntax for it, so it is not provided by this proposal.

### Remarks


The key feature of this proposal is its modesty, rather than its ambition:

- There is no new form of declaration (e.g. 'view' or 'pattern synonym').  
- The functions used in view patterns are ordinary Haskell functions, and can be called from ordinary Haskell code.  They are not special view functions.
- Since the view functions are ordinary Haskell functions, no changes are needed to import or export mechanisms.
- Both static and dynamic semantics are extremely simple.


It is essentially some simple syntactic sugar for patterns.
However, sometimes modest syntactic sugar can have profound consequences.
In this case, it's possible that people would start routinely hiding
the data representation and exporting view functions instead, which might
be an excellent thing.


All this could be done with pattern guards.  For example `parsePacket` could be written

```haskell
  parsePacket bs | Just (n, bs')    <- bits 3 bs
                 | Just (val, bs'') <- bits n bs'
		 = ...
```


Indeed, one might ask whether the extra syntax for view patterns is worth
it when they are so close to pattern guards.  
That's a good question.  I'm hoping that support for view patterns
might encourage people to export view functions (ones with `Maybe`
return types, and encourage their use in pattern matching).  That is,
just lower the barrier for abstraction a bit.

**Completeness**.  It is hard to check for completeness of pattern matching; and likewise for
overlap.  But guards already make both of these hard; and GADTs make completeness hard too.
So matters are not much worse than before.

---

## Features views can have


The main goal of views is to introduce computations into pattern matches thus introducing abstraction from hard wired constructors. To distinguish between the different proposals, we pick out the key features

### Value input feature


This features allows to introduce additional parameters into the computation. Perhaps the most basic example are (n+k) patterns

```haskell
  fib :: Int -> Int
  fib 0 = 1
  fib 1 = 1
  fib (n + 2) = fib (n + 1) + fib n
```


Here, the number 2 can be arbitrary, we are not fixed to a "finite" set of "constructors" (+1), (+2) etc.


Of course, the real power unfolds when the extra parameter can be given at runtime

```haskell
   f :: Int -> Int -> ...
   f n (n + m) = m           -- f a b = (b - a)
```


In the proposed view pattern (*expr* `->` *pat*), *expr* is an arbitrary Haskell expression. Thus, the lightweight proposal has the **value input feature**. For another example, suppose you wrote a regular expression matching function:


```haskell
  regexp :: String -> String -> Maybe (String, String)
  -- (regexp r s) parses a string matching regular expression r 
  --	the front of s, returning the matched string and remainder of s
```


then you could use it in patterns thus:

```haskell
  f :: String -> String
  f (regexp "[a-z]*" -> (name, rest)) = ...
```


Of course, the argument does not need to be a constant as it is here.


With the value input feature, in a sense, patterns become first class. For example, one could pass a pattern as an argument to a function thus:

```haskell
  g :: (Int -> Maybe Int) -> Int -> ...
  g p (p -> x) = ...
```


Here the first argument `p` can be thought of as a pattern passed to `g`, which
is used to match the second argument of `g`.


Here is another rather cute example:

```haskell
unfoldr :: (b -> Maybe (a, b)) -> b -> [a] 
unfoldr f (f -> (a, b)) = a : unfoldr f b
unfoldr f other         = []
```

### Implicit `Maybe` feature


In functional languages, pattern matching is used to inspect a sum types like `Either Int String` and to proceed with the matching alternative. We can always project a choice between multiple alternatives to choice between one alternative (`Just`) and failure (`Nothing`):

```haskell
   maybeLeft  :: Either a b -> Maybe a
   maybeRight :: Either a b -> Maybe b
```


Some proposals build their patterns entirely from from such single alternative de-constructors functions of type `a -> Maybe b`, while some allow projection to multiple alternatives.


By restricting de-constructors to be of type `a -> Maybe b`, the Maybe can be made implicit, it doesn't show up in the pattern. Example:

```haskell
    data Product = ....some big data type...
    type Size = Int
    
    smallProd, medProd, bigProd :: Product -> Maybe Size
    smallProd p = ...
    medProd   p = ...
    bigProd   p = ...
    
    f :: Product -> ...
    f (smallProd -> s) = ...
    f (medProd   -> s) = ...
    f (bigProd   -> s) = ...
```


Projection to multiple alternatives requires a new (or existing) data type for every group of alternatives introduced.

```haskell
    data Dimensions = Small | Medium | Big	-- View type
    prodSize :: Product -> Dimensions
    prodSize = ...
    
    f :: Product -> ...
    f (prodSize -> Small)  = ...
    f (prodSize -> Medium) = ...
    f (prodSize -> Big)    = ...
```


Using a fixed set of multiple alternatives makes it more obvious whether the match is exhaustive or not.


While the implicit `Maybe a` is more compositional and nicely integrates with already existing uses of the `Maybe`-type, it cannot share expensive computations across multiple alternatives. This is a strong argument against the implicit `Maybe a`. To illustrate the problem, suppose that

```haskell
   data Graph
```


represents a graph and that we want a function

```haskell
   g :: Graph -> [...]
   g (forest -> xs) = concatMap g xs
   g (tree ->)      = ...
   g (dag  ->)      = ...
```


These three properties are expensive to calculate but all three only
depend on the result of a single depth first search. By projecting the
disjoint sum to several `Maybe`s, the depth first search has to be
repeated every time. More importantly, there is \*no way\* for the compiler to optimize this because that would mean common subexpression elimination across
functions.


Some would argue that implicit the 'Maybe a' is *less* compositional than the explicit version.  If no 'Maybe' is required, then the result of the view function can be any type at all, which can be pattern-matched in the ordinary way.  Some examples of cute programming of well-known combinators:

```haskell
map f [] = []
map f (x: map f -> xs) = x:xs

foldr f z [] = z
foldr f z (x: foldr f z -> xs) =  x `f` xs
```

### Transparent ordinary Patterns


The lightweight view proposal has different syntax for view functions and ordinary pattern matches, they are not interchangeable. To use the abstraction view functions offer, you have to anticipate whether you can stick to ordinary constructors or whether you will switch to abstract constructors at some time. For example, a stack abstraction would have to use view patterns if we want to be able to change the concrete representation of stacks later on.

```haskell
    type Stack a = [a]
    
    f :: Stack a
    f (null?)     = ...
    f (pop? x xs) = ...
```


This certainly discourages ordinary pattern matching. In other words,
implementing the proposal has considerable impact on ordinary pattern
matching (not in semantics but in use).


The problem that needs to be solved is to introduce abstraction "after the fact".


On the other hand, view patterns can do arbitrary computation, perhaps expensive. So it's good to have a syntactically-distinct notation that reminds the programmer that some computation beyond ordinary pattern matching may be going on.

### Nesting


In the lightweight proposal, view patterns are just an extra syntactic form of pattern, and they nest inside other patterns, and other patterns nest inside them.  So one can write

```haskell
  f (sing -> x, True) = ...
  g (Just (sing -> x)) = ...
  h (Just (sing -> Just x)) = ...
```


And by the same token, view patterns nest inside each other:

```haskell
  k :: [[a]] -> a
  k (sing -> sing -> x) = x
```


This convenient nesting is perhaps the biggest practical 
difference between view patterns and pattern guards.


The majority of the proposals allow nesting.

### Integration with type classes


A view mechanism that integrates nicely with type classes would allow
a single "view" to decompose multiple different data types.  For
example, a view might work on any type in class Num, or in class Sequence.


A good example is Haskell's existing (n+k) patterns.  Here is how they
can be expressed using the view pattern proposed in this page (with different 
syntax, of course):

```haskell
   np :: Num a => a -> a -> Maybe a
   np k n | k <= n    = Just (n-k)
          | otherwise = Nothing

   g :: Int -> Int
   g (np 3 -> n) = n*2

   h :: Integer -> Integer
   h (np 9 -> n) = n*2

   f :: Num a => a -> Int
   f (np 10 -> n) = n		-- Matches values >= 10, f a = (a - 10)
   f (np 4  -> n) = 1		-- Matches values >= 4
   f other        = 2
```


Here a single, overloaded view, `np`, can be used 
in `g`, and `h` to match against values of different types and,
in `f`'s case, any type in class Num. (Notice too the use of the value 
input feature.)


This feature falls out very nicely from view patterns, but 
not from all other proposals.

---

## Efficiency of Views


View patterns can do arbitrary computation, perhaps expensive.


It's reasonable to expect the compiler to avoid repeated computation when
pattern line up in a column:

```haskell
  f (snoc -> x xs) True  = ...
  f (snoc -> x xs) False = ...
```


In pattern-guard form, common sub-expression should achieve the same
effect, but it's quite a bit less obvious.  We should be able to give
clear rules for when the avoidance of repeat computation is
guaranteed.

---

## Use cases and examples


Whether views are really worth it can only be decide on the base of examples. Some are situations where you programmed and thought "I wish I had a view for that". Some are those snippets of code that unexpectedly use views to good effect.

### Sequences


Lists, queues, ByteStrings and 2-3-finger trees are all implementations of sequences, but only ordinary lists can be deconstructed using pattern matching. The need for list patterns on arbitrary sequence data structures is desperate. As if to ease the pain, Data.Sequence even defines the views from the left and from the right

```haskell
   data ViewL a
   = EmptyL
   | (:<) a (Seq a)

   viewl :: Seq a -> ViewL a

   data ViewR a
   = EmptyR
   | (:>) (Seq a) a

   viewr :: Seq a -> ViewR a
```


Thus, the presence of views has a direct impact on existing Haskell libraries. Arguably, a view proposal that wants to be effective for abstract data types likely has to have the transparent ordinary patterns feature.


The observations from [Okasaki: Breadth-First Numbering - Lessons ... ](http://citeseer.ist.psu.edu/356396.html) suggest that not having abstract pattern matching (for sequences) can indeed have great impact on the abstractions functional programmers can think of.

### Designing data structures


The abstractional power views offer can also be put to good use when designing data structures, as the following papers show

- [R.Hinze: A fresh look on binary search trees](http://www.informatik.uni-bonn.de/~ralf/publications/SearchTree.ps.gz).
- [R.Hinze:  A Simple Implementation Technique for Priority Search Queues](http://www.informatik.uni-bonn.de/~ralf/publications/ICFP01.pdf)

### Sets and Inductive Graphs


Having the value input feature, even set like data structures come in reach for pattern matching. In fact, the key idea of [M.Erwig: Inductive Graphs and Functional Graph Algorithms](http://web.engr.oregonstate.edu/~erwig/papers/InductiveGraphs_JFP01.ps.gz) is to introduce a suitable view of graphs. This way, graphs can be liberated from their notoriously imperative touch.


Here is a small module that allows to decompose sets with repsect to a given element, deleting it hereby.

```haskell
module Set( Set, empty, insert, delete, has) where

  newtype Set a = S [a]
  
  has :: Eq a => a -> Set a -> Maybe (Set a)
  has x (S xs) | x `elem` xs = Just (xs \\ x)
               | otherwise   = Nothing
  
  delete :: a -> Set a -> Set a
  delete x (has x -> s) = s
  delete x s            = s
  
  insert :: a -> Set a -> Set a
  insert x s@(has x -> _) = s
  insert x (S xs)         = S (x:xs)
```


Notice that in the left-hand side `delete x (has x -> s)`, the first `x` is a binding occurrence, but the second is merely an argument to `has` and is a bound occurrence.

### Erlang-style parsing


The expression to the left of the `->` can mention variables bound in earlier patterns.
For example, Sagonas et al describe an extension to Erlang that supports pattern-matching on bit-strings (["Application, implementation and performance evaluation of bit-stream programming in Erlang", PADL'07](http://user.it.uu.se/~kostis/Papers/index.html#Conference)).  Suppose we had a parsing function thus:

```haskell
  bits :: Int -> ByteString -> Maybe2 Word ByteString
  -- (bits n bs) parses n bits from the front of bs, returning
  -- the n-bit Word, and the remainder of bs
```


Then we could write patterns like this:

```haskell
  parsePacket :: ByteString -> ...
  parsePacket (bits 3 -> n (bits n -> val bs)) = ...
```


This parses 3 bits to get the value of `n`, and then parses `n` bits to get
the value of `val`.  

### N+k patterns


You can test for values.  For example here's a view function that tests for values 
greater than or equal to n:

```haskell
   np :: Num a => a -> a -> Maybe a
   np k n | k <= n    = Just (n-k)
	  | otherwise = Nothing

   f :: Num a => a -> a
   f (np 10 -> n) = 0		-- Matches values >= 10
   f (np 4  -> n) = 1		-- Matches values >= 4
   f other        = 2
```


You will recognise these as (n+k) patterns, albeit with slightly different syntax.
(Incidentally, this example shows that the view function can be overloaded, and
that its use in a view pattern gives rise to a type-class constraint (in this case,
that in turn makes `f` overloaded).

### Naming constants in one place


We are always taught to write magic numbers, or constants, in one place only.
In C you can write

```c
  #define ERRVAL 4
```


and then use `ERRVAL` in `switch` labels.  You can't do that in Haskell, which is tiresome.
But with view pattern you can:

```haskell
  errVal :: Int -> Bool
  errVal = (== 4)

  f (errVal ->) = ...
```

---

## Related work

### Wadler's original paper (POPL 1987)


Wadler's paper was the first concrete proposal.  It proposed a top-level view
declaration, together with functions *in both directions* between the view type
and the original type, which are required to be mutually inverse.  
This allows view constructors to be used in expressions
as well as patterns, which seems cool. Unfortunately this dual role proved
problematic for equational reasoning, and every subsequent proposal restricted
view constructors to appear in patterns only.

### [Burton et al views (1996)](http://haskell.org/development/views.html)


This proposal is substantially more complicated than the one above; in particular it
requires new form of top-level declaration for a view type. For example:

```haskell
  view Backwards a of [a] = [a] `Snoc` a | Nil
     where
     backwards [] = Nil
     backwards (x:[]) = [] `Snoc` x
     backwards (x1:(xs `Snoc` xn)) = (x1:xs) `Snoc` xn
```


Furthermore, it is in some ways less expressive than the proposal here;
the (n+k) pattern, Erlang `bits` pattern, and `regexp` examples are not
definable, because all rely on the value input feature.


I think this proposal is substantially the same as "Pattern matching and
abstract data types", Burton and Cameron, JFP 3(2), Apr 1993.

### [Okasaki: views in Standard ML](http://citeseer.ist.psu.edu/okasaki98view.html)


Okasaki's design is very similar to Burton et al's, apart from differences due
to the different host language.  Again, the value input feature is not supported.

### [Erwig: active patterns](http://citeseer.ist.psu.edu/erwig96active.html)


Erwig's proposal for active patterns renders the Set example like this:

```haskell
data Set a = Empty | Add a (Set a)

pat Add' x _ =
  Add y s => if x==y then Add y s
             else let Add' x t = s
                  in Add x (Add y t)

delete x (Add' x s) = s
delete x s          = s
```


This requires a new top-level declaration form `pat`; and I don't think it's nearly 
as easy to understand as the current proposal.  Notably, in the first equation for
`delete` it's hard to see that the second `x` is a bound occurrence; this somehow
follows from the `pat` declaration.


Still the proposal does support the value input feature.

### [Palao et al: active destructors (ICFP'96)](http://portal.acm.org/citation.cfm?id=232641&coll=portal&dl=ACM)


Active Desctructors (ADs) are defined by a new form of top-level declaration.  Our
singleton example would look like this:

```wiki
  Sing x match [x]
```


Here **match** is the keyword, and `Sing` is the AD.  ADs are quite like view patterns:
the can do computation, and can fail to match.  But they are definitely not normal 
Haskell functions, and need their own form of top-level declaration.  They even have
a special form of type to describe them.


The value-input feature is supported, but only via a sort of mode declaration (indicated by a down-arrow) on the
new form of type.


They also introduce a combining form for ADs, to make a kind of and-pattern.  For
example, suppose we had

```haskell
  Head x match (x:_)
  Tail x match (_:xs)

  f :: [a] -> [a]
  f ((Head x)@(Tail ys)) = x:x:ys
```


Here `(Head x)@(Tail ys)` is a pattern that matches *both* `(Head x)` and `(Tail ys)`
against the argument, binding `x` and `ys` respectively.  We can model that with view patterns,
only a bit more clumsily:

```haskell
  headV (x:xs) = Just x
  headV []     = Nothing

  tailV (x:xs) = Just xs
  tailV []     = Nothing

  (@) :: (a -> Maybe b) -> (a -> Maybe c) -> a -> Maybe (b,c)
  (f @ g) x = do { b <- f x; c <- g x; return (b,c) }

  f :: [a] -> [a]
  f (headV @ tailV -> (x,ys)) = x:x:ys
```


The clumsiness is that the "`@`" combines functions, with a kind of positional
binding; the pattern `(x,ys)` is separated from the combiner so that it's less clear
that `headV` binds `x` and `tailV` binds `y`.


In exchange, although view patterns are a bit less convenient here, they 
are a much, much smaller language change than ADs.

### [Erwig/Peyton Jones: transformational patterns](http://citeseer.ist.psu.edu/erwig00pattern.html)


This paper describes pattern guards, but it also introduces **transformational patterns**.  (Although
it is joint-authored, the transformational-pattern idea is Martin's.)  Transformational patterns
are very close to what we propose here.  In particular, view functions are ordinary Haskell functions,
so that the only changes are to patterns themselves.


There are two main differences (apart from syntax).
First, transformational patterns didn't have the value input feature, although it'd be easy 
to add (indeed that's what we've done). Second, transformational patterns as described by
Erwig do no stripping of the `Maybe` (see "Possible extension 2" above).

### [F\# Active Patterns](http://blogs.msdn.com/dsyme/archive/2006/08/16/ActivePatterns.aspx)


Simon started this design discussion after talking to Don Syme about F\#'s **active patterns**, which serve a very similar purpose. These combine both ???total??? discrimination (views) and ???partial??? discrimination (implicit maybe) into one mechanism. It does this by embedding the names of discriminators in the names of matching functions, via ???values with structured names???.  Sample uses include matching on .NET objects and XML.


Here is [a full paper describing the design](http://blogs.msdn.com/dsyme/archive/2007/04/07/draft-paper-on-f-active-patterns.aspx), by Don Syme, Gregory Neverov, and James Margetson (April 2007).


The feature is implemented in F\# 1.9. Some code snippets are below.

```haskell
    let (|Rect|) (x:complex) = (x.RealPart, x.ImaginaryPart)
    let (|Polar|) (x:complex) = (x.Magnitude , x.Phase)

    let mulViaRect c1 c2 = 
        match c1,c2 with 
        | Rect(ar,ai), Rect(br,bi) -> Complex.mkRect(ar*br - ai*bi, ai*br + bi*ar)

    let mulViaPolar c1 c2 = 
        match c1,c2 with 
        | Polar (r1,th1),Polar (r2,th2) -> Complex.mkPolar(r1*r2, th1+th2)

    let mulViaRect2  (Rect(ar,ai))   (Rect(br,bi))   = Complex.mkRect(ar*br - ai*bi, 
                                                                      ai*br + bi*ar)
    let mulViaPolar2 (Polar(r1,th1)) (Polar(r2,th2)) = Complex.mkPolar(r1*r2, th1+th2)
```


And for views:

```wiki
    open System
    
    let (|Named|Array|Ptr|Param|) (typ : System.Type) =
        if typ.IsGenericType        then Named(typ.GetGenericTypeDefinition(), 
                                               typ.GetGenericArguments())
        elif not typ.HasElementType then Named(typ, [| |])
        elif typ.IsArray            then Array(typ.GetElementType(), 
                                               typ.GetArrayRank())
        elif typ.IsByRef            then Ptr(true,typ.GetElementType())
        elif typ.IsPointer          then Ptr(false,typ.GetElementType())
        elif typ.IsGenericParameter then Param(typ.GenericParameterPosition, 
                                               typ.GetGenericParameterConstraints())
        else failwith "MSDN says this can't happen"

    let rec freeVarsAcc typ acc =
        match typ with
        | Named (con, args) -> Array.fold_right freeVarsAcc args acc
        | Array (arg, rank) -> freeVarsAcc arg acc
        | Ptr (_,arg)       -> freeVarsAcc arg acc
        | Param(pos,cxs)    -> Array.fold_right freeVarsAcc cxs (typ :: acc) 
```

### [Emir, Odersky, Williams: Matching objects with patterns](http://lambda-the-ultimate.org/node/1960)


Scala is an OO language with lots of functional features.  It has algebraic data types and
pattern matching.  It also has a form of view called **extractors**, which are
pretty similar to view patterns, albeit in OO clothing.  Notably, by packaging a constructor
and an extractor in a class, they can use the same class name in both expressions and terms, 
implicitly meaning "use the constructor in expressions, and use the extractor in patterns".


The paper does a comparative evaluation of various OO paradigms for matching, and 
concludes that case expressions and extractors work pretty well.

### Pattern synonyms

[Pattern synonyms](http://hackage.haskell.org/trac/haskell-prime/wiki/PatternSynonyms) 
are a requested Haskell Prime feature. John Reppy had the same idea years ago for Standard ML; see 
[Abstract value constructors](http://people.cs.uchicago.edu/~jhr/papers/1992/tr-sml-const.pdf), 
Reppy & Aiken, TR 92-1290, Cornell, June 1992.


The one way in which pattern synonyms are better than view patterns is that
they define by-construction bi-directional maps.  Example

```haskell
  data Term = Var String | Term String [Term]
  
  -- 'const' introduces a pattern synonym
  const Plus a b = Term "+" [a,b]

  f :: Term -> Term
  f (Plus a b) = Plus (f a) (f b)
  f ... = ...
```


With pattern views, we'd have to write two functions for the "plus" view:

```haskell
  plus :: Term -> Term -> Term
  plus a b = Term "+" [a,b]

  isPlus :: Term -> Maybe2 Term Term
  isPlus (Term "+" [a,b]) = Just2 a b
  isPlus other		  = Nothing

  f :: Term -> Term
  f (isPlus -> a b) = plus (f a) (f b)
```


But perhaps that is not so bad.  Pattern synonyms also require a new form of top level declaration;
and are much more limited than view patterns (by design they cannot do computation).

### [Tullsen: First Class Patterns](http://citeseer.ist.psu.edu/tullsen00first.html)


First Class Patterns is an approach that attempts to
add the minimum of syntax to the language which---in combination with
pattern combinators written within the language---can achieve everything
and more that Haskell patterns can do.  They have the value-input feature.


The advantages are  1) They are simpler than Haskell's patterns;  2) Patterns are first class.
3) The binding mechanism (the pattern binder) is orthogonal to the pattern combinators:
the hope is that one can stop changing the syntax/semantics of patterns and concentrate on writing the
combinators (as Haskell functions).


The disadvantages are as follows: 1) An extra syntactic construct that binds variables, the pattern binder, is required.
2) Even with pattern binders, simple patterns look clunkier than Haskell's patterns.
3) No attempt is made to check for exhaustiveness of patterns.
4) No attempt is made to integrate with Haskell's patterns, the idea is a proposal for an alternative when one needs more than simple patterns.


The examples at the top of this page would look like this with first class patterns:

```wiki
  f = {%sing n} .-> n+1
                |>> 0

  g =  {%sing True}  .-> 0
    .| {%sing False} .-> 1
                     |>> 2  
```

### First class abstractions



Several proposals suggest first class *abstractions* rather that first-class *patterns*.  By a "first class abstraction" I mean a value of type
(*something* `->` *something*)
with a syntax something like
(`\` *pattern* `->` *result*).
The abstraction includes both the pattern and the result.  In contrast, view patterns tackle only the syntax of patterns; the pattern of a first-class abstraction.  


Here are the ones I know of

- [Claus Reinke's lambda-match proposal](http://hackage.haskell.org/trac/haskell-prime/ticket/114)
- [Matthias Blume: Extensible programming with first-class cases](http://ttic.uchicago.edu/~blume/pub-cat.html) (ICFP06)


All these proposals are more or less orthogonal to this one. For example, Reinke
proposes a compositional syntax for lambda abstractions 
`(\p -> e)` where pattern matching failure on `p` can be caught and composed
with a second abstraction. Thus

```haskell
   (| Just x -> x+1 ) +++ (| Nothing -> 0 )
```


combines two abstractions, with failure from the first falling through to the second.  


None of these proposals say
anything about the patterns themselves, which in turn is all this
proposal deals with.  Hence orthogonal.

### Barry Jay: First class patterns


A yet more ambitious scheme is to treat patterns themselves as first class, even though they have free (binding) variables.  This is the approach that Barry Jay has taken in his very interesting project on the *pattern calculus*.  His [home page](http://www-staff.it.uts.edu.au/~cbj) has more info.
