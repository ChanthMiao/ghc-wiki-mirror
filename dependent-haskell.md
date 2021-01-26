This page is to track design and implementation ideas around adding a form of dependent types to Haskell.

***Disclaimer:*** Everything below represents a research proposal. While it is my (RAE's) hope that something resembling this all will actually make it into GHC, no one should read anything too strongly into words like "will happen".

## What is Dependent Haskell?

The term "dependently typed programming language" covers a huge range of designs, and there is
a danger that we'll each have somethign different in mind.  So this wiki page outlines
one particular part of the design space, the one that Richard and Stephanie have in mind.
It's not the only possible design -- and in any case it's not a fixed design, more sub-space
of the huge design space -- but perhaps it can serve as a concrete baseline to help bring clarity
to our discussion.

Here, then, are the design principles for Dependent Haskell:

### 1. Type inference

Dependent Haskell embodies type inference, just like Haskell.  Indeed, every Haskell
program is a DH program: no extra type annotations are required.

This stands in contrast to some dependently-typed languages (which?) that require every
binder to be explicitly type-annotated.

Of course, just as in GHC-Haskell today, to reach the more sophisticated corners of
the type system the programmer must supply some type annotates, but the goal is to have
simple, predictable rules to say when such annotations are necessary.


### 2. Erasure

In DH, *the programmer knows, for sure, which bits of the program will be
retained at runtime, and which will be erased*.  Some dependently
typed languages (which?) leave this choice to a compiler analysis, but in DH
we make it fully explicit in the types.

We will see under "Quantifiers" below exactly *how* this is made explicit to the programmer,
but it is a key property that there should be absolutely no ambiguity about it.
Haskell has very strong erasure properties, and so does DH.


### 3. **Term and type syntax**

In Haskell,

* **Types** appear
  * in type or class declarations,
  * after `::` in a type or kind signature, and
  * after the "`@`" sign in visible type application.

* **Terms** appear in value declarations, such as  `f x = x+1`.

Terms and types have different name-spaces, which allows "punning". We can write
```
data Age = Age Int

f :: Age -> Age         -- Type
f (Age n) = Age (n+1)   -- Term
```
We have the type constructor `Age` in the type namespace, and an eponymous data constructor `Age` in the term namespace.
When renaming a type, we look up in the type namespace, while when renaming a term we look up in the term namespace.
("Renaming" means resolving, for each occurrence of an identifier, what is the binding site to which that occurrence refers.)

With `DataKinds` we already flex these rules a bit: when renaming a type, if `T` is not in scope in the type namespace we look in the term namespace (for a data constructor `T`).  And we provide an escape mechanism, the tick-mark: in a type, `'T` refers unconditionally to the term namespace.

In DH, *we expect to retain this dual namespace unchanged*:
* In the syntactic places where types appear in Haskell today, DH will continue to use the type namespace.
* In the syntactic places where terms appear in Haskell today, DH will continue to use the term namespace.

In a dependently typed language the distinction between "types" and
"terms" blurs or disappears entirely, so we will use the terms "type
syntax" and "term syntax" for these two syntactic location.  For GHC
aficionados, type syntax is represented with `HsType`, while term
syntax is represented with `HsExpr`.


### 4. **Quantifiers**.

There are three "attributes" to a quantifier
```
Attribute    |  What it means
-----------------------------------------------
Static-ness  |  Compile-time reasoning about equality
Visibility   |  Argument is explicit at both definition and call site
Erasure      |  Completely erased at runtime.  Aka "relevance"
```
As the Hasochism paper points out, in ML, and largely in Haskell, these
three attributes are treated differently in types and terms, thus:
```
Attribute   |    Types       |   Terms       |
-------------------------------------------  --------------
Quantifier  | forall a. ty   |   t1 -> t2    |
            |                |               |
Static-ness | Static         |  Non-static   | Compiler reasons about equality of types,
            |                |               |   but never of terms
Visibility  | Invisible      |  Visible      | Programmer never supplies type arguments,
            |                |               |   always supplies value arguments
Erasure     | Erased         | Retained      | Types completely erased at runtime;
            | aka Irrelevant | aka Relevant  |    terms never erased
```

NB: visible type application in GHC Haskell adds a refinement to this
setup, by allows the programmer to give a visible type argument `(e @ty)`
to a term `(e :: forall a.blah)`.  But the basic setup is as above.

**A key aspect of a dependently typed language is that visibility and erasure
can be chosen independently**.   (See "The Glorious Future" for static-ness.)
To cut to the chase, we have (interchanging rows and columns)
```
                  ------------  Attribute ------------------
Quantifier        Static-ness    Visibility     Erasure
------------------------------------------------------------
forall a. ty      Static         Invisible      Erased
forall a -> ty    Static         Visible        Erased
foreach a. ty     Static         Invisible      Retained
foreach a -> ty   Static         Visible        Retained
Eq a => ty        Dynamic        Invisible      Retained
t1 -> t2          Dynamic        Visible        Retained
```
You can see that
* The `forall` vs `foreach` part governs erasure: `foralls` are erased, 
* The "`.`" vs "`->`" part of

* GHC already supports `forall k -> ty`, in *kinds*, meaning that the programmer must apply
  a type `(T :: forall k -> ty)` to an explicit kind argument
  ([GHC proposal 81, visible dependent quantification](https://github.com/ghc-proposals/ghc-proposals/pull/81).  For example:
  ```
  data T k (a::k) = ...
  ```
  Here an application of `T` must look like `T Type Int`, where `T` is explicitly applied to the kind `Type`.
  We can tell that from its kind: `T :: forall k -> k -> Type`.

* The two `foreach` quantifiers are new.  They allow us to have an argument (visible or invisible)
  that:
  * Can appear in the rest of the type. E.g. `f :: foreach (a::Bool). T a -> Int`.
  * Is reasoned about at compile time.  E.g. `f True x` is type-incorrect if `x :: T False`.
  * Is passed at runtime (just like `(Eq a => blah)`).

* Even in GHC Haskell today there are no terms of type `forall a -> blah`, even though
  that is a well-formed type.  But in DH there are such terms:
  ```
  f : forall a -> a -> Int
  f a (x::a) = 4     -- The pattern signature on (x::a) is optional
  ```
  This is natural extension of what happens at the type level, where you can write
  ```
  type T :: forall k -> k -> Type
  data T k (a::k) = MkT    -- The kind signature on (a::k) is optional
  ```

* The `foreach ->` quantifier allows us to eliminate the vast mess of singleton types,
  about which the Hasochism paper is eloquent. For example, today we are sometimes forced
  to write
  ```
  data Nat = Z | S Nat
  data Natty (n::Nat) where
    Zy :: Natty Z
    Sy :: Natty n -> Natty (S n)
  zeroVec :: forall (n::Nat). Natty n -> Vec n
  zeroVec n = ...
  ```

  Here, `Natty` is a singleton type, mirroring `Nat`.  But it's
  terribly painful to construct these singleton values at call sites.  With
  `foreach` we can say what we want directly:
  ```
  zeroVec :: foreach (n::Nat)-> Vec n
  zeroVec n = ...
  ```
  and a call might look like `zeroVec 7`.

* The `foreach .` quantifier does the same thing for invisible
  arguments (not written by the programmer).  In Haskell today we have
  to encode that even further
  ```
  class NATTY (n::Nat) where
    natty :: Natty n
  ```
  Now we can write
  ```
  foo :: forall (n::Nat). NATTY n => blah
  ```
  Now, at a call site for `foo` the compiler will figure out the evidence for `NATTY n`, and will
  construct a value that is passed, at runtime, to `foo`.

  Again, the encoding is heavy (read Hasochism); with `foreach` we can write
  ```
  foo :: foreach (n::Nat). blah
  foo = ...n...
  ```
  and at call sites the compiler will work out a suitable `Nat` to pass to `foo`.

### 4. **Dependent application**

Suppose we have a function `f :: foreach (a::ty) -> blah`.  Then at a
call site the programmer must supply an explicit argument, so the call will look like
```
  f <arg>
```
**Question 1**: is `arg` written in term syntax or in type syntax?  Answer: in term syntax.

Recall that term-syntax vs type-syntax affects both which syntactic forms are allowed, and
what namespace is used during renaming.  But during parsing and renaming *we do not know the type of `f`*,
and DH maintains Haskell's separation of renaming and typechecking.  So we can only use term syntax for `arg`,
and the term namespace for resolving identifier occurrences in `arg`.

**Question 2**: can `arg` be *any* expression whatsoever? Lambdas?
List comprehensions?  Applicative do?  Local function bindings?

Ultimately we hope that the answer will be "yes", but DH is carefully crafted so that we do not need
a "big bang" to get there.  Rather, we can move incrementally, one step at a time.  Here's how:

* `arg` is *parsed* as a term (an `HsExpr` in GHC-speak)
* `arg` is *renamed* as a term
* But during *typechecking* the compiler treats an application chain `f arg1 arg2 ... argn` specially.
  If it knows that `f :: forall a -> blah`, then it checks that `arg1` is a term written only in a
  specified sub-language of terms --- initially a sub-language that maps directly to the language of (current) types.

We call this "specified sub-language of terms" the Static Subset of terms.  In GHC-speak,
a `HsExpr` in the Static Subset can readily be converted to a `HsType`.

For example, suppose `f :: foreach (a :: [Bool) -> blah`.  An initial version of DH might allow
```
  f [True]            -- Allowed
  f [True,False]      -- Allowed
  f (True : [])       -- Allowed
  f xs                -- Allowed iff xs is static

  f (reverse xs)      -- Not allowed: typechecker does not understand reverse
  f [not x | x <- xs] -- Not allowed: list comprehension
```
Over time we expect to widen Static Subset of terms, to allow more syntactic forms.

The technology for treating application chains specially is worked out in details in
[A quick look at impredicativity](https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/).
It is *already* used to govern Visible Type Application (which also requires knowledge of whether the
function part of the application has a forall-type). This aspect is well understood.

### 5. The Glorious Future

One glorious day, perhaps all terms will be understood by the static type
checker.  To put it another way, any term whatsoever will be
acceptable as an argument to `f :: foreach a -> blah`; and any term
whatsoever would be acceptable in a type or kind signature.  (NB:
Richard and Stephanie definitely want this.  Simon is not yet convinced
that the pain will be worth the gain.)

If that Glorious Day comes, the Static vs Non-static distinction will
vanish.  *That is why DH makes no attempt to offer independent
programmer control over that aspect*.  (This is in contrast to erasure
and visibility, for which we very much *do* want programmer control;
indeed that is the whole point.)

Instead DH simply imposes restrictions on the terms that can be seen by
the static type checker, and ensures that they lie within its ability
to reason.

Note: fully-spectrum dependently typed languages treat `t1 -> t2` as a mere abbreviation of
`foreach (_ :: t1) -> t2`.  But until the Glorious Day, DH will treat these two very
differently:
* If `f1 :: t1 -> t2`, then in a call `(f1 arg)`, there are no restrictions on `arg` (except of course that it has type `t1`).
* If `f2 :: forall (_ :: t2) -> t2`, then in a call `(f2 arg)` arg must lie in the Static Subset of terms.


### 6. Details, details.

* We have a way to escape from type namespace to term namespace
  (tick-marks).  But current proposal is not to supply a way to escape
  from term to type; use a synonym.


----------------------------------------
# Overview

### What does adding dependent types to Haskell mean? 

Like most other features in Haskell, dependent types can be understood as a set of extensions, designed to work together well, that enable a certain style of programming. Critically, it is expected that few (if any) changes due to dependent types would invalidate traditional Haskell syntax. (Exception: we may need to introduce new keywords, such as `forall` and `foreach`.) Because adding dependent types is a large project, it will evolve slowly over a number of years. When we are done, I imagine that we will have an extension `-XDependentTypes` that implies others as necessary.

There are several key aspects of dependent types:

* Dependency: the types of later arguments to a function (or its return type) may depend on earlier ones. We actually have this today: when I say `id @Bool True`, the expected type of `True` depends on my choice of `Bool` as the first argument. The new part is that the arguments depended on would also be relevant at runtime. An example would be a call `replicate 3 'x' :: Vec 3 Char`. Note that the result mentions the `3` passed in as an argument.

* Informative pattern-match: When we pattern-match on a value that also appears in a type, the type-checker can use the matched-against pattern to refine the type. For example, consider an implementation of `replicate`:

```hs
replicate :: foreach (n :: Nat) -> a -> Vec n a
replicate Zero     _ = Nil
replicate (Succ n) x = x :> replicate n x
```

The right-hand side must have a type `Vec n a` -- but `n` is the first pattern to be matched against. Thus, when I write `replicate Zero _`, the right-hand side can have type `Vec Zero a`. This is the essence of informative pattern-matches (also called dependent pattern-match).

* No syntactic separation between types and terms: Current Haskell uses the formidable `::` to keep types and terms separate. In a dependently typed language, there is no need or desire for such a separation. The right-hand side of the type marker `::` would be an expression of type `Type`, but the expression would be written in the same grammar as any other expression. This is a great *simplification* over the status quo.

* Type erasure: Haskell's efficiency is, in part, driven by the fact that compiling Haskell erases types. In a world with dependent types, when information from types can influence runtime decisions, how would this work? Every dependent quantification would be marked as either relevant (not erasable) or irrelevant (erasable). In the current vision, this is done by using `foreach` for relevant quantification and `forall` for irrelevant quantification. (But new research suggests that the way we denote relevance should line up with the way we denote linearity. See this [POPL 2021 paper](https://arxiv.org/abs/2011.04070).) So programmers would still have to think about what information to preserve at runtime. We can imagine implementing warnings when a programmer retains unnecessary information.

* Full expressiveness: One worry that some have about dependent types is that other dependently typed languages sometimes require all functions to be proved to terminate. (For example, Agda will not accept a transliteration of

```hs
step :: Natural -> Natural
step n
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

collatz :: Natural -> Natural
collatz 0 = 0
collatz 1 = 0
collatz n = 1 + collatz (step n)
```

without a proof that `collatz` terminates. Do let me know if you have such a [proof](https://en.wikipedia.org/wiki/Collatz_conjecture).) Backward compatibility (and the usefulness of not-known-to-terminate functions, such as interpreters) compels us to avoid adding this requirement to Haskell. Perhaps someday we will add a termination checker has an aid to programmers, but it will not be required for functions to terminate. Due to the way dependent types in Haskell are designed (e.g., as explained in this [ICFP'17 paper](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf)), it is not necessary to have a termination proof to support dependent types.

* No more singletons: In order to cope with the lack of dependent types today, we have the [`singletons`](http://hackage.haskell.org/package/singletons) package. One marker of the arrival of dependent types is when that package can be deprecated.

# Surface Language Design

## Quantifiers


As pointed out in the [Hasochism paper](dependent-haskell#), Haskell currently enjoys a confluence of design decisions. One says that compile-time arguments are elided in runtime code. For example, when calling `map :: (a -> b) -> [a] -> [b]`, the type instantiations for `a` and `b` are properly arguments to `map` (and are passed quite explicitly in Core), but these arguments are always elided in surface Haskell. Along similar lines, type arguments in Haskell are always erasable -- that is, instantiations for types are never kept at runtime. While this is generally a Good Thing and powers much of Haskell's efficiency, dependent typing relies on keeping *some* compile-time information around at runtime. Here, it is even more apparent that sometimes, we want to be able to pass in values as dependent arguments, especially if those values can be inspected at runtime.

Haskell currently has three quantifiers: `forall`, `->`, and `=>`, as classified in the following table:

| Quantifier | Dependent? | Visible? | Relevant? | Valid in types of terms? | Valid in types of types? | Extension |
| ------ | ------ | ----- | ----- | ----- | ----- | ----- |
| `forall ... .` | Yes | No (unification) | No | Yes | Yes (but these are relevant) | `-XExplicitForAll` |
| `forall ... ->` | Yes | Yes | Yes | No (proposal [#281](https://github.com/ghc-proposals/ghc-proposals/pull/281)) | Yes | `-XPolyKinds` |
| `->` | No | Yes | Yes | Yes | Yes | N/A |
| `=>` | No | No (solving) | Yes | Yes | Yes (only equality constraints) | N/A |

**Dependent**

*Dependent* means that the quantified thing (henceforth, *quantifiee*) can appear later in the type. This is clearly true for `forall`-quantified things and clearly not true for `->`-quantified things. (That is, if we have `Int -> Bool`, we can't mention the `Int` value after the `->`!)

**Visible**
*Visibility* refers to whether or not the argument must appear at *definitions* and *call sites* in the program text. If something is not visible, the table lists how GHC is to fill in the missing bit at call sites. If something is visible, we must specify how it is parsed, noting that the term- and type-level parsers are different. For example (not implemented, but see https://github.com/ghc-proposals/ghc-proposals/pull/281):

```wiki
-- Invisible ----
f1 :: forall a. a -> a
f1 x = x

g1 x = f1 True

-- Visible ----
f1 :: forall a -> a -> a
f1 a x = x 

g1 x = f1 Bool True
```

Same at the type level (but this *is* implemented):

```wiki
-- Invisible ----
type Proxy1 :: forall k. k -> *
data Proxy1 (a :: k) = P1
f1 :: Proxy1 Int -> Bool

-- Visible ----
type Proxy2 :: forall k -> k -> *
data Proxy2 k (a :: k) = P2
f2 :: Proxy2 Type Int
```

**Relevance**

*Relevance* refers to how the quantifiee can be used in the term classified by the type in question. For terms and their types, a binder is relevant iff it is not erased; that is, it is needed at runtime. In GHC terms, relevant binders are *Id*s and irrelevant ones are *TyVar*s.

For types and their kinds, we can't talk about erasure (since the are all erased!) but the relevance idea works the same, one level up.  Example


```wiki
type family Id (x::k1) (y::k2) :: (k1,k2) where
  Id True  v = (False, v)
  Id False v = (True,  v)
  Id x     v = (x,     v)
```

If `Id`'s kind was `forall k1 k2. k1 -> k2 -> (k1,k2)`, it looks parametric in both `k1` and `k2`.  But it isn't, because it can pattern-match on `k1`.  So `k1` is relevant, but `k2` is irrelevant.

(This is distinct from dependence, which says how the quantifiee can be used in the *type* that follows!) `forall`-quantifiees are not relevant. While they can textually appear in the classified term, they appear only in irrelevant positions -- that is, in type annotations and type signatures. `->`- and `=>`-quantifiees, on the other hand, can be used freely. 

Having explained our terms with the current Haskell, the proposed set of quantifiers for dependent Haskell is described in [Proposal #102](https://github.com/ghc-proposals/ghc-proposals/pull/102).

Declarations given without a type signature will need to perform *relevance inference* to figure out whether quantified variables should be `forall`-bound or `foreach`-bound. Relevance inference simply looks at usage sites; iff a variable is used in a relevant context (scrutinee of a pattern-match, or passed to a function expecting a relevant argument, among others) then it is relevant. It is tempting to perform relevance inference if the nature of a quantifier is omitted in a type signature, but this would make the type signature's meaning depend on the term, which may mean that a variable's type signature is not the authoritative type for the variable.

**Datatypes**

How is the kind of a datatype classified? After some debate, Stephanie and RAE thought that a poly-kinded datatype should be quantified with `foreach`, not `forall`. For example, consider `data Proxy (k :: *) (a :: k) = Proxy`. Is its kind `forall (k :: *). k -> *` or `foreach (k :: *). k -> *`. Let's consider the former type as if it classified a term-level function. That function would have to be a constant function, by parametricity. Yet, we do *not* want `Proxy * Bool` to be the same as `Proxy Nat Zero`. So, we choose the latter classifier.

This choice matters in how datatypes are used in pattern-matching situations. For example, is this possible: `type instance F (Proxy (a :: k)) = k`? The result uses `k` in a relevant context, so it must be that `k` is introduced in a relevant context, and thus that it must be `foreach`-quantified. If we wanted to introduce `forall`-quantification in datatypes, then the use of these variables would be restricted during matching. Is this a good idea? Is this necessary? And, it seems that `foreach` will be default in datatypes but `forall` will be default in type families. This is strange.

## Open design questions

### Parsing/namespace resolution

Parsing is a bit of a nightmare for this new language and will require some compromises.

- The type language and the term languages are more different. There are two proposals on the table to deal with types embedded in terms:

  - **Keep the parsers separate**: Since the parsers are separate and the parser certainly doesn't know anything about types, we need some indication in the code as a signal to the parser to switch to the type parser. Due to the construction of GHC's parser, this indicator would have to come *before* the phrase in question.

    - Option 1: We can use `@` to indicate that we are parsing a type. Take `id :: forall a -> a -> a`. This is just like the normal `id`, but with the type parameter explicit. Because the parser/renamer won't know the type of `id` when parsing its arguments, the first argument will have to manifestly be a type. For example, `id @Bool True`. The `@` indicates to the parser that the following thing is a *type*, not a *term*.
    - Option 2: We can use the keyword `type` to indicate that we are parsing a type.

    Both of these are somewhat painful, in that they require users to keep track of the difference between terms and types, something that really should be done away with in order to have an ergonomic dependently typed language.

  - **Merge the parsers**: It may be possible to merge the term/type parsers. This would make `forall` a proper keyword. `(->)` and `(=>)` are already unusable at the term level. `\` is already unusable at the type level. One possible conflict is that `'` is used in types to indicate namespace and it is used in terms to indicate Template Haskell quoting. In any case, this all seems at least possible to consider. 
    Even if we could write some kind of combined parser, the renamer would have major trouble distinguishing between data constructors and type constructors. 

    - Option 1: Use `'` to write data constructors in types and use `^` to write type constructors in terms. The first of these is already implemented. The second is up for debate. Do these operators work only on individual identifiers? Or, can we say `f ^(...)` to make everything in the `...` be treated like a type?
    - Option 2: Use `'` to mean "switch default" -- it goes in either direction.
    - Option 3: Discourage the use of identifiers that appear in both contexts, providing a module-based mechanism for disambiguation (much like we disambiguate other constructs). See [Proposal #270](https://github.com/ghc-proposals/ghc-proposals/pull/270).

- We will similarly need a syntax for type patterns embedded within term patterns. It would be ideal if the pattern syntax were identical to the expression syntax.

- Regardless of other choices above, simple cases might be able to remain simple. For example, `f Bool` will surely parse as a term. When the renamer can't find the data constructor `Bool`, it could be smart enough to look for a type constructor `Bool` and get on with it.

### Overriding visibility defaults

The `.`/`->` distinction in quantifiers allows programmers to specify the visibility of arguments at use sites. But, sometimes callers will want to override the defaults.

- If a visible, dependent argument is to be elided, we could allow `_` to indicate that GHC should use unification to fill in the argument. (This is similar to the approach in Coq, for example, among other languages.) Does this conflict in any way with typed holes? Perhaps a programmer wants to get an informative error message, not for GHC to plug in a value. See [Proposal #194](https://github.com/ghc-proposals/ghc-proposals/pull/194) for more discussion.

- Visible, non-dependent arguments cannot be inferred via unification, so `_` would not be applicable here, and would retain its current meaning of a typed hole.

- How to override an invisible, dependent type argument? This might be critical if a function call would be otherwise ambiguous. It should be marked with `@` -- simple.

# Type Inference


Figuring out type inference for this language is a bit of a challenge. While the full design won't be laid out here, interesting tidbits will be gathered here for easy retrieval.

### Inferring `foreach` types


Suppose a programmer writes, without a type signature

```wiki
foo @Zero y = y
```


What is `foo`'s type? It could be `foreach (n :: Nat). forall (a :: *). Vec a n -> Vec a Zero`. It could also be `forall (n :: Nat) (a :: *). a -> a`. Neither is more general than the other -- we are in the same GADT type-inference problem as described in the [OutsideIn](http://research.microsoft.com/en-us/um/people/simonpj/papers/constraints/jfp-outsidein.pdf) paper. Thus, we reject such a `foo` that matches on an implicit parameter without a type signature.


But, what about

```wiki
foo Zero y = y
```


We are actually in the same situation here. But, backward compatibility compels us to prefer non-dependent types over dependent ones, inferring `foo :: forall (a :: *). Nat -> a -> a`. (Note that `foo :: forall (a :: *). foreach (n :: Nat) -> Vec a n -> Vec a Zero` is a valid but incomparable type that we could assign.)

When do `foreach`-types get inferred, if ever? Good question.

# Implementation

*Kind equalities* will be part of GHC 8. Merging commit: [67465497](https://github.com/ghc/ghc/commit/6746549772c5cc0ac66c0fce562f297f4d4b80a2). It was developed in [ Eisenberg's nokinds tree](https://github.com/goldfirere/ghc/tree/nokinds).

# Related work

**Readers:** Please add to these lists!


There are several published works very relevant to the design:

- [System FC with Explicit Kind Equality](https://www.cis.upenn.edu/~justhsu/docs/nokinds.pdf). Stephanie Weirich, Justin Hsu, and Richard A. Eisenberg. ICFP 2013.
- [Type Inference, Haskell, and Dependent Types](http://adam.gundry.co.uk/pub/thesis/thesis-2013-12-03.pdf). Adam Gundry. PhD Thesis, 2013.
- Eisenberg's thesis: [https://github.com/goldfirere/thesis](https://github.com/goldfirere/thesis)


There are also many works addressing the use of dependent types in Haskell. Here is a selection:

- [Dependently typed programming with singletons](http://www.cis.upenn.edu/~eir/papers/2012/singletons/paper.pdf). Richard A. Eisenberg and Stephanie Weirich. Haskell Symposium 2012.
- [Hasochism: The Pleasure and Pain of Dependently Typed Haskell](https://personal.cis.strath.ac.uk/conor.mcbride/pub/hasochism.pdf). Sam Lindley and Conor McBride. Haskell Symposium 2013.
- [Promoting Functions to Type Families in Haskell](http://www.cis.upenn.edu/~eir/papers/2014/promotion/promotion.pdf). Richard A. Eisenberg and Jan Stolarek. Haskell Symposium 2014.
