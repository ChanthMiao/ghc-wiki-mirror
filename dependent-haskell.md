The term "dependently typed programming language" covers a huge range of designs, and there is
a danger that we'll each have something different in mind.  So this wiki page outlines
one particular part of the design space, the one that Richard and Stephanie have in mind.
It's not the only possible design -- and in any case it's not a fixed design, more sub-space
of the huge design space -- but perhaps it can serve as a concrete baseline to help bring clarity
to our discussion.

Readers may also want to check out https://github.com/ghc-proposals/ghc-proposals/pull/378, a proposal to make sure that GHC continues to support development toward the language imagined here.

Given the Haskell's community lack of experience with dependent types, there are also a number of misconceptions that have arisen around the design of dependent types. A section at the end of this document describes several common misconceptions   and better ways of understanding certain design points.

Here, then, are the design principles for Dependent Haskell.

[[_TOC_]]

# Design for Dependent Haskell

## 1. Type inference

Dependent Haskell embodies type inference, just like Haskell.  Indeed, every Haskell
program is a DH program: no extra type annotations are required.

This stands in contrast to some dependently-typed languages (e.g. Agda, Idris) that require every
binder to be explicitly type-annotated.

Of course, just as in GHC/Haskell today, to reach the more sophisticated corners of
the type system the programmer must supply some type annotations (for example, define
higher-rank types, guide impredicative type inference, check GADT pattern-matches), but the goal is to have
simple, predictable rules to say when such annotations are necessary.

## 2. Erasure

In DH, *the programmer knows, for sure, which bits of the program will be
retained at runtime, and which will be erased*. We shall call this the **Predictable Erasure Principle (PEP)**. Some dependently
typed languages (Idris1, but notably not Idris2) leave this choice to a compiler analysis, but in DH
we make it fully explicit in the types.

We will see under "Quantifiers" below exactly *how* this is made explicit to the programmer,
but as erasure is such a key property, there should be absolutely no ambiguity about it.
Haskell has very strong erasure properties, and so does DH.

Just as in Haskell today, some programmers may prefer to omit the annotations that guide
erasure, and GHC will infer how much it can erase (choosing to erase as much as possible).
The one exception to this is in datatypes, where erasure must always be made explicit (otherwise,
GHC has no way to know what should be erased, unlike in functions).

## 3. Lexical scoping, term-syntax and type-syntax, and renaming

### Status quo

Haskell adheres to the following principle

* **Lexical Scoping Principle (LSP)**. For every *occurrence* of an identifier, it is possible to uniquely identify its *binding site*, without involving the type system.

This allows a compiler to proceed in two phases:
* *Rename* the program, by deciding, for every occurrence, what its corresponding binder is.
* *Typecheck* the program.

This two-stage approach is not just an implementation matter: it makes the language easier to describe to Haskell's users, by separating the concerns of *scoping* and *typing*.

A Haskell program contains both types and terms:

* **Types** appear
  * in type or class declarations,
  * after `::` in a type or kind signature, and
  * after the "`@`" sign in visible type application.

  We say that the bits of the program in these places as written in **type-syntax**.

* **Terms** appear in value declarations, such as  `f x = x+1`.  We describe them as written in **term-syntax**.

(GHC aficionados know type-syntax as `HsType` and term-syntax as `HsExpr`.)

Term-syntax and type-syntax have different name-spaces, which allows "punning". We can write

```hs
data Age = Age Int

birthday :: Age -> Age         -- Type
birthday (Age n) = Age (n+1)   -- Term
```

We have the type constructor `Age` in the type namespace, and an eponymous data constructor `Age` in the term namespace.
When renaming a type, we look up in the type namespace, while when renaming a term we look up in the term namespace.
("Renaming" means resolving, for each occurrence of an identifier, what is the binding site to which that occurrence refers.)

### Changes to support dependent types

In DH, *we support the same Lexical Scoping Principle, including Haskell's dual namespace*, slightly generalized:
1. In type-syntax, DH will continue to use the type namespace.
2. In term-syntax, DH will continue to use the term namespace.
3. When a lookup in the primary namespace fails, DH will look in the other namespace.

Point (3) is a natural extension of today's `DataKinds` approach.  With `DataKinds`, when renaming a type, if `T` is not in scope in the type namespace we look in the term namespace (for a data constructor `T`).  (We also provide an escape mechanism, the tick-mark: in a type, `'T` refers unconditionally to the term namespace, and we might consider extending that escape to lower-case variables in DH.)

Due to this extra lookup, the implicit quantification in type signatures (e.g. `f :: a -> a`, where `a` is implicitly quantified, making the type read `f :: forall a. a -> a`) would happen only for variables that are in scope in neither namespace. For backward compatibility, this change to implicit quantification would likely be guarded by an extension flag.

DH programmers may find it convenient to avoid punning, so that they no longer need
to consider the context of an identifier occurrence to be able to interpret its meaning.
(That is, to understand an occurrence `Age` in the example above, we need to look around
to see what context we are in.) We expect DH to support these programmers' desire to avoid
punning by providing optional warnings,
while still also supporting easy interaction with other code that uses puns.
[Proposal 270](https://github.com/ghc-proposals/ghc-proposals/pull/270) describes
a way that might happen; the additional support of [local modules](https://github.com/ghc-proposals/ghc-proposals/pull/283)
would allow for even easier use of punned identifiers in pun-avoiding code.

### Syntactic unification

Going further, we aim to support the following principle:

**Syntactic Unification Principle (SUP).** In the absence of punning, there is no difference
between type-syntax and term-syntax.

This is a *long term* goal: see [The Glorious Future](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell#11-the-glorious-future) below.  It is *not* true of Dependent Haskell as described here: type-syntax is, for now, a proper subset of term-syntax. We describe this further in [Dependent application and the Static Subset](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell#6-dependent-application-and-the-static-subset).
However, from a *scoping* point of view, it is already true: absent punning, you do not need to reason about term-syntax vs type-syntax when resolving scopes.

The Syntactic Unification Principle means that a DH programmer who avoids punning can (in the end) simply forget about the distinction
between type-syntax and term-syntax, and the context-sensitivity these notions require. This is meant
to be a simplification available to those programmers. As we design DH, this principle informs
design decisions, so that it may be true once DH is fully realized.

### Switching syntaxes

Given that some programmers will continue to use punning, it may be necessary to explicitly tell GHC to switch syntaxes. As originally described in (#281)[https://github.com/int-index/ghc-proposals/blob/visible-forall/proposals/0000-visible-forall.rst#43secondary-change-type-herald], we propose using the keyword `type` to tell GHC to switch to interpreting type-syntax, not term-syntax. This changes both parsing and name resolution. For example, we might say `sizeof (type Bool)` to allow disambiguation between a `Bool` in the term-level namespace and one in the type-level namespace. We can similarly imagine a `data` herald to switch to the term-level namespace.

There are some details to be worked out here (e.g. the precise BNF), but a disambiguation syntax may be necessary, and this section suggests a way to accommodate one.

## 4. Quantifiers

There are three "attributes" to a quantifier

```
Attribute    |  What it means
-----------------------------------------------
Dependence   |  The argument appears later in the type
Visibility   |  Argument is explicit at both definition and call site
Erasure      |  Completely erased at runtime.  Aka "relevance"
```

As the [Hasochism](http://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf) paper points out, in ML, and largely in Haskell, these
three attributes are treated differently in types and terms, thus:

```
Attribute   |    Types       |   Terms        |
------------------------------------------------------------
Quantifier  | forall a. ty   |   t1 -> t2     |
            |                |                |
Dependence  | Dependent      |  Non-dependent | Compiler reasons about equality of types,
            |                |                |   but never of terms
Visibility  | Invisible      |  Visible       | Programmer never supplies type arguments,
            |                |                |   always supplies value arguments
Erasure     | Erased         | Retained       | Types completely erased at runtime;
            | aka Irrelevant | aka Relevant   |    terms never erased
```

NB: visible type application in GHC Haskell adds a refinement to this
setup, by allowing the programmer to give a visible type argument `(e @ty)`
to a term `(e :: forall a.blah)`.  But the basic setup is as above.

**A key aspect of a dependently typed language is that these three
can be chosen independently**.
To cut to the chase, we have (interchanging rows and columns)
```
                  ------------  Attribute ------------------
Quantifier        Dependence     Visibility     Erasure
------------------------------------------------------------
forall a. ty      Dependent      Invisible      Erased
forall a -> ty    Dependent      Visible        Erased
foreach a. ty     Dependent      Invisible      Retained
foreach a -> ty   Dependent      Visible        Retained
Eq a => ty        Non-dependent  Invisible      Retained
t1 -> t2          Non-dependent  Visible        Retained
```
You can see that
* The `forall` vs `foreach` part governs erasure: `forall`s are erased, while `foreach`s are retained. `foreach` is the default quantifier that appears in Coq, Agda, and Idris; it is also known as `∏` in the literature.

* The "`.`" vs "`->`" part governs visibility: `.` says "invisible", while `->` says "visible"

* The presence of `forall`/`foreach` (vs having neither) governs dependence: These dependent quantifiers introduce a variable that
  can be used later in the type. Other abstractions (e.g. `->`) do not.

* There appear to be two missing rows. Non-dependent, erased arguments cannot be used at compile-time
  or at runtime, and are thus useless and omitted.

* GHC already supports `forall k -> ty`, in *kinds*, meaning that the programmer must apply
  a type `(T :: forall k -> ty)` to an explicit kind argument
  ([GHC proposal 81, visible dependent quantification](https://github.com/ghc-proposals/ghc-proposals/pull/81)).  For example:
  ```
  data T k (a::k) = ...
  ```
  Here an application of `T` must look like `T Type Int`, where `T` is explicitly applied to the kind `Type`.
  We can tell that from its kind: `T :: forall k -> k -> Type`.


* [Proposal 281](https://github.com/ghc-proposals/ghc-proposals/pull/281) extends the `forall ->` quantifier to *types* as well as *kinds*.  For example, we could then write
  ```
  f :: forall a -> a -> Int
  f a (x::a) = 4     -- The pattern signature on (x::a) is optional
  ```
  This is natural extension of what happens at the type level, where you can write
  ```
  type T :: forall k -> k -> Type
  data T k (a::k) = MkT    -- The kind signature on (a::k) is optional
  ```
  This is a natural way to "fill out" GHC's current design, but it does not introduce anything fundamentally new; for example the intermediate language does not change.

* In contrast, the two `foreach` quantifiers are fundamentally new.  They allow us to have an argument (visible or invisible) that:
   * Can appear in the rest of the type. E.g. `f :: foreach (a::Bool) -> T a -> Int`.
   * Is reasoned about at compile time.  E.g. `f True x` is type-incorrect if `x :: T False`.
   * Is passed at runtime (just like `(Eq a => blah)`).


* The `foreach ->` quantifier allows us to eliminate the vast mess of singleton types,
  about which the Hasochism paper is eloquent. (That is, `foreach ->` quantifies over an
  argument usable both at compile-time *and* and runtime, the hallmark of dependent types.)
  For example, today we are sometimes forced
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
  zeroVec :: foreach (n::Nat) -> Vec n
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

* New research suggests that the way we denote relevance should line up with the way we denote linearity. See this [POPL 2021 paper](https://arxiv.org/abs/2011.04070). We may thus want to change the syntax so that the distinction between
`foreach` and `forall` is syntactically similar to the way we specify the multiplicity of a function. However, it is
also possible to line up relevance and multiplicity in the internal language without exposing it in Haskell.

* Programmers will have to think about what information to preserve at runtime. We can imagine implementing warnings when a programmer retains unnecessary information.

* [Proposal #102](https://github.com/ghc-proposals/ghc-proposals/pull/102) sets out this syntax, as well.

The `foreach` quantifier is the defining feature that makes Dependent Haskell a dependently-typed language.
We now look at how `foreach`-functions are applied (eliminated) and defined (introduced).

## 5. Dependent pattern-match

When we pattern-match on a value that also appears in a type (that is, something bound by a `foreach`), the type-checker can use the matched-against pattern to refine the type. For example, consider an implementation of `vReplicate`:

```hs
vReplicate :: foreach (n :: Nat) -> a -> Vec n a
vReplicate Zero     _ = Nil
vReplicate (Succ n) x = x :> replicate n x
```

The right-hand side must have a type `Vec n a` -- but `n` is the first pattern to be matched against. Thus, when we write `vReplicate Zero _`, the right-hand side can have type `Vec Zero a`. This is the essence of informative pattern-matches (also called dependent pattern-match).

In order to support Haskell's current type inference of the result of matches, dependent pattern-matches will happen
only when the type of the result is already known, via a type signature. (That is, we use dependent pattern-matching
only when in *checking* mode of bidirectional type-checking, never in *inference* mode.) In the `vReplicate` example
above, we do indeed know the result type: `Vec n a`. We can thus perform an informative pattern-match, as required
to accept the definition.

## 6. Dependent application and the Static Subset

Suppose we have a function `f :: foreach (a::ty) -> blah` or `f :: forall (a::ty) -> blah`.  Then at a
call site the programmer must supply an explicit argument, so the call will look like
```
  f <arg>
```
**Question 1**: is `arg` written in term syntax or in type syntax?  Our answer: in term syntax.

Recall that term-syntax vs type-syntax affects both which syntactic forms are allowed, and
what namespace is used during renaming.  But during parsing and renaming *we do not know the type of `f`*,
and DH maintains Haskell's separation of renaming and typechecking.  So we can only use term syntax for `arg`,
and the term namespace for resolving identifier occurrences in `arg`.

A consequence of writing `arg` in term-syntax is that we need to be able to write e.g. `Int -> Int`
in term-syntax. This implies a modest expansion of what can be parsed and renamed as a term. The type-checker
will know to treat `Int -> Int` as a type. It is
here, however, that a punned `Int` identifier would be annoying. 

An alternative would be to require the programmer to add a syntactic marker for dependent arguments
of a function, in which case they could be written in type-syntax. However, the syntactic
marker would be redundant once we otherwise uphold the *Syntactic Unification Principle*.

**Question 2**: can `arg` be *any* expression whatsoever? Lambdas?
List comprehensions?  Applicative-do?  Local function bindings?

Ultimately we hope that the answer will be "yes", but DH is carefully crafted so that we do not need
a "big bang" to get there.  Rather, we can move incrementally, one step at a time.  Here's how:

* `arg` is *parsed* as a term (an `HsExpr` in GHC-speak)
* `arg` is *renamed* as a term
* But during *typechecking* the compiler treats an application chain `f arg1 arg2 ... argn` specially.
  If it knows that `f :: forall a -> blah`, then it checks that `arg1` is a term written only in a
  specified sub-language of terms -- initially a sub-language that maps directly to the language of (current) types.

We call this "specified sub-language of terms" the **Static Subset** of terms.  In GHC-speak,
a `HsExpr` in the Static Subset can readily be converted to a `HsType`.

For example, suppose `f :: foreach (a :: [Bool]) -> blah`.  An initial version of DH might allow constructors and applications in the static subset, but not list comprehensions, lambdas, or case expressions:
```
  f [True]            -- Allowed
  f [True,False]      -- Allowed
  f (True : [])       -- Allowed

  f [not x | x <- xs]   -- Not allowed: list comprehension
  f (case ... of ...)   -- Not allowed: case
  f ((\y -> y) [True])  -- Not allowed: lambda
   
  f xs                -- Allowed: xs equals only itself
  f (reverse xs)      -- Allowed: reverse equals only itself and xs equals only itself
```
These dependent applications might give rise to a need for compile-time reasoning
over Haskell's very rich expression language. The Static Subset notion polices this
boundary, initially allowing only simple expressions into type inference.
Over time we expect to widen Static Subset of terms, to allow more syntactic forms.

Dependent application also requires us to extend term-syntax to include all types. For example, if `g :: forall a -> Int -> T a` we want to allow
```
  g (Int -> Int)           -- Instantiates `a` with `Int -> Int`
  g (forall b. b->b)       -- Instantiates `a` with `forall b. b->b`
```
Although these type-like forms (function arrow, forall, foreach) are now valid term-syntax, accepted anywhere in term-syntax by the parser and renamer, they are rejected by the typechecker in actual terms, just as lambda and case are rejected in actual types.  Thus:
```
f x = Int -> Int       -- Accepted by parser and renamer, rejected by typechecker
g y = forall a. a->a   -- Ditto
```
The technology for treating application chains specially is worked out in details in
[A quick look at impredicativity](https://www.microsoft.com/en-us/research/publication/a-quick-look-at-impredicativity/).
It is *already* used to govern Visible Type Application (which also requires knowledge of whether the
function part of the application has a forall-type). This aspect is well understood.

The examples above include applications to variables. These variables will be
treated exactly as skolems at compile-time, *even if they are `let`-bound with
known right-hand sides*. For example, suppose we now have `f2 :: foreach (bs :: [Bool]) -> T bs -> blah`.
Then:

```hs
g :: [Bool] -> blah
g bs t = f2 bs (undefined :: T bs)    -- this is allowed, but the second argument must have type `T bs`

h = let bs = [True]
        t :: T [True]
        t = ...
    in
    f2 bs t    -- surprisingly rejected, as bs is equal only to itself
```

In the `h` example, we might expect `f2 bs t` to be accepted, but it will not be, as
variables used in types are equal only to themselves. That is, GHC will forget the
relationship between `bs` and `[True]`. 

Similarly, if we see `f :: forall xs. T (reverse xs) -> blah`, can the `(reverse xs)` ever reduce (e.g. when `f` is instantiated at a call site)?  
Our answer for now is no: variables used in types are equal only to themselves.  (After all, `reverse` might be defined in a separately compiled module, and might be defined with arbitrary Haskell terms.)

This approach keeps things simple for now;
we might imagine retaining the knowledge that `bs = [True]` when, say, the right-hand
side of a `let` is in the Static Subset, but we leave that achievement for later.

## 7. Dependent definition

Principle: We will never *infer* a type with `foreach .`, `foreach ->`, or `forall ->`.
We will continue to infer types with `forall .`, via `let`-generalization, just as we
do today.

Just as with all the other first-class polymorphism work, users can write a type signature
to define functions with these quantifiers. Examples:

```hs
vReplicate :: foreach (n :: Nat) -> a -> Vec n a
vReplicate Zero     _ = Nil
vReplicate (Succ n) x = x :> vReplicate n x

vReplicateImplicit :: foreach (n :: Nat). a -> Vec n a
vReplicateImplicit x = case n of   -- n is in scope from -XScopedTypeVariables
  Zero   -> Nil
  Succ _ -> x :> vReplicateImplicit x

-- alternative approach, from https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst
vReplicateImplicit :: foreach (n :: Nat). a -> Vec n a
vReplicateImplicit @Zero     _ = Nil
vReplicateImplicit @(Succ _) x = x :> vReplicateImplicit x
  -- NB: This is a dependent pattern-match, where the type-checker knows, in each equation, that n is either
  -- Zero or a Succ

the :: forall (a :: Type) -> a -> a
the b x = (x :: b)    -- 'a' is not in scope here, as we're forced to bind 'b'.
-- example usage: the Int 3
```

All variables introduced in term-syntax are in the term namespace. In particular, this applies to the `b` in
the `the` example. Its use in a type relies on the lookup failing in the type namespace and succeeding in the
term namespace.

## 8. Phase distinction

Erased arguments cannot be used at runtime. More specifically, they cannot be pattern-matched against, returned
from a function, or otherwise used, except as an argument to a function expecting an erased argument. Examples:

```hs
ex1 :: forall (n :: Nat) -> Nat
ex1 n = n    -- no: cannot return an erased argument

ex2 :: foreach (n :: Nat) -> Nat
ex2 n = n    -- OK, though arguments to 'ex2' will need to be in the Static Subset

ex3 :: forall (n :: Nat) -> Bool
ex3 Zero     = True
ex3 (Succ _) = False
  -- no: cannot pattern-match on an erased argument

ex4 :: forall (a :: Type) -> a
ex4 a = the a undefined   -- OK: can pass an erased argument to 'the', expecting an erased argument

ex5 :: foreach (a :: Type) -> a
ex5 a = the a undefined   -- OK: even though a is retained, can still pass to a function expecting an erased argument
  -- ex5 would compile to a function that ignores its argument completely
  -- this argument, of type 'Type', would be a runtime representation of a type, something like TypeRep

data T where
  MkT :: forall (a :: Int) -> foreach (b :: Int) -> X a b -> T

ex6 :: T -> Int
ex6 (MkT a b x) = a   -- no: a is erased

ex7 :: T -> Int
ex7 (MkT a b x) = b   -- OK: b is retained

ex8 (MkT a b x) = x   -- no: x's type has existentially bound variables and returning it would cause skolem-escape
  -- this last one is not about phase distinction, but it seems worth mentioning
```

An open question: Can we do this?

```hs
f :: foreach (a :: Type) -> a -> a
f a x = case a of
  Bool -> not x
  _    -> x
```

The theory says "yes"; the choice of `a` is available for pattern-matching. But can we implement this in practice? I think we can, by use type representations. Yet, we may choose to defer such behavior until later; we can always make `Type` opaque and unavailable for pattern-matching.

## 9. Full expressiveness

One worry that some have about dependent types is that other dependently typed languages sometimes require all functions to be proved to terminate. (For example, Agda will not accept a transliteration of

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

## 10. Typed intermediate language

GHC has from the beginning supported a *typed* intermediate language. The type safety of this intermediate language is what allows us to say that Haskell itself is type-safe (no one has attempted a proof of type safety for Haskell itself), and the checks on this internal language allow us to catch many errors that otherwise would have crept into GHC's optimizer.

Dependent Haskell continues to support a typed intermediate language, but one supporting dependent types natively. Designing such a language is hard and has been the subject of some research. We believe that the most recent paper (listed first below) is an adequate candidate for implementation in GHC.

* [*A graded dependent type system with a usage-aware semantics*](https://richarde.dev/papers/2021/grad/grad-extended.pdf). Pritam Choudhury, Harley Eades III, Richard A. Eisenberg, and Stephanie Weirich. POPL'21. This paper combines linearity with dependent types.
* [*A role for dependent types in Haskell*](https://richarde.dev/papers/2019/dep-roles/dep-roles-extended.pdf). Stephanie Weirich, Pritam Choudhury, Antoine Voizard, and Richard A. Eisenberg. ICFP'19. This paper combines roles with dependent types.
* [*A specification for dependently-typed Haskell*](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec.pdf); [appendix](https://richarde.dev/papers/2017/dep-haskell-spec/dep-haskell-spec-appendix.pdf). Stephanie Weirich, Antoine Voizard, Pedro Henrique Azevedo de Amorim, and Richard A. Eisenberg. ICFP'17. This paper introduces homogeneous equality as a simplification over previous approaches.
* [*Dependent types in Haskell: Theory and practice*](https://richarde.dev/papers/2016/thesis/eisenberg-thesis.pdf). Richard A. Eisenberg. PhD thesis, 2016. This work describes both a surface language and intermediate language for Dependent Haskell.
* [*Type inference, Haskell, and dependent types*](https://adam.gundry.co.uk/pub/thesis/thesis-2013-12-03.pdf). Adam Gundry. PhD thesis, 2013. This work describes an intermediate language and the Static Subset included in this design document.

## 11. The Glorious Future

One glorious day, perhaps all terms will be understood by the static type
checker.  To put it another way, any term whatsoever will be
acceptable as an argument to `f :: foreach a -> blah`; and any term
whatsoever would be acceptable in a type or kind signature.  (NB:
Richard and Stephanie definitely want this.  Simon is not yet convinced
that the pain will be worth the gain.)

If that Glorious Day comes, the Static vs Non-static distinction will
vanish, and why it would be unseemly to force some syntactic marker in the
code to indicate dependent arguments.

Instead DH simply imposes restrictions on the terms that can be seen by
the static type checker, and ensures that they lie within its ability
to reason.

Note: full-spectrum dependently typed languages treat `t1 -> t2` as a mere abbreviation of
`foreach (_ :: t1) -> t2`.  But until the Glorious Day, DH will treat these two very
differently:
* If `f1 :: t1 -> t2`, then in a call `(f1 arg)`, there are no restrictions on `arg` (except of course that it has type `t1`).
* If `f2 :: forall (_ :: t2) -> t2`, then in a call `(f2 arg)` arg must lie in the Static Subset of terms.
Even once we reach the Glorious Day, nothing forces us to unify `t1 -> t2` with `foreach (_ :: t1) -> t2`, and
we may decide not to.

# Misconceptions

This section describes several common misconceptions around dependent types, in an attempt to refute them. This was cribbed from (a GHC proposal comment)[https://github.com/ghc-proposals/ghc-proposals/pull/281#issuecomment-736850050];
the "I" here is Richard.

## Punning

False: **Dependent Haskell and/or this proposal is trying to ban definitions like `data T = T`.**

There is no effort as far as I'm aware to eliminate code containing definitions like `data T = T`. This is an example of *punning*, where identifiers of the same spelling are used at the term level and at the type level. The design of DH I've been thinking about, and every concrete description I've seen, continues to allow `data T = T`, into perpetuity.

Instead, the leading design for DH introduces warnings `-Wpuns` and `-Wpun-bindings` that warn at either occurrences or binding sites (respectively) of punned identifiers. This is (in my view) the main payload of #270. (The rest of #270 is just about giving users a way to silence the warnings.) No one has to enable these warnings. All DH features work with punned identifiers, perhaps at the expense of requiring a little more disambiguation. #270 has the details.

It is true that we believe that idiomatic DH will tend to avoid punning, but it will be up to the community to see how it will all play out. Maybe the disambiguation means are easy enough (at a minimum, prefixes like `D.` or `T.`) that punning remains commonplace.

## Complexity

Overstated: **Dependent Haskell is complicated.**

Well, it's a bit hard to make a "False" statement on that one (but I did anyway), but let me try to explain what I mean. @simonpj's comment https://github.com/ghc-proposals/ghc-proposals/pull/281#issuecomment-733715402 is the source of this one. According to my understanding, the complication he refers to is twofold: (1) the need to think about two namespaces, and (2) the need for the T2T translation. 

1. In corner cases, we do need to worry about the two namespaces -- but only when the user binds an identifier in both. This proposal #281 thus irons out which namespace takes precedence. However, if a name is not punned, then the user may remain blissfully unaware of the distinction. Thus, when I say DH is not complicated in this way, I mean that idiomatic DH -- where the user disambiguates between the namespaces instead of using punning -- is not.

   Even a user who does use punning is OK: names bound to the left of a `::` are term-level names; those bound to the right of one are type-level names. Occurrences to the left of a `::` look in the term-level namespace first; those to the right of one look in the type-level namespace first. Of course, there are subtleties here, as spelled out in the proposal, but that summary is morally all there is to it.

2. The T2T translation is needed only until we merge terms and types. Note that this merger is *independent* of the namespace issue: we can imagine identical ASTs for terms and for types, but with different name-resolution characteristics. There are relatively few barriers to merging terms and types: essentially, we have to sort out the fact that `'` means something different in the two ASTs (it selects the term-level namespace in types, while it denotes a TH name quote in terms) and we will have to be able to parse type-like things such as `forall` and `->` in terms. Happily, `->` is *already* illegal in terms, so this probably boils down to making `forall` a keyword.

   There may be a stretch of time that we retain the complexity of T2T, but my hope is that this time will be limited. One of the reasons I wrote #378 is to motivate us to deal with that temporary complexity.

So I claim things are not as bad as they appear here.

## Separate syntaxes

Likely False: **It would work just fine to have dependent types but keep terms as terms and types as types.**

It is possible to have a dependently typed language that keeps terms and types separate. For example [Twelf](http://twelf.org/wiki/Main_Page) is such a language. I agree that this is possible. But I claim such a language is complicated in precisely the way that @simonpj is worried about for DH, and thus a design to avoid.

Twelf works by having a notion of type *indices*, distinct from type parameters. (I am not a Twelf expert; please correct me if I go wrong here.) Indices are terms. Thus, if we say (adapting to Haskell syntactic conventions) `x :: T (a b c)`, that `a b c` is a *term*, not a type. This is because Twelf types are indexed by terms. We thus have a clear separation between types and terms: the thing right after a `::` is a type, and all of its arguments are terms. Yet, we have dependent types.

However, Twelf is missing a feature crucial in Haskell: polymorphism. That is, Haskellers like to talk about `Maybe Int`, where the argument to a type `Maybe` is another type `Int`. This is impossible in Twelf.

To mix type arguments and term arguments, we can imagine (at least) two strategies:

1. Disambiguate according to a type's kind. That is, if we see `T (a b c) (d e f)`, we can look at `T`'s kind to determine whether each of `a b c` and `d e f` are types or terms. This is challenging for several reasons. Firstly, it would be impossible to parse using a parser generator, if types and terms have separate parsers. Let's assume we get around that hurdle by combining syntaxes. Then, it would be very hard to do name resolution. It means we would need the kind of `T` before we can do name resolution on `a b c` or `d e f`. Maybe it seems that this is not unreasonable for a type constructor like `T`. But what about `t (a b c) (d e f)`, where `t` is a type variable, perhaps subject to kind inference? We are now sliding down a slippery slope. Either we say we can't abstract over types that take terms as argument (and hobble our type system) or have strict requirements on kind annotations, etc., to make sure we know `t`'s kind before ever even doing name resolution on its arguments. I don't envy someone trying to implement this.

2. Disambiguate with syntactic markers. That is, we require users to write `T (a b c) (data d e f)` where the `data` keyword indicates that a term comes next. This would mean that *every* use of `T` would need the `data` keyword right there, which would quickly become annoying to users. It's especially annoying when there is no semantic difference between a type argument and a term argument: both would be erased during compilation. The `data` keyword would just be there to select a different sub-language, but with no semantic distinction.

Either design *also* requires a considerable amount of duplication. We would need type families in order to do computation on types, alongside functions to do computation on terms. (We already have this, and it's already painful, in my opinion.) Consider also the desire for propositional equality (i.e. `Data.Type.Equality.:~:`). Is it parameterized by types or terms? We'd need both variants, in practice. Would we need basic datatypes that work over both terms and types? Quite possibly.

So, my claim here is that, while possible, this design is unappealing. If the costs of going to a unified language were very high, then maybe it would be worth it. But I claim that the costs are small: we introduce a way to disambiguate puns (as well as a way to control the built-in puns around lists tuples), and we merge the syntaxes. Disambiguating puns is relatively low-cost: it is an opt-in feature (see my first refutation above -- no one is proposing to ban puns), and the designs for disambiguation hook nicely into the module system (another disambiguation mechanism). Unifying the syntaxes is also relatively low-cost: it means making `forall` (and perhaps `foreach`) unconditionally a keyword, and it means changing the meaning of `'` in types. These costs are non-zero. But I think they are worth paying in order to avoid having a distinction among sub-languages without a difference.

## Type erasure

False: **Dependent Haskell destroys the phase distinction and/or type erasure.**

Other dependently typed languages (notably, Agda and Idris 1) have a murky notion of what information is kept around at runtime, and what is erased during compilation. For example, I can write this in Agda:

```hs
quickLength : ∀ {a : Set} {n : ℕ} → Vec a n → ℕ
quickLength {n = n} _ = n
```

This function returns the length of a vector simply by looking at the index it is parameterized by. By contrast, we cannot write this function in Haskell, because the `n` stored as the length of the vector is a compile-time quantity, not available at runtime. To get the length of a length-indexed vector in Haskell, we must traverse the entire vector, just as we do for lists.

In the design for Dependent Haskell, this phase distinction (the fact that some data is compile-time and some data is run-time) remains, unlike in Agda. Every argument to a function, both implicit and explicit, must somehow be marked as *relevant* or *irrelevant*. (I don't mind calling irrelevant arguments as phantom, but having the terms in obvious contrast is helpful, I think. Though the most appealing opposite of *phantom* is *corporeal*, a fun word to bat around.)

Continuing our example, we could write

```hs
quickLength :: forall (a :: Type). foreach (n :: Nat). Vec a n -> Nat
quickLength @_ @n _ = n

slowLength :: forall (a :: Type) (n :: Nat). Vec a n -> Nat
slowLength Nil = Zero
slowLength (_ :> v) = Succ (slowLength v)
```

Note that `quickLength` uses `foreach (n :: Nat)`. The `foreach` quantifier (also known as `pi` or `∏`) tells us that its argument is relevant and must be passed at runtime. Accordingly, the caller of `quickLength` must somehow already know (at run-time!) the length of the vector before calling. If we were to write the implementation of `quickLength` with the type of `slowLength`, we would get an error, saying that we cannot return an input that is known only at compile-time.

A few other notes on this example:

* The kind annotations (`:: Type` and `:: Nat`) are unnecessary and could be inferred.

* Leaving off any quantification would yield `slowLength`'s type. That is, we assume irrelevant quantification in types.

* The `forall a.` is necessary in `quickLength` is necessary because of the forall-or-nothing rule.

* We could reverse the order of implicit arguments in both examples.

If a function is missing a type signature, it is actually easy to infer relevance: just look at the usages of a variable. If every usage is as an irrelevant argument, then the variable can be quantified irrelevantly. Otherwise, it must be relevant. Relevance inference could be done over a mutually recursive group much like role inference works today, by finding a fixpoint. Also, note that role inference just works -- it has needed essentially no maintenance since being written with the original implementation of roles. I would expect similar of relevance inference.

## Termination

False: **Dependent Haskell will require functions to terminate.**

This has not come up much recently, but it's a misconception I've heard. I won't refute it longhand here. But it's not true. No one is proposing a termination checker. Dependent types without a termination checker is not suitable for use as a proof assistant, but it makes for a wonderfully type-safe language.