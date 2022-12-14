# Partial Type Signatures for Haskell

*Thomas Winant, Dominique Devriese, Frank Piessens, Tom Schrijvers*


Relevant links

- [Thomas Winant's blog post](https://www.fpcomplete.com/user/thomasw/new-in-ghc-7-10-partial-type-signatures)
- Ticket to track progress: #9478


 
In Haskell, programmers have a binary choice between omitting the type
signature (and relying on type inference) or explicitly providing the
type entirely; there are no intermediate options. Partial type
signatures have been often proposed as an intermediate option.


In a partial type signature, annotated types can be mixed with
inferred types. A type signature is written like before, but can now
contain *wildcards*, written as underscores. The types of these
wildcards or unknown types will be inferred by the type checker, e.g.

```haskell
foo :: _ -> Bool
foo x = not x
-- Inferred: Bool -> Bool
```


We have been working on a design and an implementation of them for
Haskell and GHC. We have worked out the interaction with
OutsideIn(X) type inference
and generalisation in a
[paper](https://lirias.kuleuven.be/bitstream/123456789/423475/3/paper.pdf)
presented at [PADL'14](http://www.ist.unomaha.edu/padl2014/) and
a [technical report with proofs](https://lirias.kuleuven.be/bitstream/123456789/424883/1/CW649.pdf).
This document attempts to describe our design and our
[current implementation](https://github.com/mrBliss/ghc) from a more practical
point of view in the hope to get some feedback from the GHC developers
community.

## Issues

Add the label ~"partial type sigs" to indicate that an issue is relevant to this page.

Click on the label to see all the associated issues, both open and closed.

---

## Background and motivation


### Pragmatics


Before we start, we want to say something about the pragmatics of when
and how to use type signatures. Many people have an opinion about when
it is appropriate to write type signatures, e.g. we personally tend to
always write type signatures for top-level functions but sometimes
omit them for local bindings. Sometimes we are a bit more lax in
throw-away code or during development.


We emphasise that partial type signatures do not alter such
pragmatics. The point is that Haskell already allows a choice between
a full signature and no signature at all and all we add is an
intermediate technical option: partial signatures. It is still up to
the user to choose how much and what kind of type signatures to write
for different kinds of definitions. He/she may still want to use
partial signatures only for local definitions and/or non-exported
(auxiliary) top-level definitions and/or top-level definitions in
throw-away code.

### Motivation


So why would you want to use a *partial* type signatures instead of a
normal type signature?

- **Readability** Partial type signature can be useful to improve
  the readability or understandability of your types. Some types are
  so verbose that they might confuse the programmer. By replacing
  distracting boilerplate type information with wildcards, you can put
  the focus on the crucial bits of your type and guide users through
  your complex types by hiding the less relevant bits.

  In the example below, the constraints make the type signature
  bloated.

  ```wiki
  replaceLoopsRuleP :: (ProductionRule p,
    EpsProductionRule p,
    RecProductionRule p qphi r,
    TokenProductionRule p t,
    PenaltyProductionRule p) =>
    PenaltyExtendedContextFreeRule qphi r t v ->
    (forall ix. qphi ix -> p [r ix]) ->
    (forall ix. qphi ix -> p [r ix]) -> p v
  ```

  With the following partial type signature, the type becomes less
  daunting:

  ```wiki
  replaceLoopsRuleP :: _ =>
    PenaltyExtendedContextFreeRule qphi r t v ->
    (forall ix. qphi ix -> p [r ix]) ->
    (forall ix. qphi ix -> p [r ix]) -> p v
  ```

- **Development** During development, some parts of the type may
  still be uncertain, unknown or prone to change, and/or you just
  don't care to annotate them. With a partial type signature, you can
  annotate exactly the parts of the type signature you know or want to
  annotate and replace the (unknown) bits you want the type inferencer
  to figure out for you with underscores (`_`).

  Type signatures are very helpful during development, they let the
  type checker verify that the type of the program you wrote matches
  the type you wanted it to have. They also provide a machine-checked
  form of documentation. After a quick glance at the type signature,
  you instantly know quite a lot about the function you're planning to
  use and/or forgot the type of, as you wrote it more than an hour
  ago.

  But during development, your functions, and by consequence your
  types, will often change. Thus, the type signatures have to be
  updated too, but programmers tend to be lazy, and will just omit the
  signatures and add them back in the end. Unfortunately, by omitting
  the type signatures, you lose the valuable advantages described in
  the previous paragraph. A solution could be to annotate the fixed
  parts of your signatures and replace the ever-changing parts by
  wildcards.

- **Uninferrable types** Some types cannot be inferred, see the
  following program (example adapted from
  [Practical type inference for arbitrary-rank types](http://research.microsoft.com/en-us/um/people/simonpj/Papers/higher-rank/)).

  ```wiki
  foo x = (x [True, False], x ['a', 'b'])
  test = foo reverse
  -- reverse :: forall a. [a] -> [a]
  ```

  The argument of `foo`, `x`, is a polymorphic function, making the
  type of `foo` `(forall a. [a] -> [a]) -> ([Bool], [Char])`, which is
  a higher-rank type. Inferring higher-rank types is isomorphic to
  higher-order unification, which is known to be undecidable. So
  generally, higher-rank types cannot be inferred. Therefore, the
  program above will not typecheck.

  The program above *can* typecheck when the higher-rank type is
  annotated, e.g.

  ```wiki
  foo :: (forall a. [a] -> [a]) -> ([Bool], [Char])
  foo x = (x [True, False], x ['a', 'b'])
  ```

  Unfortunately, we are forced to write a whole type signature, even
  though only the type of the argument is strictly required to be
  annotated, the rest could easily be inferred. With a partial type
  signature, you can annotate just the required bits and leave out the
  rest to be inferred for you, e.g.

  ```wiki
  foo :: (forall a. [a] -> [a]) -> _
  foo x = (x [True, False], x ['a', 'b'])
  ```

- **Community** Feature requests or wiki pages for partial type
  signatures in some form or other already exist: #5248,
  [PartialTypeSigs](https://ghc.haskell.org/trac/haskell-prime/wiki/PartialTypeSigs),
  [PartialTypeAnnotations](https://ghc.haskell.org/trac/haskell-prime/wiki/PartialTypeAnnotations).
  More recently,
  [a question popped up on Stack Overflow](http://stackoverflow.com/questions/21658438/how-to-define-function-signatures-partially-in-haskell)
  asking if there is such a thing as partial type signatures for
  Haskell. Judging from the comments on our answer, people seemed to
  be interested. After presenting the paper at PADL'14, people told us
  they would like to see it in GHC. All this evidence combined makes
  us believe there is a real community demand for partial type
  signatures in GHC.

### Existing Workarounds


In many situations, the effect of partial signatures can be achieved
using other means. Consider for example the following partial
signature

```wiki
foo :: (forall a. [a] -> [a]) -> _
```


A similar thing can already be done with the
ScopedTypeVariables
extension, which allows pattern signatures, e.g.

```wiki
foo (x :: forall a. [a] -> [a]) =
  (x [True, False], x ['a', 'b'])
```


A downside to this approach is that the programmer is sprinkling type
hints throughout his or her programs, instead of nicely separating
types from terms with a type signature. It's also not directly clear
how this can be applied to arbitrary parts of a type, e.g. how would
you say something like `monadicComputation :: _ Int` etc.


Other workarounds exist as well (see the
[paper](https://lirias.kuleuven.be/bitstream/123456789/423475/3/paper.pdf)
for a discussion). They are typically based on adding computationally
useless functions or dead code and we find them all less natural and
elegant than our partial signatures.

---

## Our Design


A good way to think of a partial type signature is as follows. If a
function has a type signature, GHC checks that it has *exactly* that
type. But if it has a *partial* type signature GHC proceeds exactly
as if it were inferring the type for the function (especially
including generalisation), except that it additionally forces the
function's type to have the shape given by the partial type signature.


We now describe the syntax and the semantics of our partial type
signatures.

### Type Wildcards


A (partial) type signature has the following form:

```wiki
forall a b .. . (C1, C2, ..) => tau
```


It consists of three parts:

1. The type variables: `a b ..`
1. The constraints: `(C1, C2, ..)`
1. The (mono)type: `tau`


We call wildcards occurring within the monotype (`tau`) part of the
type signature **type wildcards**. Type wildcards can be instantiated
to any monotype like `Bool` or `Maybe [Bool]`, e.g.

```wiki
not' :: Bool -> _
not' x = not x
-- Inferred: Bool -> Bool

maybools :: _
maybools = Just [True]
-- Inferred: Maybe [Bool]
```


Wildcards can unify with function types, e.g.

```wiki
qux :: Int -> _
qux i b = i == i && b
-- Inferred: Int -> Bool -> Bool
```


Additionally, when they are not instantiated to a monotype, they
will be generalised over, e.g.

```wiki
bar :: _ -> _
bar x = x
-- Inferred: forall a. a -> a

bar2 :: _ -> _ -> _
bar2 x f = f x
-- Inferred: forall a b. a -> (a -> b) -> b
```


Each wildcard will be independently instantiated (see
[Named wildcards](partial-type-signatures#) for dependent instantiation), e.g.
the three wildcards in `bar2` are each instantiated to a different
type.


As type wildcards can be generalised over, additional type variables
can be universally quantified. One should expect an implicit
'wildcard' in the `forall` part of the type signature, e.g.

```wiki
bar3 :: forall a. a -> (a -> _) -> _
bar3 x f = f x
-- Inferred: forall a b. a -> (a -> b) -> b
```


In addition to the explicitly quantified type variable `a`, the
inferred type now contains a new type variable `b`. As type variables
are implicitly universally quantified in Haskell, we chose not to make
this kind of `forall` 'wildcard' explicit.


Wildcards can also unify with annotated type variables, e.g.

```wiki
filter' :: _ -> [a] -> [a]
filter' = filter
-- Inferred: forall a. (a -> Bool) -> [a] -> [a]
-- Same as the type of filter
```


As wildcards can unify with functions, just one type wildcard is
enough to infer the whole type of a function of any arity (albeit
without constraints).

```wiki
filter'' :: _
filter'' = filter
-- Inferred: forall a. (a -> Bool) -> [a] -> [a]
-- Same as the type of filter and filter'
```


And type-constructors, e.g.

```wiki
justify :: a -> _ a
justify = Just
-- Inferred: forall a. a -> Maybe a

tupleIt :: a -> _ a a
tupleIt x = (x, x)
-- Inferred: forall a. a -> (a, a)

nestedTCs :: a -> _ (_ (_ _ _))
nestedTCs = Just . (: []) . Left
-- Inferred: forall a b. a -> Maybe [Either a b]
```

### Named Wildcards


Type wildcards can also be named by giving the
underscore an indentifier as suffix, i.e. `_a`. These are called **named wildcards**.
All occurrences of the
same named wildcard within one type signature will unify to the
same type. For example

```wiki
f :: _x -> _x
f ('c', y) = ('d', error "Urk")
 -- Inferrred: forall a. (Char, a) -> (Char, a)
```


The named wildcard forces the arugment and result types to be the same.
Lacking a signature, GHC would have inferred `forall a b. (Char, a) -> (Char, b)`.
A named wildcard can be mentioned in constraints,
provided it also occurs in the monotype part of the type signature to make sure that
[unify with something](partial-type-signatures#):

```wiki
somethingShowable :: Show _x => _x -> _
somethingShowable x = show x
-- Inferred type: Show x => x -> String

somethingShowable' :: Show _x => _x -> _
somethingShowable' x = show (not x)
-- Inferred type: Bool -> String
```


Besides an extra-constraints wildcard (see below), only named wildcards can occur
in the constraints, e.g. the `_x` in `Show _x`.


Named wildcards *should not be confused with type variables*. Even
though syntactically similar, named wildcards can unify with concrete
types as well as be generalised over (and behave as type variables).


In the first example above, `_x` is generalised over (and is
effectively replaced by a fresh type variable). In the second example,
`_x` is unified with the `Bool` type, and as `Bool` implements the
`Show` typeclass, the constraint `Show Bool` can be simplified away.


Currently, a named wildcard is in scope in the type signature where it
appears, but also in signatures in the right-hand side of the
implementation. See [the issues section](partial-type-signatures#)
for more discussion.

### Extra-constraints Wildcard


Another kind of wildcard we propose is the **extra-constraints
wildcard**. The presence of an extra-constraints wildcard indicates
that an arbitrary number of extra constraints may be inferred during
type checking and will be added to the type signature. In the example
below, the extra-constraints wildcard is used to infer three extra
constraints.

```wiki
arbitCs :: _ => a -> String
arbitCs x = show (succ x) ++ show (x == x)
-- Inferred:
--   forall a. (Show a, Enum a, Eq a) => a -> String
```


An extra-constraints wildcard shouldn't prevent the programmer from
already listing the constraints he knows or wants to annotate, e.g.

```wiki
-- Also a correct partial type signature:
arbitCs' :: (Enum a, _) => a -> String
arbitCs' = arbitCs
-- Inferred:
--   forall a. (Enum a, Show a, Eq a) => a -> String
```


An extra-constraints wildcard can also lead to zero extra constraints
to be inferred, e.g.

```wiki
noCs :: _ => String
noCs = "noCs"
-- Inferred: String
```


As a single extra-constraints wildcard is enough to infer any number
of constraints, only one is allowed in a type signature and it should
come last in the list of constraints.


Extra-constraints wildcards cannot be named.

**NOTE** In spite of SLPJ's reasonable proposal to simplify things by not
taking the annotated constraints into account when an extra-constraints
wildcard is present and just inferring everything from scratch, we still do.

### Holes


Previously, underscores in types were disallowed by GHC and Haskell
2010, so to remain backwards compatible, wildcards or 'holes in types'
should still result in errors. However, the generated error messages
can now be much more informative, i.e. they should inform the user of
the type each wildcard/hole was instantiated to. As this does not
change the set of accepted programs nor the behaviour of accepted
programs, this doesn't have to be an extension (similar to
TypedHoles).


Furthermore, when the user enables the PartialTypeSignatures
extension, the errors are not reported anymore, the inferred type is
simply used.


However, named wildcards (`_a`) are currently parsed as type
variables. To also remain compatible on this front, we propose to
introduce a separate extension, NamedWildcards.
When this extension is enabled, a type variable like `_a` will be
parsed as a named wildcard.


To summarise, the four different cases, depending on the enabled
extensions:


<table><tr><th>                                                                 </th>
<th> PartialTypeSignatures OFF </th>
<th> PartialTypeSignatures ON 
</th></tr>
<tr><th> NamedWildcards OFF </th>
<th> Informative errors are reported for hole instantiations, but only for unnamed wildcards. Named wildcards are still parsed as type variables, as before. </th>
<th> The types of unnamed wildcards are inferred and used. Named wildcards are still parsed as type variables. 
</th></tr>
<tr><th> NamedWildcards ON  </th>
<th> Informative errors are reported for hole instantiations, both for unnamed and named wildcards. </th>
<th> The types of both unnamed and named wildcards are inferred and used. 
</th></tr></table>


Along with informative errors, we also suggest the user to turn on the
PartialTypeSignatures
extension.


Let's demonstrate the described behaviour with an example. An example
program:

```wiki
module Example where

foo :: (Show _a, _) => _a -> _
foo x = show (succ x)
```


Compiled with a prior version of GHC gives:

```wiki
Example.hs:3:18: parse error on input ???_???
```


When compiled with a version of GHC that implements the proposal:

```wiki
Example.hs:3:18:
    Found hole ???_??? with inferred constraints: Enum _a
    Where: ???_a??? is a rigid type variable bound by
                the inferred type of foo :: Enum _a => _a -> String at Example.hs:3:8
    To use the inferred type, enable PartialTypeSignatures
    In the type signature for ???foo???: foo :: (Show _a, _) => _a -> _

Example.hs:3:30:
    Found hole ???_??? with type: String
    To use the inferred type, enable PartialTypeSignatures
    In the type signature for ???foo???: foo :: (Show _a, _) => _a -> _
```


Now the types the wildcards were instantiated to are reported. Note
that `_a` is still treated as a type variable, as prescribed in
Haskell 2010. To treat it as a *named wildcard*, enable the
NamedWildcards extension to
get:

```wiki
Example.hs:3:14:
    Found hole ???_a??? with type: w_a
    Where: ???w_a??? is a rigid type variable bound by
                 the inferred type of foo :: Enum w_a => w_a -> String
                 at Example.hs:4:1
    To use the inferred type, enable PartialTypeSignatures
    In the type signature for ???foo???: foo :: (Show _a, _) => _a -> _

[..]
Example.hs:3:24:
    Found hole ???_a??? with type: w_a
    Where: ???w_a??? is a rigid type variable bound by
                 the inferred type of foo :: Enum w_a => w_a -> String
                 at Example.hs:4:1
    To use the inferred type, enable PartialTypeSignatures
    In the type signature for ???foo???: foo :: (Show _a, _) => _a -> _

[..]
```


An extra error message appears, reporting that `_a` was instantiated to a new
type variable (`w_a`).


Finally, when the PartialTypeSignatures
extension is enabled, the type checker just uses the inferred types for
the wildcards and compiles the program without generating any
messages.


Alejandro Serrano Mena has added Emacs support for interacting with TypedHoles
during his
[GSOC project](http://serras-haskell-gsoc.blogspot.com/2014/08/summer-of-code-on-emacs.html).
He told me this should work fine with [PartialTypeSignatures](partial-type-signatures) as it is, but we
would like to add the ability to let Emacs fill in the hole's inferred type
at the press of a key. We will look into this.

### Defer type errors


GHC's `-fdefer-type-errors` flag defers compile-time type errors to run-time.
This flag overlaps partially with our `-XPartialTypeSignatures` flag.


Holes in type signatures will cause compile-time type errors, but deferring
them to run-time with `-fdefer-type-errors` will make them disappear, as they
will never be generated at run-time, unlike real type errors. To be clear,
holes in type signatures will **not** cause run-time type errors with
`-fdefer-type-errors` enabled.


The `-XPartialTypeSignatures` flag will also make the compiler swallow type
errors caused by holes in type signatures, but other type errors will still be
generated at compile time.

### Local Definitions


With the introduction of OutsideIn(X),
the GADTs
extension and the TypeFamilies
extension, trigger the MonoLocalBinds
flag. When it is enabled, types
of local bindings without a signature no longer get generalised as
described in OutsideIn(X).


For the same reasons, we intend to perform no generalisation over
wildcards in partial type signatures for local bindings, e.g.

```wiki
monoLoc :: forall a. a -> ((a, Bool), (a, Char))
monoLoc x =  let g :: _ -> _
                 g y = (x, y)
             in (g True, g 'v')
```


When MonoLocalBinds is
disabled, the program should work with or without the partial type
signature. But when the MonoLocalBinds
extension is enabled, it should no longer typecheck, as
the type of `g` is no longer generalised, again with or without the
partial type signature.


GHC is slightly more liberal than the strict rule described in the
OutsideIn(X) paper though.
As explained in [Let generalisation in GHC 7.0](https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7),
even with the MonoLocalBinds
extension enabled, a local binding without a type signature will still
be generalised when all its free variables are closed. Translating
this to partial type signatures: the following example should type
check, as it does without a type signature.

```wiki
safeLoc :: (Bool, Char)
safeLoc = let f :: _ -> _
              f x = x
          in (f True, f 'v')
```


Furthermore, we believe it necessary to disallow extra-constraints
wildcards in partial type signatures for local bindings, as the
generalisation over constraints is exactly what led to *let should
not be generalised*.

### Partial Expression and Pattern Signatures


Wildcards are allowed in expression and pattern signatures, e.g.

```wiki
bar1 :: _a -> Bool
bar1 x = (x :: _a)
-- Inferred: Bool -> Bool

bar2 :: Bool -> _a
bar2 (x :: _a) = x
-- Inferred: Bool -> Bool
```


These wildcards are quantified in the expression or pattern signature
they appear in.


We don't support extra-constraints wildcards in such signatures, as
the implementation difficulties they pose don't outweigh their
usefulness.

---

## Formalisation


We worked out a rigorous formalisation of partial type signatures
including typing rules, extending the existing formalisation of GHC's
type inference system, OutsideIn(X)
(as [described](http://research.microsoft.com/apps/pubs/default.aspx?id=162516)
in *OutsideIn(X): Modular
type inference with local assumptions*). Additionally, we proved
soundness of our new typing rules.


We mentioned before that a single type wildcard is enough to infer any
type, albeit without constraints. When we combine this with the fact
that the presence of a single extra-constraints wildcard is enough to
infer any number of constraints, one can see that the partial type
signature `_ => _` is the same as omitting the type signature and
relying completely on type inference. We formulated this, i.e. that `_ => _`
is equivalent to omitting the type signature, as a theorem and proved
it. We also showed that on non-partial type signatures, our new rules
have the same effect as the old rules.


See the
[technical report](https://lirias.kuleuven.be/bitstream/123456789/424883/1/CW649.pdf)
for more details.

---

## Implementation


We implemented our proposal as a branch of GHC (master), available at
[github](https://github.com/mrBliss/ghc). This version can be compiled and
should type check the majority of the examples in this proposal.


Here is a summary of the changes we have made in the implementation:

- The parser (`Parser.y.pp` and `RdrHsSyn.lhs`) was extended to parse
  wildcards in types when the appropriate extensions are enabled, and also to
  check that no wildcards occur in undesired places, e.g. typeclass
  definitions.

- `HsType` has two new constructors: `HsWildcardTy` for unnamed type wildcards
  (`_`) and `HsNamedWildcardTy name` for named wildcards (`_a`).

- The `HsForAllTy` constructor of `HsType` has an extra field of type `Maybe SrcSpan`
  that stores the extra-constraints wildcard's position, if present. A `Maybe SrcSpan`
  was used instead of a boolean for pretty-printing purposes and error
  messages.

- The renamer: when renaming `TypeSig`s, we do a pass over the annotated type
  signature. In this pass, unnamed wildcards (`_`) are given a generated name
  and are converted to named wildcards. The named wildcards are collected and
  stored in the `TypeSig`, as if they were existentially quantified in that
  `TypeSig`. Duplicates and wildcards that are already into scope are filtered
  out. A similar thing is done for expression signatures (`ExprWithTySig`) and
  pattern signatures (`HsWithBndrs`).

- In the type checker, in `tcTySig`, we create new meta-variables for the
  wildcards and add them to the type variable environment and store them in
  the returned `TcSigInfo` to get them brought in scope when checking the
  RHSs. A `Maybe TyVar` is also stored in the `TcSigInfo`, which will
  eventually be unified with the inferred extra constraints.

- Analogously to TypedHoles, when a wildcard is encountered during type
  checking, an insoluble `CHoleCan` constraint which contains information
  about the expected type, is emitted. When the type checker and the
  constraint solver have finished, these insoluble constraints will be
  leftover, and are converted to error messages mentioning the inferred type
  for each wildcard. A similar thing is done for the extra-constraints
  wildcard which contains a meta-variable that is unified with the inferred
  extra constraints after the constraints of one group of bindings are solved.
  If the PartialTypeSignatures
  extension is enabled, no constraints are emitted as no error messages have
  to be produced and reported.

- To typecheck a binding (actually a group of bindings) with a partial type
  signature, `tcPolyInfer`, the function used for typechecking bindings
  without type signatures is reused and modified. The partial type signature
  is taken into account, as well as the presence of an extra-constraints
  wildcard.

- The constraint solver function `simplifyInfer` is extended such that it also
  accepts an optional set of annotated constraints as input.

- The error reporter for Holes is extended to deal with different holes, e.g.
  a `ConstraintsHole` originating from an extra-constraints wildcard is
  treated differently from an `ExprHole` or `TypeHole` (all are constructors
  of the new `HoleSort` data type).

- We extended Haddock as well to handle partial type signatures. A single
  commit updating the GHC API and adding printing capabilities can be found in
  our fork of Haddock on [github](https://github.com/mrBliss/haddock).

### Notes


In this section we'll quickly mention some inelegant solutions, or explain
some things that may be non-obvious.

- Analogously to TypedHoles, we generate insoluble constraints (`CHoleCan`) to
  report the types wildcards are instantiated to, however, we might have
  reused a bit too much of the existing infrastructure for TypedHoles.

  - Errors after the holes reporter (see `reportAllUnsolved` in
    `TcErrors.lhs`) are no longer suppressed, as, contrarily to TypedHoles,
    wildcards in types don't prevent a program from being complete. In the
    case of TypedHoles, the holes should be filled before continuing type
    checking, but this is not needed for wildcards in types, thus other type
    errors may be reported.
  - There is some code duplication in the two branches of `mkHoleError` in
    `TcErrors.lhs`.
  - We differentiate Holes (i.e. values of `CHoleCan`) with the `HoleSort`
    type, which in case of an extra-constraints wildcard hole contains an
    `TcTyVar` field that will unify with the extra inferred constraints. We
    do this in order to be able to reported the inferred constraints when
    reporting the hole error.
  - In order to capture the `CHoleCan` for the extra-constraints wildcard, we
    wrap a call to `emitInsoluble` in `captureConstraints` in the `tcTySig`
    function in `TcBinds.lhs`. This captured constraint, which we'll call
    `wantedSig`, is stored in the signature (`TcSigInfo`) and later added to
    the `wantedBody` constraints captured while type checking the body of the
    function. Both sets of wanted constraints are later merged together in
    the local definition `makeWanted` of `tcPolyInfer` in `TcBinds.lhs`. This
    feels kind of dirty.
  - The `CHoleCan` constraint needs evidence (`CtEvidence`). For TypedHoles
    and holes of type wildcards, we can use the inferred type. For the
    `CHoleCan` generated by an extra-constraints wildcard, we currently use
    `unitTy`.

- Constraints are solved (i.e. calls to `simplifyInfer`) per recursive group
  of bindings, thus, the extra constraints are inferred per recursive group as
  well. Therefore, they are added to each binding of the group with an
  extra-constraints wildcards.

### TODOs


>
>
> The TODOs are sorted by severity, i.e. the first TODO is the most
> important one, the last one is only of minor importance.
>
>

1. TODO A partial type signature for a local definition is not generalised
  when the MonoLocalBinds
  flag is enabled (this flag is implied by
  GADTs and
  TypeFamilies!),
  as [intended](partial-type-signatures#). However, for functions affected
  by this restriction (i.e. [not all free variables are closed](https://ghc.haskell.org/trac/ghc/blog/LetGeneralisationInGhc7#Whichbindingsareaffected)),
  partial type signatures aren't properly handled.

  ```wiki
  {-# LANGUAGE MonoLocalBinds, ScopedTypeVariables #-}
  monoLoc :: forall a. a -> ((a, String), (a, String))
  monoLoc x = (g True , g 'v')
  where
    -- g :: b -> (a, String) -- This signature is ok
    g :: _                   -- This signature is not
    g y = (x, "foo")
  ```

  The example above will produce a number of errors of which the following is the vital one:

  ```wiki
  Couldn't match expected type ???Char -> (a, String)???
              with actual type ???_???
    because type variable ???a??? would escape its scope
  ```


   


1. TODO There is a bug in the handling of pattern bindings with partial type
  signatures, i.e. the partial type signatures are ignored, e.g.

  ```wiki
  foo :: Bool -> _
  Just foo = Just id
  -- Inferred: forall a. a -> a
  -- Instead of: Bool -> Bool
  ```

  See test case `PatBind2`.

>
>
> There is a TODO for this in the code.
>
>

1. TODO One of the examples described in the [section](partial-type-signatures#) about the monomorphism
  restriction doesn't work:

  ```wiki
  {-# LANGUAGE MonomorphismRestriction, PartialTypeSignatures #-}
  charlie :: _ => a
  charlie = 3
  ```

  We think that, as the type variable is annotated and thus unrestricted, the
  constraint `Num a` should be inferred as an extra constraint. Unfortunately,
  this doesn't happen:

  ```wiki
  No instance for (Num a) arising from the literal ???3???
  In the expression: 3
  In an equation for ???charlie???: charlie = 3
  ```

  See test case `ExtraNumAMROn`. Either this needs to be fixed, or our
  expectation of how the type checker should behave is wrong.

1. TODO When reporting the types wildcards were instantiated to in error messages,
  we mention the inferred type of the binding, but this doesn't include the
  annotated constraints that weren't inferred during typechecking, e.g.

  ```wiki
  bar :: (Num a, _) => a -> a -> _
  bar x y = x == y
  ```

  Produces the following error message:

  ```wiki
  Example.hs:3:16:
      Found hole ???_??? with inferred constraints: Eq a
      Where: ???a??? is a rigid type variable bound by
                 the inferred type of bar :: Eq a => a -> a -> Bool at Example.hs:3:8
      To use the inferred type, enable PartialTypeSignatures
      In the type signature for ???bar???: bar :: (Num a, _) => a -> a -> _

  Example.hs:3:32:
      Found hole ???_??? with type: Bool
      To use the inferred type, enable PartialTypeSignatures
      In the type signature for ???bar???: bar :: (Num a, _) => a -> a -> _
  ```

  In the 4th line of the output shown above, the inferred type is displayed,
  but the `Num a` constraint has disappeared, as it wasn't inferred. Two lines
  below, it is displayed, but that is because the annotated partial type
  signature is shown. The actual type of `bar`, `(Num a, Eq a) => a -> a -> Bool`,
  is not shown to the user, as it isn't easily accessible when reporting the
  error.

>
>
> Related feature request for TypedHoles: #9091.
>
>

1. TODO The order in which type variables are tidied for printing is incorrect.
  Wildcards which are generalised over become type variables with names
  starting with `w_`. If a type variable with such a name already exists, the
  existing *tidying* infrastructure should make sure that the suffixes `1`,
  `2`, ... are added in order to differentiate the similarly named type
  variables. However, the order in which the suffixes are added is
  unsatisfactory, e.g.

  ```wiki
  barry :: _ -> w_
  barry _ = undefined
  -- Inferred (modulo alpha-equivalence): a -> b
  ```

  Which causes the following hole error message:

  ```wiki
  Found hole ???_??? with type: w_
  Where: ???w_??? is a rigid type variable bound by
              the inferred type of barry :: w_ -> w_1 at Example.hs:3:1
  To use the inferred type, enable PartialTypeSignatures
  In the type signature for ???barry???: barry :: _ -> w_
  ```

  The problem is that the existing type variable `w_` is renamed to `w_1` and
  the type variable generated by generalising over the wildcard gets `w_` as
  name, instead of the inverse. See test case `TidyClash2`.

## Monomorphism restriction


Regarding the
[monomorphism restriction](http://www.haskell.org/onlinereport/decls.html#sect4.5.5),
the existing rules apply. Declarations with a partial type signature
are treated as declarations without a type signature. Therefore, these
declarations will be *restricted*, and constrained type variables of
such declarations may not be generalised over, and thus defaulting
will occur when possible.


We illustrate this behaviour with some examples:

```wiki
-- Remember: the principal type of 3 is forall a. Num a => a
alpha :: _
alpha = 3
-- with MR on, the type variable is restricted and defaults to Integer
-- with MR off, generalise over _, error: no instance for (Num _)

bravo :: _ => _
bravo = 3
-- with MR on, the type variable is restricted and defaults to Integer
-- with MR off, type: forall a. Num a => a

charlie :: _ => a
charlie = 3
-- with MR on, type variable is annotated and thus unrestricted, type: forall a. Num a => a
-- with MR off, type: forall a. Num a => a
```


The last example (with the monomorphism restriction) on, `charlie`,
might be surprising. As `a` is a generalised type variable that is
already annotated, we don't have to generalise over it anymore. The
monomorphism restriction simply doesn't apply.


## Questions and issues


-  **Constraint wildcards**:
  **NOTE**: we no longer intend to support constraint wildcards.
  Only [named wildcards](partial-type-signatures#) also occurring in monotype
  and an [extra-constraints wildcard](partial-type-signatures#)
  will be allowed. The examples below demonstrating the named wildcard
  in the constraints look useful to us (and already work in the
  implementation).

  ```wiki
  somethingShowable :: Show _x => _x -> _
  somethingShowable x = show x
  -- Inferred type: Show x => x -> String

  somethingShowable' :: Show _x => _x -> _
  somethingShowable' x = show (not x)
  -- Inferred type: Bool -> String
  ```

- **The scope of named wildcards**:
  We currently treat all named wildcards as scoped type variables,
  i.e. if we mention a named wildcard in a top-level partial type
  signature, then it can also be mentioned in local bindings and it
  will refer to *the same* type variable. This means, for example,
  that the following example won't work:

  ```wiki
  test2 :: _a -> _a
  test2 x = let y :: _a
                y = (x, x)
            in fst y
  ```

  The problem is that we will get a constraint `_a ~ (_a, _a)`, and we
  will get an error about not being able to construct the infinite
  type `_a`. If `_a` were not scoped, then the inner `_a` could be
  unified with `(_a', _a')` where `_a'` is the outer `_a` and
  everything would work.

  A remaining question here is if we should treat named wildcards as
  scoped *only if* the ScopedTypeVariables
  extension is enabled. Our understanding is that *not* treating
  type variables as scoped is only done for backwards compatibility,
  so there seems little point in doing it for named wildcards?
  **Update:** we now follow the ScopedTypeVariables
  extension.

- **Local definitions**: no generalisation and no extra constraints
  wildcards in partial type signatures for local definitions, are we
  too strict? What when NoMonoLocalBinds
  is turned on in conjunction with GADTs
  or TypeFamilies?

- Generally, we are interested in any problems you may find in this
  design: major problems or small corner cases that we didn't think of
  or look at yet, etc.

## Some detailed notes

- **Generalising over equality constraints?** Consider the following
  example:

  ```wiki
  anyTypeThatIsBool :: _ => a
  anyTypeThatIsBool = True
  ```

  A question that we have considered is whether one would like this to
  type-check. Note that it can be typed as

  ```wiki
  anyTypeThatIsBool :: a ~ Bool => a
  anyTypeThatIsBool = True
  ```

  We currently do not infer this kind of equality constraints (even
  when there is an extra constraints wildcard): technically, `a` will
  be instantiated to a skolem type variable and an equality for such a
  variable will be considered absurd, similarly to a constraint like
  `Int ~ Bool`. As a result, it will not be generalised over. Note
  that if we were to allow inferring this kind of constraints, then
  any universally quantified type variable would behave as a wildcard
  in the presence of an extra constraints wildcard.

  Note however that this is only the case for equality constraints
  that directly apply to universally quantified type variables. For
  example, the following does work:

  ```wiki
  type family F a

  data Proxy a = Proxy a

  anyTypeWhoseFIsBool :: _ => Proxy a -> F a
  anyTypeWhoseFIsBool _ = True
  ```

  This will type-check fine and the constraint `F a ~ Bool` will be
  happily inferred. Unlike `a ~ Bool`, it is not considered absurd
  since it does not directly apply to a universally quantified type
  variable.

- **Disallow other uses**: partial type signatures should not be
  allowed in every case a type (signature) is needed, for instance in
  type class definitions, data type definitions, foreign declarations,
  type synonyms, associated type instantiations, ... Our
  implementation currently checks for these cases, but we might have
  missed one. We believe that in nearly every case but value bindings,
  partial type signatures (or types containing wildcards) should be
  disallowed. However, we think they should be admissible in
  InstanceSigs (not yet
  implemented).

-  **Extra-constraints wildcard position**: We only allow one
  extra-constraints wildcard in a signature: at the outer
  quantification of the signature. Consider for example the function
  `multiCs`:

  ```wiki
  multiCs x = show (succ x) ++ show (x == x)
  ```

  GHC infers the type `(Show a, Enum a, Eq a) => a -> String` for it,
  but the type signature provided below is also valid (with the
  RankNTypes extension
  enabled).

  ```wiki
  multiCs :: (Show a) => a -> (Enum a, Eq a) => String
  ```

  We currently only allow one extra-constraints wildcard, in the
  outermost set of constraints. Otherwise, we might get ambiguous
  situations like this:

  ```wiki
  multiCs :: (Show a, _) => a -> (Enum a, _) => String
  ```

-  **Higher-rank types**: Consider the
  following partial type signature:

  ```wiki
  forall a. a -> (forall b. (b, _c) -> b) -> Int
  ```

  We believe that generalising over the `_c` named wildcard should
  lead to a top-level quantification (where `a` is quantified) of the
  resulting type variable:

  ```wiki
  forall a w_c. a -> (forall b. (b, w_c) -> b) -> Int
  ```

  We will never infer a quantification in the nested quantification
  (where `b` is quantified):

  ```wiki
  forall a. a -> (forall b w_c. (b, w_c) -> b) -> Int
  ```

  The latter is equivalent to inferring higher-rank types, which, as
  we mentioned before, is not something we can do.

  Additionally, we do not allow an extra-constraints wildcard in a
  nested 'forall', e.g.

  ```wiki
  f :: (forall a. _ => a -> a) -> b -> b
  ```

## Appendices

### Not supported: Constraint Wildcards

**NOTE**: we no longer intend to support constraint wildcards as
described below. Only [named wildcards](partial-type-signatures#) also
occurring in monotype and an [extra-constraints wildcard](partial-type-signatures#)
will be allowed.


We call wildcards occurring within a constraint (inside a `C` in `(C1, C2, ..)`)
*constraint wildcards*, e.g.

```wiki
fstIsBool :: (Bool, _) ~ a => a -> Bool
fstIsBool (b1, b2) = not b1 && b2
-- Inferred: (Bool, Bool) -> Bool

class Two a b | a -> b where
  one :: a -> a
  two :: b -> b

-- Ignore the second parameter of the typeclass
secondParam :: Two a _ => a -> a
secondParam x = one x
-- Inferred type: forall a b. Two a b -> a -> a
```


GHC's constraint solver doesn't unify constraints with each other.
E.g. `Eq _` or `_ a` will never be unified with `Eq a`. The problem
the constraint solver is faced with is "deduce `Eq a` from `Eq _`,
figuring out what the `_` should be instantiated to". Or, worse,
"deduce `Eq a` from `_ a`" or something even less constrained. The
constraint solver is absolutely not set up to figure out how to fill
in existential variables in the "givens".


So the following program will not
work:

```wiki
-- Neither partial type signature will work
impossible :: Eq _ => a -> a -> Bool
impossible :: _  a => a -> a -> Bool
impossible x y = x == y
-- Actual type: forall a. Eq a => a -> a -> Bool
```


Note that constraints are not unified for good reasons. One immediate
problem is already that it could lead to ambiguities, consider for
instance the following program.

```wiki
-- Incorrect partial type signature
ambi :: _ a => a -> String
ambi x = show (succ x) ++ show (x == x)
-- Actual type:
--   forall a. (Enum a, Eq a, Show a) => a -> String
```


As constraints are unordered, the constraint solver wouldn't know
which one of the inferred constraints `(Enum a, Eq a, Show a)` the
partially annotated constraint `(_ a)` should be unified with, it
would have to guess. Regardless of whether constraints are unified,
this program would have been rejected anyway, as only one constraint
is partially annotated instead of all three.
