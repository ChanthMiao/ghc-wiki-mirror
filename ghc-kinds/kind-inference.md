# Kind inference for types and classes


This page summarises several alternative designs for doing kind inference for
types and classes, under `-XPolyKinds`.
Though originally written with respect to #9200, there are more issues at play here. Relevant other tickets: #9201, #9427, #14451, #14847, #15142, and (to a lesser extent) #12088, #12643, #14668, #15561, and #15987, which are all about instances.


See also [GhcKinds/KindInference/Examples](ghc-kinds/kind-inference/examples) for a growing set of examples to consider when evaluating any new algorithm.

The design of TLKSs (top-level kind signatures) is discussed at [GhcKinds/KindInference/TLKS](ghc-kinds/kind-inference/tlks).

## Simplifying `getInitialKinds` (#15142)


In terms, we make a drastic distinction between how we check a term
with a type signature and how we check a term without. When a term
has a type signature, that signature is used for the term, and (for
example) polymorphic recursion is possible. Without a type signature,
the term is assigned type `alpha`. During type checking, `alpha` will
unify only with a monotype, and thus polymorphic recursion is impossible.
(Of course, after constraint generation and solving, we then generalize
any unconstrained variables.)


The goal here is to do the same in type declarations. When a type
declaration has a kind signature (in other words, a CUSK, which is
a sloppy encoding of a kind signature), we check it with that kind
signature. Otherwise, we assign it kind `kappa` and gather constraints
on `kappa`, allowing `kappa` only to be a monokind, generalizing later.


For CUSKs, this is pretty close to accurate. But it's nothing like what
we do in the non-CUSK case. Currently, non-CUSKs get an "initial kind",
formed from whatever we can divine from the type head. (For example, `data T a` gets `kappa -> Type`, using `T`'s arity and the fact that it's a `data` type.) Then we do constraint solving. Simon wonders if we can simplify by just guessing `kappa`.



The problem is that we will lose some currently-accepted definitions. For example, consider


```
data Prox k (a :: k)
```


The kind of `Prox` is `forall k -> k -> Type`. Note that this type contains `forall k ->`, not `forall k .`. If we had assigned `Prox :: kappa`, we would be unable to infer the correct kind for `Prox`. This kind is currently produced right in `getInitialKind`, with no extra quantification necessary. The `k` is lexically dependent, and so GHC uses `forall k ->` for it.



Another interesting case is


```
data Prox2 k a = MkP2 (Prox k a)
```


Here, it's "obvious" that `Prox2` should have the same kind as `Prox`, even though `k` isn't obviously a dependent variable. This is actually rejected today, because GHC requires that all dependent variables be manifestly so, in the LHS.


Here is a good place to note that the term-level scheme has an exception: if the definition is non-recursive and top-level, then we don't guess a type `alpha`; instead, we just infer the type of the term from the RHS. This allows the term to be assigned a polytype or higher-rank type. If we did the same in types (noting that `Prox2` isn't recursive), then perhaps we could accept `Prox2`.


Note that we want to have a good understanding of what will be accepted and what will not be. And we do not want to accept polymorphic recursion without a signature.

### Simon's suggestion



Here was one idea: assign every tyvar in the LHS a fresh kind variable `kappa_n`. Then use those to check any kind signatures on the LHS and the definition on the RHS. So, given


```
data S2 k (a :: k) b = MkS (S2 k b a)
```


we would start with `k :: kappa1`, `a :: kappa2`, and `b :: kappa3`. We would soon get the following wanteds:

```wiki
kappa1 ~ Type    -- from the ":: k"
kappa2 ~ k       -- from the ":: k"
kappa3 ~ kappa2  -- because b is used as the second arg to S2 in the RHS
```


and all will work out.



The problem with this approach is that it *also* accepts the polymorphic recursive `S3`:


```
data S3 k (a :: k) b = MkS (S3 Type b a)
```


So we think this isn't the right answer.

### Adam's suggestion


One other possibility in trying to wrangle this is to not let the `kappa_n` to unify with other tyvars introduced in the datatype definition. Part of this plan would require checking any LHS kind annotations *before* assigning the `kappa_n`. In this scenario, both `S2` and `S3` would be rejected, because we could never get `kappa3` to unify with `k`.


One big drawback of Adam's idea is that it is very different from what happens in terms. Annotating some tyvar kinds is just like having a partial type signature. Yet, if I say `f :: forall a. a -> _`, GHC allows the wildcard to be replaced with `a`. Simon pointed out that partial type signatures are themselves a mess, and that we're not sure we want to duplicate it all "one level up". But we all agreed that they are useful and that uniformity is a Good Thing.

### Comparison to Agda



Incredibly, all the following are accepted in Agda:


```
data Q (k : Set) (a : k) : Set1 where

data Q2 k a : Set1 where
  MkQ2 : Q k a -> Q2 k a

data Q3 k a : Set1 where
  MkQ3 : Q3 k a -> Q k a -> Q3 k a

data Q4 k a : Set1 where
  MkQ4 : Q4 ??? 5 -> Q k a -> Q4 k a

data Q5 k a : Set1 where
  MkQ5 : Q5 ??? 3 -> Q5 Bool true -> Q k a -> Q5 k a
```


I have no clue how they pull that off!

## Trouble when fixing #14066


Ticket #14066 requests a more robust way of tracking the scope of type
variables. A full discussion is out of scope here, but it required hard
thought about kind inference. Here are some notes I (Richard) have 
drawn up around it:

### Introduction



The central challenge is this:


```
data Proxy1 a where
  Mk :: Proxy1 (a :: k)
```


To infer the right kind for `Proxy1`, GHC has to guess a kind for `a` -- call it kappa -- and then process the type of `Mk`. What's very bizarre is that the occurrence of `Proxy1` in the type of `Mk` leads GHC to unify kappa with `k`, even though `k` has a local scope. In other words, kappa unifies with an out-of-scope skolem. GHC's current (8.4) kind-generalization process ends up quantifying over the `k` in the kind of `Proxy1`, and so disaster is averted. But it really works only by accident.



And it doesn't always work.


```
data Proxy2 a where
  Mk1 :: Proxy2 (a :: k)
  Mk2 :: Proxy2 (b :: j)
```


GHC does the same thing here, but it rejects the definition because `k` doesn't unify with `j`. This is absurd, because these two variables are skolems that share no scope whatsoever. Interestingly, GHC 7.10 accepts this last definition, so the rejection is a hitherto-unknown regression.

### Simon's Proposed Solution



Datatype declarations are kind-checked in two passes. The first pass looks through all the constructors, accumulating constraints on the type's kind. Then, once the kind is known, all the constructors are checked *again* with respect to the known kind. Note that we need to look at constructors to determine the kind of the datatype; otherwise, GHC would choke on declarations like


```
data App a b = MkApp (a b)
```


The kinds for `a` and `b` can be known only by looking at `MkApp`.


Simon suggested that, in the first pass, we instantiate all the user-written type variables in a constructor's declaration with unification variables and then proceed to infer the kind. This would mean that the `j` and `k` in `Proxy2`'s constructors would both be instantiated with unification variables. These would all unify but would remain unconstrained. GHC would then quantify over this kind, as it should. Note that it's possible for GHC to infer something silly here, but it would then be caught on the second pass.

### Aside: Polymorphic Recursion and Type Inference


GHC claims that it infers most general types. But this claim is simply not true in the presence of polymorphic recursion. For example, consider this equation defining `f`:

```
f x y = f y x
```


The most general type for `f` is `forall a b c. a -> b -> c`. GHC indeed accepts this type signature for `f`. However, if `f` is written without a type signature, GHC infers `forall a b. a -> a -> b`.


What has happened to GHC's claim of inferring most general types? It is true, but it is true only with respect to the fragment of Haskell that prohibits polymorphic recursion. Note that GHC's inferred type leads to no polymorphic recursion. The lesson here is that a claim of most general types makes sense only with respect to some restriction on the features in the language. (For example, we all know that GHC won't infer a higher-rank type, even if one is more general than another type.)

### Simon's Algorithm Breaks Principal Types



Under Simon's algorithms, some forms of polymorphic recursion are indeed accepted. For example:


```
data T a where
  Mk :: forall k1 k2 (a :: k1) (b :: k2). T b -> T a
```


Simon's algorithm instantiates `k1`, `k2`, `a`, and `b` with fresh unification variables. Suppose `T` is guessed to have kind `kappa -> Type`. Then, `kappa`, `k1`, and `k2` all get unified together, with no constraints. GHC will quantify, giving `T` the kind `forall k. k -> Type`. During the second pass, this kind works fine, instantiated to `k1` and `k2`. Thus, GHC accept the polymorphic-recursive `T`.



So: what forms of polymorphic recursion are accepted? Not all of them. Take this example:


```
data T2 a where
  Mk :: forall (a :: k). T2 Maybe -> T2 a
```


Under Simon's algorithm, the `T2` is guessed to have kind `kappa -> Type`, and then `kappa` will unify with `Type -> Type`. `T2` is assigned the kind `(Type -> Type) -> Type`. On the second pass, `Mk` is rejected, applying `T2` to something of kind `k`. We can see, though, that `T2` could be assigned the kind `forall k. k -> Type` quite profitably. Nevertheless, I agree with Simon's algorithm that rejecting is the correct behavior -- we don't want to infer polymorphic recursion.


After some thinking, Adam, Csongor and I came up with this rule: Simon's algorithm accepts polymorphic recursive definitions when the recursive occurrences are at types that instantiate kind variables with kind variables, but never concrete kinds. Note that in `T`, the recursive occurrence replaces `k1` with `k2`, but `T2` replaces `k` with `Type -> Type`. Let's call a polymorphic recursive definition where recursive occurrences replace variables with variables "pseudopolymorphic recursive".



However, Simon's algorithm does not always infer the most general type with respect to the fragment of the language containing pseudopolymorphic recursion (but not general polymorphic recursion). For example:


```
data T3 a b where
  Mk :: T3 b a -> T3 a b
```


Simon's algorithm infers the kind `forall k. k -> k -> Type` for `T3`, even though it could have the kind `forall k1 k2. k1 -> k2 -> Type` and remain pseudopolymorphic recursive. Thus, I claim that Simon's algorithm does not infer most general types.

## Inferring dependency


A related problem to the above is inferring dependency between binders, some notes follow.



In GHC 8.4, the following declaration for `Proxy2` is rejected:


```
data Proxy k (a :: k)
data Proxy2 k a = MkP (Proxy k a)
```


`Proxy`'s kind is `forall k -> k -> Type`. According to the rule that dependent variables must be manifestly so (implemented by `getInitialKind`), the declaration for `Proxy2` is rejected, because only after checking the RHS do we find out about the dependency. (This rule is documented in [Section 10.11.13](https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/glasgow_exts.html#inferring-dependency-in-datatype-declarations) of the user manual).


However, arguably inference should be able to determine that `Proxy2 :: forall k -> k -> Type`. A possible way would be to skip `getInitialKind` and look at the RHS immediately (according to Richard, implementing this is not that simple).

### GADTs



Consider the following, GADT-style definition of `Proxy2`:


```
data Proxy2 k a where
  MkP :: Proxy k a -> Proxy2 k a
```


This definition is accepted today (GHC 8.4), but why?



With `-fprint-explicit-foralls`, `Proxy2`'s signature is actually the following:


```
data Proxy2 k k1 (a :: k) where
  MkP :: forall k (a :: k). (Proxy k a) -> Proxy2 k k a
```


It is a kind-indexed GADT, with the `MkP` constructor holding `k ~ k1`, and its kind
`forall {k}. Type -> k -> Type` is not manifestly dependent. This is not *wrong*, but there are certainly cases where (short of having a CUSK) it is desired that the inferred signature of a GADT is not kind-indexed.



Consider the singleton type for `Bool`:


```
data SingBool b where
  SingTrue :: SingBool 'True
  SingFalse :: SingBool 'False
```


The inferred kind is `Bool -> Type`, but `forall {k}. k -> Type` would not be invalid either.



A related problem, where it's much more apparent that the more specialised kind is better is type families:


```
type family F a where
  F Int = Bool
```


Here, `F` is inferred to be `Type -> Type`. Indeed, if instead it was generalised to `k -> j`, then `F Int` could only be reduced to `Bool` if its return kind was specified.


So the question is: if the non-GADT `Proxy2` will be inferred to have the dependent kind, should the GADT `Proxy2` be too?

## New Proposal

### Preprocessing: Translation from GADT syntax to H98 syntax


Translation from GADT syntax to H98 syntax has several advantages:

1. The scope of the type/kind variables are explicit;
1. Avoid the appearance of recursion in return types;
1. Make sure the different representations of the same data type behave the same.


Example:


```
data Proxy2 a where
  Mk1 :: Proxy2 (a :: k)
  Mk2 :: Proxy2 (b :: j)

-- is translated to

data Proxy2 c
  = forall k a. ((a :: k) ~ c) => Mk1
  | forall j b. ((b :: j) ~ c) => Mk2
```


After the translation step, we can focus on the kind inference algorithm for only H98 syntax.


### Kind Inference Strategy for H98 syntax



**CUSK**. If the data type has CUSK, we use the provided kind to do type-checking.



**Non-CUSK**. If there is no CUSK:


1. Depending on whether the datatype is defined recursively, we use different strategies.

  1. If the definition is recursive, we guess a **monokind** `kappa` as the initial kind of the data type. Reject any dependency. E.g.

    ```
     -- P :: kappa
     data P a b = ...
    ```
  1. If the definition is non-recursive, for all variables, we guess `kappa` kind for them. Allow dependency. There is no need to get an initial kind, as the data type is not used in RHS. E.g.

    ```
     --  a::kappa1, b::kappa2, and kappa2 might be unified with a
      data P a b = ...
    ```
1. Collect constraints on `kappa` from LHS and RHS
1. Solve constraints
1. Do generalization

### Examples of non-CUSKs under the new strategy


```

-- Accept. Proxy :: forall k -> k -> Type
data Proxy k (a::k)

-- Reject. As it is recursive, it cannot have dependency
data T1 k (a::k) = MkT1 (T1 k a)

-- Accept. Proxy2 :: forall k -> k -> Type
data Proxy2 k a = MkP2 (Proxy k a)

-- Reject. As it is recursive, it cannot have dependency
data T2 k a = MkT2 (Proxy k a) | MkT2' (T2 * Int)
```

## Current strategy (GHC 7.8)


Running example:

```wiki
  data T a f = T1 Int (T a f) | T2 (f a)

  data S a f = S1 (S Maybe (S Int)) 
```


Here is GHC 7.8's strategy:


1. Sort data/type/class declarations into strongly connected components.  In our example, there are two recursive SCCs, one for `T` and one for `S`.


  


1. Then, for each SCC in turn:

- Bind the type constructor to a fresh meta-kind variable:

  ```wiki
          T :: kappa0
  ```
- Kind-check all the declarations of the SCC in this environment.  This will generate some unifications, so in the end we get

  ```wiki
          kappa0 ~ kappa1 -> (kappa1 -> *) -> *
  ```
- Generalise.  So we get

  ```wiki
          T :: forall k. k -> (k -> *) -> *
  ```


Following the same three steps for `S` fails, because we need polymorphic recursion.  Srarting with `S :: kappa0`, we get

```wiki
  kappa0 ~ kappa1 -> kappa2 -> *    -- S has two arguments
  kappa1 ~ *->*                     -- From (S Maybe)
  kappa1 ~ *                        -- From (S Int)
  ...and some more...
```


So in GHC 7.8 you can supply a "complete user-supplied kind" (CUSK) for `S`, thus:

```wiki
  data S (a :: k) (f :: k -> *) where
     S1 :: S Maybe (S Int) -> S a f
```


Then step 2 works thus:

- Bind the each non-CUSK type constructor to a fresh meta-kind variable, and each CUSK type constructor to its polymorphic kind.

  ```wiki
          S :: forall k. k -> (k->*) -> *
  ```
- Kind-check all the declarations of the SCC in this environment.  With this new kind for `S`, kind checking succeeds.

- Generalise any non-CUSK type constructors.

## Proposed new strategy

**Note:** This is implemented. See the commit messages in #9200.


The main proposed change is to the definition of a "complete user-supplied kind" (CUSK).  The current story is in [Section 7.8.3 of the user manual](http://www.haskell.org/ghc/docs/latest/html/users_guide/kind-polymorphism.html#complete-kind-signatures).  Alas, it does not allow CUSKs for class declarations.
The new proposal is this:

- A class or datatype is said to have a CUSK if and only if all of its type variables are annotated.  Its result kind is, by construction, `Constraint` or `*` respectively.

- A type synonym has a CUSK if and only if all of its type variables and its RHS are annotated with kinds.

- A closed type family is said to have a CUSK if and only if all of its type variables and its return type are annotated. 

- An open type family always has a CUSK -- unannotated type variables (and return type) default to `*`. (This is not a change.)

- **Update for `TypeInType`:** If `-XTypeInType` is in effect, any top-level kind given to a datatype must introduce all of its kind variables explicitly, allowing `data X :: forall k. k -> *` but saying that `data X :: k -> *` does not have a CUSK.


This is somewhat simpler, it covers classes. See [comment:19:ticket:9200](https://gitlab.haskell.org/ghc/ghc/issues/9200) for more exposition.
This change alone is enough to satisfy #9200.


Examples:

```wiki
  class C (a::k) (f::k->*) where ...   -- Has CUSK
  class D a (f :: k -> *)  where ...   -- No CUSK (a is un-annotated)
  data T (f :: k -> *) where ...       -- Has CUSK

  type T (a::k) = a :: k               -- Has CUSK
  type S (a::k) = a                    -- No CUSK (RHS is un-annotated)

  type family F a :: *                 -- Has CUSK (a defaults to *)

  type family F (a::*) :: k where ...  -- Has CUSK
  type family F a where ...            -- No CUSK (neither arg nor result annotated)
```


If a datatype has a CUSK and its kind has any unsolved metavariables after inference (possible with `-XTypeInType`), an error is reported.

**Reasons for Update:** It's nice to be able to say `data X :: Proxy k -> *` and get inference to be able to deduce that you mean `X :: forall j (k :: j). Proxy k -> *`. Of course, this is possible only with `-XTypeInType`, so we make it a tad harder to specify a CUSK. Ideally, we'll one day move to a separate `type X :: <X's kind>` syntax that will obviate this delicate syntactic definition.

## A possible variation

**RAE:** This variation is partially implemented in tag `re-sort-non-cusk-decls` at [my GitHub fork](https://github.com/goldfirere/ghc). It is tracked in ticket #9427.


This algorithm is not quite as expressive as it could be.  Consider

```wiki
   data SS f a b = MkSS (TT a f) (SS f a b)
   data TT (a::k) (f::k -> *) :: * where
      MkTT :: f a -> SS f a Maybe -> SS f a Int -> TT a f
```


Here `TT` has a CUSK, but `SS` does not.  Hence in step 1 we'd bind `SS :: kappa0`. But in the RHS of `TT` we use `SS` at two different kinds, so inference will fail.  We could solve this by giving a CUSK to `SS` as well.


However, we can *also* solve it using a plan due to Mark Jones, and one that GHC 7.8 already follows for ordinary, recursive term-level functions.  As usual, divide the declarations into SCCs, and then for each SCC do the following:

- Identify which type constructors have Complete User Type Signatures (CUSK).  In this example, `TT` does. Extend the environment with these, fixed, kinds:

  ```wiki
         TT :: forall k. k -> (k->*) -> *
  ```
- Perform a new strongly-connected component (SCC) analysis on the non-CUSK decls in the SCC, *ignoring dependencies on a type constructor with a CUSK*.  In our example, we get a single recursive SCC, containing `SS`.

- For each SCC in turn:

  - Bind the type constructor to a fresh meta-kind variable:

    ```wiki
            SS :: kappa0
    ```
  - Kind-check all the declarations of the SCC in this environment.  This will generate some unifications, so in the end we get

    ```wiki
            kappa0 ~ (kappa1 -> *) -> kappa1 -> kappa2 -> *
    ```

    The `kappa1` arises from instantiating `TT` at its call site in `SS`
  - Generalise.  So we get

    ```wiki
            SS :: forall k1 k2. (k1->*) -> k1 -> k2 -> *
    ```
- Extend the environment with these generalised kind bindings, and kind-check the CUSK declarations.


The Key Point is that we can kind-check `SS` *without looking at `TT`'s definition at all*, because we completely know `TT`'s kind.  That in turn means that we can exploit *inferred* polymorphism for `SS` when kind-checking `TT`.  As we do here: `TT` uses `SS` in two different ways `(SS f a Maybe)` and `(SS f a Int)`.



This strategy is more complicated than the initial proposal, but allows fewer annotations.  It's not clear whether it is wroth the bother.

### Typing rule for the new proposal


An algorithm is all very well, but what about the typing judgements?
We will pretend that data, type, type family, class declarations look something like this:

```wiki
  T :: forall k1 k2. (k1 -> *) -> k1 -> _ -> *
  data/type/class T a b c = rhs
```


That is, there is a possibly-partial kind signature, with holes denoted by "_", and a definition "rhs" (eg the consructors of a data type, or equations of a closed type family). In reality there isn't a separate kind signature; instead, it is integrated in the definition; e.g.

```wiki
  data T (a::k1 -> *) (b::k1) c = MkT (a b) c
```


The kind variables mentioned in the partial kind signature scope over the "rhs".


Then we have two typing rules, one for CUSK, and one for non-CUSK.
We give the rule for data types but modulo keyword it works for classes, closed 
type families, etc.  We ignore arity/saturation issues for type families.

```wiki
k has at least one missing bit (non-CUSK)
k2 = k[k'1 .. k'n/_]     -- k'1 .. k'n are magically known
kvs2 = fkv(k2)
G, kvs2, T : k2 |- (data T tvs = rhs) ok    -- Monomorphic recursion
----------------------------------------------------- NoCUSK
G |- (T :: forall kvs. k; data T tvs = rhs) :: {T :: forall kvs2. k2}

k has no missing bits (CUSK)
G, T : forall kvs. k |- (T tvs = rhs) ok  -- Polymorphic recursion
----------------------------------------------------- CUSK
G |- (T :: forall kvs. k; data T tvs = rhs) :: {T :: forall kvs. k}
```


We need two rules, depending on whether or not a CUSK is detected. 


The first rule requires the equations to be fully parametric in its kinds, whereas the second allows  polymorphic recursion.


For closed type families, these rules are *different* than the implementation today, because kind inference for closed type families today is ill-specified. See [comment:18:ticket:9200](https://gitlab.haskell.org/ghc/ghc/issues/9200).

---

# OUT-DATED MATERIAL


Now here are some notes and design alternatives that we rejected.  It's just here because I hate
deleting stuff!

## Notes about CUSKs on type synonyms

**Simon** What about type synonym declarations? Don't we need a kind signature on the RHS?  Also what about specifying the return kind of a type family (open or closed)?  Does it default to `*`, or must you specify it to get a CUSK?

**Richard** Type synonym declarations can never be recursive, right? So, this issue doesn't affect them. I've answered the other questions above.

**Simon** Wrong: type synonyms can be recursive through a data type:

```wiki
  data S (a :: k) (f :: k -> *) = S1 (SSyn (S Int) Maybe) 
  type SSyn f a = S a f
```


This will fail despite the CUSK for `S` because `SSyn` lacks one.  (The variation below would fix this particular example.)  I think #9151 is another example.

**Richard:** Good example. But, wouldn't this be fixed with the more-careful dependency checking described immediately below? Then, `S`, with its CUSK, would be added to the environment and generalized before looking at `SSyn`, and all is well. I don't like the idea of saying that a type synonym has a CUSK when its tyvars and its RHS are annotated, because the RHS is just a plain type -- there's not really a place in the syntax to put the RHS's kind annotation. Looking at the code, something approaching this is done already, where all non-synonyms are first added to the environment (with `getInitialKinds`) and then all the synonyms are fully kind-checked, and then all the other decls are kind-checked. The more-careful dependency checking below could be implemented simply by having `getInitialKinds` notice the CUSK and generalize, I think. Indeed, if I give `S` a CUSK using the current CUSK syntax, the example above compiles today.

**Simon:** I agree that, since type synonyms can't be recursive except via a data type, if you use "A possible variation" below, then you can get away with saying that type synonyms cannot have CUSKs.  It seems a bit non-orthogonal; and could occasionally be tiresome.  Imagine

```wiki
data T1 a b c = ...S...
data T2 p q r = ...S...
data T3 x y q = ...S...
type S f g h = ...T1...T2...T3...
```


Then, since you can't decorate S with a CUSK, you might need to annotate all of T1, T2 and T3 with CUSKs.


It would not be hard to say what a CUSK for a type synonym was: just wrap the whole RHS in a kind signature:

```wiki
type T (a::k1) (b::k1->k2) = b a :: k2
```


I think I'd argue mildy for that, just to make the design orthogonal.

## Partial kind signature strategy (PARTIAL)


The key idea is that *all polymorphism is declared*, so nothing gets to be kind-polymorphic unless you say so.  But the payoff is that you can give partial kind signatures.  Here's the strategy.

1. Sort the declarations into SCCs.  No special treatment for CUSKs.

1. For each declaration, extend the environment with a kind binding that has a forall for each *explicit* user-written kind variable, but meta-kind variables otherwise.  These kind annotations amount to partial kind signatures.  For example

  ```wiki
        data Foo (a :: k1 -> k1) b c = ...
  ```

  would get a kind binding

  ```wiki
        Foo :: forall k1. (k1->k1) -> kappa1 -> kappa2 -> *
  ```

  Our earlier example would give

  ```wiki
        T :: forall k. k -> (k->*) -> *
        S :: kappa3 -> kappa4 -> kappa5 -> *
  ```

1. Kind-check the declartions in this environment.  At a call of `Foo`, say, we'd instantiate the `forall k1` with a fresh meta-kind variable, but would share `kappa1`, `kappa2` among all calls to `Foo`.

1. Default any unconstrained meta kind variables to `*`


That's it!   No generalisation step.  The *only* polymorphism is that declared by the user.


So our earlier `SS`/`TT` example would be rejected because it relies on S being polymorphic in its third parameter. If you want the `SS`/`TT` example to work you could write

```wiki
   data SS (f::k1->*) (a::k1) (b::k2) = MkSS (TT a f) (SS f a b)
   data TT (a::k) (f::k->*) where
      MkTT :: f a -> SS f a Maybe -> SS f a Int -> TT a f
```

### Declarative typing rules for (PARTIAL)


I think that (PARTIAL) has a nice declarative typing rule.


Here is what the conventional declarative typing rule, *in the absence of polymorphism* for a single self-recursive function looks like.  (I'm using the term language but the same thing happens at the type level.)

```wiki
        G, f:t |- e:t
        G, f:t |- b:t'
      ---------------------------
        G |- letrec f = e in b : t'
```


Here the "t" is a monotype (no foralls) that the declarative typing rules clairvoyantly conjures up out of thin air.


Once you add Hindley-Milner style polymorphism, the rule gets a bit more complicated

```wiki
        G, f:t |- e:t
        G, f:gen(G,t) |- b:t'
      ---------------------------
        G |- letrec f = e in b : t'
```


where 'gen' is generalising.


The (PARTIAL) rule might look like this:

```wiki
        t = forall vs. sig[t1..tn/_]
        vs \not\in ti
        G, f : t |- e : forall vs.t
        G, f : t |- b:t'
      --------------------------- (T-PARTIAL)
        G |- letrec f :: forall vs. sig; f = e in b : t'
```


Here I'm expressing the user-specified knowledge as a signature `forall vs.sig`, with '_' for bits you don't want to specify.

```wiki
       f :: forall a. _ -> a -> _
```


Then the rule instantiates each '_' independently with 
a clairvoyantly guessed monotype (provided it does not mention
the 'vs', or 'a' in this example), and off you go.

### A tricky point about (PARTIAL)


Notice that in this typing rule I say `vs \not\in ti`.  If you don't have that side condition I think
complete inference becomes very hard.  Suppose `MT :: (*->*) -> *`, and consider

```wiki
   data Foo f (a::k) = MkFoo (Foo Maybe Int) (Foo MT Maybe)
```


Because of the partial kind signature we'll kind-check `Foo`'s RHS with this kind signature for `Foo`:

```wiki
   Foo :: forall k. kappa1 -> k -> *
```


using the unification variable `kapp1` for `f`.  Now, if we clairvoyantly decide `kappa1 := k->*`, as would be allowed by (T-PARTIAL), then indeed the definition if well-kinded.  So we'd better infer that, if we are to be complete wrt (T-PARTIAL).  But the algorithm will share `kappa1` among both calls to `Foo`, and will therefore unify `Maybe` with `MT` and fail.


To gain completeness we need to be less ambitious; hence the side condition in (T-PARTIAL) `vs \not\in ti`.


But that side condition, in turn, means that this will fail:

```wiki
  data Foo f (a::k) = MkFoo (f a) (Foo f a)
```


because here `kappa1` must be unified with `k->*`, which isn't allowed by (T-PARTIAL).
Maybe that is acceptable; you can always decorate both of `Foo`'s arguments.

## Generalised partial kind signature strategy (PARGEN)


The (PARGEN) strategy is exactly like (PARTIAL) except that step 4 is different:

1. Generalise over any unconstrained meta kind variable, rather than defaulting to `*`.  Since we are operating at top level, there are no kind variables mentioned in the environment, so no need for the ususal "not free in the environment" check.


So we use the partial kind signatures to express any polymorphism necessary for recursion *inside* the SCC,
but perhaps infer yet more polymorphism that can be used *after* the SCC.  Thus:

```wiki
data T1 f a = MkT1 (f a) (T f a)
  -- Success:  T1 :: forall k. (k->*) -> k -> *

data T1a f (a::k) = MkT1a (f a) (T f a)
  -- Failure:  f's kind is unified with skolem k
  -- See "tricky point" above

data T2 f a = MkT2 (f a) (T2 Maybe Int) (T2 Monad Maybe)
  -- Failure: needs polymorphic recursion

data T3 (f::k->*) (a::k) = MkT3 (f a) (T3 Maybe Int) (T3 Monad Maybe)
  -- Success: polymorphic recursion declared

data T4 (f::k->*) a = MkT4 (f a) (T4 Maybe Int) (T4 Monad Maybe)
  -- Failure: not all polymorphism in k is declared
  -- See "tricky point" above
```

### Declarative typing rules for (PARGEN)


The declarative rule for (PARGEN) is a combination of the one for (PARTIAL) with 
the standard generalisation:

```wiki
        t = forall vs. sig[t1..tn/_]
        vs \not\in ti
        G, f : t |- e : forall vs.t
        G, f : gen(G,t) |- b:t'
      --------------------------- (T-PARGEN)
        G |- letrec f :: forall vs. sig; f = e in b : t'
```


The difference from (PARTIAL) is that before type-checking `b` we generalise `t`.


Here is the declarative rule for closed type families:

```wiki
k2 = forall fkv(k1). k1[k'1 .. k'n/_]
fkv(k1) \not\in k'j
forall i: G, F:k2 |- (F ps_i = t_i) ok
k3 = gen(G, k2)
---------------------------------------------------
G |- type family F :: k1 where { F ps_i = t_i } : k3
```


This is very similar to the declarative typing rule for `letrec` above. Here, I am ignoring issues with arity/saturation and using a syntax where the kind signature of a type family is given as `k1` with blanks, instead of the tyvarbndr syntax used in source code.


This is in fact an improvement over the current state of affairs, which is essentially this rule *without* the side condition. Because of the omitted side condition, we don't have principal types! For example,

```wiki
type family X (a :: k) where
  X True = False
```

`X` could be `X :: k -> k` or `X :: k -> Bool`. Neither is more general than the other. GHC chooses `X :: k -> Bool`, but it's not principled. This is what I get for implementing kind inference for closed type families without writing out declarative rules! In any case, the solution to this problem (closed type families) seems to be the same as the solution for datatypes and classes, quite happily: add the side condition.

### How does (PARGEN) differ from (BASELINE)?


(PARGEN) and (BASELINE) are incomparable.

- The `SS/TT` example under (BASELINE) will be rejected by (PARGEN) because `SS` will get kind `kappa1 -> kappa2 -> kappa3 -> *` when kind-checking the `SS/TT` strongly connected component.  But (BASELINE) accepts it by breaking the SCC into two.
- There are obviously examples that are accepted by (PARGEN) but not (BASELINE).


So moving from (BASELINE) to (PARGEN) would be a breaking change, but only in rather obscure circumstances.  I am intensely relaxed about that particular backward-compatibility problem!

## All of the above (ALL)


Combine (BASELINE) and (NEWCUSK), for the CUSK stuff, with (PARGEN) for type with partial kind signatures.  This would type the
most programs, but is the most complicated.

## Type signatures


Another place that we currently (i.e. using (BASELINE)) do kind generalisation is in *type signatures*. If you write

```wiki
f :: m a -> m a 
f = ...
```


then the type signature is kind-generalised thus:

```wiki
This user-written signature 
  f :: m a -> m a 
means this (BASELINE)
  f :: forall k (a:k) (m:k->*). m a -> m a
```


And f's RHS had better *be* that polymorphic.  


However (PARTIAL) does no kind generalisation, and it would be consistent to cease doing so for type signatures too.  so:

```wiki
This user-written signature 
  f :: m a -> m a 
means this (PARTIAL)
  f :: forall (a:*) (m:k->*). m a -> m a
```


If you want the kind-polymorphic one, you'd have to write thus

```wiki
This user-written signature 
  f :: forall k (a:k) (m:k->*). m a -> m a
means this (PARTIAL)
  f :: forall k (a:k) (m:k->*). m a -> m a
```

## Reflection


I think we could reasonably switch to (PARTIAL) throughout.


As Richard's comments in `TcHsType` point out, we don't want maximal polymorphism.  His example is:

```wiki
    type family F a where
      F Int = Bool
      F Bool = Char
```


We could generate  

```wiki
   F :: forall k1 k2. k1 -> k2
```


so that `(F Maybe)` is well-kinded, but stuck. But that's probably not what we want. It would be better to get `F :: * -> *`


But what about

```wiki
    type family G a f b where
      G Int  f b = f b
      G Bool f b = Char -> f b
```


You could just about argue that the programmer intends

```wiki
   F :: forall k. * -> (k->*) -> k -> *
```


It's quite similar to this:

```wiki
  data PT f a = MkPT (f a)
```


which today, using (BASELINE), we infer to have kind

```wiki
  PT :: forall k. (k->*) -> k -> *
```


But I'd be perfectly happy if PT got a *monomorphic* inferred kind,
which is what (PARTIAL) would do:

```wiki
  PT :: (*->*) -> * -> *
```


If you want the poly-kinded PT, use a signature:

```wiki
  -- Any of these would do
  data PT f             (a :: k) = MkPT (f a)
  data PT (f :: k -> *) a        = MkPT (f a)
  data PT (f :: k -> *) (a :: k) = MkPT (f a)
```


One oddity is that we'd do (BASELINE) for terms and (PARTIAL) for types.  But perhaps that's ok.  They are different.

- Terms ought to be as polymorphic as possible but arguably not types. Examples above.  Also, since kind polymorphism is still in its infancy, maybe it's no bad thing if all kind polymorphism is explicitly signalled every time a kind-polymorphic binder is introduced.

- Terms have well-established separate type signatures, but we don't have a syntax for separate kind signatures of types and classes.


If we moved from (BASELINE) to (PARTIAL), some programs that work now would fail:

- the original S/T example above
- a data type like `PT` where the user did actually want the kind-polymorphic version.


But that might be a price worth paying for the simplicity, uniformity, and predictability you'd get in exchange.

**Richard:** I think changing to (PARTIAL) throughout would be a mistake, as lots of code would fail to compile. Kind polymorphism by default in datatypes and classes has been around since 7.4, and I suspect there is quite a bit of code that such a change would disrupt.


On the other hand, I think changing to (PARGEN) throughout would work nicely. I believe that it would allow all current code to type-check (except for the weird example that probably should be rejected in #9201). If we were to choose (PARGEN) over (ALL), it's possible that some code would become *more* polymorphic, as (PARGEN) is more polymorphic than (BASELINE) in the presence of a CUSK. However, I don't believe that this could be a *breaking* change, and I would prefer going with (PARGEN) over (ALL) for the sake of simplicity -- no need to have two systems around.


I can't figure out a way that (BASELINE) and (PARGEN) are different in type signatures for terms. This version doesn't have quite as nice a declarative typing rule because the type is generalized over kind variables that go completely unmentioned in the type -- a straightforward `forall ftv(t). t` doesn't quite do it. We need to generalize over seen variables, infer kinds, and then generalize over meta-kind variables. But, this is what is done today.


(Because open type families do not have a body, they *would* still be considered to have a CUSK, where un-annotated type variables default to have kind `*`.)



In [comment:5:ticket:9200](https://gitlab.haskell.org/ghc/ghc/issues/9200), I discuss "good" polymorphism and "bad" polymorphism. This discussion, in retrospect, seems tangential at this point. It really only makes sense when discussing closed type families, which aren't at the heart of the problems here. **End Richard**


