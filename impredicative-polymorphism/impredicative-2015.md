# New impredicativity (June 2015)


The **goal** is to build a better story for impredicative and higher-rank polymorphism in GHC. For that aim we introduce a new type of constraint, `InstanceOf t1 t2`, which expresses that *type `t2` is an instance of `t1`*. This new type of constraint is inspired on ideas from the MLF and HML systems.


This is the result of discussion between Alejandro Serrano Mena \<A.SerranoMena@…\>, Jurriaan Hage, Dimitrios Vytiniotis, and Simon PJ.

**The most up-to-date description is available here: [Impredicativity in GHC (PDF)](uploads/d5bbefb947e5312c88fd07df5269a05a/impredicativity.pdf)**



The rest of the document is kept for historical purposes, and because it contains useful information about how the design is implemented inside GHC.


## Notation


<table><tr><th>Type variables                   </th>
<th> <tt>alpha</tt>, <tt>beta</tt>, <tt>gamma</tt> 
</th></tr>
<tr><th>Type constructors                </th>
<th> <tt>T</tt>                      
</th></tr>
<tr><th>Type families                    </th>
<th> <tt>F</tt>                      
</th></tr>
<tr><th>Constraints                      </th>
<th> <tt>Q</tt>                      
</th></tr>
<tr><th>Monomorphic types                </th>
<th> <tt>mu    ::= alpha | a | mu -> mu | T mu ... mu | F mu ... mu</tt>                   
</th></tr>
<tr><th>Types without top-level <tt>forall</tt> </th>
<th> <tt>tau   ::= alpha | a | sigma -> sigma | T sigma ... sigma | F sigma ... sigma</tt> 
</th></tr>
<tr><th>Polymorphic types                </th>
<th> <tt>sigma ::= forall a. Q => tau</tt>                                                 
</th></tr></table>


## Some basic facts

- `InstanceOf` has kind `* -> * -> Constraint`.
- The evidence for `InstanceOf sigma1 sigma2` is a function `sigma1 -> sigma2`. This accounts for the weird order of parameters in `InstanceOf`.
- The canonical forms associated with the constraint are `InstanceOf sigma1 alpha1` and `InstanceOf alpha2 sigma2`, where `sigma2` is not a type variable.

*Implementation note*: `InstanceOf` needs to be defined in `libraries/ghc-prim/GHC/Types.hs`.

*Implementation note*: new canonical forms need to be defined in `compiler/typecheck/TcRnTypes.hs` by extending the `Ct` data type.

## Changes to constraint solver


Luckily, in order to work with `InstanceOf` constraints, we only need to add new rules to the canonicalization step in the solver. These rules are:

- \[IOCan1\] `InstanceOf (T sigma1 ... sigman)  t1                    ` ----\> `(T sigma1 ... sigman) ~ t1`
- \[IOCan2\] `InstanceOf (forall a. Q2 => tau2) (T sigma1 ... sigman) ` ----\> `[a/alpha]tau2 ~ (T sigma1 ... sigman)  /\  [a/alpha]Q2`
- \[IOCan3\] `InstanceOf sigma2`                (forall a. Q1 =\> tau1)` ----> `forall a. (Q1 =\> InstanceOf sigma2 tau1)\`


But we also need to generate evidence for each of these steps!

*Implementation note*: implement these rules in `compiler/typecheck/TcCanonical.hs`.

*Implementation note*: add the new types of evidence to `EvTerm` in `compiler/typecheck/TcEvidence.hs`.

*Implementation note*: extend the desugarer to convert from evidence to actual functions.

### Example


Suppose we want to type check `runST ($) (e :: forall s. ST s Int)`. Let us denote `alpha` the type of `runST`, `beta` the type of `e` and `gamma` the type of the entire expression. The initial set of constraints which are generated (details on generation below) are:

```haskell
InstanceOf (forall a b. (a -> b) -> a -> b) (alpha -> beta -> gamma)  [from ($)]
InstanceOf (forall a. (forall s. ST s a) -> a) alpha                  [from runST]
InstanceOf (forall s. ST s Int)                beta                   [from e]
```


The series of solving steps are:

```haskell
InstanceOf (forall a b. (a -> b) -> a -> b) (alpha -> beta -> gamma)  [1]
InstanceOf (forall a. (forall s. ST s a) -> a) alpha                  [2]
InstanceOf (forall s. ST s Int)                beta                   [3]

----> [IOCan2] over [1]

((delta -> epsilon) -> delta -> epsilon) ~ (alpha -> beta -> gamma)   [4]
+ [2] and [3]

----> type deconstruction in [4]

alpha ~ delta -> epsilon
beta  ~ delta
gamma ~ epsilon
+ [2] and [3]

----> substitution in [2] and [3]

InstanceOf (forall a. (forall s. ST s a) -> a) (delta -> epsilon)     [5]
InstanceOf (forall s. ST s Int) delta                                 [6]

----> [IOCan2] over [5]

((forall s. ST s eta) -> eta) ~ (delta -> epsilon)                    [7]
InstanceOf (forall s. ST s Int) delta

----> type deconstruction in [7]

delta   ~ forall s. ST s eta
epsilon ~ eta
InstanceOf (forall s. ST s Int) delta

----> substitution

InstanceOf (forall s. ST s Int) (forall s. ST s eta)                  [8]

----> [IOCan3] over [8]

forall s. (_ => InstanceOf (forall s'. ST s' Int) (ST s eta))         [9]

----> [IOCan2] under (=>) of [9]

forall s. (_ => Instance (ST pi Int) (ST s eta))                      [10]

----> canonicalization under (=>)

forall s. (_ => s ~ pi /\ eta ~ Int)                                  [11]

----> float constraints out of (=>)

eta ~ Int
forall s. (_ => s ~ pi)

----> FINISHED!
```


We get that the type assigned to the whole expression is `gamma ~ epsilon ~ eta ~ Int`, as we expected :)

### Evidence generation


For \[IOCan1\] we want to find evidence for `W1 :: InstanceOf (T sigma1 ... sigman) t1` from `W2 :: (T sigma1 ... sigman) ~ t1`. Such an evidence must be a function `W1 :: (T sigma1 ... sigman) -> t1`. We can get it by applying the coercion resulting from `W2`. More schematically:

```haskell
W1 :: InstanceOf (T sigma1 ... sigman) t1

---->

W1 :: T sigma1 ... sigman -> t1
W1 = \x -> x |> W2

W2 :: (T sigma1 ... sigman) ~ t1
```

```wiki
W1 :: InstanceOf (forall a. Q1 ... Qn => tau2) (T sigma1 ... sigman)

---->

W1 :: (forall a. Q1 ... Qn => tau2) -> T sigma1 ... sigman
W1 = \x -> (x alpha V1 ... Vn) |> W2

W2 :: [a/alpha]tau2 ~ (T sigma1 ... sigman)
V1 :: [a/alpha]Q1, ..., Vn :: [a/alpha]Qn
```


The case \[IOCan3\] is the most complex one: we need to generate a function from the evidence generated by an implication. Such an implication generates a series of bindings, which we denote here using `[]`. Note that we abstract by values, types and constraints, but this is OK, because it is a System FC term.

```wiki
W1 :: InstanceOf sigma2 (forall a. Q1 => tau1)

---->

W1 :: sigma2 -> (forall a. Q1 => tau1)
W1 = \x -> /\a -> \(d : Q1) -> let [] in (W2 x)

W2 :: forall a. (d : Q1) => (W2 :: InstanceOf sigma2 tau1)
```

### Zonking


While in the solver we want `InstanceOf` constraints to have their own identity. However, when converted to Core, they must be converted into functions. This means that the types of `EvTerms` with `InstanceOf` constraints also need to change to functions. This is done in the zonking phase.

### Design choice: `InstanceOf` and `->`


In the designed proposed above, `->` is treated as any other type constructor. That means that if we are canonicalizing `InstanceOf (sigma3 -> sigma4) (sigma1 -> sigma2)`, the result is `sigma1 ~ sigma3 /\ sigma2 ~ sigma4`. That is, `->` is treated invariantly in both arguments. Other possible design choices are:

- `->` treated co- and contravariantly, leading to `InstanceOf sigma3 sigma1 /\ InstanceOf sigma2 sigma4`.
- Treat only the co-domain covariantly, leading to `sigma1 ~ sigma3 /\ InstanceOf sigma2 sigma4`.


Which are the benefits of each option?

## Changes to approximation


One nasty side-effect of this approach is that the solver may produce non-Haskell 2010 types. For example, when type checking `singleton id`, where `singleton :: forall a. a -> [a]` and `id :: forall a. a -> a`, the result would be `forall a. InstanceOf (forall b. b -> b) a => [a]`. In short, we want to get rid of the `InstanceOf` constraints once a binding has been found to type check. This process is part of a larger one which in GHC is known as **approximation**.


There are two main procedures to move to types without `InstanceOf` constraints:

- Convert all `InstanceOf` into type equality. In the previous case, the type of `singleton id` is `forall a. a ~ forall b. b -> b => [a]`, or equivalently, `[forall b. b -> b]`.
- Generate a type with the less possible polymorphism, by moving quantifiers out of the `InstanceOf` constraints to top-level. In this case, the type given to `singleton id` is `forall b. [b -> b]`.


We aim to implement the second option, since it leads to types which are more similar to those already inferred by GHC. Note that this approximation only applies to unannotated top-level bindings: the user can always ask to give `[forall a. a -> a]` as a type for `singleton id` via an annotation.


The procedure works by appling repeatedly the following rules:

```wiki
InstanceOf (forall b. Q => tau) a  ---->  a ~ [b/beta]tau  /\  [b/beta]Q
InstanceOf a (forall b. Q => tau)  ---->  a ~ (forall b. Q => tau)
```


The first rule is a version of \[IOCon2\] which applies to canonical `InstanceOf` constraints. The second rule ensures that the `InstanceOf` constraint is satisfied.

*Implementation note*: change the `simplifyInfer` function in `compiler/typecheck/TcSimplify.hs` to generate candidate approximations using the previous two rules.

## Changes to constraint generation


Constraint generation is the phase prior to solving, in which constraints reflecting the relations between types in the program are created. We describe constraint generation rules in this section using the same formalism as OutsideIn(X), that is, as a judgement `Gamma |- e: tau --> C`: under a environment `Gamma`, the expression `e` is assigned type `tau` subject to constraints `C`.


In principle, the only rule that needs to change is that of variables in the term level, which is the point in which instantiation may happen:

```wiki
   x : sigma \in \Gamma        alpha fresh
--------------------------------------------- [VAR]
Gamma |- x : alpha --> InstanceOf sigma alpha
```


Unfortunately, this is not enough. Suppose we have the following piece of code:

```wiki
(\f -> (f 1, f True)) (if ... then id else not)
```


We want to typecheck it, and we give the argument `f` a type variable `alpha`, and each of its appearances the types variables `beta` and `gamma`. The constraints that are generated are:

```wiki
InstanceOf alpha beta  [usage in (f 1)]
InstanceOf alpha gamma [usage in (f True)]
InstanceOf (forall a. a -> a) alpha
InstanceOf (Bool -> Bool)     alpha
```


At this point we are stuck, since we have no rule that could be applied. One might think about applying transitivity of `InstanceOf`, but this is just calling trouble, because it is not clear how to do this without losing information.


Our solution is to make this situation impossible by generating `beta ~ alpha` and `gamma ~ alpha` instead of their `InstanceOf` counterparts. We do this by changing the \[VAR\] rule in such a way that `~` is generated when the variable comes from an unannotated abstraction or unannotated `let`. The environment is responsible for keeping track of this fact for each binding, by a small tag.

```wiki
    x :_~ sigma \in \Gamma
------------------------------ [VAR~]
Gamma |- x : sigma --> nothing
```


Notice the change from `:` to `:_~` in the rule. As stated above, some other rules need to be changed in order to generate this tag for their enclosed variables:

```wiki
alpha fresh    Gamma, (x :_~ alpha) |- e : tau --> C
----------------------------------------------------
       Gamma |- \x -> e : alpha -> tau --> C

Gamma, (x :_~ alpha) |- e : tau1 --> C1    Gamma, (x :_~ alpha) |- b : tau2 --> C2
----------------------------------------------------------------------------------
           Gamma |- let x = e in b : tau2 --> C1 /\ C2 /\ alpha ~ tau1
```


With this change, our initial example leads to an error (`f cannot be applied to both Bool and Int`), from which one can recover by adding an extra annotation. This is a better situation, though, that getting stuck in the middle of the solving process.

**Summary**: a PDF with the entire set of rules is available as an attachment. [only-gen.pdf](/trac/ghc/attachment/wiki/ImpredicativePolymorphism/Impredicative-2015/only-gen.pdf)

*Implementation note*: the type of local environments, `TcLclEnv` in `compiler/typecheck/TcExpr.hs`, needs to be upgraded to take into account whether a variable is tagged as generating `~`. Maybe just change `type TcTypeEnv = NameEnv (TcTyThing, Bool)`?

*Implementation note*: constraint generation appears in GHC source code as `tcExpr` in `compiler/typecheck/TcExpr.hs`.

### Adding propagation


Still, this is not enough! Suppose you write the following code:

```wiki
f :: (forall a. a -> a) -> (Int, Bool)
f x = (x 1, x True)

g = f (\x -> x)
```


None of them will work! The problem is that, in the first case, we do not use the information in the signature when generating constraints for the function. Thus, `x` will be added to the environment with the `~` tag, effectively forbidding to be applied to both `Int`s and `Bool`s.


In the second case the solver does not know that it should generalize at the point of the `\x -> x` expression. Thus, we will come to a point where we have `tau -> tau ~ forall a. a -> a`, which leads to an error, since quantified and not quantified types cannot be equated.


However, we expect both cases to work. After all, the information is there, we only have to make it flow to the right place. This is exactly the goal of adding **propagation** to the constraint generation phase. Luckily, GHC already does some propagation now, as reflected in the type of the function `tcExpr`. The main change is that, whereas the current implementation pushes down and infers shapes of functions, the new one is simpler, and only pushes information down. A **PDF with the rules** is available as an attachment. [only-prop.pdf](/trac/ghc/attachment/wiki/ImpredicativePolymorphism/Impredicative-2015/only-prop.pdf)


The most surprising rule is the one named \[AppFun\], which applies when we have a block of known expressions `f1 ... fm` whose type can be recovered from the environment followed by some other freely-shaped expressions. For example, the case of `f (\x -> x)` above, where `f` is in the environment of `g`. In that case, we compute the type that the first block ought to have, and propagate it to the rest of arguments.


The reason for including a block of `fi`s is to cover cases such as `runST $ do ...`, or more clearly `($) runST (do ...)`, where some combinators are used between functions. Should the rule \[AppFun\] only include the case `f e1 ... fm`, the common `runST $ do ...` could not be typed without an annotation.

## Type classes and families


There are some unwanted interactions between type classes and families and the `InstanceOf` constraint. For example, if we want to type `[] == []`, we obtain as canonical constraints:

```haskell
Eq a /\ InstanceOf (forall b. [b]) a
```


At this point we are stuck. We need to instantiate `b` before `Eq` can scrutinize its argument to check whether an instance is available. One possibility is to instantiate by default every type linked to a variable appearing in a type class or type family.


That solution poses its own problems. Consider the following type family:

```haskell
type family F a b
type instance F [a] b = b -> b
```


Using the rule of always instantiating, the result of `gamma ~ F [Int] b, InstanceOf (forall a. a -> a) b` is `gamma ~ (delta -> delta) -> (delta -> delta)`. We have lost polymorphism in a way which was not expected. What we hoped is to get `gamma ~ (forall a. a -> a) -> (forall a. a -> a)`.


Thus, we need to have a way to instantiate variables appearing in type classes and families, but only as necessary. We do this by temporarily instantiating variables when checking for axiom application, and returning extra constraints which make this instantiation possible if the match is successful. For example, in the previous case we want to apply the axiom `forall e. Eq e => Eq [e]`, and thus we need to instantiate `a`. We return as residual constraints `Eq e /\ Eq a ~ Eq [e]`, and the solver takes care of the rest, that is, `InstanceOf (forall b. [b]) [e]`.

*Implementation note*: the changes need to be done in the `lookupInstEnv'` function in `compiler/types/InstEnv.hs`. The solver needs to be changed at `compiler/typecheck/TcInteract.hs` to use the new information.
