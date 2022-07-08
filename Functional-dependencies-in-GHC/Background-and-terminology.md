This page summarises background and terminology related to functional dependencies.

Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

**Contents**

[[_TOC_]]

# 1. Terminology

## 1.1 Confluence

**Confluence** means that:

* A program will either typecheck or not; it can't typecheck one day and fail the next day. Specifically: minor revisions to GHC (which may change the solver algorithm) cannot change the result of typechecking. Similarly, adding e.g. irrelevant constraints cannot change the result of typechecking.
* If it typechecks, it'll have the same meaning.  (Exception: with overlapping instances and different instances in scope in different modules.)
* You can re-order the constraints in a signature without affecting whether the program typechecks, or what it means
  ```
  f :: (C a, D a b) => blah   -- These two should
  f :: (D a b, C a) => blah   -- behave the same
  ```
* You can re-order instance declarations without affecting whether the program typechecks, or what it means

Ticket #18851 has some interesting examples of non-confluence.

## 1.2 Termination

**Termination** means that type inference terminates.  For example
```
instance Eq a => Eq [a] where ...
instance Eq Int where ...
```
If you want to solve `[W] Eq [[[Int]]]` you can use the first instance to reduce it to `[W] Eq [[Int]]`, and then again
to `[W] Eq [Int]` and then `[W] Eq Int`.  Now use the second instance decl.

But if you had
```
instance C [[a]] => C [a] where ...
```
and wanted to solve `[W] C [a]`, you could use the instance decl, to get a new sub-goal `[W] C [[a]]`.  Then repeat ad infinitum.

## 1.3 The Paterson conditions

The Paterson conditions try to ensure termination, by ensuring that, when you use an instance decl, the sub-goals are "smaller" than the head. E.g.
```
instance Eq a => Eq [a]
```
If you are trying to solve `Eq [Maybe Int]`, you can use the instance decl to get the smaller goal `Eq (Maybe Int)`.

The Paterson conditions are described in the user manual under [Instance termination rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=undecidable#instance-termination-rules).

Note: the Paterson conditions subsume the Bound Variable Condition (Defn 8) of the JFP-paper.

## 1.4 The relative importance of confluence and termination

Generally,
* We'd like to be able to guarantee both termination and confluence.
* We are happy to risk non-termination when we ask for it; insisting on guaranteed termination is very restrictive
* We are extremely reluctant to risk non-confluence.

## 1.5 Full functional dependencies

In a class decl, we say that *a functional dependency is **full** if all but one of the class tyvars appears on the LHS of the fundep*.  For example.

```
class C1 x y z |  x y -> z            -- Full
class C2 x y z |  x -> z              -- Not Full
class C3 x y z |  x z -> y, y -> x    -- (x z -> y) is Full; but (y -> x) is not
```

## 1.6 Multi-range functional dependencies

See Section 6.3 of the JFP paper.

If the SCC holds, then a multi-range fundep `a b -> c d` is just short for `a b -> c, a b -> d`; that is, we can always put just one tyvar on the RHS.  But this is not true of the LCC.  E.g.
```
class C a b c | a -> b c
instance C a b b => C [a] [b] [b]
```
Suppose we have `[W] C [p] q r`.  Then 
* A multi-range FD `a -> b c` will emit improvement equalities `q ~ [beta], r ~ [beta]`; but 
* Two single-range FDs `a -> b, a -> c` will emit `q ~ [beta1], r ~ [beta2]`, with distinct unification variables `beta1`, `beta2`.


-------------------------

# 2. Coverage conditions

The *coverage condition* applies to each individual instance declaration.

## 2.1 Strict coverage condition (SCC)

The **(strict) coverage condition** is given in the user manual under [Instance termination rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=undecidable#instance-termination-rules) and in Defn 7 of of the JFP-paper.  Consider
```
class C2 a b | a -> b
instance C2 [p] p     -- Satisfies strict coverage
instance C2 [p] [q]   -- Does not satisfy strict coverage
```
From the class decl for `C2` the first parameter should fix the second; that is true of the first instance here, but not of the second, because `q` is not fixed when you fix `p`.

## 2.2 Liberal coverage condition (LCC)

The **liberal coverage condition** is more liberal because it takes into account the context of the instance.  For example
```
instance {-# LIBERAL #-} C2 p q => C2 [p] [q]
```
I'm using `{-# LIBERAL #-}` to signal that the instance only satisfies the liberal coverage condition, not the strict one.
The intuition is that since `p` fixes `q` in the context, so `[p]` indirectly fixes `[q]`.

See Defn 12 of the JFP-paper (Section 6.1), which calls it the "weak coverage condition".

-------------------------

# 3. Instance consistency conditions

The *instance consistency condition* applies to each pair-wise combination of instance declarations.

## 3.1 Strict instance consistency condition (SICC)

Consider these two instances
```
class C2 a b | a -> b
instance C2 Int Bool
instance C2 Int Char
```
They are mutually inconsistent. The fundep on `C2` says that `a` determines `b`; but if `a`=`Int`, then `b` can be both `Bool` and `Char`.

The instance consistency condition ensures that instances are pair-wise consistent. It appears not to be documented in GHC's user manual, but it is Defn 6 in the JFP-paper.

**(Strict) instance consistency condition**.  Consider a declaration for class TC, and any pair of instance declarations for that class:
```
class blah => TC a1 ... an | fd1; ...; fdm
instance D1 => TC t1...tn
instance D2 => TC s1...sn
```
Then, for each functional dependency `fdi`, of form `ai1; ...; aik -> ai0`, the following
condition must hold: for any substitution S such that S(ti1; ..., tik) = S(si1; ..., sik)
we must have that S(ti0) = S(si0).

## 3.2 Liberal instance consistency condition (LICC)

Consider
```
class D p q | p -> q
class C a b c | b -> c
instance {-# LIBERAL #-} D p q => C Int  p [q]
instance {-# LIBERAL #-} D r s => C Bool r [s]
```
These instances do not satisfy the (strict) instance consistency condition.
*Yet if they are not allowed, then the liberal coverage condition (LCC) is not very useful.*
If either instance of the pair satisfies LCC but not SCC, then the pair will not satisfy strict instance consistency (SICC).
In short, LCC is incompatible with SICC.

That motivates the definition of liberal instance consistency:

**Liberal instance consistency condition**.  Consider a declaration for class TC, and any pair of instance declarations for that class:
```
class blah => TC a1 ... an | fd1; ...; fdm
instance D1 => TC t1...tn
instance D2 => TC s1...sn
```
Then, for each functional dependency `fdi`, of form `ai1; ...; aik -> ai0`, the following
condition must hold: for any substitution S such that S(ti1; ..., tik) = S(si1; ..., sik)
there must exist a substitution T such that
we must have that T(S(ti0)) = T(S(si0)); or equivalently S(ti0) and S(si0) are unifiable.

## 3.3 Relevant tickets

See #15632 (Undependable dependencies).