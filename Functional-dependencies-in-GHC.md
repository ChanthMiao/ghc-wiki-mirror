This page summarises examples and issues about functional dependencies in GHC.

[[_TOC_]]

## 1. Key papers

* [Type classes with functional dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html), Mark Jones, ESOP 2000.  The original fundep paper.
* [Understanding functional dependencies via constraint handling rules](https://www.microsoft.com/en-us/research/publication/understanding-functional-dependencies-via-constraint-handling-rules/), Sulzmann et al, JFP 2006.  This journal paper has a lot of examples and we cite it frequently below as "JFP-paper".
* [Elaboration on functional dependencies](https://people.cs.kuleuven.be/~tom.schrijvers/portfolio/haskell2017a.html), Karachalias and Schrijvers, Haskell Symposium 2017.  This paper shows how to translate functional dependencies into type families.

Key proposals:
* Coverage condition: [Per-instance DYSFUNCTIONAL pragma](https://github.com/ghc-proposals/ghc-proposals/pull/374).
* Instance consistency condition: [Explore ways to weaken or abandon the Instance Consistency Condition](https://github.com/ghc-proposals/ghc-proposals/issues/391).

-------------------------------

## 2. Terminology

### Confluence

**Confluence** means that:

* A program will either typecheck or not; it can't typecheck one day and fail the next day. Specifically: minor revisions to GHC (which may change the solver algorithm) cannot change the result of typechecking. Similarly, adding e.g. irrelevant constraints cannot change the result of typechecking.
* If it typechecks, it'll have the same meaning.  (Exception: with overlapping instances and different instances in scope in different modules.)
* You can re-order the constraints in a signature without affecting whether the program typechecks, or what it means
  ```
  f :: (C a, D a b) => blah   -- These two should
  f :: (D a b, C a) => blah   -- behave the same
  ```
* You can re-order instance declarations without affecting whether the program typechecks, or what it means

#18851 has some interesting examples of non-confluence.

### Termination

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

### The Paterson conditions

The Paterson conditions try to ensure termination, by ensuring that, when you use an instance decl, the sub-goals are "smaller" than the head. E.g.
```
instance Eq a => Eq [a]
```
If you are trying to solve `Eq [Maybe Int]`, you can use the instance decl to get the smaller goal `Eq (Maybe Int)`.

The Paterson conditions are described in the user manual under [Instance termination rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=undecidable#instance-termination-rules).

Note: the Paterson conditions subsume the Bound Variable Condition (Defn 8) of the JFP-paper.

### The relative importance of confluence and termination

Generally,
* We'd like to be able to guarantee both termination and confluence.
* We are happy to risk non-termination when we ask for it; insisting on guaranteed termination is very restrictive
* We are extremely reluctant to risk non-confluence.


-------------------------

## 3. Coverage conditions

The *coverage condition* applies to each individual instance declaration.

### Strict coverage condition (SCC)

The **(strict) coverage condition** is given in the user manual under [Instance termination rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html?highlight=undecidable#instance-termination-rules) and in Defn 7 of of the JFP-paper.  Consider
```
class C2 a b | a -> b
instance C2 [p] p     -- Satisfies strict coverage
instance C2 [p] [q]   -- Does not satisfy strict coverage
```
From the class decl for `C2` the first parameter should fix the second; that is true of the first instance here, but not of the second, because `q` is not fixed when you fix `p`.

### Liberal coverage condition (LCC)

The **liberal coverage condition** is more liberal because it takes into account the context of the instance.  For example
```
instance {-# LIBERAL #-} C2 p q => C2 [p] [q]
```
I'm using `{-# LIBERAL #-}` to signal that the instance only satisfies the liberal coverage condition, not the strict one.
The intuition is that since `p` fixes `q` in the context, so `[p]` indirectly fixes `[q]`.

See Defn 12 of the JFP-paper (Section 6.1), which calls it the "weak coverage condition".

-------------------------

## 4. Instance consistency conditions

The *instance consistency condition* applies to each pair-wise combination of instance declarations.

### Strict instance consistency condition (SICC)

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

### Liberal instance consistency condition (LICC)

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

### Relevant tickets

See #15632 (Undependable dependencies).


-----------------------

## 5. GHC today

GHC today does this:

* By default: GHC uses the strict coverage condition and imposes the Paterson conditions.

* If `UndecidableInstances` is on, GHC
  * uses the liberal (not strict) coverage condition
  * lifts the Paterson conditions

* Always: GHC implements liberal instance consistency unconditionally.  See `Note [Bogus consistency check]` in `GHC.Tc.Instance.Fundeps`.  (GHC presumably does this because SICC is incompatible with LCC.)

-------------------------------

## 6. Some key examples

### Examples 0: liberal coverage condition is highly desirable

Many real-life examples need the LCC. Here are some [everyone: add more]:

* Example 19 from JFP-paper (and Control.Monad) library
  ```
  class (Monad m) => MonadReader r m | m -> r
  instance (Monoid w, MonadReader r m) => MonadReader r (WriterT w m)
  ```
* Example 6 from JFP-paper (also demonstrates the risk of non-termination)
  ```
  class Mul a b c | a b -> c where
    (*)::a->b->c
  instance Mul a b c => Mul a (Vec b) (Vec c) where ...
  ```

### Example 1: liberal coverage breaks termination

The liberal coverage condition means that type inference can diverge.
Examle from 5.2 of the JFP-paper:
```
class Mul a b c | a b -> c
instance {-# LIBERAL #-} Mul a b c => Mul a (Vec b) (Vec c)
```
This satisfies Paterson and LIBERAL.
Now suppose we are solving the constraint `[W] Mul alpha (Vec beta) beta`
* Fundeps give us `beta := Vec delta`
* Substituting we have `[W] Mul alpha (Vec (Vec delta)) (Vec delta)`
* That matches the instance decl, giving `[W] Mul alpha (Vec delta) delta`
* And now we are back where we began.

### Example 2: LCC and LICC do weird improvement (#10675 OP)

Consider
```
class CX x a b | a -> b where
  op :: x -> a -> b
instance                                CX Bool [x] [x]
instance {-# LIBERAL #-} CX Char x y => CX Char [x] [Maybe y]

f x = op True [x]
```
The instance decls require LICC.  But notice that they do not overlap, because of the first parameter.

From `f` we get `[W] CX Bool [alpha] beta`.
* Now GHC takes fundeps from *both* instances, giving `beta ~ [alpha]` and `beta ~ [Maybe gamma]`
* That leaves us with `CX Bool [Maybe gamma] [Maybe gamma]`
* We can solve that from the first instance decl.
* So we infer `f :: Maybe g -> [Maybe g]`.
  Bizarre.  Where did that `Maybe` come from?  It's nothing to do with it.

### Example 3: LCC and LICC threaten confluence

Consider:
```
class D a b c | b -> c
instance {-# LIBERAL #-} (q ~ Int)  => D Int  p (Int,q)
instance {-# LIBERAL #-} (s ~ Bool) => D Bool r (s,Bool)
```
These instances satisfy the Liberal Coverage and Liberal Instance Consistency conditions.

Now suppose we are trying to solve a Wanted constraint `[W] C alpha beta (gamma, delta)`.
* We'll get fundeps from both instances, yielding `gamma ~ Int` and `delta ~ Bool`.
* But if `alpha` later turns out to be `Int`, we'll select the first instance decl, getting `delta ~ Int`, resulting in a contradiction.
* If, on the other hand, we learned `alpha := Int` and `gamma := Int` earlier, we'd have picked the first instance immediately, and succeeded.

This reflects a loss of confluence.

### Example 4: Even LICC is too restrictive

Consider (assuming overlapping instances):
```
class TypeEq a b (res :: Bool)  | a b -> res
instance TypeEq a a True
instance TypeEq a b False
```
These instances satisfy SCC, but not SICC.
Nor does it satisfy the more liberal LICC, because True and False are not unifiable!  But imagine we rewrote it like this:
```
instance r ~ True  => TypeEq a a r
instance r ~ False => TypeEq a b r
```
Now the fundep is effectively vacuous, but if it remains we'd need LCC and LICC.  But the program works fine: the overlapping-instance technology will pick an instance only when it is the unique one, and that will fix `r`.

But it's a bit un-satisfying to have to encode our desired behaviour like this.
(Question: with bidirectional fundeps is this encoding even always possible?)

### Example 5: Even LCC is too restrictive

We can use fundeps to support record selection in records with polymorphic fields (#18759).  Consider
```
class HasField (name :: Symbol) s a | name s -> a where
  getField :: s -> a

data T = MkT { fld :: forall a. [a] -> [a] }

instance {-# DYSFUNCTIONAL #-} HasField "fld" T ([p] -> [p])
  getField (MkT f) = f

f x = (getField @"fld" x, True)
```
Here the instance doesn't even satisfy the LCC, so I've marked it DYSFUNCTIONAL.  And yet it is very useful!
* From `f` we get `[W] HasField "fld" T alpha`.
* Using the fundep we can get `alpha ~ ([beta] -> [beta])`, which is just what we want.

In effect, the fundep gives the *shape* of `alpha` but not its complete type.  This is a pretty compelling example.

Here is [a real-world example of someone wanting DYSFUNCTIONAL](https://stackoverflow.com/questions/65514023/how-to-require-functional-dependencies-in-kind-signature).

### Example 6: LIBERAL can get you DYSFUNCTIONAL

It turns out that with LIBERAL and UNDECIDABLE you can trick GHC into lifting the coverage condition algotether, effectively achieving DYSFUNCTIONAL.  Consider, this variant of Example 5:
```
instance {-# LIBERAL, UNDECIDABLE #-}
         HasField "fld" T ([p] -> [p])
         => HasField "fld" T ([p] -> [p])
  getField (MkT f) = f
```
We have added a strange context to the instance declaration, equal to itself!  Now the LCC is satisfied.  You might think that the instance is now non-terminating, because solving `HasField "fld" T ([p]->[p])` via the intance gives us a new sub-goal `HasField "fld" T ([p]->[p])`, and so on.

But GHC's type-class constraint solver has a long-standing trick whereby it solves goals co-inductively. I think it was first documented in [Scrap your boilerplate with class](https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-with-class/), where it is *essential* to allow SYB-with-class to work at all.  You might enjoy the paper; the coinductive part is discussed in Section 5.   Coinduction is switched on all the time, but it only has an effect when you have `UndecidableInstances`, which allows instance declarations that don't provably terminate.

So in priciple, LIBERAL+UNDECIDABLE lets you express DYSFUNCTIONAL (no coverage condition at all).  But it's a weird coding trick, and so we leave DYSFUNCTIONAL in our vocabulary, for now anyway, to mean "lift coverage condition".
