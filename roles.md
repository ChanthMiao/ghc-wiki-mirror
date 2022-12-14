# Roles


The idea of *roles* comes from the paper [Generative Type Abstraction and Type-level Computation](http://www.seas.upenn.edu/~sweirich/papers/popl163af-weirich.pdf), published at POPL 2011. The implementation of roles in GHC, however, is somewhat different than stated in that paper. This page focuses on the user-visible features of roles. 


Role annotations are enabled using `{-# LANGUAGE RoleAnnotations #-}`.


See also

- Our ICFP 2014 paper [Safe Coercions](https://richarde.dev/papers/2014/coercible/coercible.pdf), which gives lots of motivation and details, including the `Coercible` class.\]
- Richard's unpublished paper [An overabundance of equality](http://cs.brynmawr.edu/~rae/pubs.html)
- The user-level [wiki page about Coercible](http://www.haskell.org/haskellwiki/GHC/Coercible)
- [Roles2](roles2) which identifies a difficulty with the design in the paper, and some possibilities for solving it.
- [RolesImplementation](roles-implementation) talks about the implementation in GHC. 
- [Richard's blog post about roles](http://typesandkinds.wordpress.com/2013/08/15/roles-a-new-feature-of-ghc/). (Note: some aspects of that blog post are out of date, as of December 17, 2013.)
- This email thread: [More GND + role inference woes](http://www.haskell.org/pipermail/ghc-devs/2013-October/003003.html).
- [Safe Roles](safe-roles) discusses safety issues (from abstraction, not memory-safety point-of-view) around Roles and how they might be addressed. The specific focus is on Safe Haskell.

## Tickets

See the ~roles label.

## The problem we wish to solve


GHC has had a hole in its type system for several years, as documented in #1496, #4846, #5498, and #7148. The common cause behind all of this is the magic behind `-XGeneralizedNewtypeDeriving` (GND). Here is an example:

```haskell
newtype Age = MkAge { unAge :: Int }

type family Inspect x
type instance Inspect Age = Int
type instance Inspect Int = Bool

class BadIdea a where
  bad :: a -> Inspect a

instance BadIdea Int where
  bad = (> 0)

deriving instance BadIdea Age
```


This code is accepted by GHC 7.6.3. Yet, it goes wrong when you say `bad (MkAge 5)` -- we see the internal encoding of `Bool`! Let's trace what is happening here.


A newtype is a new algebraic datatype that wraps up exactly one field (in our example, of type `Int`). Yet, the semantics of Haskell makes a guarantee that wrapping and unwrapping a value (with `MkAge` or `unAge`) has no runtime cost. Thus, internally, we must consider `Age` to be wholly equivalent to `Int`.



The problem with this idea comes with type families. (There are other ways to tickle the bug, but one example is enough here.) A type family can branch on *Haskell* type, and of course, in Haskell (unlike in the internals of a compiler), `Age` is most certainly *not* `Int`. (If it were, newtypes would be useless for controlling instance selection, a very popular use case.) So, in our example, we see that `Inspect Age` is `Int`, but `Inspect Int` is `Bool`. Now, note the type of `bad`, the method in class `BadIdea`. When passed an `Int`, `bad` will return a `Bool`. When passed an `Age`, `bad` will return an `Int`. What happens on the last line above, when we use GND? Internally, we take the existing instance for `Int` and just transform it into an instance for `Age`. But, this transformation is very dumb -- because `Age` and `Int` are the same, internally, the code for the `Age` instance and the code for the `Int` instance are the same. This means that when we call `bad (MkAge 5)`, we run `5` through the existing implementation for `bad`, which produces a `Bool`. But, of course, the type of `bad (MkAge 5)` is `Int`, and so we have effectively converted a `Bool` to an `Int`. Yuck.


## The solution


What to do? It turns out we need a subtler definition of type equality than what we have had. Specifically, we must differentiate between *nominal* equality and *representational* equality. Nominal equality (called C in the paper cited above) is the Haskell equality we all know and love. If two types have the same name, they are nominally equal. If they don't have the same name (expanding type synonyms), they are not nominally equal. Representational equality, on the other hand, shows that two types have the same *representation*. This is the equality that newtypes produce -- `Age` is representationally equal to `Int`, but they are not nominally equal.

Note/Request: Could we please define the terms *nominal* and *representational* a bit more clearly? Nominal is defined as "having the same name" fairly obviously, but representational - is this when two types are composed of the same types, or is it more involved? Is `(Int, String)` representationally equivalent to `(String, Int)`? Is `{ one: String, two: Int }` representationally equivalent to `{ three: String, four: Int }`? Please say more about this, and define the important terms we're using.

Datatypes, classes, and type synonyms can be parametric in their type arguments or not. By "parametric", I mean that they do not *inspect* the type argument. A non-parametric type variable is inspected. Here are some examples:

```haskell
data List a = Nil | Cons a (List a)    -- parametric
data GADT a where                      -- non-parametric
  GAge :: GADT Age
  GInt :: GADT Int

class C1 a where                       -- parametric
  foo :: a -> List a

class C2 a where                       -- non-parametric
  bar :: a -> GADT a

class BadIdea a where                  -- non-parametric
  bad :: a -> Inspect a
```


In the terminology here, non-parametric types and classes care, in some fundamental way, what type parameter they are given. Parametric ones don't. We can generalize this idea a bit further to label each type variable as either parametric or not. For example,

```haskell
data Mixed a b where
  MInt :: a -> Mixed a Int
  MAge :: a -> Mixed a Age
```


is parametric in its first parameter but not its second. We say that a parametric type variable has a representational role and a non-parametric one has a nominal role.

## `Coercible`


The libraries with GHC 7.8 offer a new class

```haskell
class Coercible a b where
  coerce :: a -> b
```


The idea is that a `Coercible` instance exists allowing coercions between any two types that are representationally equal. A programmer can then use `coerce` to get between the types. The instances themselves are magically generated as necessary; it is not allowed for programmers to declare their own `Coercible` instances. So, we have `Coercible Age Int` but never `Coercible Bool Int`.


The reason we need roles is to describe how these representational equalities (or, equivalently, `Coercible` instances) "lift" through other types. For example, is `[Age]` representationally equal to `[Int]`? Sure. But, is `GADT Age` representationally equal to `GADT Int`? I hope not!


The rule is this: we have `instance Coercible a b => Coercible (T a) (T b)` if and only if the first parameter of `T` has a representational role. Thus, we have `instance Coercible a b => Coercible [a] [b]` but not `instance Coercible a b => Coercible (GADT a) (GADT b)`. This generalizes straightforwardly when there are multiple parameters, and it's worth noting that `Coercible` is always reflexive, even when nominal roles are involved.

## GeneralizedNewtypeDeriving implemented using `coerce`


Now that we have all of this `Coercible` machinery, we can define the behavior of GND in terms of it -- we simply `coerce` each method of the derived class. For example:

```haskell
newtype RestrictedIO a = MkRIO { unRIO :: IO a }
  deriving Monad
```


generates

```haskell
instance Monad RestrictedIO where
  return = coerce (return :: a -> IO a) :: forall a. a -> RestrictedIO a
  (>>=) = coerce ((>>=) :: IO a -> (a -> IO b) -> IO b) :: forall a b. RestrictedIO a -> (a -> RestrictedIO b) -> RestrictedIO b
  fail = coerce (fail :: String -> IO a) :: forall a. String -> RestrictedIO a
```


Note that each of these is just a call to `coerce` over the method in the instance for the newtype's representation type (in this case, `IO a`). All those type annotations are necessary to make sure that the type checker does the right conversion (and that scoped type variables are bound appropriately).


Putting all of this together, GND works exactly when each of the methods being derived is `Coercible` into the new type.

## Phantom parameters


It turns out that a third role is also useful (though unnecessary for type soundness): the phantom role. It is often the case that programmers use type variables simply to constrain the type checker, not to make any statement about the runtime representation of a type. For example `data Phant a = MkPhant Int`. Because `a` doesn't appear on the right-hand side, we say that `a` is at role phantom. Why is this nice? Because it allows us to say that, say, `Phant Int` and `Phant Bool` are representationally equal, because they really do have the same representation. Thus, there would be `instance Coercible (Phant a) (Phant b)` for any `a` and `b`.

## Role inference


How do we know what role a type parameter should have? We use role inference! We start with a few base facts: `(->)` has two representational parameters; `(~)` has two nominal parameters; and all type families' parameters are nominal. Then, we just propagate the information. By defaulting parameters to role phantom, any parameters unused in the right-hand side (or used only in other types in phantom positions) will be phantom. Whenever a parameter is used in a representational position (that is, used as a type argument to a constructor whose corresponding variable is at role representational), we raise its role from phantom to representational. Similarly, when a parameter is used in a nominal position, its role is upgraded to nominal. We never downgrade a role from nominal to phantom or representational, or from representational to phantom. In this way, we infer the most-general role for each parameter.


The exception to the above algorithm is for classes: all parameters for a class default to a nominal role. This is because we generally consider, say, `Ord Age` and `Ord Int` to be quite distinct, even if their representation is the same under the hood. Changing the behavior of type classes is a major use case for newtypes, and we wouldn't want to subvert that!

## Role annotations


As we have learned with type and kind inference, sometimes the programmer wants to constrain the inference process. For example, the base library contains the following definition:

```haskell
data Ptr a = Ptr Addr#
```


The idea is that `a` should really be a representational parameter, but role inference assigns it to phantom. This makes some level of sense: a pointer to an `Int` really *is* representationally the same as a pointer to a `Bool`. But, that's not at all how we want to use `Ptr`s! So, we want to be able to say

```haskell
type role Ptr representational
data Ptr a = Ptr Addr#
```


The `type role` annotation forces the parameter `a` to be at role representational, not role phantom. We, then, of course, need to *check* the user-supplied roles to make sure they don't break any promises. It would be bad if the user could make `BadIdea`'s role be representational!


If `Ptr` were to have multiple type parameter we would have used multiple `nominal`/`representational` annotations 

```haskell
type role Foo representational representational
data Foo a b = Foo Int
```


The other place where role annotations may be necessary are in .hs-boot files, where the right-hand sides of definitions can be omitted. As usual, the types/classes declared in an .hs-boot file must match up with the definitions in the .hs file, including down to the roles. The default role is representational in hs-boot files, corresponding to the common use case. Note that this **will break code**. But, the change is necessary to close the type-safety hole discussed above.


Role annotations are allowed on type variables in `data`, `newtype`, and `class`, declarations. They will not be allowed on type/data family declarations or in explicit `forall`s in function type signatures.

## Roles and `Traversable`


Though a minor issue in the overall scheme, work on Roles had led to an interesting interaction with `Traversable`, excerpted here:

```haskell
class Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
```


According to the rules for roles, the parameter `t` must be at role nominal, as it is used as a parameter to the type variable `f`. We must account for the possibility that `f` will be instantiated with a type whose last parameter is at role nominal, so we force `t` to be at role nominal as well.


This means that GND no longer works with Traversable. But, DeriveTraversable *does* still work. However, GHC previously preferred using GND over DeriveTraversable when both were available, which assumedly produced the same code. How is this all possible? If GND doesn't work anymore, is there something wrong with DeriveTraversable? The answer is that GND and DeriveTraversable make *different* instances, contrary to expectations. The reason is that DeriveTraversable has to use `fmap` to cast the result of `traverse` from the representation type back to the newtype. According to the Functor laws, `fmap`ping this cast should be a no-op (the law of interest is `fmap id == id`). But, if that law is not obeyed, `fmap`ping the cast may change the result of the `traverse`. Contrast this with a GND instance, which magically casts the result, without using `fmap`. If the Functor law is not obeyed, these two instances have different behavior.


Despite this, I believe that using GND with `Traversable` is indeed type-safe. Why? Because of the parametricity guaranteed in `Functor` and `Applicative`. The reason GND is prohibited with `Traversable` is that we are worried `f`'s last parameter will be at role nominal. While it is possible to write `Functor` and `Applicative` instances for such a type, the methods of those classes can't really use the any constructors that force the role to be nominal. For example, consider this:

```haskell
data G a where
  GInt :: a -> G Int
  Ga   :: a -> G a

instance Functor G where
  fmap f (GInt _) = error "urk"  -- no way out here
  fmap f (Ga a)   = Ga (f a)

instance Applicative G where
  pure a = Ga a
  (Ga f) <*> (Ga a) = Ga (f a)
  _ <*> _ = error "urk" -- no way out here, either
```


There's no way to usefully interact with the `GInt` constructor and get the code to type-check. Thus, I believe (but haven't yet proved), that using GND with `Traversable` is safe, because the `f` in `traverse` can't ever do bad things with its argument. If you, the reader, have more insight into this (or a counterexample!), please write!

# Changing default role to nominal


In GHC 7.8, unannotated datatype parameters default to phantom. This means that most normal parameters are given a representational role. It has been argued that perhaps nominal is a better (safer) default, and that users should specify representational when they want it. The problem with a nominal default is that it breaks all current usages of GND by default. Furthering the problem, when a user is unable to use GND it's the *library* that has to change, not the user's code.


On Mar 31, 2014, Dominique Devriese writes the following suggestion:


What I was wondering about is if the dilemma could be solved by
choosing nominal-by-default in the long term for the role inference
(so that library writers cannot accidentally leave abstraction holes
open by forgetting to add role annotations) and use them in the
long-term-supported SafeNewtypeDeriving extension, but provide a
deprecated not-quite-as-safe GND extension for helping out users of
libraries that have not yet added role annotations. I would fancy that
this not-quite-as-safe GND could use unsafeCoerce wherever the safe
one would give an error about annotated roles.

# Proposal: roles for type families


Currently, the type constructors for all type families and data families all conservatively assign role `nominal` to all their parameters. This is a safe choice, but a restrictive one, as it rules out some useful, coercion-safe programs. In this section, I propose a way to allow type families to have parameters with phantom and representational roles.

## Examples we cannot write today



This example ([courtesy of glguy](https://ghc.haskell.org/trac/ghc/ticket/8177#comment:32)) will not typecheck:


```haskell
-- | Family of N-ary operator types.
type family Op n a b where
  Op 'Z     a b = b
  Op ('S n) a b = a -> Op n a b

coerceOp :: Coercible a b => Op n a c -> Op n b c
coerceOp = coerce
```


Since the role signature for `Op` is `type role Op nominal nominal nominal`. But in an ideal world, the role signature for `Op` would be inferred as `type role Op nominal representational representational`. After all, neither `a` nor `b` is "scrutinized" in any sense, so it feels perfectly safe to coerce them freely. **RAE:** "feels"? Let's prove it! **End RAE**



Another example ([courtesy of int-index](https://ghc.haskell.org/trac/ghc/ticket/8177#comment:33)) is:


```haskell
-- represents effect methods for some monad `m`
data family EffDict (eff :: k) (m :: Type -> Type)

-- Note this matches on `eff`, not `m`
data StateEff s
data instance EffDict (StateEff s) m =
  StateDict
    { _state :: forall a . (s -> (a,s)) -> m a,
      _get :: m s,
      _put :: s -> m () }

-- composition of monad transformers
newtype TComp t1 t2 m a = TComp (t1 (t2 m) a)

coerceDict :: EffDict eff (t1 (t2 m)) -> EffDict eff (TComp t1 t2 m)
coerceDict = coerce
```


Again, `coerceDict` will not typecheck due to the role of `m` in `EffDict` being `nominal`. But there's no good reason why this *must* be the case???we ought to be able to tell GHC to allow `m` to have `representational role`. (Of course, this would prevent any `EffDict` instance from using `m` at a `nominal` role, but them's the breaks.)



Additionally, we might like to have roles for *associated* type families. For instance, consider this example ([courtesy of dmcclean](https://ghc.haskell.org/trac/ghc/ticket/8177#comment:20)):


```haskell
data Variant = DQuantity | DUnit Prefixability
data Dimension

class KnownVariant (var :: Variant) where
  data Dimensional var :: Dimension -> * -> *

instance KnownVariant DQuantity where
  newtype Dimensional DQuantity d v = Quantity' v

instance KnownVariant (DUnit p) where
  data Dimensional (DUnit p) d v = Unit' UnitName v

type Quantity = Dimensional DQuantity
coerceQuantity :: Coercible v w => Quantity d v -> Quantity d w
coerceQuantity = coerce
```


Once again, `coerceQuantity` is ill typed, simply because of the conservative `nominal` role that the last type parameter of `Dimensional` has. Associated type families are an interesting case, since they can have extra type parameters (and thus extra roles) that the parent class does not have.

## Syntax



Implementing roles for type families would not require too many changes to the syntax of the language, as most of the required pieces are already there. The biggest current restriction is the fact that one cannot declare role annotations for type families, e.g.,


```haskell
type role F nominal
type family F a
```


But this is a restriction that is easily overcome. In addition, the parser does not currently recognize role annotations for associated type families:


```haskell
class C a where
  type role Assoc nominal nominal
  type Assoc a b
```


But this could be added without much difficulty.

**RAE:** There is a difference between roles for
data families and data instances. And both might usefully
have role annotations. For example:


```haskell
data family DF a b
type role DF nominal representational

data instance DF Int b = MkDFInt b
 -- NB: No scrutinizing the second parameter.
 -- Also, b is not used in a nominal context

data instance DF [c] b = MkDFList c b
type role DF [nominal] representational

data instance DF (Maybe d) b = MkDFMaybe d b
type role DF (Maybe representational) representational
```


With this, we have `Coercible (DF (Maybe Age) Int) (DF (Maybe Int) Age)` but not `Coercible (DF [Age] Int) (DF [Int] Age)`.


I *think* (but have not ever tried to prove) that these instance roles would work out, with the following restrictions:

- If a data family parameter is not scrutinized (that is, it's just a bare variable), then the instance role must match the family role exactly.

- Corollary: all representational/phantom roles from the family must be repeated exactly in the instances.

- Other variables may be given new roles according to the usual rules, including the usage of the variable in the instance head as a usage site. (That is, if the variable is used in a nominal context in the instance head, then it must be marked nominal.)


I'm a bit worried about problems with what happens if a type constructor that appears as part of a type pattern for an instance is actually a newtype with a role annotation -- could we be breaking soundness with this? Need to think harder.

**End RAE**

## Role inference for type families


Regardless of whether we're dealing with a closed, open, or associated type family, GHC will need to infer the most permissive roles possible for every type family, and possibly check these roles against a user-provided role signature. This section describes how role inference will operate.

### Example



Consider this type family:


```haskell
type family F (e :: *) (f :: *) (g :: *) (h :: *) :: k where
  F Int       b c d = c
  F (Maybe a) b a d = Maybe b
  F a         b c d = a
```


There are five type parameters for `F`: `k`, `e`, `f`, `g`, and `h`. What should be the roles for each one? We will start off by assuming each parameter has role `phantom`, and then walk the structure of the type family, progressively marking parameters with more restrictive roles.

### The type family kind


First, we gather all of the free variables in the type family's kind and mark each as `nominal`. This is under the observation that only type variables can be at role `phantom` or `representational`, never kind variables. Therefore, `k` would be marked as nominal.

### The type family equations


Next, we descend into each defining equation of the type family and inspect the left-hand and right-hand sides. The right-hand sides are analyzed just like the fields of a data constructor; see the [Role inference](https://ghc.haskell.org/trac/ghc/wiki/Roles#Roleinference) section above for more details. From the right-hand sides, we learn that the roles of `e`, `f`, and `g` should be (at least) `representational`.



The more interesting analysis comes when inspecting the left-hand sides. We want to mark any type variable that is *scrutinized* as `nominal`. By "scrutinized", we mean a variable that is being used in a non-parametric fashion. For instance, we want to rule out scenarios like this one:


```haskell
type family Inspect x where
  Inspect Bool = Int
  Inspect Int  = Bool

coerceInspect :: Coercible a b => Inspect a -> Inspect b
coerceInspect = coerce

unsafeBoolToInt :: Bool -> Int
unsafeBoolToInt = (coerceInspect :: Inspect Int -> Inspect Age)
```


To accomplish this, we check for any occurences of the either of the following sorts of scrutinization:

1. A type pattern that is not a single type variable. For instance, all of these equations provde examples of type patterns which do scrutinize a particular type variable:

```haskell
type family Inspect x where
  Inspect Int          = Bool
  Inspect (Either a b) = a
  Inspect (f a)        = a
```

>
>
> Any type variable that is scrutinized in this fashion (`x` in the above example) is marked as `nominal`.
>
>

1. Type patterns that are syntactically equal are all marked as nominal. For instance:

```haskell
type family Eq w x y z where
  Eq a b (Either b a) c = a
```

>
>
> Here, we have two variable names that are used in multiple places: `a` and `b`. As a result, the type variables which they inhabit (`w`, `x`, and `y`) are all marked as `nominal`.
>
>


Returning to the earlier `F` example, we would learn that `e` and `g` should be marked nominal, as they are both scrutinized. Therefore, the final inferred roles for `k`, `e`, `f`, `g`, and `h` are `nominal`, `nominal`, `representational`, `nominal`, and `phantom`.

**RAE:** This works well for *closed* type families, but is ineffective with open type/data families (associated or not). I propose that open families default to nominal roles. This is quite like how open families' type variables default to kind `Type`. Edit: I see this addressed below, but the opening paragraph for this section mentions inference for open families. **End RAE**

## Role checking for type families



Users can also specify role annotations for type families that should be checked against the inferred roles. For instance:


```haskell
type role G nominal nominal
type family G a b where
  G a b = Either a b
```


If the user hadn't written the role annotation for `G`, its role signature would have been inferred to be `type role G representational representational`. However, role checking overrides the inferred roles and assigns the more conservative roles of `type role G nominal nominal`.



Note that while writing role annotations for *closed* type families is purely optional, it is somewhat more important for open type families. For instance, what should be the roles for this type family?


```haskell
type family Open a b
```


Here, we have a choice to make. We could decide to make the roles for open type families default to, say, `representational`. While this would give us the freedom to `coerce` values of type `Open a b` more freely, it simultaneously restricts the instances we can give for `Open`, since every type instance must be checked to ensure that neither `a` nor `b` is used at a `nominal` role.



For the sake of backwards compatibility and the principle of least surprise, roles for open type families default to `nominal`. This allows more instances to be written, but makes it harder to `coerce` them. If a user wishes to `coerce` open type families, the onus is on her to write a role annotation, e.g.,


```haskell
type role Open representational representational
type family Open a b
```

## Type family roles and hs-boot files


Just like we default roles for open type families to `nominal`, we do the same for type families declared in `hs-boot` files.
