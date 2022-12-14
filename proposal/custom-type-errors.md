## Custom Type Errors


This page outlines the design for a GHC feature to provide support for user-specified type errors.
The main idea was [presented by Lennart Augustsson at the 2015 Haskell Symposium in Vancouver BC](https://youtu.be/ZlZbSiYrqcQ). 


The relevant ticket is #9637.

## Status

See the ~"custom type errors" label.


### The Problem


When designing EDSLs in Haskell, it is useful to have something like `error` at the type level.
In this way, the EDSL designer may show a type error that is specific to the DSL, rather than the standard GHC type error.


For example, consider a type classes that is not intended to be used with functions, but the EDSL user accidentally used
it at a function type, perhaps because they missed an argument to some function.  Then, instead of getting a the standard
GHC message about a missing instance, it would be nicer to emit a more friendly message specific to the EDSL.


Similarly, the reduction of a type-level function may get stuck due to an error, at which point it would be nice to
report an EDSL specific error, rather than a generic error about an ambiguous type or something.

### A Solution

One way to solve the above problem is by adding a single uninterpreted type-function as follows:


```haskell
type family TypeError (msg :: ErrorMessage) :: k
```


Note that `TypeError` is polymorphic in the kind of result it returns, so it may be used in any context.  In this respect it resembles to polymorphic value `error`.  
The intention is that users will never define instances of `TypeError`, so one may think of it as closed type family with no equations.  In other words, `TypeError`
will never evaluate to anything, and it is basically very similar to the polymorphic type-level value `Any`, which is already present in GHC.



The kind `ErrorMessage` is a small DSL for constructing error messages, and may be defined using a promoted datatype:


```haskell
data {-kind-} ErrorMessage =
    Text Symbol                        -- Show this text as is
  | forall t. ShowType t               -- Pretty print a type
  | ErrorMessage :<>: ErrorMessage     -- Put two chunks of error message next to each other
  | ErrorMessage :$$: ErrorMessage     -- Put two chunks of error message above each other
```

### Examples

Here are some examples of how one might use these:


```haskell
instance TypeError (Text "Cannot 'Show' functions." :$$: 
                    Text "Perhaps there is a missing argument?")
         => Show (a -> b) where
   showsPrec = error "unreachable"
```


The resulting error message from GHC is:

```wiki
Cannot 'Show' functions.
Perhaps there is a missing argument?
In the expression: show id
In an equation for ???f???: f = show id
```


Similarly, one may use the same sort of technique in type family instances;

```haskell
type family F a where
  F Int     = Bool
  F Integer = Bool
  F Char    = Int
  F a       = TypeError (Text "Function F does not work for type " :<>:
                        ShowType a :<>: Text "." :$$:
                        Text "It works only on integers and characters.")
```


The resulting error message from GHC is:

```wiki
Function F does not work for type [Int].
It works only on integers and characters.
In the expression: undefined
In an equation for ???f???: f = undefined
```

### Implementation


The implementation in GHC is fairly straight-forward:

- `TypeError` is a type function.  So the flattener already automatically extracts it from inside any types, into a `CFunEqCan`.  We don't need to hunt for it!

- When we are reducing a type function, and we notice that the RHS is of the form `TyperError msg :: k`, then we stop reducing the type function, and instead we emit a new insoluble wanted constraint `TypeError msg :: Constraint`.  (**SLPJ**: I hate this.  See SLPJ alternative below.)

- The new insoluble wanted constraint is very like the current `CHoleCan` constraint that we use for type and term wildcards.

  - We mark it as insoluble right away.
  - It is kept in the residual `WantedConstraints` returned by the constraint solver (i.e. we do *not* report the error right away).
  - The error is reported later, by `TcErrors.reportUnsolved`.
  - Because of this delay, the ultimate error message will embody the result of all unifications that take place, which makes the error message more informative.

- We add custom pretty printing (in `TcErrors`) for unsolved constraints of the form `TypeError msg :: Constraint`.  The custom pretty printing code examines `msg` and interprets it using the standard pretty printing combinators.

- When interpreting `msg` it is useful to evaluate type level functions that we encounter along the way. This allows the programmer to use type-level functions to construct the `msg`.  OTOH, the types within `ShowType` are printed as is, without evaluating function any more than usual.  **SLPJ** Here I'm not sure.  A good deal of evaluation will happen automatically simply through the usual flattening mechanism. Including in the arugment of `ShowType` unless we take measures to stop it. **Iavor:** I think it is OK for GHC to evaluate as much as it wants in the printed types.  I added this point because I had to add some extra code to force some additional evaluation in the `msg`, *outside* of ShowType.  For example, if the error message is itself computed using a type-level function, we want to make sure that we fully evaluate this function, so that we get to the combinators of the DSL.

- `TypeError msg :: k` where `k` is not constraint is printed simply as `(type error)`.  The reason for this is that on occasion such types may appear in other parts of the error message and we don't want to print the error multiple times, or in its ugly DSL form.

### SLPJ alternative design

- I think the easiest way to both describe and implement the feature is this:

  - `TypeError` is a type function
  - It reduces immediately to `Any` (or possibly to a variant of `Any`, whose name is `(type error)`).
  - When it reduces to `Any`, GHC emits a custom error message

- There are two reasons I suggest that it reduces to `Any`:

  - To avoid emitting the same error message multiple times, as the constraint solver repeatedly re-considers the same `(TypeError m)` function call.
  - Reducing to `Any` (or `(type error)`) automatically means that any types involving `TypeError` will, if displayed, print `Any` (or `(type error)`), not an elaborate call to `TypeError`.

- The "emit a custom error message" bit is implemented as you have it now, by emitting a special insoluble.


This approach would be quite a bit simpler than your current story, where you specially recognise type functions whose RHS is `TypeError`, and also have to suppress calls to `TypeError` that appear embedded in types.


### Alternative Design


It is our intention that `TypeError` will usually in the RHS of a type-level function defintion,
or as the sole participant in the context of a class `instance`.   It is probably not useful to write `TypeError` in
a user-defined signature or a datatype declaration, but it also does not appear to be harmful to do so---at least
not any more harmful than any other partial type-level function.


Instead of using the type function `TypeError`, we could ad special language syntax for error-reporting class instances
and type-family instances.  This seems like a bit heavy-weight, and it is not clear that it buys us much.  Also, we'd
have to think of more syntax and implement it.

## Design questions (RAE)

1. What happens with Given `TypeError` constraints? Naively, the `TypeError` constraint on an instance would seem to lead to an "inaccessible code" error. (And this point would be right! It *is* inaccessible code.)  *Lennart*:  Yes, we know any methods are inaccessible, but I don't think the compiler needs to know specially about `TypeError`.  Instead the method definition will be treated as usual.  This can always be refined later if we want. *RAE:* But I think people will want this:

  ```
  instance TypeError "You can't compare functions for equality" => Eq (a -> b)
  ```

>
>
> If that's inaccessible code and an error, they can't do this.
>
>

>
>
> *Lennart:* I don't think it will be so bad to write
>
>
> ```
> instance TypeError "You can't compare functions for equality" => Eq (a -> b) where (==) = undefined
> ```


>
>
> I see no reason to complicate the compiler for this.
>
>

>
>
> *RAE:* But that code will issue an "inaccessible code" error. I'm OK (but I don't love it) if the user has to write out bogus method definitions -- that's not what I'm worried about.
>
>

>
>
> *Lennart:* I don't understand how you can get an inaccessible code error, unless you perform a link-time check if all instances have actually been used.  Since instances are always exported there is no module level check you can make to see if an instance is used.
>
>

1. Relatedly, when definition an instance with a `TypeError` constraint, what should users write in the body? Leaving it empty causes warnings, but anything written in there would never be called.  *Lennart*: See above.

1. Do we support `foo :: TypeError (Text "") -> TypeError (Text ""); foo = id`? I don't have a strong feeling one way or the other, but it would be nice to have this specified.  *Lennart*: I would expect this to result in a type error at some point.  If, after typechecking, there are any residual `TypeError` occurrences in types then something has gone wrong.

#### Iavor's Answers to RAE's Questions


I think of `TypeErrror` as a rather special type function, which only makes sense in two places in the syntax of the language:

1. as the only constraint in the context of an instance declaration, and
1. as the right-hand side of a type family instance.


The idea is that whenever a type function is about to reduce to `TypeError`, we stop the evaluation and record a custom type error to be reported by GHC.  It doesn't matter if the function evaluation was in the context of a given or wanted constraint.  Similarly, if an instance ever reduces to `TypeError`, then we also report a custom type error.


With this in mind the answers to your questions would be:

1. `TypeError` should not appear directly in a given context. *RAE:* But this is exactly what happens when typechecking method definitions in an instance with a `TypeError` context. Or is there special reasoning in the compiler not to typecheck these methods?
1. As mentioned above, the methods of an instance with `TypeError` in the context are irrelevant.  I think it is OK to simply leave them unspecified, or define them as `undefined`.
1. `TypeError` should not appear in types written by the user, except for the two special cases above.  So no `TypeError` in user-defined type signatures, type declarations, class instance heads, parameters to type functions, etc.


The current implementation does not have any explicit checks to make sure that programmers don't write `TypeError` in places where they are not supposed to. It might be nice to implement such a check, but I am not sure it is really necessary.   With the current implementation, if you wrote `TypeError` in a place that you were not supposed to, you would get the same behavior as for any other type function with no instances.

*RAE:* I don't really follow. If `TypeError` can be used in the RHS of a type family equation, then it can appear anywhere any type can. Having a syntactic restriction around the literal use of `TypeError` in code seems unhelpful.

---

### Some design questions (Dominique Devriese):


I found this page by accident, but find this a very interesting idea.  Very useful and much additional power for little cost. Some constructive comments/questions below:

1. Most of your examples seem like they would work with `TypeError :: ErrorMessage -> Constraint`? This would be much simpler to implement and address SLPJ's concern mentioned about "detecting TypeErrors in RHSs" above.   Perhaps you have examples that use `TypeError` at a different kind, but I'm wondering if you can't easily encode those with the simpler alternative.

1. Why not also `TypeWarning :: ErrorMessage -> Constraint` which simply produces a compile-time \*warning\*?

1. Why are we calling this \*Type\*Error?  I suspect we can use this for many not strictly type-related features.  For example

```
     deprecatedFn :: TypeWarning (Text "deprecatedFn is deprecated.  Use someOtherFn instead!") => 
                     SomeType

     removedFn :: TypeError (Text "removedFn has been removed. Use someOtherFn instead!") => ItsOldType
```

>
> >
> >
> > What about `Error` and `Warning` instead?
> >
> >
>

## Extended Example


Here is a longer example, which illustrates how one might use this feature in the implementation of a safe API to raw memory using C-like packed structures:

```wiki


{-# LANGUAGE TypeFamilies, TypeOperators, UndecidableInstances, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

import GHC.TypeLits
import Data.Proxy
import Data.Word
import Foreign.Ptr

type OffsetOf l xs = GetOffset 0 l xs

type family ByteSize x where
  ByteSize Word64   = 8
  ByteSize Word32   = 4
  ByteSize Word16   = 2
  ByteSize Word8    = 1
  ByteSize a        = TypeError (Text "The type " :<>: ShowType a :<>:
                                 Text " is not exportable.")

type family GetOffset n (l :: Symbol) xs where
  GetOffset n l ( '(l,a) ': xs) = '(n,a)
  GetOffset n l ( '(x,a)  : xs) = GetOffset (n+ByteSize a) l xs
  GetOffset n l '[]             = TypeError (Text "Missing field: " :<>:
                                                                    ShowType l)

newtype Struct (a :: [(Symbol,*)]) = Struct (Ptr ())


get :: forall l fs n a.
      (OffsetOf l fs ~ '(n,a), KnownNat n) =>
      Struct fs ->
      Proxy l   ->
      Ptr a
get (Struct p) _ = plusPtr p (fromInteger (natVal (Proxy :: Proxy n)))


type MyStruct = ['("A",Word8), '("B",Word8), '("C",Int) ]

testOk :: Struct MyStruct -> Ptr Word8
testOk s = get s (Proxy :: Proxy "B")


{-
testNotOk :: Struct MyStruct -> Ptr Word8
testNotOk s = get s (Proxy :: Proxy "X")
--}

{-
type MyOtherStruct = ['("A",Int), '("B",Word8) ]

testNotOk :: Struct MyOtherStruct -> Ptr Word8
testNotOk s = get s (Proxy :: Proxy "B")
--}

```

## See also


The Helium people have also addressed the problem of customizable error messages.
See Heeren, Hage, Swierstra on [Scripting the Type Inference Process](http://www.staff.science.uu.nl/~hage0101/scriptingthetypeinferencer.pdf).
