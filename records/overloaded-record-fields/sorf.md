# Simple Overloaded Record Fields (SORF)

**This page describes the original design by Simon PJ for Simple Overloaded Record Fields, which eventually became the [Overloaded Record Fields](records/overloaded-record-fields) family of extensions. It is retained for historical purposes, and does not reflect the final design as implemented.**


This page summarises a possible design that would allow different records to share a single field label.  Although it's a simple enough idea there are numerous ramifications.  Records are a swamp! Nevertheless, this is the simplest proposal that I know that satisfies the main request, that of having multiple records with the same field name.


See also a similar [2003 proposal by Simon PJ and Greg Morrisset](http://research.microsoft.com/en-us/um/people/simonpj/Haskell/records.html).  It is essentially the same as the proposal below, but (a) has less detail and (b) adds anonymous record types.   Anonymous type could be an add-on feature to the design described here.

# The base design


The **base design** has the following distinct components:

- A library class

  ```wiki
  class Has (r :: *) (f :: String) (t :: *) where
    get :: r -> t
  ```

- A record declaration generates an instance declaration for each field. For example

  ```wiki
  data T a = T1 { x :: a, y :: Bool }
           | T2 { x :: a }
  ```

  would generate

  ```wiki
  instance (t~a) => Has (T a) "x" t where
    get (T1 x _) = x
    get (T2 x)   = x
  instance (t~Bool) => Has (T a) "y" t where
    get (T1 _ y) = y
  ```

- Dot-notation would typecheck (and desugar to) as a call of the `get` method.  So `r.x` would typecheck just as if you had written `(get @ T _ @ "x" @ _ r)`, where the `@` means type application, and `_` means "some type".  (GHC doesn't support explicit type appplication yet, but you get the idea.)


Each of these components embodies choices which are described, with their variants, below.


Note that the proposal naturally supports record-polymorphic functions, such as

```wiki
fullName :: (Has r "firstName" String, Has r "lastName" String)
         => r -> String
fullName r = r.firstName ++ " " ++ r.lastName
```


admittedly with a rather verbose type (but see below).

---

# Design choices and variations

## The `String` type parameter to `Has`


In the base design, the field is a `String` type parameter to the `Has` class.  Another possiblity would be to invent a new fake class for each field, thus

```wiki
class Has_x (r :: *) (t :: *) where
  get_x :: r -> t
```


This seems clunky to me, and I don't really want to deal with this infinite family of classes (eg how are the brought into scope).  I see no disadvantages to a `String` parameter, which will form part of GHC's new kind system.

## Scope control by generalising the `String` type in `Has`


The proposal above doesn't allow label names to be scoped: if one module internally uses `"field"` as a label name then another module can break the abstraction by using the same string `"field"`.


We can fix this by instead of having

```wiki
class Has (r :: *) (f :: String)       (t :: *) where
```


having something like

```wiki
class Has (r :: *) (ft :: *) (f :: ft) (t :: *) where
```


(where `ft` stands for field type).


The expression

```wiki
foo.field
```


(starting with a lowercase letter) would behave as described above, with `ft ~ String`.


But

```wiki
foo.Field
```


(starting with an uppercase letter) would use the Field constructor that is in scope, e.g. if we had

```wiki
data FieldT = Field
```


then `ft ~ FieldT`. Then we can choose whether or not to export `FieldT(Field)`.


In my opinion, this is ugly, since the selector can be either a type name or a label and the semantics are nonsame.
Rather, we need scoped instances.
???strake888

## Should `get` have a proxy argument?


The type of `get` is very odd:

```wiki
get :: Has r f t => r -> t
```


It's odd because "f" is not nentioned in the type after the "`=>`".  That in turn means you could not write an unambiguous call of `get`, at least not without support for explicit type application (which is another story).


But I think it's OK not to be able to write a call to `get`, *because that's what the dot-notation generates*.  The dot-notation produces a suitably-instantiated call to `get`.  


It would be equally possible to have the slightly clunkier definition

```wiki
data Proxy (a :: String)

class Has (r :: *) (f :: String) (t :: *) where
   get :: r -> Proxy f -> t
```


Now `r.x` would typecheck and desugar as `get r (undefined :: Proxy "x")`.  This choice does not make a major difference either way.

## Higher rank types and type functions


It is very tempting to define `Has` with an associated type, like this:

```wiki
class Has (r :: *) (f :: String) where
  type FieldTy r f :: *
  get :: r -> FieldTy r f
```


After all, the field type is fixed by the record type, isn't it?
For example, we could get these instance declarations:

```wiki
instance Has (T a) "x" where
  type FieldTy (T a) "x" = a
  get = ...as before...
instance Has (T a) "y" where
  type FieldTy (T a) "y" = Bool 
  get = ...as before...
```


But this approach fails for fields with higher rank types.  For example, this should work:

```wiki
data HR = HR { rev :: forall a. [a] -> [a] }

f :: HR -> ([Bool], [Char])
f r = (r.rev [True], r.rev "hello")
```


Records with polymorphic fields are very important in practice, so it would be a
major wart not to support them.


However we are not allowed to say

```wiki
instance Has HR "rev" where
  type FieldTy HR "rev" = forall a. [a] -> [a]
  get (HR rev) = rev
```


because we are not allowed to have a type instance whose RHS is a polytype, because then
we don't konw when to instantiate it.  This restriction is **not** easy to lift.



The base design instead gives `Has` *three* parameters, and uses 
a functional-dependency-like mechanism (but using equalities) for the result type.
Using this we can deal with `HR`:

```wiki
instance (t ~ [a] -> [a]) => Has HR "rev" t where
  get (HR rev) = rev
```


and all is well.

## Virtual record selectors


Suppose we have

```wiki
data Shape = Rect Float Float | Circle Float | ...

area :: Shape -> Float
area (Rect x y) = x*y
area (Circle r) = ...
...etc...
```


Can the user write this?

```wiki
instance Has Shape "area" Float where
  get = area 
```


Then he can write `shape.area` with the expected result.  I'll call
`area` a **virtual record selector** because it can be used as if it
is a record selector, but is implemented with more complex behaviour.


The same idea works
even for overloaded functions like `fullName` (whose definition appears above):

```wiki
instance (Has r "firstName", Has r "lastName") 
       => Has r "fullName" String where
  get = fullName
```


I see no reason to prohibit virtual record selectors; indeed
we would have to go to extra trouble to do so.  
On the contrary, one might want a pragma to generate the 
two lines of boilerplate.

## Unboxed fields


The mechanism does not work at all for records with unboxed fields:

```wiki
data T = MkT { x :: Int# }
```


Reason: type-class constraints can only be instantiate with lifted types.  There is a good reason for this restriction, because unboxed types can have varying widths, so you can't generate code that works uniformly for boxed and unboxed types.


This is a real problem, but it is one that we might be willing to live with.  In particular, it's fine to have UNPACKed fields:

```wiki
data T = MkT { x :: {-# UNPACK #-} Int }
```


So you can have efficiently-represented records without having to expose the unboxed types.

## Selectors as functions


Currently a record declaration brings into scope its selectors, so that

```wiki
data T = MkT { x,y :: Int }
```


brings into scope (as top-level functions) `x :: T -> Int` and `y :: T -> Int`.


Does that still happen under the new design?  The simple answer is "yes,for backard compatibility",
but note that if there were more than one record
with the same field name, then there would be *two* top level functions both
called `x`, so you could not call either of them directly, 

## Representation hiding


Currently if you say

```wiki
module M( T( x ) )
  data T = MkT { x,y :: Int }
```


then the selector `x` is exported, but not `y`.  This is quite important; we need 
a way to hide the representation of T, include the existence or otherwise of a 'y' field.


What happens under the new system?  If we generate instances for \`(Has
T "x" Int)` and `(Has T "y" Int)\`, then presumably they'd *both* be
exported (since we have no way to restrict instances).  But that means
we cannot ever hide the existence of a field, which seems Plain Wrong.


I can think of three solutions, all unsatisfactory in one way or another:

- An instance `(Has T "x" ty)` is exported only if the record selector for
  T is exported.  Again, this doesn't work for virtual record selectors;
  and it would require a new implementation mechanism to record exactly
  which instances were exported from a module.

- An instance `(Has T "x" ty)` is ignored unless the record selector
  `x` of type `T` is in scope.  This would require an *ad hoc* hack to
  the instance lookup mechanism; and it doesn't work at all for 
  virtual record selectors.  

- Require that every instance of `Has` looks like

  ```wiki
  instance blah => Has T "f" ty where
     get = the_getter
  ```

  where `the_getter` is a named function.  
  An instance `(Has T "x" ty)` is ignored unless the function 
  witnessing the instance (`the_getter`) is in scope.  This has the
  merit of working for virtual selectors, but it means that instances
  of `Has` must be restricted to a special form (more to specify).  
  However it comes with a reasonable story: the instances are simply
  a way to choose among the many in-scope "x"'s.

- Use the "Scope control by generalising the `String` type in `Has`" proposal above


Solving this issue is important.

---

# Syntax

## The dot notation


It is critical to support dot-notation. There are quite a few things to think about:

- Dot notation must work in cascades (left-associatively), and with an expression to the left:

  ```wiki
    r.x
    r.x.y
    (foo v).y
  ```

- There is an overlap with the function composition operator, but that is already true with qualified names.  It comes down  this: 

  - Function composition will only work when surrounded by spaces, thus `(f . g)`.
  - Dot-notation for record selection only works with no spaces.

- The expression `(f r.x)` must, I think, mean `(f (r.x))`.  That is, dot notation binds more tightly than function application.

- Should it be possible to partly apply dot-notation, in a section, like this: `map (.x) ts`?  (Note that the dual, `map (r.) fs` makes no sense.).  I'm inclined to take the simple approach and say "no", you have to write `map (\r -> r.x) ts`.

## Syntactic sugar for `Has`


The type of `fullName` above is rather verbose, and it might be nice to have
some syntactic sugar for `Has`, perhaps something like this:

```wiki
fullName :: r { firstName :: String, lastName :: String }
         => r -> String
fullName r = r.firstName ++ " " ++ r.lastName

instance r { firstName :: String, lastName :: String }
      => r { fullName :: String } where
  get = fullName
```


We replace "`Has r f t`" by "`r { f :: t }`", and allow multiple such constraints
with the same `r` to be combined.  Similarly, multiple constraints with the 
same `r` and `t` could be combined further, just as they are in record declarations:

```wiki
fullName :: r { firstName, lastName :: String }
         => r -> String
```

---

# Record updates


At first it seems that record update could be handled readily, by adding a 'set' field to `Has`:

```wiki
class Has (r :: *) (f :: String) (t :: *) where
  get :: r -> t
  set :: t -> r -> r
```


Now, similar to dot-notation, record update syntax `(r { x = e1, y = e2 })` would typecheck and
desugar as if you had written `(set @ "x" e1 (set @ "y" e2 r))`.  (I've left out a couple of 
type parameters to save clutter.) 


There are three problems:

- There is no natural `set` for "pseudo" fields like `area` or `fullName` above.  Maybe this could be solved by stratifying the classes:

  ```wiki
  class Has (r :: *) (f :: String) (t :: *) where
    get :: r -> t

  class Has r f t => Upd (r :: *) (f :: String) (t :: *) where
    set :: t -> r -> r
  ```

  But that adds complexity, and casts into doubt the proposed syntactic sugar for `Has` predicates.

- Haskell 98's record update can change the type of the record
  (there is a type-checked response to this bullet point [here](https://raw.github.com/ntc2/haskell-records/master/GHCWiki_SimpleOverloadedRecordFields.lhs)):

  ```wiki
  data R a = R { x :: a }

  upd :: R Int -> R Bool
  upd r = r { x = True }
  ```

  But `set` cannot accommodate this change of type, at least not without much more complexity.  Moreover, **NO** field-at-a-time encoding can deal with the case when more than one field shares the changed type. For example, this is legal Haskell:

  ```wiki
  data S a = S { x,y :: a }
  upd :: S Int -> S Bool
  upd s = s { x = True, y = False }
  ```

  But any system that updates `x` and then `y` will have an ill-typed intermediate in which `x` has a `Bool` while `y` has an `Int`.  This is really the death-knell for any field-at-a-time story that seeks to be compatible with Haskell as she is now.

- This approach doesn't work at all in the higher-rank case.  For example, if `r :: HR`, we are currently allowed to say

  ```wiki
     r { rev = reverse } 
  ```

  but if we say `set @ "rev" reverse r`, the `reverse` will be instantiated.  Another way to say it is that the putative  instance declaration

  ```wiki
  instance (t ~ [a] -> [a]) => Has HR "rev" t where
    set rv r = r { rev = rv }
  ```

  will be rejected because `rv` is not polymorphic.  Again, this is **not** easy to fix.   

>
>
> This problem seems to be a killer: if record-update syntax is interpreted as a call to `set`, we cannot, ever, use record-update syntax to update a record with a polymorphic field. (We could use alternative notation; instead of `r { rev = reverse }` we could say
>
>
> ```wiki
>   case r of { HR { .. } -> HR { rev = reverse, .. } }
> ```
>
>
> where I'm using GHC's dot-dot notation to fill in the missing fields.  But it's clumsy and would get worse if the data type had several constructors.)  And even if we were willing to give up on record-update syntax for polymorphic fields, we'd need a rule to say which fields are or are not made an instance of `Has` and `Upd`.
>
>


I'm not very happy with any of this.  Faced with these difficulties,
another alternative is to stick with the status quo for record
updates; that is, not to support any sort of overloading.  But even
*that* is problematic: what does `e { x = True }` mean if there are lots of "x" fields
in scope (which is precisely what we want to allow). Haskell's current record-update
syntax really relies on learning which type is involved, from the record selector; but if
there are many x's, it can't figure it out.  Insisting that only one "x" selector must
be in scope puts us back to square one.  I think the only decent solution is to 
invent a new syntax for monomorphic (non-overloaded) record update, something like this:

```wiki
   HR { r | rev = reverse }
```


In general

```wiki
   <type constructor> { expression | field = expression, ... }
```


Of course this isn't very satisfactory either

- New syntax required
- Not overloaded, so you can't abstract over it.

## Alternative Proposal


First, we define a class for types with a member at 'k':

```wiki
class Has r k v where select :: r -> k -> v;
```


Next, we define a class for types with a mutable member of certain type:

```wiki
class Quasifunctor r s k u v where qfmap :: k -> (u -> v) -> r -> s;
```

`Quasifunctor r s k u v` means that `r` and `s` have members of types `u` and `v`, in turn, both with selector `k`; thus, one can mutate the member at 'k' with an arbitrary function of type `u -> v`, and the overall function is of type `r -> s`; i.e. one can lift a function of type `u -> v` to a function of type `r -> s`. This is the record update.

`qfmap` is the lifter function. The first argument serves to specify which member is meant.


This ought to allow polymorphic mutation; to set member at "x" to value `x` is simply `qfmap (undefined :: "x") (const x)`, though this mechanism is of course more general; one could `qfmap` any arbitrary function, not just a constant function.


For example:

```wiki
data R a = R { x :: a };

-- automatically-generated instances
instance Has (R a) "x" a where ...
instance Quasifunctor (R a) (R b) "x" a b where ...

r = R { x = 256 };
-- assign string value to r.x
let s = qfmap (undefined :: "x") (const "Hello, world!") r in ...
```

---

# Relationship to Type Directed Name Resolution


This proposal is quite closely related to the [Type Directed Name Resolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) idea, because TDNR 
would internally generate `Has` constraints exactly as described above.  The difference is
that TDNR wasn't intended to support *abstraction* over the constraint, and was explained
rather differently.
