# Unboxed sum types


This page explains the motivation and implementation of unpacking for sum
types.

`-XUnboxedSums` have been available since GHC 8.2.1.


See also [UnliftedDataTypes](unlifted-data-types)

## Current status

See the ~UnboxedSums label.


## Motivation


GHC does a good job of unpacking product types. Given a declaration like

```haskell
data T1 a b = C1 a b
data T2 a b = C2 {-# UNPACK #-} !(T1 a b)
```

`C2` will have a representation where all the overhead of the `C1` constructor,
both the pointer to it in the `C2` constructor and the info table pointer in
the `C1` constructor, has been removed. This saves two words and one
indirection  compared to a packed representation, which uses five words.


Unfortunately, a similar example using sum types cannot be unpacked today:

```haskell
data T1 a = Some a | None
data T2 a = C !(T1 a)  -- Cannot UNPACK here
```


Here the representation of the `C` constructor will contain a pointer to e.g.
the `Some` constructor. The `Some` constructor will be a separate heap object
and thus needs one word to store its info table pointer.


In this example there is an alternative, unpacked representation that is more
memory efficient and has fewer indirections. We could store a constructor tag
together with the union of the fields of `T1` inside `C`. Conceptually the
memory layout would look like this (in practice we group pointer and
non-pointer fields together):


<table><tr><th> T2 info table pointer </th>
<th> T1 constructor tag </th>
<th> Fields of <tt>Some</tt> </th>
<th> Fields of <tt>None</tt> 
</th></tr></table>



(In this case `None` has no fields.)


This representation saves one word and one indirection compared to the packed
representation, which uses four words.

## Source language design


We add new built-in types for anonymous unboxed sums. These are directly
analogous to the existing anonymous unboxed tuples. Specifically:

- A new language extension `UnboxedSums`.

- We add a family of new built-in **type constructors** for unboxed sums:

  ```wiki
  (#|#), (#||#), (#|||#), (#||||#), etc
  ```

  A sum of n "\|"s is a n+1 ary sum.  (Just like tuples `(,)`, `(,,)`, etc.)

- Each n-ary-sum type constructor comes with n **data constructors**, with
  systematically-derived names, thus:

  ```haskell
  data (#||#) a b c = (# _ | | #) a
                    | (# | _ | #) b
                    | (# | | _ #) c
  ```

  The `_` indicates which disjunct of the sum we mean.

- You use the type constructor in a distfix way, like so:

  ```haskell
  (# Int | Bool #)        means   (#|#) Int Bool
  (# Int | Bool | Int #)  means   (#||#) Int Bool Int
  (# Int | Bool #)        means   (#|#) Int Bool
  ```

  And similarly the data constructors:

  ```wiki
  (# | True #)     means   (# | _ #) True
  (# | 'c' | #)    means   (# | _ | #) 'c'
  ```

- You can use the data constructors both in terms (to construct) and in
  patterns (to decompose).

  ```wiki
  case x of
      (# x | | | #) -> ...
      (# | y | | #) -> ...
      ...two more disjuncts needed to be exhaustive
  ```

- Unboxed sums are first class values. They can be passed as an argument to a
  function, returned as its result, be the type of a data constructor field,
  and so on.  Of course, unboxed sums are unlifted (cannot be bottom), and
  should be represented efficiently (more on that below).

- Just as for unboxed tuples: The components of an unboxed sum type may be of
  kind `*` or `#`.  So `(# Int# | Bool #)` is fine.  And you can nest unboxed
  sums and tuple arbitrarily, e.g.

  - `(# (# Int,Bool #) | Char# #)`
  - `(# (# Int# | Char # #) | Int #)`


All of these rules follow the same pattern as the rules for boxed/unboxed tuples.

### Design questions


For large-arity anonymous sums, the data constructor syntax requires counting
vertical bars. This is annoying. Might we consider switching to a new syntax
where `(# 0 of 3 | x #)` means `(# x | | #)` and `(# 2 of 6 | y #)` means \`(\# \|
\| y \| \| \| \#)\`? I (Richard) saw this syntax in an email and thought it might be
an improvement.

## Implementation

## Wired-in types


Unboxed sums get implemented very like boxed and unboxed tuples; see
[compiler/GHC/Builtin/Types.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Builtin/Types.hs).

## The Core language


No changes in Core.

### Core to STG


When going to STG we need to eliminate the unboxed sums. This can be done in
[compiler/GHC/Stg/Unarise.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Stg/Unarise.hs), just like for tuples.


Given the Core function

```wiki
f :: (# t_1 | ... | t_n #) -> ...
```


we convert it to a call to STG which includes the tag and the maximum number of
pointer and non-pointer arguments we might need. Example:

<table><tr><th> Core </th>
<th> STG 
</th></tr>
<tr><th> <tt> f :: (# Int# | Bool #) -> ... </tt> </th>
<th> <tt> f :: Word -> Word -> Pointer -> ... </tt> 
</th></tr>
<tr><th> <tt> f :: (# Int# | Word# #) -> ... </tt> </th>
<th> <tt> f :: Word -> Word -> ... </tt> 
</th></tr></table>



See notes in [compiler/GHC/Stg/Unarise.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Stg/Unarise.hs) for more details.

### Code generation


New `StgArg` constructor `StgRubbishArg` and new `CmmArg` are added for
efficient compilation of sums. See `StgRubbishArg` in
[compiler/GHC/Stg/Syntax.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Stg/Syntax.hs).

### Unpacking


The associated merge request for this feature is https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4140


Given

```haskell
data T1 a = Some a | None
data T2 a = C {-# UNPACK #-} !(T1 a)
```


we generate a "worker" constructor

```haskell
C (# a | (# #) #)
```


(`(# #)` playing the role of void.)


We then translate the construction of `C` as follows:

```wiki
C x

===> (translates to)

case x of
    Some y -> C (# y | #)
    None   -> C (# | (# #) #)
```


We then translate the elimination of `C` as follows:

```wiki
case e of
    C x -> ... x ...

===> (translates to)

case e of
    C x' ->
        let x = case x' of
            (# y | #) -> Some y
            (# | _ #) -> None
        in ... x ...
```


This above reboxing will go away, using case-of-case and
case-of-known-constructor, if we scrutinize `x` again.

---

# Exploiting nullary constructors


Joachim [writes](https://mail.haskell.org/pipermail/ghc-devs/2015-September/009831.html): The current proposed layout for a

```wiki
    data D a = D a {-# UNPACK #-} !(Maybe a) would be
    [D???s pointer] [a] [tag (0 or 1)] [Just???s a]
```


So the representation of

```wiki
         D foo (Just bar)     is     [D_info] [&foo] [1] [&bar]
and of   D foo Nothing        is     [D_info] [&foo] [0] [&dummy]
```


where `dummy` is something that makes the GC happy.


But assuming this dummy object is something that is never a valid heap objects of its own, then this should be sufficient to distinguish the two cases, and we could actually have that the representation of

```wiki
         D foo (Just bar)     is     [D_info] [&foo] [&bar]
and of   D foo Nothing        is     [D_info] [&foo] [&dummy]
```


and an case analysis on D would compare the pointer in the third word with the well-known address of dummy to determine if we have Nothing or Just. This saves one word.


If we generate a number of such static dummy objects, we can generalize this tag-field avoiding trick to other data types than Maybe. It seems that it is worth doing that if

- the number of constructors is no more than the number of static dummy objects, and
- there is one constructor which has more pointer fields than all other constructors.


Also, this trick cannot be applied repeatedly: If we have

```wiki
  data D = D {-# UNPACK #-} !(Maybe a) | D'Nothing
  data E = E {-# UNPACK #-} !(D a)
```


then it cannot be applied when unpacking `D` into `E`. (Or maybe it can, but care has to be taken that `D`???s `Nothing` is represented by a different dummy object than `Maybe`???s `Nothing`.)


Anyways, this is an optimization that can be implemented once unboxed sum type are finished and working reliably.
