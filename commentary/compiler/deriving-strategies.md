# Deriving strategies


Deriving strategies grant users finer-grained control over how instances may be derived. The `DerivingStrategies` language extension landed in GHC 8.2.1.

## Motivation


GHC Trac #10598 revealed a limitation of GHC's current instance deriving mechanism. Consider the following program which uses both `DeriveAnyClass` and `GeneralizedNewtypeDeriving`:

```haskell
{-# LANGUAGE DeriveAnyClass, GeneralizedNewtypeDeriving #-}

class C a where
  c :: a -> String
  c _ = "default"

instance C Int where
  c = show

newtype T = MkT Int deriving C
```


What `C` instance should be derived for `T`? GHC could use `GeneralizedNewtypeDeriving` and use the underlying instance for `Int`. On the other hand, GHC could just as well use `DeriveAnyClass` and give `T` a default implementation for `c`! We've uncovered an ambiguity.


Currently, GHC will accept the above code by defaulting to the `DeriveAnyClass` strategy (after emitting a warning). This is an unfortunate outcome, because it now
prevents users from using `GeneralizedNewtypeDeriving` and `DeriveAnyClass` simultaneously.


There are some other shortcomings of instance deriving as well. For instance, one cannot derive "newtype-style" `Read` or `Show` instances. For example:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype S = MkS Int deriving Show

sOne :: String
sOne = show (MkS 1)
```


Despite our best efforts, the value of `sOne` will be `MkS 1` instead of `1`. The behavior of a `deriving Show` clause is to always produce a `Show` instance that includes the name of the constructor, even if `GeneralizedNewtypeDeriving` is on. While this is usually what you want, there are rare occasions where you simply want to use the underlying type's `Show` instance instead of constructing an entirely new one. Unfortunately, GHC does not give you a way to express this.

## Deriving strategies


A solution to the above issues is to introduce a syntax extension called *deriving strategies*. They are named as such because they allow users to state explicitly in a deriving clause what approach GHC should take when attempting to derive an instance for a typeclass. There are currently three strategies that GHC is aware of:

- Deriving stock instances: This is the usual approach that GHC takes. For certain classes that GHC is aware of, such as `Eq`, `Ord`, `Functor`, `Generic`, and others, GHC can use an algorithm to derive an instance of the class for a particular datatype mechanically. For example, a stock derived `Eq` instance for `data Foo = Foo Int` is:

```haskell
instance Eq Foo where
  Foo a == Foo b = a == b
```

>
> Stock applies to the "standard" derivable typeclasses mentioned in the Haskell Report like `Eq` and `Show`, as well as some GHC-specific classes like `Data` and `Generic`. The stock strategy only requires enabling language extensions in certain cases (`DeriveFunctor`, `DeriveGeneric`, etc.).

- `GeneralizedNewtypeDeriving`: An approach that GHC only uses if the eponymous language extension is enabled, and if an instance is being derived for a newtype. GHC will reuse the instance of the newtype's underlying type to generate an instance for the newtype itself. For more information, see [http://downloads.haskell.org/\~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html\#generalised-derived-instances-for-newtypes](http://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#generalised-derived-instances-for-newtypes)
- `DeriveAnyClass`: An approach that GHC only uses if the eponymous language extension is enabled. When this strategy is invoked, GHC will simply generate an instance with empty implementations for all methods. For more information, see [http://downloads.haskell.org/\~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html\#deriving-any-other-class](http://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#deriving-any-other-class)


While GHC can pick a strategy internally, users don't have a reliable way to pick a strategy other than enabling language extensions and hoping that GHC does the right thing (which it often doesn't, as evidenced in the above problematic examples). The deriving strategies proposal aims to:

1. Introduce a new `-XDerivingStrategies` language extension.
1. Allocate three keywords (`stock`, `newtype`, and `anyclass`) that can be used in `deriving` clauses or standalone `deriving` declarations to indicate which strategy to use when `-XDerivingStrategies` is enabled. (Note that `stock` and `anyclass` would still be able to used outside the context of deriving as, say, function argument names.)
1. Allow datatypes to have multiple `deriving` clauses when `-XDerivingStrategies` is enabled.

### Examples


Here is an example showing off what `-XDerivingStrategies` allows:

```haskell
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

newtype T a = T a
  deriving          Show
  deriving stock    (Eq, Foldable)
  deriving newtype  Ord
  deriving anyclass Read

deriving stock instance Functor T
```


This demonstrates why part 3 is important: with multiple `deriving` clauses, one can fine-tune which instances should be derived with particular deriving strategies.

### The deriving strategy resolution algorithm


With `-XDerivingStrategies` in the picture, we can now state how GHC figures out which deriving strategy to use for a particular derived instance:

1. Look for a deriving strategy. If one is present, use that. This will throw an error if you try to do something impossible, like using the `newtype` strategy on a non-newtype or the `stock` keyword with a non-stock typeclass.

1. If deriving a stock class:

   (a) If deriving `Eq`, `Ord`, `Ix`, or `Bounded` for a newtype, use the `GeneralizedNewtypeDeriving` strategy (even if the language extension isn't enabled).

   (b) If deriving `Functor`, `Foldable`, or `Enum` for a newtype, the datatype can be successfully used with `GeneralizedNewtypeDeriving`, and `-XGeneralizedNewtypeDeriving` has been enabled, use the `GeneralizedNewtypeDeriving` strategy.

   (c) Otherwise, if deriving a stock class and the corresponding language extension is enabled (if necessary), use the stock strategy. If the language extension is not enabled, throw an error. 

1. If not deriving a stock class:

   (a) If deriving an instance for a newtype and both `-XGeneralizedNewtypeDeriving` and `-XDeriveAnyClass` are enabled, default to `DeriveAnyClass`, but emit a warning stating the ambiguity.

   (b) Otherwise, if `-XDeriveAnyClass` is enabled, use `DeriveAnyClass`.

   (c) Otherwise, if deriving an instance for a newtype, the datatype and typeclass can be successfully used with `GeneralizedNewtypeDeriving`, and `-XGeneralizedNewtypeDeriving` is enabled, do so.

   (d) Otherwise, throw an error.


The stock classes are:

- `Bounded`
- `Enum`
- `Eq`
- `Ix`
- `Ord`
- `Read`
- `Show`
- `Functor` (with `-XDeriveFunctor`)
- `Foldable` (with `-XDeriveFoldable`)
- `Traversable` (with `-XDeriveTraversable`)
- `Generic` and `Generic1` (with `-XDeriveGeneric`)
- `Data` (with `-XDeriveDataTypeable`)
- `Lift` (with `-XDeriveLift`)


The relationship between stock classes and `DeriveAnyClass` can be be summarized as follows: In the absence of an explicit `anyclass` keyword, GHC will never attempt to derive a stock class instance using `DeriveAnyClass`, since it is guaranteed that doing so would not produce the instance you'd want.


Step 2 is fairly intricate since GHC tries to use `GeneralizedNewtypeDeriving` in certain special cases whenever it can to optimize the generated instances. In addition, the phrase "can be successfully used with `GeneralizedNewtypeDeriving`" must be invoked since it is possible for `GeneralizedNewtypeDeriving` to fail for certain datatypes. For example, you cannot have a newtype-derived `Functor` instance for `newtype Compose f g a = Compose (f (g a))`, since the last type variable `a` cannot be eta-reduced.


To help visualize things, here's a table summarizing which typeclasses GHC decides it can use the `newtype` strategy for (thanks to ??rjan Johansen):


<table><tr><th>                      </th>
<th> No extension required            </th>
<th> Requires language extension to use 
</th>
<th></th>
<th></th></tr>
<tr><th> GND when possible  </th>
<th> 2(a) </th>
<th> <tt>Eq</tt>, <tt>Ord</tt>, <tt>Ix</tt>, <tt>Bounded</tt> </th>
<th>                             
</th>
<th></th></tr>
<tr><th> GND with extension </th>
<th> 2(b) </th>
<th> <tt>Enum</tt>                       </th>
<th> 2(b) </th>
<th> <tt>Functor</tt>, <tt>Foldable</tt> 
</th></tr>
<tr><th> Never select GND   </th>
<th> 2(c) </th>
<th> <tt>Read</tt>, <tt>Show</tt>               </th>
<th> 2(c) </th>
<th> <tt>Data</tt>, <tt>Generic</tt>, <tt>Generic1</tt>, <tt>Typeable</tt>, <tt>Traversable</tt>, <tt>Lift</tt> 
</th></tr></table>



This provides another reason to use `-XDerivingStrategies`: trying to memorize this algorithm is almost impossible!

### Interaction with Safe Haskell


Safe Haskell has some things to say about derived `Typeable` and `Generic` instances, so it's worth mentioning how `-XDerivingStrategies` fits into the picture.


GHC currently disallows manually implementing `Typeable` instances, and derived `Typeable` instances are ignored, as GHC automatically generates `Typeable` instances for all datatypes, typeclasses, and promoted data constructors. Similarly, GHC will ignore derived `Typeable` instances even if a deriving strategy is used.


GHC also disallows manually implementing `Generic` instances when `-XSafe` is enabled, so the only way to declare `Generic` instances in Safe Haskell is to use the `-XDeriveGeneric` extension. To preserve this property, it is forbidden to derive a `Generic` instance with a deriving strategy other than `stock`.

### What `-XDerivingStrategies` is not

`-XDerivingStrategies` is not intended to be a catch-all language extension that enables all of `-XDeriveFunctor`, `-XDeriveAnyClass`, `-XGeneralizedNewtypeDeriving`, `-XDeriveGeneric`, and all other exotic `deriving` language extensions. To see why consider the following code:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
newtype T = MkT S deriving (Foo, Bar)
```


This code compiles without issue, and uses `GeneralizedNewtypeDeriving` to derive `Foo` and `Bar` instances for `T`. But if you turn on `-XDerivingStrategies` as well, suddenly the above code will change in semantics: it will emit a warning about `GeneralizedNewtypeDeriving` and `DeriveAnyClass` both being on, and default to `DeriveAnyClass`! The intention of `-XDerivingStrategies` is to simply enable new syntactic forms that allow strictly more code to compile, and in particular, it is not intended to change the semantics of any existing code.


In addition, having `-XDerivingStrategies` imply `-XGeneralizedNewtypeDeriving` would have Safe Haskell repercussions, since one cannot currently use `-XSafe` in combination with `-XGeneralizedNewtypeDeriving` (see Trac #8827).

### Alternative syntax


Several alternative syntaxes and keyword suggestions have been proposed in the original track ticket (#10598) and on the ghc-devs mailing list ([https://mail.haskell.org/pipermail/ghc-devs/2016-July/012442.html](https://mail.haskell.org/pipermail/ghc-devs/2016-July/012442.html)). Here is an overview of some previous ideas:

- Use pragmas instead of keywords. We could indicate the use of deriving strategies like so:

```haskell
newtype T a = T a
  deriving          Show
  deriving {-# STOCK    #-} (Eq, Foldable)
  deriving {-# NEWTYPE  #-} Ord
  deriving {-# ANYCLASS #-} Read

deriving {-# STOCK #-} instance Functor T
```

>
> This has the advantage of being backwards compatible. On the other hand, several people objected to this idea on the basis that the presence of pragmas shouldn't affect the semantics of programs.

- Use type synonyms instead of keywords. We could have three builtin type syonyms:

```haskell
type Stock    (a :: k)  = a
type Newtype  (a :: k)  = a
type AnyClass (a :: k) = a
```

>
> that we imbue with compiler magic to indicate the presence of a deriving strategy. For example:

```haskell
newtype T a = T a deriving (Stock Eq, Newtype Ord, AnyClass Read, Show)
deriving instance Stock (Functor T)
```

>
> This is fairly backwards compatible (back to GHC 7.6), and would require absolutely no Template Haskell or parser changes. On the other hand, it requires making type synonyms behave magically, and it muddies up the actual class being derived, perhaps making it more confusing to look at.

- Instead of allowing multiple `deriving` clauses per datatype, one could indicate the presence of a deriving strategy by preceding every derived class with the appropriate keyword. For example:

```haskell
newtype T a = T a
  deriving (          Show
           , stock    Eq
           , stock    Foldable
           , newtype  Ord
           , anyclass Read
           )
```

>
> This is cleaner in some respects, as it more closely resembles an English-language description of how to derive all the instances that `T` needs (`Eq` via stock, `Ord` via newtype, `Read` via anyclass...). A downside is that this is much trickier (though likely not impossible) to parse, since all-lowercase words can be confused for type variables.

>
> Another factor to consider is the semantic noise that this suggestion brings. Unlike with multiple `deriving` clauses, this suggestion requires the use of a keyword next to *every* class. This can lead to more keystrokes than the multi-clause suggestion would when you derive several instances with the same deriving strategy. For instance, in the above example you need to type `stock` twice, whereas in the multi-clause proposal you need only type `stock` once. This can really add up when you derive loads of instances at a time, e.g.,

```haskell
newtype T a = T a
  deriving ( newtype A
           , newtype B
           , newtype C
           , newtype D
           , newtype E
           , newtype F
           )
```

>
> This can be expressed much more succintly as:

```haskell
newtype T a = T a
  deriving newtype (A, B, C, D, E, F)
```

>
> In addition, I (the proposal author, Ryan) would argue that it's tidier to put the strategy keyword outside of the parentheses, since it makes it clear that these keywords aren't modifying the type we're deriving, only the *means* by which we're deriving it.

>
> One could also argue that GHC should support both syntaxes, although it would combine the downsides of both for questionable gain.

- Previous alternative suggestions for the `stock` keyword were `bespoke`, `builtin`, `magic`, `wiredin`, `standard`, `native`, `original`, and `specialized`. In particular, `builtin` is what I (Ryan) originally suggested, but it was poorly received since *all* deriving extensions are, to some extent, built-in to GHC. I then championed `bespoke`, but others expressed reservations about the word's relative obscurity outside of Commonwealth English.
- A previous alternative suggestion for the `anyclass` keyword was `default`, since it's already a keyword, and the connection to `-XDefaultSignatures` would be evocative of generic programming, which `-XDeriveAnyClass` is often used for. On the other hand, I (Ryan) felt it would be too easy to confuse with what `stock` accomplishes (i.e., the "default" GHC behavior when deriving an instance), so I proposed `anyclass` to make it very explicit what's going on there.
