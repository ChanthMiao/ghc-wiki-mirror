# Class Method Deprecations (#10071)

## What?


Class method deprecations are a `DEPRECATED`-variant to be attached to method declarations (in `class`-definitions) that triggers warnings when

1. an `instance` overrides the default implementation of that method, and/or when
1. the method is explicitly im/(re)exported via `Class(method)`-syntax.


But not when

1. imported as e.g. `import Mod1 (Class, method)` (which doesn't require `method` to be a method of `Class`), nor when
1. merely referring to `method` in an expression (as that doesn't require `method` to be a method either).


Or put differently, the warning shall forecast the compile errors that would occur if a method `bar` as in the class `C` (see syntax section) was moved out of the class `C` to either a different class `D` or to a top-level binding.

## Syntax



It's already possible to attach top-level (deprecation) warnings to class methods. However, we need a different syntax for this new variant of warnings attached to methods:


```
module M1 where

class C a where
  foo :: a

  bar :: a -> a
  bar x = x

  -- New class-method deprecation annotation
  -- NB: the pragma is indented at the class body level!
  {-# DEPRECATED bar "'bar' will cease to be a method of C, please avoid referring to it as a method of C!" #-}

  doo :: a
  doo x = x

-- This is an ordinary (old-style) top-level indented deprecation
{-# DEPRECATED foo "'foo' is obsolete and going away soon, please use 'doo' instead" #-}
```

## Review of DEPRECATED


The existing `{-# DEPRECATED foo ".." #}` pragma in a module `Def` causes a deprecation warning to be emitted whenever `foo` is referred to.  But note that we do **not** warn when `foo` is used

- In the same module that it was defined.
- In import and export lists.


So, deliberately, a module that gathers and re-exports deprecated functions does not cause a complaint:

```wiki
module Gather( foo, f, g ) wehre
  import Def( foo, f )
  import Blah( g )
```

## Specification of deprecated class methods


Under this new deprecated-class-methods proposal, when `{-# DEPRECATED bar ".." #}` (in module `Def`) is attached to the definition site of a class method in a class `C`, the principle is that *a deprecation warning is emitted only when `bar` is referred to in such a way that it must be a class method*.


More precisely, a deprecation warning is emitted under the following circumstances (only):

- An instance of `C`, in a module other than `Def`, gives a definition for method `bar`,
- `bar` is mentioned by a declaration in a module other than `Def`, and `bar` is in scope only via import items that imply `bar` is a class method, namely `C(..)` or `C(bar)`.


A possible alternative is to tighten up the latter to complain if *any* of the import items are offensive.  E.g.

```wiki
module Use where
  import Def( C( bar ) )
  imoprt Def( bar )
  x = bar {}
```


Here `bar` is in scope in two ways.  Does this cause a complaint?

## Examples



For the example of class `C` from the previous section, the following code fragments exemplify the expected warnings


```
import M1

x = bar () -- no warning, because the import doesn't limit `bar` to be a method of `C`

instance C () where
  foo = () -- no warning

instance C Bool where
  foo = True
  bar = not -- triggers warning, because this requires `bar` to be a method of C
```

```
import M1 (C, bar)

x = bar () -- no warning, because the import doesn't limit `bar` to be a method of `C`
```

```
import M1 (C(foo,bar))

x = bar () -- triggers warning, because `bar` is imported via `C(foo,bar)` syntax
```

```
import M1 (C(..))

x = bar () -- triggers warning, because `bar` is imported via `C(..)` syntax
```

```
import M1 (C(..), bar)

x = bar () -- no warning, because the import doesn't limit `bar` to be a method of `C`
```

## Practical Use Case


This would aid long-term transitions like phasing out class-methods, such as e.g. `Monad(return)` in the spirit of #4834:



With the AMP, `Monad(return)` and `Monad((>>))` being a class method becomes an historic artifact. The ideal long-term situation would rather be to have `return` become a top-level definition (i.e. outside the `Monad`-class), generalised to `Applicative` just aliasing `Applicative(pure)`. Moreover, the [MonadFail](design/monad-fail) proposal would have the `fail` method move to a new `MonadFail` class. I.e.


```
-- Haskell 2010 Report

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  return :: a -> m a
  fail   :: String -> m a
```


is to become


```
-- Hypothetical Haskell 201x Report

class Applicative m => Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b

class Monad m => MonadFail where
  fail   :: String -> m a

-- legacy synonym generalised to Applicative
return :: Applicative f => a -> f a
return = pure

-- legacy synonym generalised to Applicative
(>>) :: Applicative f => f a -> f b -> f b
(>>) = (*>)
```


The generalised `return` alias may be beneficial for things like `ApplicativeDo` which otherwise would require a `return` to be handled is if it was `pure` in order to weaken the type-constraint in an `do`-expression like e.g.


```
do { x <- f; return (fst x) }
```


Right now, we can attach a `DEPRECATED`-pragma to the `return`-class-method. That however would trigger warnings on *all* uses of `return`, rather than only when using `return` in a context that requires it to be a class-method (like overriding `return` in instance definitions, or importing/exporting it via the explicit `Monad(return)`-syntax)
