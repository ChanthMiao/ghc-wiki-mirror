# REMOVED pragma (#10933)

## What?

When removing a previously DEPRECATED declaration, have an error message that explains the removal as opposed to the generic "symbol not found".
This allows for continued useful messaging to consumers where an author of some declaration can give detailed migration steps as needed.

## Syntax

The syntax should largely be the same as the `DEPRECATED` pragma. That is `{-# REMOVED foo ".." #-}`. Where the given annotation is presented when attempting to use the now removed declaration.

There is some complication with resolving a removed type constructor versus a removed data constructor. Using existing extensions this can ambiguity can be resolved:

- In the presence of the `ExplicitNamespaces` language extension:
  The pragma would be extended to allow inclusion of the " type " prefix in the same manner as import or export lists.

  Becoming `{-# REMOVED type Foo ".." #-}`.

  Thus allowing to disambiguate type constructor removals as those with the " type " prefix versus data constructor removals as those without.

## Specification of deprecated class methods

Under this removed-pragma proposal when `{-# REMOVED foo ".." #}` is present then references to `foo` will emit the given error message _in place of_ the "symbol not found" error message. If the DEPRECATED pragma is extended to typeclass methods the REMOVED pragma should be extended along with it.


## Examples

Suppose we have a module as follows:

```
module M1 where

bar :: a -> a
bar x = x

{#- REMOVED foo "'foo' has been removed, please use 'bar' instead" #-}
```

Then in a second module that previously used 'foo' from the above:
```
module M2 where

import M1

fizz = foo
```
This would now have the error `"'foo' has been removed, please use 'bar' instead"`


## Practical Use Case

This could have been an aid in the [MonadFail](https://gitlab.haskell.org/ghc/ghc/-/wikis/proposal/monad-fail) transition.

Also it could be used with the proposal for changing [Eq](https://github.com/haskell/core-libraries-committee/issues/3).
