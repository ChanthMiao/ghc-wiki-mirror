# Warn On All Deprecated Imports (#2119)

## What?

Currently a `DEPRECATED` and explicit import that is unused does not trigger a warning.

This would add a new warning `-Wdeprecations-explicit-unused` for this scenario.

## Syntax

This would not directly change any syntax, rather only add a new warning.

## Examples

Given a module with a deprecated export as follows:
```
module Foo where
{-# DEPRECATED foo "don't use foo please, use ..." #-}
foo = ...
```

And a module that will explicitly import `foo` but not use it, like so:
```
module Bar where
import Foo (foo)
```

Then we would see a warning upon compiling with `-Wdeprecations-explicit-unused` like the following:

```
In the import of ‘foo’ (imported from Foo):
Deprecated: "don't use foo please, use ..."
```
Which is much like the current `-Wdeprecations` warning message.

## Practical Use Case

This allows a user to be more thorough in eliminating the use of and dependence on deprecated items.

In the case of a module that simply serves to re-export, one may desire to import deprecated items. Because of this, it is believed to reasonable to desire `-Wdeprecations` but the proposed behavior. For this reason it is suggested to be a separate warning from `-Wdeprecations`.


## Treatment of reexports

Consider the case:
```haskell
module Lib (foo) where
{-# DEPRECATED foo "don't use this" #-}
foo = ...

module Reexp (module Lib) where
import Lib

module App where
import Reexp (foo)
bar = ... foo ...
```

Which warnings, if any, should this elicit? There are a few possible places where we might warn:

1. Warn on the `module Lib` export item in `Reexp`'s export list
2. The import of `Lib` in `Reexp`
3. The import of `foo` in `App`
4. The occurrence of `foo` in `App`

Today (as of GHC 9.2) we will see only a warning from (4) (from `-Wdeprecations`). However, even without the above proposal it seems plausible that `-Wdeprecations` should also warn on (1) (since the export list item is essentially an occurrence of `foo`). The author of `Reexpr`, if they desired, is able to silence this warning by providing an explicit `hiding` list on `import Lib`.

Then there is the question of the semantics of `-Wdeprecations-explicit-unused`, which will determine whether to warn in (2) and (3). (2) isn't an explicit import and therefore seems not to be covered by the proposed `-Wdeprecations-explicit-unused`. Consequently, it stands to reason that `-Wdeprecations-explicit-unused`, should only warn on (3).