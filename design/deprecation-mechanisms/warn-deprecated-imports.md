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
