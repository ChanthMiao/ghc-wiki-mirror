# Deprecate Class Instances

## What?

Add the ability to attach a `DEPRECATED` pragma to a specific instance of a class.

## Syntax

This would be expanding where the `DEPRECATED` pragma can occur, but would not introduce any new
syntax on it's own.

The location before the `where` matches that for deprecating modules.

## Examples

```
instance Show Foo {-# DEPRECATED "Don't use the Show instance of Foo" #-} where
  ...
```

## Practical Use Case

Suppose you have a library `bar` that exposes a type `Foo` with an instance of `Show`. If for any
reason you wish to remove the instance of `Show`, that could impact any consumer of `bar`. By
providing a mechanism to deprecate, impacted users are given another communication channel and time
to adjust.
