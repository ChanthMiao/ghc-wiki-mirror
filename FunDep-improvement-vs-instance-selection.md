Go up to [Key examples # 3](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC/Key-examples#example-3-lcc-and-licc-threaten-confluence)

## Q's for SPJ

[AntC] Following an email exchange with SPJ:

<blockquote>
@simonpj Don't forget the (a) improvement vs (b) instance selection separation.  It's important because consider

```haskell
class SomeC a b c |  a -> b
instance SomeC Int Bool Char
```

and [W] C Int alpha v, where v is (say) a skolem variable.   Then improvement spits out alpha ~ Bool, even though the instance doesn't match.

-- previously (see Example 4, although the discussion to be moved from there)

GHC does two *entirely separate* steps:

1. Improvement of a Wanted. The only effect is to emit new equality constraints
2. Discharging a Wanted using an instance.

</blockquote>

These instances are accepted along with the one above:


```haskell
instance {-# LIBERAL #-} (b ~ String) => SomeC Int b ()

instance {-# OVERLAPPABLE, LIBERAL #-} (b ~ String) => SomeC Int b Char
```

And it's easy to get `[W] SomeC Int String Char` accepted -- even though it directly contradicts the `OVERLAPPING` first instance.

Now with `~ String` in a constraint (rather than `String` in the head), GHC never 'spits out' an improvement `alpha ~ String` -- which is the trick to getting a type-level `TypeEq` to work. And yet `~` constraints are supposed to be morally equivalent to writing the equated type directly(?)