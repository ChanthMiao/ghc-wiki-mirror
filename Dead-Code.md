What does "dead code" mean in the context of a higher-order language?
This is a question with more subtlety than at might first appear.
It is relevant not just to what sorts of dead code diagnostics we might want, but also issues like incompleteness and totality checks, and potential new features like absurd patterns.
I hope establishing some principles for dead code in Haskell can shed light on all these things.

## First-order languages, in general

I mentioned "higher order" because I think that's the crux of the what dead code subtler than first appears.
So first let's go over the first-order case, which really is simple.

In a first order language, functions are not values.
Avoiding borderline cases like C's function pointers, We'll say there isn't even be any notion of "references to functions" which are values.
In such a language:

1. Function definitions are a single notion: `f x = ...` isn't sugar.

2. The only thing to do with a function is call it.

Let's now return to dead code.
Dead code is code which is never used.
This is trivial to understand, if not trivial to compute, notion in a closed world setting.
Any complexity there is stems from the open world setting we actually come together to write code in.
Here, it is not always decidable when code is used: it could be "known-used", "known-unused", or "unknown" because of the open world setting.
"Know-used" is simple enough: some other code we have will reference the function definition.
But what about deciding between "unknown" and known-unused"?
Here are two basic cases:

1. A definition (function or otherwise) that isn't used, and is out of scope in all possible additional code is known-unused.

2. A function definition that is uncallable is dead code.

The first one of these is purely a matter of name resolution.
The second is "more semantic", and stems from parameter for which there is no argument.

We'll henceforth also call "known-unused", "dead".

## Higher-order languages, in general

In the higher order case, functions are values.
In such a language:

1. Function definitions are a compound notation: the definition and the function are separate.
   For example, `f x = ...` is sugar for `f = \x -> ...`.

2. Functions can not only be called, but also passed to other functions.

What does this mean for dead code?
The first case of for "known-unused", which held for definitions in general, still stands.
The second however doesn't.
Functions that are uncallable can still be passed around, and so are not by their uncallability alone dead code.
That may sound extreme, but I just mean the function expression as a whole.
If a function is uncallable, it's *body* is still dead code.

This is a new distinction.
In the first order case, the entire function---function definition even---was or wasn't dead.
Now, the definition can be alive (used variable), the function can be alive (passed to higher order function, etc.), but the body isn't.
This is especially confusing when first-order syntax is used:
```haskell
f x = ...
--------- alive
  ------- alive
      --- dead
```

Yes, an uncallable function does no work at run time, and so it is passed around in vain, but that doesn't mean we can get rid of it.
It is a non-local, if even decidable, if even possible transformation to get rid of it.

### Explicit Uncallable functions

Let's assume dead code gets a warning.
We've already established that uncallable functions are cannot always be excised from the program.
That means there might be programs which dead function bodies causing warnings which cannot be removed.
This is no good --- we want all warnings to be actionable, so they are helpful to the user and not just a nuisance.

What can we do?
We could have syntax for an explicitly uncallable function with no body.
Because the uncallable is explicit, and there is no body to deam dead, there is rightfully nothing to warn about!

### Polarity

Now that we have higher order functions, we can have uncallable parameters.
How are they to be treated?
A parameter is name without a denotion --- call it an "indefinite definition" for the lovely etymological inconsistency.

Our notion of dead definitions only involved the LHS (name) and not RHS, so it still applies.
As it turns out, only possible-position names have an open-world scope.
That makes applying it very easy.
A parameter is either used and alive, or unused and dead.
Of course, we cannot locally rip away a function abstraction to remove the unused parameter any more than we could for an uncallable function, and that's why Haskell has no-warn `_` patterns.

> N.B. wildcard binders and an explicitly uncallabable function syntax are somewhat dual.
> One says the input might as well be `()`, the other says the output might was well be `Void`.

What about the second sort?
If we decide whether functions are uncallable based on their type, parameters have types, and so parameters can be uncallable too.
But only the body of a function expression is dead per the above, and their is no body with an abstract parameter.
Thus, the ability to call the function or lack thereof doesn't contribute to whether the parameter is dead.

## Finally, Haskell in particular

Let's now return to some more Haskell-specific aspects of this discussion.

### Patterns

Haskell has very rich negative postion syntax in the form of patterns.
This means that there could be not just one callable function syntax, but many.
The patterns ideally would prove the uncallability of the function.

### Uncallabable function syntax?

While we don't yet have proper proof-carrying absurd pattern syntax, we do have a one-size-fits all uncallable function syntax: `\case {}`.
Note that I am not using the more basic `\x -> case x of {}`.
The problem with the latter is non-locality: it is the function that is uncallable, but the body of the function that witnesses the non-possibility of the parameter.
(Yes there is the bottom value, but we have unboxed runtime reps which avoid that, and are getting are getting unlifted data types which do also, so let's ignore that muddle.)

If GHC determines without aid of the `case x of {}` that the function is uncallable, well, too bad, `case x of {}` is a non-trivial body, and thus worthy of a dead code warning.
How annoying!
Conversely, `\case {}` is a single bit of syntax.
The "proof-by-asking-the-magic-totality-checker" via `{}` is not part of the function body, because there isn't a function body.
There is thus nothing to annoyingly receive an annoying warning here.

### Constraints

The implementation of constraints as dictionaries makes clear that constraints act as parameters.
In particular, terms can have an absurd constraint.
But whereas we have `\case {}` to indicate an uncallable regular function, we have nothing similar for absurd constraint parameters.
That means we are stuck with no good way to allow the users to avoid dead code warnings.
