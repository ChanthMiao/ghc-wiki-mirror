This is collects ideas from a discussion among @rae, @simonpj and @nomeata.

The assumption is that pattern could have _universal arguments_. In a way, they already do: The pattern `Just` has one universal type argument, and one existential term arguments. We could also imagine a pattern synonym `LessThan` that could take a universal _term_ argument.

In expressions, the distinction doesn’t matter and both kind of arguments are passe as normal (with or without `@`, that’s orthogonal). But in patterns universal arguments are better distinguished, because they behave differently with regard to scoping: Identifiers in existential (“normal”) arguments are binding sites; identifiers in universal arguments are occurrences.

In an email thread started in Sept 2021, we brainstromed some syntactic ideas, which I (Joachim) collect here with brevity.

## A Modifier

Plain, simple, blunt.

```
f (Just @(%parameter Int) x) (LessThan (%parameter x)) = … 
```

## Half a view pattern

Because there already the left side is an expression embedded in a pattern:

```
f (Just @(Int ->) x) (LessThan (x ->)) = … 
```

## Universals to the left, existentials to the right

Assuming all universal arguments come before all existential ones.

```
f (@Int Just x) (x LessThan) = …
```

But: this *is* ambiguous: `f (Nothing LessThan) = …`  Is that a match for `Nothing` with a nested `LessThan` match? Or a match for `LessThan` with a `Nothing` input? Can't tell.

## Universals to the left, existentials to the right, with an arrow

```
f (@Int -> Just x) (x -> LessThan) = …
```

Disambiguation rules: Given a pattern of the form `(expr -> pat)`, we must decide whether this is a view-pattern or a pattern-with-inputs,

1. If pat is not a ConPat, then the pattern is a view-pattern. Stop.
2. Otherwise, let K be the constructor. If K requires an input, then the pattern is a pattern-with-inputs. Stop.
3. Otherwise, if the left-most part of the expression is a type application form (that is, begins with a prefix-@), then the pattern is a pattern-with-inputs. Stop.
4. Otherwise, the pattern is a view-pattern.

## Universals to the left, existentials to the right, with an arrow and a modifier

Like above, but disambiguated with a modifier

```
f (%Input @Int -> Just x) (%Input x -> LessThan) = …
```

## Mark end of argument sequence, not each argument

Attractive due to analogy to the `=>` in pattern types.

```
f (Just @Int => x) (LessThan x =>) = …
```