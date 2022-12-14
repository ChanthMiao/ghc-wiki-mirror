## Total Families


To guarantee the termination and completeness of the solving of equality constraints, we need to impose rather draconian restrictions on the instances of type synonym families.  More specifically, to achieve both termination and completeness, we need the *Strong Termination Condition* and if we settle for termination alone (accepting to be incomplete for some rather exotic programs), we need the *Relaxed (Termination) Condition* as defined in [Type Checking with Open Type Functions](http://www.cse.unsw.edu.au/~chak/papers/tc-tfs.pdf).  The *Strong Termination Condition* corresponds closely to the conditions imposed on functional dependencies.  The *Relaxed Condition* is somewhat more liberal, but still does not permit, for example, nested applications of type families in the right-hand side of a `type instance`.  For many practically attractive uses of type families, where the system is actually terminating, this is still to restrictive.

### Two examples

```wiki
data Z; data S a;

-- meets the Relaxed Condition
type family x :+ y
type instance Z   :+ y = y
type instance S x :+ y = S (x :+ y)

-- does not even meet the Relaxed Condition
type family x :* y
type instance Z   :* y = Z
type instance S x :* y = x :* y :+ y
```


The family `(:+)` meets the *Relaxed Condition*, but not the *Strong Termination Condition*.  However, `(:*)` meets not even the *Relaxed Condition*.  Nevertheless, we would expect that families defined by structural recursion should not compromise termination.  Another somewhat irritating aspect of this example is that we would like these definitions to be closed, but they are open in their present form.


We might want to define equality on types as

```wiki
data TFalse; data TTrue;

type family TypeEq s t
type instance TypeEq s s = TTrue
type instance TypeEq s t = FFalse  -- matches only if the previous instance does not
```


Unfortunately, the two instances are overlapping and there is no means by which we can disambiguate the overlap by using the same textual ordering as that which we are used to from value-level functions.

### Defining total families


To enable textual disambiguation of overlapping instances, we declare the equalities together (by transferring GADT syntax to type synonyms):

```wiki
type TypeEq s t where
  TypeEq s s = TTrue
  TypeEq s t = TFalse
```

`TypeEq` is a standard type family, but by virtue of being total (i.e., exhaustive) it is also closed.  Further equalities cannot define it any further.


Let's use the same idea for the addition/multiplication examples:

```wiki
type x :+ y where
  Z   :+ y = y
  S x :+ y = S (x :+ y)

type x :* y where
  Z   :* y = Z
  S x :* y = x :* y :+ y
```


In contrast to `TypeEq`, `(:+)` and `(:*)` is not total without an extra equality.  We take another idea from value-level function definitions and implicitly complete each of these definitions by a final catch all equality.  So, for `(:+)`, we assume a final

```wiki
x :+ y = VOID
```


and for `(:*)` a final

```wiki
x :* y = VOID
```

### A rewrite system with total families


We denote the rewrite system for `TypeEq` as

```wiki
Et: {TypeEq s s ~ TTrue; TypeEq s t ~ TFalse}
```


and that of the type-level addition and multiplication as

```wiki
Et: {Z   :+ y ~ y;
     S x :+ y ~ S (x :+ y); 
     _   :+ _ ~ VOID}
    {Z   :* y ~ Z; 
     S x :* y ~ x :* y :+ y; 
     _   :* _ ~ VOID}
```


Matching on such *rewrite rule blocks* starts with the first equality.  If a given family application cannot possibly match on the first equality, the second is considered, and so on.  The last one is guaranteed to match, and this is what makes the definitions total.  Consider the following examples:

- `TypeEq Int Int  -->  TTrue`
- `TypeEq Int Bool  -->  TFalse`
- `TypeEq a b` where `a` and `b` are two rigid type variables, can't be rewritten without further information about `a` and `b`.

### Critical examples concerning termination


Example 1 of the paper:

```wiki
Et: {F Bool ~ F (G Int); F _ ~ VOID}
    {G _ ~ VOID}

Eg: G Int ~ Bool
```


Completion gives:

```wiki
  Eg
=> (TOP)
  VOID ~ Bool
=> FAIL
```


We can be a bit more tricky and use

```wiki
Et: {F [a] ~ F (G a); F _ ~ VOID}
    {G _ ~ VOID}

Eg: G x ~ [x]
```


Here the idea is to use a new symbol in the local given, namely a rigid type variable.  Nevertheless, the only equality of `G` matches `G x` and completion of `Eg` leads to an inconsistency.


However, if we add another equality to `G`, the situation changes:

```wiki
Et: {F [a] ~ F (G a); F _ ~ VOID}
    {G Int ~ Bool; G _ ~ VOID}

Eg: G x ~ [x]
```


Here completion can't rewrite `G x`, as the outcome depends on getting more information about `x`.  However, we have the following infinite derivation:

```wiki
F [x]  -->  F (G x)  -->  F [x]  -->  ...
```


This is although `Eg` is inconsistent (and hence shouldn't be used for rewriting at all).


Can we fix this???
