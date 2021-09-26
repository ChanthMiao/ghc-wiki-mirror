This is a proposal for some new ways of compiling case expressions in ghc.  I'll set up the problem, describe what ghc currently does, and propose some new ideas on compiling case expressions.

By the time we get to the Cmm level, case expressions are desugared into the much simpler problem of dispatching to the right Label given the expression integer.  So the input to the code generator is a Map Int Label, an optional default label, and a (lb, ub)representing the possible lower value and upper value an expression can have (we are also given signedness info on the case expression).  For example for this program:

```
f :: Int -> Int
f 0 = 100
f 1 = 200
f 2 = 200
f _ = 400
```
we will be given:
```
m        = Map.fromList [(0, label_100), (1, label_200), (2, label_200)]
defOpt   = Just label_400
(lb, ub) = (INT_MIN, INT_MAX)
signed   = True
```
Notice that the previous ghc stages have identified common subexpressions so the labels for 1 and 2 are identical since in both cases we return 200 -- this happens not only for simple expessions like integers but also for any complex expression -- we will make use of this property in the new code generator to great effect.

As a second example for a program like the following:
```
data T = T1 | T2 | T3

f :: T -> Int
f T1 = 100
f T2 = 200
F T3 = 300
```
we will be given:
```
m        = Map.fromList [(1, label_100), (2, label_200), (3, label_300)]
defOpt   = Nothing
(lb, ub) = (1, 3)
signed   = False
```
Note that here we have no default label, so there is an entry in the map for every possible value of the range (1, 3).  This is typically the case, although this is not an invariant guranteed by the ghc compilation -- it is possible not to have a default and given some range (lb, ub) not to have entries in the map m for some integers in (lb, ub).  This can happen (during compilation of ADTs), when ghc can prove to itself that those integers can never happen (I have added an example in the code for those interested -- see Note [Denseness of Case Expressions Without Default]).

Compiling such expressions in ghc proceeds by first identifying some cases we want to deal with bespoke code.  These are:

1. A singleton Map [(x, label)] with defLabel. This we compile into:
```
   IfEqual x label defLabel
```
2. A map with two entries [(x1, label1), (x2, label2)] and no default (this would be an if expression). This we compile into:
```
  IfEqual x1 label1 label2
```
