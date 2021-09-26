This is a proposal for some new ways of compiling case expressions in ghc.  I'll set up the problem, describe what ghc currently does, and propose some new ideas on compiling case expressions.

By the time we get to the Cmm level, case expressions are desugared into the much simpler problem of dispatching to the right Label given the expression integer.  So the input to the code generator is a Map Integer Label, an optional default label, and a (lb, ub) representing the possible lower value and upper value an expression can have (we are also given signedness info on the case expression).  For example for this program:

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
Note that here we have no default label, so there is an entry in the map for every possible value of the range (1, 3).  This is typically the case, although this is not an invariant guaranteed by the ghc compilation -- it is possible not to have a default and given some range (lb, ub) not to have entries in the map m for some integers in (lb, ub).  This can happen (during compilation of ADTs), when ghc can prove to itself that those integers can never happen (I have added an example in the code for those interested -- see Note [Denseness of Case Expressions Without Default]).

Existing algorithm
------------------
The function of interest lives in compile/GHC/Cmm/Switch.hs and is called createSwitchPlan().  The goal is to create a `SwitchPlan` defined as follows:

```
data SwitchPlan
    = Unconditionally Label
    | IfEqual Integer Label SwitchPlan
    | IfLT Bool Integer SwitchPlan SwitchPlan
    | JumpTable SwitchTargets
```
It should be obvious from their names what the four constructors mean.

In compiling case expressions ghc proceeds by first identifying some cases we want to deal with bespoke code.  These are:

1. A singleton Map [(x, label)] with defLabel. This we compile into:
```
   IfEqual x label defLabel
```
2. A map with two entries [(x1, label1), (x2, label2)] and no default (this would be an if expression). This we compile into:
```
  IfEqual x1 label1 label2
```
3. Two alternatives [(x1, label1), (x2, label2)] with defLabel which we compile into:
```
  IfEqual x1 label1 (IfEqual x2 label2 defLabel)
```

If none of the above apply then we proceed as follows.  We split the case numbers into regions based on the location of holes (if two consecutive integers are a distance of > 7 apart then we consider a hole being present between them).  So now we have a list of Maps.  The intent is that each of those maps will become a MultuWayJump table in the compiled code.

Some of those Maps though may be very small (consisting of one less than 5 elements), and we don't want those to become MultuWayJump tables -- so we break those up into singleton Maps.  So now we have a list of Maps, with some being singletons and the others maps with more than 5 elements.  All these maps will become the leaves of our binary search tree.

We then construct a FlatSwitchPlan, which is a list SwitchPlans sepatated by an integer -- with the semantics being that if the expression is less than the integer we go to the left plan, otherwise the right plan.  During this construction, if we have a default, we interleave segments that jump to default between the maps.
Finally we are ready to building the tree.  We split the plans in the FlatSwitchPlan in two, introduce an `IfLT` node to construct a node in the tree, and recurs on the left and right side of the list of plans.


New algorithm:
--------------
The algorithm works quite well producing fairly balanced binary trees, with either simple values at the leaves or JumpTables.  This proposal 



We want to note here that the binary tree produced, is balanced with respect to the various flat plans produced by the algorithm and may not be optimal.  For example suppose the input consisted of N consecutive cases in the beginning (with N large) and then M scattered values after that with M smaller than N.  The N values at the start would be considered 1 plan, and we would do binary search on the M + 1 values (it wouldn't be exactly M + 1 because of the introduction of more plans to deal with defaults but anyway).  But since N >> M a better plan here would be to first examine the boundary of the N values first because with one comparison you can identify the largest interval of interest.  So do binary search taking into account the weight (the number of cases being dispatched) by each of the leaves of the tree.

