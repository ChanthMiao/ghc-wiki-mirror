This is a proposal for some improvements to the way we compile case expressions in ghc.  I'll set up the problem, describe what ghc currently does, and propose some new ideas on compiling case expressions.

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
See Note [Two alts + default] for a justification of this rule in Switch.hs -- this will be relevant to our discussions below.

3. Two alternatives [(x1, label1), (x2, label2)] with defLabel which we compile into:
```
  IfEqual x1 label1 (IfEqual x2 label2 defLabel)
```

If none of the above apply then we proceed as follows.  We split the case numbers into regions based on the location of holes (if two consecutive integers are a distance of > 7 apart then we consider a hole being present between them).  So now we have a list of Maps.  The intent is that each of those maps will become a MultuWayJump table in the compiled code.

Some of those Maps though may be very small (consisting of one less than 5 elements), and we don't want those to become MultuWayJump tables -- so we break those up into singleton Maps.  So now we have a list of Maps, with some being singletons and the others maps with more than 5 elements.  All these maps will become the leaves of our binary search tree.

We then construct a FlatSwitchPlan, which is a list SwitchPlans sepatated by an integer -- with the semantics being that if the expression is less than the integer we go to the left plan, otherwise the right plan.  During this construction, if we have a default, we interleave segments that jump to default between the maps.

Following that, we perform an optimization that replaces two less-than branches by a single equal-to-test (findSingleValues in the source), and finally we are ready to building the tree.  We split the plans in the FlatSwitchPlan in two, introduce an `IfLT` node to construct a node in the tree, and recurs on the left and right side of the list of plans.

Deficiencies of the existing algorithm:
---------------------------------------
The algorithm works quite well producing fairly good plans with either simple values at the leaves or JumpTables.  If you stare at the generated plans for a bit though it's not difficult to see how we could do better.

Here are some examples:
1. Sometimes it generates superfluous comparisons like for a example the issue https://gitlab.haskell.org/ghc/ghc/-/issues/19290 with an analysis by Henry on what goes wrong.

2. For a function like the following:
```
data ZZ = A1 | A2 | A3 | A4 | A5 | A6

f A1 = 1
f A2 = 2
f A3 = 2
f A4 = 2
f A5 = 3
f A6 = 3
```

we would typically get a jump table as below:

```
.LcXT_info:
	movq 8(%rbp),%rax
	andl $7,%ebx
	jmp *.LnYt(,%rbx,8)
.LcY1:
	movl $Foo_f1_closure+1,%r14d
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcXY:
	movl $Foo_f2_closure+1,%r14d
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcY5:
	movl $Foo_zdwf_closure,%ebx
	jmp *-8(%r13)
.LcXX:
	movl $Foo_f3_closure+1,%r14d
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
	.size Foo_zdwf_info, .-Foo_zdwf_info
.section .rodata
.align 8
.align 1
.LnYt:
	.quad	0
	.quad	.LcXX
	.quad	.LcXY
	.quad	.LcXY
	.quad	.LcXY
	.quad	.LcY1
	.quad	.LcY1
```
Jump tables have an O(1) cost of finding the  target label but they come with their own costs.  First, they take precious space in the cpu cache, space that would typically be used for instructions themselves.  Moreover we sometimes tolerate missing values which end up as entries in the array that are never read, wasting even more space.  Second (and more importantly), they obscure the continuation instruction, so the cpu can do no pre-fetching or analysis of the rest of the instruction stream until the actual array is indexed and the target fetched -- this decreases instruction level parallelism.

In this case it only takes a minute of come up with a better plan:
```
  if (x < 5)
    if (x < 2) return 1;
    else return 2;  
  else return 3;
```
what we have done here is used the structure of the target labels themselves which the existing algorithm makes limited use of. The jump table is gone -- two comparisons are always going to be better than a jump table.
Here is the assembly produced:
```
.LcZo_info:
	movq 8(%rbp),%rax
	andl $7,%ebx
	cmpq $5,%rbx
	jae .LcZw
.LuZU:
	cmpq $2,%rbx
	jb .LcZs
.LcZt:
	movl $Foo_f2_closure+1,%r14d
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcZs:
	movl $Foo_f3_closure+1,%r14d
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcZw:
	movl $Foo_f1_closure+1,%r14d
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
```

3. Consider the following function:
```
f :: Int -> Int
f 0  = 10
f 1  = 10
f 2  = 10
f 3  = 10  
f 23 = 10
f 44 = 10 
f 60 = 10
f 61 = 10
f  _ =  100
```

This is now compiled into the following maze of if-statements:
```
 if (x >= 45)
   if (x >= 61)
     if (x < 62) return 10;
     else return 100;
   else
     if (x < 60) return 100;
     else return 10;
else
  if (x >= 4)
    if (x >= 44) return 10;
    else
      if (x != 23) return 100;
      else return 10;
  else
    if (x >= 3) return 10;
    else
      if (x >= 0) return 10;
      else return 100;
```
That is a lot of code.  Could we do better here?  Can we use the fact that this function is returning only two values somehow?  See below for a new approach.

4.  Consider a function like the following:
```
f :: Int -> Int
f 1  = 1
f 2  = 2
f 3  = 3
f 4  = 4
f 5  = 5
...           -- Consecutive values up to N
f N = 6       -- with N is large
f 100 = 7
f 200 = 8
f 300 = 9
f 400 = 10    -- M scattered values with M << N
f 500 = 7
f 600 = 8
f 700 = 9
f 800 = 10
f _ = 9999
```
Here we have a large chunk at the start of the interval that is a perfect candidate for a JumpTable (consecutive values) followed by some scattered value at the end.

The existing algorithm produces a binary tree with the JumpTable as a leaf, but the binary tree produced, is balanced with respect to the various flat plans produced by the algorithm.  This is not optimal if our goal is to minimize the expected number of comparisons and proceed with the rest of the computation as quickly as possible.  The N values at the start are considered 1 plan, and we do binary search on the M + 1 values (it wouldn't be exactly M + 1 because of the introduction of more plans to deal with defaults but the point remains).  But since N >> M a better plan here would be to first examine the boundary of the N values first because with one comparison you can identify the largest interval of interest.  So it is always better to do binary search taking into account the weight (the number of cases being dispatched) of each of the leaves of the tree and not the number of leaves that you have.  Assuming the cases occur uniformly, this approach minimizes the expected number of comparisons until the label is found.

New algorithm:
--------------
First we propose 3 new kinds of segment types (leaves) to be added to the current list of 1 (the JumpTable) that we have now.  A segment type is a sequence of [(Int, Label)] pairs that can be compiled into code that executes in O(1) number of comparisons.  As you will see below, the new segments are motivated by the number of structure of the labels the cases jump to.

The new segment types are:
1. ContiguousRegions segments.
2. TwoLabels segments.
3. FourLabels segments.

We go through each of them in turn.

Contiguous segments:
--------------------
This kind of segment is trying to capture the pattern when consecutive integers go to the same label.  Here is an example:
```
data KindOfHexDigit = IsUpper | IsLower | IsDigit | IsNone

isHexDigit :: Char -> KindOfHexDigit
isHexDigit '0' = IsDigit
isHexDigit '1' = IsDigit
isHexDigit '2' = IsDigit
isHexDigit '3' = IsDigit
isHexDigit '4' = IsDigit
isHexDigit '5' = IsDigit
isHexDigit '6' = IsDigit
isHexDigit '7' = IsDigit
isHexDigit '8' = IsDigit
isHexDigit '9' = IsDigit
isHexDigit 'A' = IsUpper
isHexDigit 'B' = IsUpper
isHexDigit 'C' = IsUpper
isHexDigit 'D' = IsUpper
isHexDigit 'E' = IsUpper
isHexDigit 'F' = IsUpper
isHexDigit 'a' = IsLower
isHexDigit 'b' = IsLower
isHexDigit 'c' = IsLower
isHexDigit 'd' = IsLower
isHexDigit 'e' = IsLower
isHexDigit 'f' = IsLower
isHexDigit _   = IsNone
```
In this kind of segment we identify regions with cases consecutive going to the same label.  For the above we would have identified three such regions:
```
region1 = { label = IsDigitLabel, lb = '0', ub = '9' }
region2 = { label = IsLower, lb = 'a', ub = 'f' }
region3 = { label = IsUpper, lb = 'A', ub = 'F' }
```
Note that the regions don't have to be consecutive among themselves, only within.  But if they are this provides us with more opportunities for better plans -- we explore this below.  Also note that we impose no restriction on the number of labels, only on the number of regions.  The label for region2 and region3 could for example have been the same and we also take advantage of such happy coincidences as we will see below.

This is then compiled into the following:
```
if (x < '0')
  goto DefaultLabel;
if (x <= '9')
  goto isDigitLabel;
if (x < 'A')
  goto DefaultLabel;
if (x <= 'F')
  goto isLowerLabel;
if (x < 'a')
  goto DefaultLabel;
if (x <= 'f')
  goto isUpperLabel;
goto DefaultLabel;
```
If there is default we limit the number of labels we allow the pattern to capture to <= 3 and if there isn't to <= 4.  This segment type can be thought of a generalization of the special case of our existing algorithm described in Note [Two alts + default] in Switch.hs with the number of alts increased 
by one.  We do that because it affords us the flexibility to find better plans we certain cases as we describe below.  The number of segments is 4 for the no-default case because typically the fourth segment is identified for free due to the constrains imposed by input ADT (we will see examples below).

Compilation of Contiguous Regions Segment:
------------------------------------------

No-default case:
----------------