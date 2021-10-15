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

As a second example consider a program doing case on an ADT:
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
It should be obvious from their names what the four constructors mean (the Bool is the IfLT constructor just tells us if the input case expression is signed or not so that the right Cmm operators will be used for the comparison -- for equality sign doesn't matter).

In compiling case expressions ghc proceeds by first identifying some cases we want to deal with bespoke code.  These are:

1. A singleton Map [(x, label)] with defLabel. This we compile into:
```
   IfEqual x label defLabel
```
2. A map with two entries [(x1, label1), (x2, label2)] and no default (this could be for example an if expression). This we compile into:
```
  IfEqual x1 label1 label2
```
3. Two alternatives [(x1, label1), (x2, label2)] with defLabel which we compile into:
```
  IfEqual x1 label1 (IfEqual x2 label2 defLabel)
```
See Note [Two alts + default] for a justification of this rule in Switch.hs -- this will be relevant to our discussions below.

If none of the above apply then we proceed as follows.  We split the case numbers into regions based on the location of holes (if two consecutive integers are a distance of > 7 apart then we consider a hole being present between them).  So now we have a list of Maps.  The intent is that each of those maps will become a MultuWayJump table in the compiled code.

Some of those Maps though may be very small (consisting of one less than 5 elements), and we don't want those to become MultuWayJump tables -- so we break those up into singleton Maps.  So now we have a list of Maps, with some being singletons and the others maps with more than 5 elements.  All these maps will become the leaves of our binary search tree.

We then construct a FlatSwitchPlan, which is a list SwitchPlans sepatated by an integer -- with the semantics being that if the expression is less than the integer we go to the left plan, otherwise the right plan.  During this construction, if we have a default, we interleave segments that jump to default between the maps.

Following that, we perform an optimization that replaces two less-than branches by a single equal-to-test (findSingleValues in the source), and finally we are ready to building the tree.  We split the plans in the FlatSwitchPlan in two by introducing an `IfLT` node, and recur on the left and right side of the list of plans.

Deficiencies of the existing algorithm:
---------------------------------------
The algorithm works quite well producing fairly good plans with either simple values at the leaves or JumpTables.  This proposal is about some new ideas that could allow us to do better for certain (I want to claim wide) range of cases occurring in practice.

Here are some examples where the existing algorithm doesn't do so well:
1. Sometimes it generates superfluous comparisons as in the following example:
```
data DD = A1 | A2 | A3 | A4 | A5 

f2 :: DD -> Bool
f2 A1 = False
f2 A2 = True
f2 A3 = True
f2 A4 = True
f2 A5 = True
```

ghc 8.8.4 (at -O2) generates the following assembly code:

```
.Lc1dy_info:
.Lc1dy:
	andl $7,%ebx
	cmpq $5,%rbx     <<<  superfluous
	jae .Lc1dD       <<<  superfluous
.Lu1e0:
	cmpq $2,%rbx
	jae .Lc1dD
.Lc1dC:
	movl $ghczmprim_GHCziTypes_False_closure+1,%ebx
	addq $8,%rbp
	jmp *(%rbp)
.Lc1dD:
	movl $ghczmprim_GHCziTypes_True_closure+2,%ebx
	addq $8,%rbp
	jmp *(%rbp)
.Lc1dJ:
	movl $ZZZZ_f2_closure,%ebx
	jmp *-8(%r13)
```

The 2nd and 3rd instructions are superfluous.  All we need here is the test against 2 and if we are greater or equal to 2 we can return True.  Comparing against 5 does nothing.

See issue https://gitlab.haskell.org/ghc/ghc/-/issues/19290 with an analysis by Henry on what goes wrong.

2. The only tool of the current algorithm for producing efficiently compiled leaves is the jump table.  For a function like the following:
```
data ZZ = A1 | A2 | A3 | A4 | A5 | A6

f A1 = 1
f A2 = 2
f A3 = 2
f A4 = 2
f A5 = 3
f A6 = 3
```

we would get a jump table as below:

```
.LcXT_info:
	movq 8(%rbp),%rax
	andl $7,%ebx
	jmp *.LnYt(,%rbx,8)
.LcY1:
	movl $Foo_f1_closure+1,%r14d       -- return 3
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcXY:
	movl $Foo_f2_closure+1,%r14d       -- return 2
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcY5:
	movl $Foo_zdwf_closure,%ebx        -- return 1
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
	.quad	0                          -- wasted space
	.quad	.LcXX
	.quad	.LcXY
	.quad	.LcXY
	.quad	.LcXY
	.quad	.LcY1
	.quad	.LcY1
```
Jump tables have an O(1) cost of finding the target label which is great, but they come with other hidden  costs.  First our executables tend to be larger because we need space for the table.  Second these tables take precious space in the cpu cache, space that would typically be used for instructions themselves.  Moreover since we  tolerate missing values in the lookup arrays, which end up as more wasted space in the cache as entries that are never actually read, wasting even more space.  Third (and more importantly), they obscure the continuation instruction, so the cpu can do no pre-fetching or analysis of the rest of the instruction stream until the array is actually indexed and the target fetched -- this decreases instruction level parallelism, drugging down performance.

In this particular case it only takes a minute of come up with a better plan than the jump table:
```
  if (x < 5)
    if (x < 2) return 1;
    else return 2;  
  else return 3;
```
What we have done here is used the structure of the target labels themselves which the existing algorithm makes limited use of. The jump table is gone -- two comparisons are always going to be better than a jump table.
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
	movl $Foo_f2_closure+1,%r14d      -- return 2
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcZs:
	movl $Foo_f3_closure+1,%r14d      -- return 1
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcZw:
	movl $Foo_f1_closure+1,%r14d      -- return 3
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
```

This is ever more pronounced in the following predicate-like function:
```
data I = A1 | A2 | A3 | A4 | A5 | A6
data R = R1 | R2

foo A1 = R1
foo A2 = R2
foo A3 = R2
foo A4 = R2
foo A5 = R2
foo A6 = R2
```
for which we currently get a switch table:
```
.LcTN_info:
	andl $7,%ebx
	jmp *.LnUn(,%rbx,8)
.LcTS:
	movl $Foo_R2_closure+2,%ebx
	addq $8,%rbp
	jmp *(%rbp)
.LcTR:
	movl $Foo_R1_closure+1,%ebx
	addq $8,%rbp
	jmp *(%rbp)
.LcTZ:
	movl $Foo_foo_closure,%ebx
	jmp *-8(%r13)
	.size Foo_foo_info, .-Foo_foo_info
.section .rodata
.align 8
.align 1
.LnUn:
	.quad	0
	.quad	.LcTR
	.quad	.LcTS
	.quad	.LcTS
	.quad	.LcTS
	.quad	.LcTS
	.quad	.LcTS
```
but clearly all we need is simple if-statement:
```
   IfEqual 1 LabelR1 (Unconditionally LabelR2)
```
in assembly:
```
.LcSI_info:
	andl $7,%ebx
	cmpq $1,%rbx
	je .LcSM
.LcSN:
	movl $Foo_R2_closure+2,%ebx
	addq $8,%rbp
	jmp *(%rbp)
.LcSM:
	movl $Foo_R1_closure+1,%ebx
	addq $8,%rbp
	jmp *(%rbp)
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
Our integers are interspersed, thus no jump table, so we get a binary tree.  The compiled code is now the following maze of if-statements:
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
That is a lot of code.  Could we do better here?  Can we use the fact that this function is returning only two values somehow?  We describe our new approach below which now generates the following code:

```
.Lc15M_info:
	movq 7(%rbx),%rax
	testq %rax,%rax
	jl .Lc15X
.Lu166:
	cmpq $61,%rax
	jg .Lc15X
.Lu167:
	movq $-1152920405094694900,%rbx
	movq %rax,%rcx
	shlq %cl,%rbx
	testq %rbx,%rbx
	jge .Lc15X
.Lu168:
	movl $stg_INTLIKE_closure+417,%ebx     -- return 10
	addq $8,%rbp
	jmp *(%rbp)
.Lc15X:
	movl $stg_INTLIKE_closure+1857,%ebx     -- return 100
	addq $8,%rbp
	jmp *(%rbp)
```
For comparison this is what is generated by ghc 8.10.7:
```
Foo_zdwf_info:
	cmpq $45,%r14
	jl .Lu1Fk
.Lu1Fq:
	cmpq $61,%r14
	jl .Lu1Fr
.Lu1Fs:
	cmpq $62,%r14
	jl .Lc1F9
.Lc1F8:
	movl $100,%ebx
	jmp *(%rbp)
.Lu1Fr:
	cmpq $60,%r14
	jl .Lc1F8
.Lc1F9:
	movl $10,%ebx
	jmp *(%rbp)
.Lu1Fk:
	cmpq $4,%r14
	jl .Lu1Fl
.Lu1Fo:
	cmpq $44,%r14
	jge .Lc1F9
.Lu1Fp:
	cmpq $23,%r14
	jne .Lc1F8
	jmp .Lc1F9
.Lu1Fl:
	cmpq $3,%r14
	jge .Lc1F9
.Lu1Fm:
	testq %r14,%r14
	jl .Lc1F8
	jmp .Lc1F9
```

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
Here we have a large chunk at the start of the interval that is a perfect candidate for a JumpTable (which the current algorithm correctly identifies), followed by some scattered value at the end.

The existing algorithm produces a binary tree with the JumpTable as a leaf, but the binary tree produced, is balanced with respect to the various flat plans produced by the algorithm.  This is not optimal if our goal is to minimize the expected number of comparisons and proceed with the rest of the computation as quickly as possible.  The N values at the start are considered 1 leaf, and we do binary search on the M + 1 leaves (it wouldn't be exactly M + 1 because of the introduction of more plans to deal with defaults but the point remains).  But since N >> M a better plan here would be to first examine the boundary of the N values first because with one comparison you can identify the largest interval of interest.  Under the assumption of uniformity, it is always better to do binary search taking into account the weight (the number of cases being dispatched) of each of the leaves of the tree and not the number of leaves that you have.  This approach minimizes the expected number of comparisons until the label is found.

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
region1 = { label = IsDigitLabel, lb = '0', ub = '9' }   -- 48 to 57
region2 = { label = IsUpperLabel, lb = 'A', ub = 'F' }   -- 65 to 70
region3 = { label = IsLowerLabel, lb = 'a', ub = 'f' }   -- 97 to 102
```
Note that the regions don't have to be consecutive among themselves, only within.  But if they are, this provides us with more opportunities for better plans -- we explore this below.  Also note that we impose no restriction on the number of labels, only on the number of regions.  The label for region2 and region3 could for example have been the same and we also take advantage of such happy coincidences as we will see below.

This is then compiled into the following:
```
Foo_zdwisHexDigit_info:
.LcTs:
	cmpq $65,%r14
	jb .LuTv
.LuTx:
	cmpq $70,%r14
	jbe .LcTg
.LuTy:
	cmpq $97,%r14
	jb .LcT5
.LuTz:
	cmpq $102,%r14
	ja .LcT5
.LcTm:
	movl $Foo_IsLower_closure+2,%ebx       -- return IsLower
	jmp *(%rbp)
.LuTv:
	cmpq $48,%r14
	jb .LcT5
.LuTw:
	cmpq $57,%r14
	ja .LcT5
.LcT6:
	movl $Foo_IsDigit_closure+3,%ebx       -- return IsDigit
	jmp *(%rbp)
.LcT5:
	movl $Foo_IsNone_closure+4,%ebx       -- return IsNone
	jmp *(%rbp)
.LcTg:
	movl $Foo_IsUpper_closure+1,%ebx      -- return IsUpper
	jmp *(%rbp)
```

If there is default we limit the number of labels we allow the pattern
to capture to <= 3 and if there isn't to <= 4.  This segment type can
be thought of a generalization of the special case of the existing
algorithm described in Note [Two alts + default] in Switch.hs with the
number of alts increased by one.  We do that because it affords us the
flexibility to find better plans we certain cases as we describe
below.  The number of segments is 4 for the no-default case because
typically the fourth segment is identified for free due to the
constrains imposed by input ADT (we will see examples below).

Compilation of Contiguous Regions Segment:
------------------------------------------
We now go through the details of how Contiguous Regions are compiled.
We deal with the the presence and absence of default label separately.

No-default case:
----------------

First we try to build what we call a Dichotomy Plan.  Suppose `m` is
a `Map Label [Integer]` formed by reversing the association between
integers and labels and merging integers going to the label into a
list.  If the number of labels is 2 (equivalently if `M.size m == 2`)
and there is in `m` a list of Integers of size <= 2 (let's pretend it
is found in the pair (Label, [n1, n2])) then this means that there a
label that <= 2 integers go to it.  Since there are in total only 2
labels total, we can form the simple plan of:
```
  IfEqual n1 Label (IfEqual n2 Label (Unconditionally otherLabel))
```
where otherLabel is the other label found in the map (remember there
were only two).

Here is an example where the dichotomy plan kicks in.

```
data Z = A1 | A2 | A3 | A4 | A5 | A6

f :: Z -> Int
f A1 = 2
f A2 = 1
f A3 = 2
f A4 = 2
f A5 = 2
f A6 = 1
```
We have 4 regions ([2], [1], [2,2,2], [1]) but only two labels.  The
label for 1, has exactly 2 integers going to it (those corresponding
to A2 and A6).  So our plan is:
```
IfEqual 6 LabelFor1 (IfEqual 2 LabelFor1 (Unconditionally LabelFor2))
```
In assembly:
```
.LcS6_info:
	andl $7,%ebx
	cmpq $6,%rbx
	je .LcSb
.LuSq:
	cmpq $2,%rbx
	je .LcSb
.LcSa:
	movl $stg_INTLIKE_closure+289,%ebx
	addq $8,%rbp
	jmp *(%rbp)
.LcSb:
	movl $stg_INTLIKE_closure+273,%ebx
	addq $8,%rbp
	jmp *(%rbp)
```
The existing algorithm produces identical code here (because of the
findSingleValues optimization we described above).

If a dichotomy plan is not possible, we then proceed to build a
standard plan.  First we determine the `Heaviness` of the regions
where heaviness is the ADT:
```
  data Heaviness
    = LeftHeavy
    | Balanced
    | RightHeavy
```
Let regions = [r0, r1, ..., rn] (n as we described earlier is <= 3 if
there is default, and <= 4 if there isn't).  We sum up the number of
elements of each region in first half of the list, and in the last
half of the list (if the list has an odd number of regions the middle
region is ignored).  Let `prefix` be the left sum and `postfix` the
right sum.  Let `total` be the sum of all regions including the middle
region (if there is one).

We consider the region list `LeftHeavy` if `prefix > (4 / 7) * total`,
`RightHeavy` if `postfix > (4 / 7) * total` and `Balanced` otherwise.

The Heaviness of the list or regions tells us whether we should
generate a left-leaning or a right-leaning plan.  A left-leaning plan
proceeds by comparing the input expression to the upper bound of r0 --
since the regions are adjacent to each other with no gaps (a
consequence of no-default), this allows up to dispatch of `r0`
(meaning we know if we are in r0 or not) in one comparison.  Then we
compare against the upper bound bound of r1 etc. For example for the
following program:

```
f :: data Z = A1 | A2 | A3 | A4 | A5 | A6 | A7

f A1 = 1
f A2 = 1
f A3 = 1
f A4 = 2
f A5 = 2
f A6 = 2
f A7 = 3
```
we generate the following left-leaning plan:
```
  IfLE False 3
    (Unconditionally LabelFor1)
    (IfLE False 6
      (Unconditionally LabelFor2)
      (Unconditionally LabelFor3))
```
while if the function was defined as follows:
```
f :: data Z = A1 | A2 | A3 | A4 | A5 | A6 | A7

f A1 = 1
f A2 = 2
f A3 = 2
f A4 = 3
f A5 = 3
f A6 = 3
f A7 = 3
```
then we would generate a right leaning plan as follows:
```
  IfLT False 4
    (IfLT False 2
      (Unconditionally LabelFor1)
      (Unconditionally LabelFor2))
    (Unconditionally LabelFor3)
```
This way (under the assumption of uniformity) we minimize the number
of comparisons until the label is found.

Here is an example when we are dealing with 4 labels:
```
f :: data Z = A1 | A2 | A3 | A4 | A5 | A6 | A7

f A1 = 1
f A2 = 2
f A3 = 2
f A4 = 3
f A5 = 3
f A6 = 3
f A7 = 4
```
The four regions here are (LabelFor1, [1]), (LabelFor2, [2,2]),
(LabelFor3, [3,3,3]), and (LabelFor4, [4]).  This layout turns out to
be balanced (because `(1 + 2) <= (4 / 7) * 7 && (3 + 1) <= (4 / 7) *
7`) so we produce the balanced plan:
```
  IfLT False 4 (IfLT False 2 LabelFor1 LabelFor2) (IfLE False 6 LabelFor3 LabelFor4)
```
in assembly:
```
.LcWp_info:
	movq 8(%rbp),%rax
	andl $7,%ebx
	cmpq $4,%rbx
	jb .LuWZ
.LuX0:
	cmpq $6,%rbx
	jbe .LcWw
.LcWz:
	movl $Foo_f1_closure+1,%r14d    -- return 4
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcWw:
	movl $Foo_f2_closure+1,%r14d    -- return 3
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LuWZ:
	cmpq $2,%rbx
	jb .LcWt
.LcWu:
	movl $Foo_f3_closure+1,%r14d     -- return 2
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcWt:
	movl $Foo_f4_closure+1,%r14d     -- return 1
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
```

In instead f was defined as follows:
```
data Z = A1 | A2 | A3 | A4 | A5 | A6 | A7

f A1 = 1
f A2 = 2
f A3 = 3
f A4 = 4
f A5 = 4
f A6 = 4
f A7 = 4
```
it would be considered right-heavy (since `5 > (4 / 7) * 7`) for 
which we generate the following plan:
```
  IfLT False 4 (IfLT False 3 (IfLT False 2 LabelFor1 LabelFor2) LabelFor3) LabelFor4
```
so we determine that LabelFor4 is the correct target label in just one comparison.
In assembly:
```
.LcWp_info:
	movq 8(%rbp),%rax
	andl $7,%ebx
	cmpq $4,%rbx
	jae .LcWw
.LuWZ:
	cmpq $3,%rbx
	jae .LcWv
.LuX0:
	cmpq $2,%rbx
	jb .LcWt
.LcWu:
	movl $Foo_f3_closure+1,%r14d     -- return 2
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcWt:
	movl $Foo_f4_closure+1,%r14d     -- return 1
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcWw:
	movl $Foo_f1_closure+1,%r14d     -- return 4
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
.LcWC:
	movl $Foo_zdwf_closure,%ebx
	jmp *-8(%r13)
.LcWv:
	movl $Foo_f2_closure+1,%r14d     -- return 3
	movq %rax,%rbx
	addq $16,%rbp
	jmp stg_ap_p_fast
```
