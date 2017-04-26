# Planned Backend Optimizations


This page gathers together a list of ideas and plans for improving the backend of the compiler. Any progress made will also be noted here.

## Removal of Proc Point Splitting


The reason we needed proc-point splitting for the LLVM backend is detailed [here](commentary/compiler/backends/llvm/wip#get-rid-of-proc-point-splitting).

#### Rationale


Ideally, we would expose an entire Cmm function to LLVM for better optimization and machine code generation, instead of breaking the functions apart just to grab a valid label.
The proc-point splitting is also a rather ugly transformation we would like to remove from the pipeline.

#### Challenges


The address of a basic block is [ not a well-defined concept](http://llvm.org/docs/LangRef.html#addresses-of-basic-blocks) outside of an LLVM function.
There has been no movement on this in LLVM due to the key assumption made by LLVM's IR that all basic blocks have *known* predecessors, forming a complete control-flow graph amongst blocks.
An escaping block label opens up the possibility of an edge from an unknown location.
This is why an indirect branch instruction must conservatively list [ all possible block targets](http://llvm.org/docs/LangRef.html#i-indirectbr).

#### Proposal


The current proposal can be found [ here.](http://lists.llvm.org/pipermail/llvm-dev/2017-April/112144.html)

## Improving Heap Checks


See [\#8905](https://gitlab.haskell.org//ghc/ghc/issues/8905) and [\#12231](https://gitlab.haskell.org//ghc/ghc/issues/12231) and \[Compiling case expressions\] in StgCmmExpr

---


Here's something that's probably worth improving.


Code like this

```wiki
if y > 17
  then joinP (y - x)
  else joinP 10
```


Turns into this:

```wiki
  call GHC.Classes.>_info(R2) returns to c2mn, args: 32, res: 8, upd: 8;   // CmmCall
c2mn: // global
  _s2lC::P64 = P64[Sp + 8];   // CmmAssign
  _s2lE::P64 = P64[Sp + 24];   // CmmAssign
  if (R1 & 7 != 1) goto c2n1; else goto c2mX;   // CmmCondBranch
c2n1: // global
  Hp = Hp + 40;   // CmmAssign
  if (Hp > HpLim) (likely: False) goto c2n4; else goto c2n3;   // CmmCondBranch
c2mX: // global
  Hp = Hp + 24;   // CmmAssign
  if (Hp > HpLim) (likely: False) goto c2n0; else goto c2mZ;   // CmmCondBranch
```


Since both branches of the first conditional test lead to a heap check, it's better to commute the conditional tests here so we only have one heap test, since the heap tests produce extra code to enter the GC. The above would transform into:

```wiki
  call GHC.Classes.>_info(R2) returns to mergedHeapTest, args: 32, res: 8, upd: 8;   // CmmCall
mergedHeapTest:
  _localTemp = Hp + 40; // max(40, 24)
  if (_localTemp > HpLim) (likely: False) goto needGC; else goto continue;
needGC:
  HpAlloc = 40; // max(40, 24)
  R1 = R1;
  call stg_gc_unpt_r1(R1) returns to mergedHeapTest
continue:
  _s2lC::P64 = P64[Sp + 8];   // CmmAssign
  _s2lE::P64 = P64[Sp + 24];   // CmmAssign
  if (R1 & 7 != 1) goto leftSide; else goto rightSide;
leftSide:
  Hp = Hp + 40;  // could be merged into c2n3 if it has one pred.
  goto c2n3;
rightSide:
  Hp = Hp + 24;  // could be merged into c2mZ if it has one pred.
  goto c2mZ;
```

---

### Other Performance Bugs To Consider

- [\#12798](https://gitlab.haskell.org//ghc/ghc/issues/12798)
- [\#12808](https://gitlab.haskell.org//ghc/ghc/issues/12808)