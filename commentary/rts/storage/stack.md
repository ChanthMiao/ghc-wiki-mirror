# Layout of the stack


Every [TSO object](commentary/rts/storage/heap-objects#thread-state-objects) contains a stack (`tso->stackobj`).  The stack of a TSO grows downwards, with the topmost (most recently pushed) word pointed to by `tso->stackobj->sp`, and the bottom of the stack given by `tso->stackobj->stack + tso->stackobj->stack_size`.


The stack consists of a sequence of *stack frames* (also sometimes called *activation records*) where each frame has the same layout as a heap object:

<table><tr><th> Header </th>
<th> Payload... 
</th></tr></table>


There are several kinds of [stack frames](commentary/rts/storage/stack#kinds-of-stack-frame), but the most common types are those pushed when evaluating a `case` expression:

```haskell
  case e0 of p1 -> e1; ...; pn -> en 
```


The code for evaluating a `case` pushes a new stack frame representing the alternatives of the case, and continues by evaluating `e0`.  When `e0` completes, it returns to the stack frame pushed earlier, which inspects the value and selects the appropriate branch of the case.  The stack frame for a `case` includes the values of all the free variables in the case alternatives.

## Info tables for stack frames


The info table for a stack frame has a couple of extra fields in addition to the [basic info table layout](commentary/rts/storage/heap-objects#info-tables).  A stack-frame info table is defined by `StgRetInfoTable` in [includes/rts/storage/InfoTables.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/InfoTables.h).

![](ret-itbl-no-rv.png)


The *SRT* field points to the static reference table (SRT) for this stack frame (see [Commentary/Rts/Storage/GC/CAFs](commentary/rts/storage/gc/CAFs) for details of SRTs).

## Layout of the payload


Unlike heap objects which mainly have "pointers first" layout, in a stack frame the pointers and non-pointers are intermingled.  This is so that we can support "stack stubbing" whereby a live variable stored on the stack can be later marked as dead simply by pushing a new stack frame that identifies that slot as containing a non-pointer, so the GC will not follow it.


Stack frames therefore have [bitmap layout](commentary/rts/storage/heap-objects#bitmap-layout).

## Kinds of Stack Frame


The constants for the different types of stack frame are defined in [includes/rts/storage/ClosureTypes.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/ClosureTypes.h).  More details about the layouts are available in [includes/rts/storage/Closures.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/rts/storage/Closures.h)

- `RET_BCO`
- `RET_SMALL`
- `RET_BIG`
- `RET_FUN` - (Explained a bit here: [Commentary/Compiler/CPS\#Notes](commentary/compiler/cps#notes))
- `UPDATE_FRAME`
- `CATCH_FRAME`
- `UNDERFLOW_FRAME` - The stack is chunked now. Connected as a linked list. (Since Dec 2010: [f30d527344db528618f64a25250a3be557d9f287](f30d527344db528618f64a25250a3be557d9f287),  [Blogpost](https://www.haskell.org/ghc/blog/20101215-stack-chunks.html))
- `STOP_FRAME`
- `ATOMICALLY_FRAME`
- `CATCH_RETRY_FRAME`
- `CATCH_STM_FRAME`
