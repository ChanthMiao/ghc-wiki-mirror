# Resource Limits


This page describes a proposed resource limits capabilities for GHC. The idea is to give users the ability to create and utilize resource containers inside programs, and then provide in-program access to heap census and other information. The semantics of resource containers are quite similar to cost centers used in profiling, except that they do not have "stack" semantics (more on this later). The end result is the ability to impose resource limits on space usage.

## Code generation changes


Resource limits is a new way (similar to profiled and dynamic). Here are the relevant changes:

### Dynamic closure allocation

[compiler/GHC/StgToCmm/Heap.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Heap.hs):allocDynClosureCmm (via StgCmmCon, also handles StgCmmBind:mkRhsClosure/cgRhsStdThunk. link_caf needs special treatment.)

```wiki
// profDynAlloc rep use_cc
// use_cc == CurCCS for the cases we're looking at
         I64[CCCS + 72] = I64[CCCS + 72] + %MO_UU_Conv_W64_W64(4 - 2);
// ALLOCATE THE OBJECT
// emitSetDynHdr base info_ptr use_cc
         I64[Hp - 24] = Data.Maybe.Just_con_info; // info_ptr
         I64[Hp - 16] = CCCS;                     // use_cc
         I64[Hp - 8] = (%MO_UU_Conv_W32_W64(I32[era]) << 30) | 0; // dynLdvInit
// let (cmm_args, offsets) = unzip amodes_w_offsets
// hpStore base cmm_args offsets
         I64[Hp + 0] = I64[R1 + 32];
```


Changes to:

```wiki
// invariant: Hp points to nursery of current resource container
         I64[Hp - 8] = Data.Maybe.Just_con_info; // info_ptr
         I64[Hp + 0] = I64[R1 + 32];
```


I.e. no change from un-profiled.

### CAF Allocation

[compiler/GHC/StgToCmm/Bind.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Bind.hs):thunkCode


Here is an interesting bugger:

```wiki
// ldvEnterClosure
     c17Q:
         if (%MO_UU_Conv_W32_W64(I32[era]) > 0) goto c17R;
         goto c17S;
     c17R:
         I64[R1 + 16] = I64[R1 + 16] & 1152921503533105152 | %MO_UU_Conv_W32_W64(I32[era]) | 1152921504606846976;
         goto c17S;
// entryHeapCheck
     c17S:
         if (Sp - 80 < SpLim) goto c17U;
         Hp = Hp + 64;
         if (Hp > HpLim) goto c17W;
// setupUpdate
//// linkCaf
         I64[I64[R1 + 8] + 72] = I64[I64[R1 + 8] + 72] + %MO_UU_Conv_W64_W64(4 - 2);
         I64[Hp - 56] = stg_CAF_BLACKHOLE_info;
         I64[Hp - 48] = I64[R1 + 8];
         I64[Hp - 40] = (%MO_UU_Conv_W32_W64(I32[era]) << 30) | 0;
         I64[Hp - 32] = CurrentTSO;
         (_c17X::I64,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 56, PtrHint));
         if (_c17X::I64 == 0) goto c17Y;
         goto c17Z;
     c17Z:
//// pushUpdateFrame
         I64[Sp - 32] = stg_bh_upd_frame_info;
         I64[Sp - 8] = Hp - 56;
         I64[Sp - 24] = CCCS;
// enterCostCentreThunk
         CCCS = I64[R1 + 8];
         I64[CCCS + 72] = I64[CCCS + 72] + %MO_UU_Conv_W64_W64(4 - 2);
// cgExpr body
         I64[Hp - 24] = GHC.Integer.Type.S#_con_info;
         I64[Hp - 16] = CCCS;
         I64[Hp - 8] = (%MO_UU_Conv_W32_W64(I32[era]) << 30) | 0;
         I64[Hp + 0] = 2;
         I64[Sp - 40] = CCCS;
         I64[Sp - 56] = Hp - 23;
         I64[Sp - 64] = stg_ap_p_info;
         I64[Sp - 72] = CCCS;
         I64[Sp - 80] = stg_restore_cccs_info;
         R2 = Foreign.C.Types.$fNumCInt_closure;
         I64[Sp - 48] = s15P_info;
         Sp = Sp - 80;
         jump GHC.Num.fromInteger_info; // [R2]
     c17U: jump stg_gc_enter_1; // [R1]
     c17W:
         HpAlloc = 64;
         goto c17U;
     c17Y: jump I64[R1]; // [R1]
```


Notice the heap check serves for the later branch too. On the other hand, the CCCS coincides with the later change. This seems to be the general pattern. So we might be able to handle this CAF by special-casing CAFs.

```wiki
         _crc = Bdescr(Hp)->rc;
         CHANGE_NURSERY(I64[R1 + 8]);
// entryHeapCheck
     c17S:
         if (Sp - 80 < SpLim) goto c17U;
         Hp = Hp + 32;
         if (Hp > HpLim) goto c17W;
// setupUpdate
//// linkCaf
         I64[Hp - 24] = stg_CAF_BLACKHOLE_info;
         I64[Hp - 16] = CurrentTSO;
         (_c17X::I64,) = foreign "ccall"
           newCAF((BaseReg, PtrHint), (R1, PtrHint), (Hp - 56, PtrHint));
         if (_c17X::I64 == 0) goto c17Y;
         goto c17Z;
     c17Z:
//// pushUpdateFrame
         I64[Sp - 32] = stg_bh_upd_frame_info;
         I64[Sp - 8] = Hp - 56;
         I64[Sp - 24] = _crc; // ***
// cgExpr body
         I64[Hp - 8] = GHC.Integer.Type.S#_con_info;
         I64[Hp + 0] = 2;
         I64[Sp - 56] = Hp - 23;
         I64[Sp - 64] = stg_ap_p_info;
         I64[Sp - 72] = Bdescr(Hp)->rc; // ***
         I64[Sp - 80] = stg_restore_crc_info; // ***
         R2 = Foreign.C.Types.$fNumCInt_closure;
         I64[Sp - 48] = s15P_info;
         Sp = Sp - 80;
         jump GHC.Num.fromInteger_info; // [R2]
     c17U: jump stg_gc_enter_1; // [R1]
     c17W:
         HpAlloc = 32;
         goto c17U;
     c17Y: jump I64[R1]; // [R1]
```


We also hit the slow function application path.

### Thunk code

[compiler/GHC/StgToCmm/Bind.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Bind.hs):thunkCode

```wiki
// ldvEnterClosure cl_info
     clc:
         if (%MO_UU_Conv_W32_W64(I32[era]) > 0) goto cld;
         goto cle;
     cld:
         I64[R1 + 16] = I64[R1 + 16] & 1152921503533105152 | %MO_UU_Conv_W32_W64(I32[era]) | 1152921504606846976;
         goto cle;
     cle:
// entryHeapCheck cl_info node' arity []
         if (Sp - 32 < SpLim) goto clg;
         Hp = Hp + 40;
         if (Hp > HpLim) goto cli;
// when (blackHoleOnEntry cl_info && node_points)
//    (blackHoleIt node)
         // empty
// setupUpdate cl_info node
         I64[Sp - 32] = stg_upd_frame_info;
         I64[Sp - 8] = R1;
         I64[Sp - 24] = CCCS;
// enterCostCentreThunk (CmmReg nodeReg)
         CCCS = I64[R1 + 8];
// let lf_info = closureLFInfo cl_info
// fv_bindings <- mapM bind_fv fv_details
// load_fvs node lf_info fv_bindings
         // empty
// cgExpr body
         ...
// due to 'entryHeapCheck cl_info node' arity []'
     clg: jump stg_gc_enter_1; // [R1]
     cli:
         HpAlloc = 40;
         goto clg;
```


Changes to:

```wiki
         if (Sp - 32 < SpLim) goto clg;
         _crc = Bdescr(Hp);
         // XXX ugh this is really expensive
         // XXX is it worth avoiding the rigamorole when it's the same? maybe.
         // closeNursery
         I64[CurrentNursery + 8] = Hp + 8;
         // change the nursery
         CurrentNursery = Bdescr(R1)->rc[CurrentCap]
         // openNursery
         Hp = I64[CurrentNursery + 8] - 8;
         HpLim = I64[CurrentNursery] + (%MO_SS_Conv_W32_W64(I32[CurrentNursery + 48]) * 4096 - 1);
         // XXX end expensive bit
         Hp = Hp + 40; // or whatever this adjusts to
         if (Hp > HpLim) goto cli;
         // (blackhole)
         I64[Sp - 32] = stg_upd_frame_info;
         I64[Sp - 8] = R1;
         // XXX or maybe omit this
         I64[Sp - 24] = _crc; // so we can restore it on return, NOT the same as current nursery
         // (load free variables)
         ...
     clg: jump stg_gc_enter_1; // [R1]
     cli:
         HpAlloc = 40;
         goto clg;
```

### Foreign calls

```wiki
// savethreadState
//   mkStore (cmmOffset dflags (CmmLoad (cmmOffset dflags stgCurrentTSO (tso_stackobj dflags)) (bWord dflags)) (stack_SP dflags)) stgSp
         I64[I64[CurrentTSO + 40] + 32] = Sp;
//   closeNursery dflags
         I64[CurrentNursery + 8] = Hp + 8;
//   mkStore (cmmOffset dflags stgCurrentTSO (tso_CCCS dflags)) curCCS
         I64[CurrentTSO + 120] = CCCS;
// caller_save
         // (empty)
// mkMiddle (callSuspendThread dflags id intrbl)
         (_c17B::I64, PtrHint) = foreign "ccall"
           suspendThread((BaseReg, PtrHint), (0,));
// mkUnsafeCall tgt res args
         (_c17l::I64, `signed') = foreign "ccall"
           cos((_c17e::I64, `signed'));
// mkMiddle (callResumeThread new_base id)
         (_c17D::I64, PtrHint) = foreign "ccall"
           resumeThread((_c17B::I64, PtrHint));
// mkAssign (CmmGlobal BaseReg) (CmmReg (CmmLocal new_base))
         BaseReg = _c17D::I64;
// caller_load
         // empty
// loadThreadState
//   tso = CurrentTSO
         _c17F::I64 = CurrentTSO;
//   stack = tso->stackobj
         _c17H::I64 = I64[_c17F::I64 + 40];
//   Sp = stack->sp
         Sp = I64[_c17H::I64 + 32];
//   SpLim = stack->stack + RESERVED_STACK_WORDS
         SpLim = _c17H::I64 + 208;
//   HpAlloc = 0
         HpAlloc = 0;
//   openNursery dflags
         Hp = I64[CurrentNursery + 8] - 8;
         HpLim = I64[CurrentNursery] + (%MO_SS_Conv_W32_W64(I32[CurrentNursery + 48]) * 4096 - 1);
//   storeCurCCSm (CmmLoad (cmmOffset dflags (CmmReg (CmmLocal tso)) (tso_CCCS dflags)) (ccsType dflags))
         CCCS = I64[_c17F::I64 + 120];
```


Changes to:

```wiki
         I64[I64[CurrentTSO + 40] + 32] = Sp;
         I64[CurrentNursery + 8] = Hp + 8;
         // assume CurrentResourceContainer and nursery are consistent
         (_c17B::I64, PtrHint) = foreign "ccall" suspendThread((BaseReg, PtrHint), (0,));
         (_c17l::I64, `signed') = foreign "ccall" cos((_c17e::I64, `signed'));
         (_c17D::I64, PtrHint) = foreign "ccall" resumeThread((_c17B::I64, PtrHint));
         BaseReg = _c17D::I64;
         _c17F::I64 = CurrentTSO;
         _c17H::I64 = I64[_c17F::I64 + 40];
         Sp = I64[_c17H::I64 + 32];
         SpLim = _c17H::I64 + 208;
         HpAlloc = 0;
         Hp = I64[CurrentNursery + 8] - 8;
         HpLim = I64[CurrentNursery] + (%MO_SS_Conv_W32_W64(I32[CurrentNursery + 48]) * 4096 - 1);
         // (once again, using implicit Hp)
         R1 = _c17l::I64;
```


No change from unprofiled

## Case split


Do a nursery swap.

**Warning:** The rest of this document describes an old iteration of the system, which directly used 

## Front-end changes


The basic idea behind this patch is that data collected during **profiling** can also be used at runtime to enforce limits. So most of the API involves (1) dynamically setting cost-centres, which GHC uses to do profiling, and (2) querying and receiving callbacks when certain events happen during profiling.  Costs can be collected anywhere you could have placed an `SCC` annotation statically.

```wiki
-- | A cost-centre; not garbage-collected.
data CostCentre

-- | A cost-centre stack.  Cost-centres are composed into cost-centre
-- stacks, for which costs are actually attributed.  Cost-centre stacks
-- are not garbage-collected.
data CostCentreStack

-- | A listener on a cost-centre stack.  Active listeners are considered
-- roots, so be sure to unlisten when you are done.
data Listener

-- | Type of profiling information to track.  We currently support two
-- types: instantaneous heap residence, and overall memory allocation
-- (which is monotonically increasing).
data ProfType = Resident | Allocated

-- | Allocates a new cost-centre.
newCC :: IO CostCentre

-- | Pushes a cost-centre onto a new cost-centre stack.  This function
-- is memoized, so if you push the same CostCentre onto the same CostCentreStack, you will
-- get the same CostCentreStack back.
pushCC :: CostCentreStack -> CostCentre -> IO CostCentreStack

-- | Attaches a listener to a cost-centre.  The resolution of the
-- listener depends on the type and runtime options.  For resident
-- memory listeners, listeners are checked whenever a heap census is run
-- (which is controllable using @-i@).  For allocated memory listeners,
-- listeners are checked every GC.  When you are no longer interested
-- in events from a listener, make sure you unregister the listener, or
-- you will leak memory.
listenCCS :: CostCentreStack -> ProfType -> Int -> IO () -> IO Listener

-- | Unregisters a listener, so that it action will no longer be run.
unlistenCCS :: Listener -> IO ()

-- | Sets the cost-centre of a object on the heap.
setCCSOf :: CostCentreStack -> a -> IO ()

-- | Runs an IO action with the CostCentreCS set to @ccs@.
withCCS :: CostCentreStack -> IO a -> IO a

-- | Allocates a new dynamic cost-centre stack; generally, if you want
-- something to check usage, this is what you want.
newCCS :: IO CostCentreStack

-- | Queries for memory statistics about a cost-centre stack.
queryCCS :: CostCentreStack -> ProfType -> IO Int

-- | Root cost-center stack for dynamically allocated cost center
-- stacks.
ccsDynamic :: CostCentreStack
```


The general usage of this API goes like:

```wiki
f n =
    let xs = [1..n::Integer]
    in  sum xs * product xs

newCCS :: IO CCS
newCCS = pushCC ccsDynamic =<< newCC

main = do
  m <- newEmptyMVar
  forkIO $ do
    x <- newCCS
    tid <- myThreadId
    l <- listenCCS x 2000 (putStrLn "Too much memory is being used" >> killThread tid)
    withCCS x $ do
      evaluate (f 20000)
    unlistenCostCentreStack l
    putMVar m ()
  takeMVar m
```


Another use-case is more fine-grained SCCs based on runtime properties, not source-level features.


I am planning on providing semantics, based on GHC???s current profiling semantics. Notice that this current story says nothing about RETAINERS (so there is some careful library writing to be done, to prevent untrusted code from holding onto large system structures for too long).


Some points to bikeshed:

- Naming: CostCentreStack/CostCentre or CCS/CC? Old API used CCS/CC

- Instead of the current interface, we could publish STM variables which are updated by the GC; listening is then just an ordinary STM transaction. This might be tricky to implement.

## Runtime changes

- `Listener` is a new garbage collected object; we expect it can be implemented as a simple `PRIM` using techniques similar to `StgMVarTSOQueue`.
- Checks for listeners occur during heap census; you'll need to pass the `-hc` flag for this machinery to do anything. Actually, we added a new flag `-hl` which is `-hc` but without writing an `.hp` file. See also #7751 which will dramatically improve performance. Right now, running heap census is very slow; if we can make censuses incremental their cost can be amortized with ordinary garbage collector behavior.

## Commentary

### CAFs


A CAF is never attributed to a dynamic CCs, because CAFs are always get their own cost-centre for their evaluation.  This is correct in a sense, because multiple dynamic CCs can attempt to evaluate a CAF, and we should not unduly penalize the first one to evaluate the CAF.  However, this means you have to be very careful about optimizations that introduce CAFs that were not present at the source level, e.g. `-ffull-laziness`. Dynamically loaded CAFs from untrusted code, of course, need to be relabeled appropriately.

### Interaction with traditional profiling


Resource limits must be compiled with `-prof`; we???d like to treat profiling as semantics preserving but resource limits are anything but.  In the long term, it is probably desirable to consider these distinctive systems which happen to share a common mechanism. As a first draft, we don???t intend on supporting profiling and resource limits simultaneously; the dynamic SCC machinery can be used for enhanced profiling or for marking resource limits, but not both. It may be possible to extend the resource limit machinery to handle ???superfluous??? cost-centres, but this would be more complicated and costly, since a costs will now be spattered over many `CostCentre` objects and need to be recombined. Currently, the profiling machinery can perform this calculation, but only calculates inherited resource usage at the very end, so this could be expensive.


Since non-dynamic SCCs can interfere with accurate cost attribution, we add a new flag `-fprof-drop` which drops all `SCC` pragmas.

### Memory leaks


One of the most important intended use-cases of resource limits is when you are rapidly loading and unloading large amounts of untrusted code (think [http://tryhaskell.org/](http://tryhaskell.org/)). So an important thing to get right is avoiding long term memory leakage, either from leftover objects from the untrusted code or related infrastructure.


On the unloading code front, one technique that could be employed is to replace all third-party closures with ???error??? frames upon unloading. Similar techniques are already being employed in GHC, and it is semantically sound even if another thread has already witnessed the full value of the data structure: one can imagine some supervisor process sending an asynchronous exception when some unloaded data is accessed. (XXX this may have bad interactions with mask and uninterruptibleMask).


On the cost centre front, the runtime currently assumes that cost centres are permanent and never deallocated. One technique for deallocating a cost-centre goes as follows. We first allocate a distinguished ???catch-all??? cost-centre which tracks all deallocated cost centres. When we would like to deallocate a cost centre, we mark the cost centre as killed, and upon the next major garbage collection, we look at the cost-centres pointed to by all of the heap objects and rewrite them if they correspond to a killed cost-centre.  We also donate all of the cost-centre???s statistics to the catch-all.  It is also necessary to rewrite any in-Haskell references to the cost-centre (so we need a new infotable to mark these references.)  Once this is done, we???ve removed all references to the cost-centre and it can be dropped.  (This is not quite true; CC_LIST and any cost-centre stacks also have to be updated.)

### Callback triple fault


Finalizer could trigger a new finalizer, ad infinitum. However, if you don't allocate a new finalizer in the callback, you should be fine.

### Discussion


These mailing list threads may be of interest:

- [http://www.haskell.org/pipermail/ghc-devs/2013-March/000680.html](http://www.haskell.org/pipermail/ghc-devs/2013-March/000680.html) (first announce of this)
- [http://www.haskell.org/pipermail/glasgow-haskell-users/2012-April/022258.html](http://www.haskell.org/pipermail/glasgow-haskell-users/2012-April/022258.html) (earlier discussion about space resource limits)
