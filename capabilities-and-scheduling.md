
Back to [GarbageCollectorNotes](garbage-collector-notes)

## The Scheduler


Most of the interesting things related to scheduling and multithreading in Haskell center around the function schedule() that is define in Schedule.c. This is the part of schedule that takes a thread from the run and decides what to do with it. 

```wiki
static Capability * schedule (Capability *initialCapability, Task *task)
```


In schedule() is a pretty classical scheduler loop. I have stripped away several parts of the code here to get down to the essentials.


```c
    t = popRunQueue(cap);
    prev_what_next = t->what_next;

    switch (prev_what_next) {
        
    case ThreadKilled:
    case ThreadComplete:
        /* Thread already finished, return to scheduler. */
        ret = ThreadFinished;
        break;
        
    case ThreadRunGHC:
    {
        StgRegTable *r;
        r = StgRun((StgFunPtr) stg_returnToStackTop, &cap->r);
        cap = regTableToCapability(r);
        ret = r->rRet;
        break;
    }
    
    case ThreadInterpret:
        cap = interpretBCO(cap);
        ret = cap->r.rRet;
        break;
        
    default:
        barf("schedule: invalid what_next field");
    }
```


The scheduler picks up a thread off the run queue and decides what to do with it. If it is runnable, then it calls the function StgRun() to run it. At the end of the code block, the variable “ret” is set to indicate why the thread stopped. 



Haskell threads are not time-sliced via a timer (potentially a timer interrupt) the way OS threads are \[cross check if there is some time sliced mechanism\]. Instead they are interrupted by certain commonly occurring events. Due to the lazy nature of Haskell, thunks need to be created and values need to be computed very often. Hence the execution of a thread entails lots of of memory allocation. One of the ways the execution of a thread is interrupted is when a thread has run out of space in its current block - it then returns control back to the scheduler. 

>
>
> I stand corrected about the above - *We do have a time-slice mechanism: the timer interrupt (see Timer.c) sets the context_switch flag, which causes the running thread to return to the scheduler the next time a heap check fails (at the end of the current nursery block). When a heap check fails, the thread doesn't necessarily always return to the scheduler: as long as the context_switch flag isn't set, and there is another block in the nursery, it resets Hp and HpLim to point to the new block, and continues.*
>
>


A GHC block is a 4k page that is page aligned for the OS VM system.  



Here is what the scheduler does with the "ret" - 


```c
    switch (ret) {
    case HeapOverflow:
        ready_to_gc = scheduleHandleHeapOverflow(cap,t);
        break;

    case StackOverflow:
        scheduleHandleStackOverflow(cap,task,t);
        break;

    case ThreadYielding:
        if (scheduleHandleYield(cap, t, prev_what_next)) {
            // shortcut for switching between compiler/interpreter:
            goto run_thread; 
        }
        break;

    case ThreadBlocked:
        scheduleHandleThreadBlocked(t);
        break;

    case ThreadFinished:
        if (scheduleHandleThreadFinished(cap, task, t)) return cap;
        ASSERT_FULL_CAPABILITY_INVARIANTS(cap,task);
        break;

    default:
      barf("schedule: invalid thread return code %d", (int)ret);
    }
```


The scheduleHandleHeapOverflow(cap,t) call decides to give the thread another block, (or a set of blocks if the thread was asking for allocation of a large object (a large object is one that is larger than a block). If the scheduleHandleHeapOverflow() function feels that there aren't enough free blocks left, it decides to Garbage Collect. This is the point at which everything else stops and the GC kicks in. 

## More about Capabilities


It is useful to understand capabilities well because it is closely tied to the maintenance of the program roots and multithreading in Haskell - all of which the GC has to be aware of. If however you are reading this the first time, you may want to skip this section and come back to it later. 


Capabilities are defined in capability.h. The file OSThreads.h provide an platform neutral abstraction for OS level threads used by Haskell. 

```c
struct Capability_ {
    // State required by the STG virtual machine when running Haskell
    // code.  During STG execution, the BaseReg register always points
    // to the StgRegTable of the current Capability (&cap->r).
    StgFunTable f;
    StgRegTable r;

    nat no;  // capability number.

    // The Task currently holding this Capability.  This task has
    // exclusive access to the contents of this Capability (apart from
    // returning_tasks_hd/returning_tasks_tl).
    // Locks required: cap->lock.
    Task *running_task;

    // true if this Capability is running Haskell code, used for
    // catching unsafe call-ins.
    rtsBool in_haskell;

    // The run queue.  The Task owning this Capability has exclusive
    // access to its run queue, so can wake up threads without
    // taking a lock, and the common path through the scheduler is
    // also lock-free.
    StgTSO *run_queue_hd;
    StgTSO *run_queue_tl;

    // Tasks currently making safe foreign calls.  Doubly-linked.
    // When returning, a task first acquires the Capability before
    // removing itself from this list, so that the GC can find all
    // the suspended TSOs easily.  Hence, when migrating a Task from
    // the returning_tasks list, we must also migrate its entry from
    // this list.
    Task *suspended_ccalling_tasks;

    // One mutable list per generation, so we don't need to take any
    // locks when updating an old-generation thunk.  These
    // mini-mut-lists are moved onto the respective gen->mut_list at
    // each GC.
    bdescr **mut_lists;

#if defined(THREADED_RTS)
    // Worker Tasks waiting in the wings.  Singly-linked.
    Task *spare_workers;

    // This lock protects running_task, returning_tasks_{hd,tl}, wakeup_queue.
    Mutex lock;

    // Tasks waiting to return from a foreign call, or waiting to make
    // a new call-in using this Capability (NULL if empty).
    // NB. this field needs to be modified by tasks other than the
    // running_task, so it requires cap->lock to modify.  A task can
    // check whether it is NULL without taking the lock, however.
    Task *returning_tasks_hd; // Singly-linked, with head/tail
    Task *returning_tasks_tl;

    // A list of threads to append to this Capability's run queue at
    // the earliest opportunity.  These are threads that have been
    // woken up by another Capability.
    StgTSO *wakeup_queue_hd;
    StgTSO *wakeup_queue_tl;
#endif

    // Per-capability STM-related data
    StgTVarWaitQueue *free_tvar_wait_queues;
    StgTRecChunk *free_trec_chunks;
    StgTRecHeader *free_trec_headers;
    nat transaction_tokens;
}; // typedef Capability, defined in RtsAPI.h
```

>
>
> Question - if a capability can consist of multiple OS threads then in THREADED_RTS, where is the list of current threads in execution? 
>
>


Here are some important observations about a capability: it consists of essentially a collection of OS threads, a register set and a set of TSOs. The register set is the member of type 'r'. Real hardware may or may not provide mappings of these to actual registers. \[Anything else to add here?\].

## TSO


TSO stands for Thread State Object and is the abstract for a haskell thread from the perspective of the RTS. TSO's are defined in TSO.h. 

```c
typedef struct StgTSO_ {
    StgHeader               header;

    struct StgTSO_*         link;       /* Links threads onto blocking queues */
    struct StgTSO_*         global_link;    /* Links all threads together */
    
    StgWord16               what_next;      /* Values defined in Constants.h */
    StgWord16               why_blocked;    /* Values defined in Constants.h */
    StgWord32               flags;
    StgTSOBlockInfo         block_info;
    struct StgTSO_*         blocked_exceptions;
    StgThreadID             id;
    int                     saved_errno;
    struct Task_*           bound;
    struct Capability_*     cap;
    struct StgTRecHeader_ * trec;       /* STM transaction record */

#ifdef TICKY_TICKY
    /* TICKY-specific stuff would go here. */
#endif
#ifdef PROFILING
    StgTSOProfInfo prof;
#endif
#ifdef PAR
    StgTSOParInfo par;
#endif
#ifdef GRAN
    StgTSOGranInfo gran;
#endif
#ifdef DIST
    StgTSODistInfo dist;
#endif

    /* The thread stack... */
    StgWord32	       stack_size;     /* stack size in *words* */
    StgWord32          max_stack_size; /* maximum stack size in *words* */
    StgPtr             sp;
    
    StgWord            stack[FLEXIBLE_ARRAY];
} StgTSO;
```


Probably the single most important part of a TSO from the perspective of the GC is the stack that it contains. This stack is essentially the 'roots of the program'.


A TSO is treated like any other object by the GC and it resides in the program heap. A TSO is always part of its mut list. A TSO is considered "clean" if it does not contain pointers to previous generations. This happens in the case where a TSO went through a GC and the objects it refers to got promoted. It then didn't allocate any memory since (maybe it didn't get scheduled) till the next GC. In that case it is considered "clean" - being clean is indicated by its dirty bit (tso-\>flags & TSO_DIRTY) not being set. 

## Terminology



This is a good point to introduce some terminology related to the above - 


- task - is essentially an OS thread executing a foreign function call. The haskell thread that needed to execute the FFI call is attached to this thread for the entire duration of the foreign call. \[is there something more that I can say here?\]

