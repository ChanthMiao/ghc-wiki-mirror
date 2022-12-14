# GHC Commentary: Garbage Collecting CAFs


Files: [rts/sm/GC.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/GC.c), function `scavenge_fun_srt`/`scavenge_thunk_srt` in [rts/sm/Scav.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Scav.c)


A **static closure** is a heap object that is allocated statically, once and for all, at compile time. It lives at a fixed address.


Example: `reverse_closure`, the static, top level closure object for the function `reverse`.  If you evaluate `map reverse xs`, then it's `reverse_closure` that is passed as the first argument to `map`.


Some static closures are thunks; we call these **CAFs**, or Constant Applicative Forms. Example

```haskell
squares :: [Int]
squares = map square [1..]
```

## Why CAFs are awkward


CAFs are awkward because:

- A thunk must be updated so that, after its first use, all subsequent uses get the benefit.
- The value to which the thunk is updated will be in the dynamic heap.  For example, when `map square [1..]` is evaluated, it'll allocate cons cells in the dynamic heap.
- So the CAF constitutes a "root", pointing into the dynamic heap, *which the garbage collector must know about*.

## What we do about it


We could just say that *all* CAFs are roots; but then people complain about space leaks, when a big CAF (i.e. one pointing to a large data structure) is retained even when it can no longer be referred to.  So GHC goes to strenuous efforts to track when a CAF "can no longer be referred to".


So here's what we do:

- A static closure is **CAFFY** if a CAF can be reached from it
- Every info-table points to a **Static Reference Table** (SRT) object, which is a static constructor that points to all the CAFFY closures that are directly mentioned by the code corresponding to this info table
- When following the reachable pointers in a closure, the garbage collector also follows the pointer to the SRT


For all the details on how SRTs work, see `Note [SRTs]` in [compiler/GHC/Cmm/Info/Build.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Cmm/Info/Build.hs).


We say that a static closure (whether a thunk or not) is CAFFY if

- It is a CAF, or
- One or more of its free variables is CAFFY


For example

```haskell
foo = 1 : []
bar = 2 : squares
wob = 3 : bar
```


Here `foo` is not CAFFY, but `bar` is CAFFY because it has `squares` (a CAF) as a free variable. And likewise `wob` is CAFFY because it has `bar` as a free variable.


Now, say that we have a nested let-binding

```haskell
f x = let g = \y -> x + y + head squares
      in ...
```


The heap-allocated closure for `g` has a pointer to its free variable `x`.  But its info table also has an SRT,
and that SRT points to `squares_closure`, because the latter is CAFFY.  So if the `g`-closure is alive,
that keeps `squares_closure` alive, and hence keeps alive the list that `squares` has evaluated to.


Some implementation details

- A CAFFY closure has a `CafInfo` of `MayHaveCafRefs`; a definitely non-CAFFY closure has a `CafInfo` of `NoCafRefs`.
- See Note \[CAF management\] in [rts/sm/Storage.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Storage.c) for more information.

## Static Reference Tables


For all the details on how SRTs work, see `Note [SRTs]` in [compiler/GHC/Cmm/Info/Build.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Cmm/Info/Build.hs).

## Evacuating Static Objects


Files: [rts/sm/GCThread.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/GCThread.h), [rts/sm/Evac.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/Evac.c), [rts/sm/GC.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/sm/GC.c)


While scavenging objects, we also process (aka "evacuate") any static objects that need to be kept alive.  When a GC thread discovers a live static object, it places it on its `static_objects`
list.  Later, this list is used to scavange the static objects, potentially finding more live objects.
Note that this process might find more static objects, and thus further extend the `static_objects` list.


When a static object is scavenged, it is removed from `static_objects` and placed on another list, called `scavenged_static_objects`.  Later, we use this list to "clean up" the liveness markers from these static objects, so that we can repeat the process on the next garbage collection.
Note that we can't "clean up" the liveness markers as we go along because we use them to notice
cycles among the static objects.
