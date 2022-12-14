# I know kung fu: learning STG by example

The STG machine is an essential part of GHC, the world's leading
Haskell compiler. It defines how the Haskell evaluation model
should be efficiently implemented on standard hardware. Despite
this key role, it is generally poorly understood amongst GHC users.
This document aims to provide an overview of the STG machine in
its modern, eval/apply-based, pointer-tagged incarnation by a
series of simple examples showing how Haskell source code is
compiled.


## What is STG, exactly?

Haskell code being sucked through GHC has a complex lifecycle.
Broadly speaking, it transitions between five representations:

```wiki

                                                                           +---------+
                                                         LLVM backend /--->| LLVM IR |--\
                                                                      |    +---------+  | LLVM
                                                                      |                 v
 +------------+ Desugar  +------+ STGify  +-----+ CodeGen  +-----+    |  NCG    +----------+
 | Parse tree |--------->| Core |-------->| STG |--------->| C-- |----+-------->| Assembly |
 +------------+          +------+         +-----+          +-----+    |         +----------+
                                                                      |            ^
                                                                      |     +---+  | GCC
                                                            C backend \---->| C |--/
                                                                            +---+

```

The path from C-- to assembly varies: the three possible backends
are C (`-fvia-c` (**[OUTDATED](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/codegens.html#c-code-generator-fvia-c)**)), LLVM (`-fllvm`), and the default backend -- the
native code genarator (or NCG), which generates assembly directly
from the GHC-internal C-- data type.

STG is a simple functional language, rather like the more famous
Core language. It differs in the following main respects:

1. In its current incarnation, it isn't typed in the Haskell sense,
   though it does know about *representation* types
1. It is in A-normal form (ANF), which is where every
   subexpression is given a name
1. Every $`\lambda`$, constructor application, and primitive operator
   is $`\eta`$-expanded
1. It is annotated with a ton of information that the code
   generator is interested in knowing

STG expressions can be one of the following:

1. Atoms (i.e. literals and variables)
1. `let`-bindings (both recursive and non-recursive) over another
   expression, where let-bound things are one of:
   - A function value with explicit lambdas
   - An unsaturated application
   - A constructor applied to atoms
   - A thunk (i.e. any expression not fitting into one of the above
     categories)
1. Saturated primitive application of a primitive to variables
1. Application of a variable to one or more atoms
1. Case deconstruction of an expression, where each branch may also
   be an expression

The job of the *STG machine* is to evaluate these expressions in
a way which is efficiently implementable on standard hardware. This
document will look at how exactly this is achieved by looking at
real examples of the C-- code GHC generates for various Haskell
expressions.

This document will take a very low-level view of the machine, so if
you want to get comfortable with how the STG machine executes at a
more abstract level before reading this document, you might want to
read the paper
[How to make a fast curry: push/enter vs. eval/apply](http://research.microsoft.com/en-us/um/people/simonpj/papers/eval-apply/).
It presents the STG machine without reference to an explicit stack
or registers, but instead as a transition system. This transition
system has also been implemented as a Haskell program called
[ministg](http://hackage.haskell.org/package/ministg) by
[Bernie Pope](https://github.com/bjpop/ministg), for those who wish to
see it in action on some simple examples.


## An overview of the STG machine

Before we dive in, a note: this document will describe the STG
machine as it is implemented on x86-style architectures. I will use
the terms "the STG machine" and "the STG machine as implemented on
x86 by GHC" interchangeably. The implementation is somewhat
different on x64, not least due to the greater number of available
registers.

This overview section is rather bare. Readers might be able to fill
in any gaps in my explanation by using some of the following
sources:

- [The Haskell Execution Model](commentary/rts/haskell-execution)
- [Storage](commentary/rts/storage)
- [The Spineless Tagless G-machine](https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf)
  - now sadly rather out of date
- [Faster laziness through dynamic pointer tagging](http://research.microsoft.com/en-us/um/people/simonpj/papers/ptr-tag/ptr-tagging.pdf)


### Components of the machine

In its bare essentials, the STG machine consists of three parts:

1. The STG registers:

   - There are rather a lot of registers here: more than can be
     practicably stored in actual available processor registers on most
     architectures.
   - To deal with the lack of processor registers, most of the STG 
     registers are actually kept on the stack in a block of memory
     pointed to by a special STG register called the "base register" (or 
     `BaseReg`). To get or set values of registers which are not kept in
     processor registers, the STG machine generates an instruction to
     load or store from an address relative to the `BaseReg`.
   - The most important four registers are the `BaseReg`, the stack
     pointer (`Sp`), the heap pointer (`Hp`), and the general purpose
     register `R1` which is used for intermediate values, as well as for 
     returning evaluated values when unwinding the stack. These are the 
     four registers which are assigned actual processor registers when
     implementing the STG machine on x86.

1. The STG stack:

   - Stores function arguments and continuations (i.e. the stack
     frames which are executed when a function returns)
   - Grows downwards in memory
   - The top of the stack is pointed to by the STG register `Sp`, and 
     the maximum available stack pointer is stored in `SpLim`. There is
     no frame pointer.

1. The heap:

   - Used to store many different sorts of heap object: notably
     functions, thunks and data constructors
   - Grows upwards in memory, towards the stack
   - All allocation occurs using a bump-allocator: the heap pointer is
     simply incremented by the number of bytes desired (subject to to a
     check that this does not exhaust available memory). The garbage
     collector is responsible for moving objects out of the area of the 
     heap managed by the bump allocator and into the care of its 
     generational collector.
   - The last address in the bump-allocated part of the heap that has 
     been used is pointed to by the STG register `Hp`, with `HpLim`
     holding the maximum address available for bump-allocation.


### Important concepts in the machine

Some of the key concepts in the STG machine include *closures*,
*info tables* and *entry code*. We tackle them in reverse
order:

* **Entry code**

  The actual machine code that the STG machine will execute upon "entry". Entry means different things for different heap objects.

  - For *thunks*, entry is when the thunk is forced by some demand for its value, such as a `case` expression scrutinising it
  - For *functions*, entry is when the function is applied to as many arguments as are demanded by the arity recorded in its info table
  - For *continuations*, entry occurs when a value is returned from a nested call, and hence the need arises to consume the value and
  continue evaluation

* **Info table**

  A block of memory allocated statically, which contains metadata about a closure. The most important fields for our purposes are the
entry code pointer and the arity information (if this is the info table for a thunk, function or partial application)

* **Closure**

  Essentially a heap-allocated pair of the free variables of some code, and a pointer to its info table (i.e. its info pointer).

For an example of how these parts work together, consider the
following code

```
my_fun x zs = map (\y -> y + x) zs
```

The nested lambda will give rise to all of the above objects.

The closure will store a pointer to `x`'s closure (as it is a free
variable of the lambda), along with a pointer to an info table.
That info table will contain information relevant to a function
value, recording information such as the fact that it has an arity
of 1 (i.e. the binding for `y`), and the pointer to the entry code
for the function `\y -> y + x` itself. This entry code will
implement the addition by combining the closure for the free
variable `x` (taken from the closure) with the stack-passed `y`
variable's closure.

Upon entry to some code, pointers to closures are made available in
`R1`. That is to say, before entry code is jumped to, `R1` is set
up to point to the associated closure, so that the entry code can
access free variables (if any).

Closures for code which contain no free variables (such as the
closure for `True` and `False`, and functions applied to no
arguments such as `(:)` and `id`) are allocated statically by the
compiler in the same manner as info tables are.


### Overview of execution model of the machine

This will be covered in more detail in the examples below, so I
will use this section to make some general points.

The goal of the STG machine is to reduce the current expression to
a value. When it has done so, it:

1. Stores a tagged pointer to evaluated closure in the STG register
  `R1`
1. Jumps to the entry code of the info table pointed to by the
   value at the top of the STG stack
   - This may also be called the info table of the *continuation* of
     the expression

The continuation code is responsible for popping its info pointer
(and stack-allocated free variables, if any) from the stack before
returning.

Arguments are passed on the stack, and are popped by the callee.
Upon a jump to the entry code for a function, there are always
precisely as many arguments on the stack as the (statically known)
arity of that function, and those arguments will be followed by the
info pointer of a continuation.


## Saturated application to known functions

Handling application in the STG machine is a big topic, and so in
this first section we only look at the case of *saturated*
applications to *known* functions - i.e. those functions that the
compiler statically knows information such as the entry code
pointer and arity for.


### Example 1: function application with sufficient stack space

Application of functions is the bread and butter of the STG
machine. Correspondingly, this first Haskell program

```haskell
{-# NOINLINE known_fun #-}
known_fun :: a -> a
known_fun x = x

known_app :: () -> Int
known_app _ = known_fun 10
```

compiles to very simple C-- code

```
Main_knownzuapp_entry() {
    cl3:
        I32[Sp + 0] = stg_INTLIKE_closure+209;
        jump Main_knownzufun_entry ();
}
```

```asm
_Main_knownzuapp_entry:
Lcl3:
    movl L_stg_INTLIKE_closure$non_lazy_ptr,%eax
    addl $209,%eax
    movl %eax,(%ebp)
    jmp _Main_knownzufun_entry
```

The STG machine passes arguments to functions on the STG stack, and
a pointer to the stack top is stored in the STG register `Sp`.
Furthermore, because GHC currently uses the eval/apply variant of
the STG machine, exactly as many arguments as the function expects
to receive are guaranteed to present on the stack.

Therefore, upon entry to the `known_app` function, we are
guaranteed that the STG stack has a pointer to a closure of type
`()` on top of it. In order to call `known_fun`, we just modify the
top of the stack to replace that pointer with a pointer to the
statically allocated closure for the literal `10`, and then
tail-call into the entry code of `known_fun`.


### Example 2: function application that needs to grow the stack

This Haskell code is apparently little more complicated than the
previous example

```haskell
{-# NOINLINE known_fun_2 #-}
known_fun_2 :: a -> a -> a
known_fun_2 x _ = x

known_app_2 :: () -> Int
known_app_2 _ = known_fun_2 10 10
```

however, it generates radically different C-- code:

```
Main_knownzuappzu2_entry() {
    clE:
        if (Sp - 4 < SpLim) goto clH;
        I32[Sp + 0] = stg_INTLIKE_closure+209;
        I32[Sp - 4] = stg_INTLIKE_closure+209;
        Sp = Sp - 4;
        jump Main_knownzufunzu2_entry ();
    clH:
        R1 = Main_knownzuappzu2_closure;
        jump stg_gc_fun ();
}
```

```asm
_Main_knownzuappzu2_entry:
LclE:
    leal -4(%ebp),%eax
    cmpl 84(%ebx),%eax
    jb LclH
    movl L_stg_INTLIKE_closure$non_lazy_ptr,%eax
    addl $209,%eax
    movl %eax,(%ebp)
    movl L_stg_INTLIKE_closure$non_lazy_ptr,%eax
    addl $209,%eax
    movl %eax,-4(%ebp)
    addl $-4,%ebp
    jmp _Main_knownzufunzu2_entry
LclH:
    movl $_Main_knownzuappzu2_closure,%esi
    jmp *-4(%ebx)
```

As before, upon entry the STG stack is guaranteed to have a single
closure pointer at its top. However, in order to call into
known_fun_2 we need at least two free stack slots at the top for
arguments, which means that we have to grow the stack by one word
before we can make the call.


#### Checking for sufficient stack space

First, we check to see if growing the stack would overflow
allocated stack space, by comparing the STG stack pointer register
`Sp` with the stack limit register `SpLim`:

```
    if (Sp - 4 < SpLim) goto clH;
```

(The stack grows downwards, hence the *subtraction* of 4 from the
current `Sp`). If the stack check fails, we branch to `clH`:

```
clH:
    R1 = Main_knownzuappzu2_closure;
    jump stg_gc_fun ();
```

This stores the closure of the current function in `R1`, and then
jumps into the hand-written garbage collector code to force it to
grow the stack. After the stack has been grown, the collector will
call back into `Main_knownzuappzu2_entry` by using the information
stored in the (statically-allocated) `Main_knownzuappzu2_closure`
closure pointed to by `R1`, and the stack check will be run again -
hopefully succeeding this time!


#### Making the known call

Given that the stack check succeeds, it is easy to make the actual
call we are after. We simply grow the stack by the required amount,
and write the two arguments to `known_fun_2` into the top two stack
slots (overwriting our own first argument in the process, of
course):

```
    I32[Sp + 0] = stg_INTLIKE_closure+209;
    I32[Sp - 4] = stg_INTLIKE_closure+209;
    Sp = Sp - 4;
```

A simple tail call to the new function finishes us off:

```
    jump Main_knownzufunzu2_entry ();
```

## Example 3: Unsaturated applications to known functions

Despite describing an undersaturated call, this Haskell code

```haskell
{-# NOINLINE known_fun_2 #-}
known_fun_2 :: a -> a -> a
known_fun_2 x _ = x

known_undersaturated_app :: () -> Int -> Int
known_undersaturated_app _ = known_fun_2 10
```

compiles to straightforward C-- as follows

```
Main_knownzuundersaturatedzuapp_entry() {
    cmd:
        I32[Sp + 0] = stg_INTLIKE_closure+209;
        jump Main_knownzufunzu2_entry ();
}
```

```
_Main_knownzuundersaturatedzuapp_entry:
Lcmd:
    movl L_stg_INTLIKE_closure$non_lazy_ptr,%eax
    addl $209,%eax
    movl %eax,(%ebp)
    jmp _Main_knownzufunzu2_entry
```

The reason that there is no special magic to deal with
undersaturated applications to known functions is simple: GHC
simply gives `known_undersaturated_app` an arity of 2, so by the
time we jump to the entry code the stack must already contain any
arguments required by `known_fun_2`.


## Example 4: Applications to unknown functions

We aren't going to tackle oversaturated calls to known functions
until we've considered happens to calls to statically-unknown
functions. To see what these look like, we are going to use the
following Haskell code

```haskell
unknown_app :: (Int -> Int) -> Int -> Int
unknown_app f x = f x
```

Which compiles to this C-- function

```
Main_unknownzuapp_entry() {
    cnO:
        R1 = I32[Sp + 0];
        Sp = Sp + 4;
        jump stg_ap_p_fast ();
}
```

```
_Main_unknownzuapp_entry:
Lcn0:
    movl (%ebp),%esi
    addl $4,%ebp
    jmp _stg_ap_p_fast
```

Unlike the previous cases we have looked at, we are compiling an
application where we don't statically know either the arity or the
info pointer of the function being applied. To deal with such
cases, the STG machine uses several pre-compiled "generic apply"
functions which inspect the info-table for the function in question
and decide how the available arguments should be applied to it.


### Dealing with generic application

There are three cases the generic apply functions have to deal
with:

1. The function's arity (recorded in the function closure's info
   table) exactly matches the number of arguments available on the
   stack

   - This is the best case. In this case, the generic apply function
     simply makes a tail call into the function's entry code

1. The function's arity is greater than the number of arguments
   available on the stack

   - In this case, the generic apply code allocates a PAP (partial
     application) closure which closes over both the new arguments and
     the function pointer, and returns that value, in the normal STGish
     way, to the continuation on the top of the stack

1. The function's arity is less than the number of arguments
   available on the stack

   - In this case, a number of arguments matching the arity are pushed
     on top of the stack, followed by a continuation which uses another
     of the generic apply functions to apply the remaining arguments.
     The code for the original function is then entered
   - Eventually the code for the continuation is entered and another
     generic apply function will be tail-called to deal with the
     result

Potentially, one generic apply function is required for every
"argument pattern". Some example argument patterns are:

Because the number of patterns is large (actually unbounded,
because functions might be of any arity), GHC only generates
generic apply functions for enough patterns so that 99.9% of all
calls observed in practice have a generic apply function. Generic
apply functions for calls of larger arity can be simulated by
chaining together several smaller generic apply functions, in a
similar manner as when dealing with oversaturated function
applications.


### Making the call to the generic application code

Let's remind ourselves of the original code:

```
    R1 = I32[Sp + 0];
    Sp = Sp + 4;
    jump stg_ap_p_fast ();
```

Knowing about generic apply functions, the call itself is easy to
understand. We pop the top of the stack (the function argument)
into `R1` and then jump into the generic application code for the
case where the stack contains a single pointer argument, which
deals with all the cases for `f` described above.


## Example 5: oversaturated applications to known functions

This Haskell code

```haskell
{-# NOINLINE known_fun_2 #-}
known_fun_2 :: a -> a -> a
known_fun_2 x _ = x

known_oversat_app :: () -> Int
known_oversat_app _ = known_fun_2 id id 10
```

compiles to the following C-- function

```
Main_knownzuoversatzuapp_entry() {
    cmj:
        if (Sp - 12 < SpLim) goto cmm;
        I32[Sp + 0] = stg_INTLIKE_closure+209;
        I32[Sp - 4] = stg_ap_p_info;
        I32[Sp - 8] = base_GHCziBase_id_closure;
        I32[Sp - 12] = base_GHCziBase_id_closure;
        Sp = Sp - 12;
        jump Main_knownzufunzu2_entry ();
    cmm:
        R1 = Main_knownzuoversatzuapp_closure;
        jump stg_gc_fun ();
}
```

```
_Main_knownzuoversatzuapp_entry:
Lcmj:
    leal -12(%ebp),%eax
    cmpl 84(%ebx),%eax
    jb Lcmm
    movl L_stg_INTLIKE_closure$non_lazy_ptr,%eax
    addl $209,%eax
    movl %eax,(%ebp)
    movl L_stg_ap_p_info$non_lazy_ptr,%eax
    movl %eax,-4(%ebp)
    movl $_base_GHCziBase_id_closure,-8(%ebp)
    movl $_base_GHCziBase_id_closure,-12(%ebp)
    addl $-12,%ebp
    jmp _Main_knownzufunzu2_entry
Lcmm:
    movl $_Main_knownzuoversatzuapp_closure,%esi
    jmp *-4(%ebx)
```

As you might see, despite being a call to a known function, this
code makes use of the generic apply functions we discussed in the
last section. Let's pick the function apart and see how it works.

First, we do the usual stack check. What differs from the last time
we saw this check is that we are not only allocating space for
arguments on the stack, but also for a *continuation*. We set up
these new stack entries as follows:

```
    I32[Sp + 0] = stg_INTLIKE_closure+209;
    I32[Sp - 4] = stg_ap_p_info;
    I32[Sp - 8] = base_GHCziBase_id_closure;
    I32[Sp - 12] = base_GHCziBase_id_closure;
    Sp = Sp - 12;
```

i.e. the final stack looks as follows (note that the code
overwrites the old pointer to a closure of type ()):

```wiki

 /----\    +---------------------------+
 | Sp |--->| base_GHCziBase_id_closure |
 \----/    +---------------------------+
           | base_GHCziBase_id_closure |
           +---------------------------+
           |       stg_ap_p_info       |
           +---------------------------+
           |  stg_INTLIKE_closure+209  |
           +---------------------------+
           |           ...             |

```

Because `known_fun_2` is of arity 2, when we jump to its entry
code, it will only consume the top two arguments from the stack:
i.e. the two pointers to `base_GHCziBase_id_closure`. It will then
evaluate to some sort of value and transfer control to the entry
code for `stg_ap_p_info`.

This is where the magic happens: the entry code for `stg_ap_p_info`
will apply the function value that was returned from `known_fun_2`
to the (pointer) argument in the "free variable" of its (stack
allocated) closure -- and we have arranged that that is
`stg_INTLIKE_closure+209`, i.e. the closure for the `Int` literal
`10`. This code is shared with the generic application functions
for calls to unknown functions, so this will make use of the
`stg_ap_p_fast` function we saw before.

Finally, control will be transferred back to the caller for
`known_oversat_app`, and all will be well.


## Example 6: allocation of thunks and data

Something that happens all the time in Haskell is allocation. There
are three principal types of thing that get allocated: function
closures, thunks, and data. These are all treated pretty much the
same in the STG machine for the simple reason that they share many
common characteristics:

- Entry code which the STG machine jumps to, in order to evaluate
  them

  - Note that for constructors, the entry code is trivial, as they
    are always already evaluated! In this case, control will be
    transferred directly back to the caller's continuation.

- Free variables stored in a closure

  - For data, these "free variables" will be the values in the fields
    of the particular data constructor

- Info-tables containing various miscellaneous metadata about the
  heap object, such as function arity

Let us look at how a thunk and a data constructor get allocated in
a simple setting:

```haskell
build_data :: Int -> Maybe Int
build_data x = Just (x + 1)
```

This compiles into the following C--:

```
Main_buildzudata_entry() {
    clE:
        Hp = Hp + 20;
        if (Hp > HpLim) goto clH;
        I32[Hp - 16] = slk_info;
        I32[Hp - 8] = I32[Sp + 0];
        I32[Hp - 4] = base_DataziMaybe_Just_con_info;
        I32[Hp + 0] = Hp - 16;
        R1 = Hp - 2;
        Sp = Sp + 4;
        jump (I32[I32[Sp + 0]]) ();
    clI:
        R1 = Main_buildzudata_closure;
        jump stg_gc_fun ();
    clH:
        HpAlloc = 20;
        goto clI;
}
```

```asm
_Main_buildzudata_entry:
LclE:
    addl $20,%edi
    cmpl 92(%ebx),%edi
    ja LclH
    movl $_slk_info,-16(%edi)
    movl (%ebp),%eax
    movl %eax,-8(%edi)
    movl $_base_DataziMaybe_Just_con_info,-4(%edi)
    leal -16(%edi),%eax
    movl %eax,(%edi)
    leal -2(%edi),%esi
    addl $4,%ebp
    movl (%ebp),%eax
    jmp *(%eax)
LclH:
    movl $20,112(%ebx)
LclI:
    movl $_Main_buildzudata_closure,%esi
    jmp *-4(%ebx)
```

Let's break this function down slowly.


### Checking for sufficient heap space

Any function that needs to allocate memory might find that the heap
has been exhausted. If that happens, it needs to call into the
garbage collector in order to get the heap cleaned up and
(possibly) enlarged.

Hence, the first thing any such function does is check to see if
enough memory is available for its purposes:

```
clE:
    Hp = Hp + 20;
    if (Hp > HpLim) goto clH;
...
clI:
    R1 = Main_buildzudata_closure;
    jump stg_gc_fun ();
clH:
    HpAlloc = 20;
    goto clI;
```

This is simple enough. The function needs to allocate 20 bytes (the
data constructor takes up 2 words, and the thunk will take up 3),
so it speculatively increments Hp and then checks the STG registers
`Hp` and `HpLim` (the pointer to the top of the available heap
space) against each other.

If memory is insufficient (i.e. we have moved `Hp` past the top of
the available heap), the code deals with it by setting the
`HpAlloc` register to the number of bytes needed and `R1` to the
closure for the function in which the heap check failed, before
jumping into the hand-written garbage collector code for the
cleanup. The garbage collector will resume execution of the code by
using the information from `R1`, after it has freed up enough
memory.

Side note: I believe that the line setting `R1` is unnecessary
here, because `R1` should anyway always be set to the address of
the closure when executing the closure entry code. I could be
wrong, though.

### Performing the actual allocation

Once the heap check succeeds, we will be able to enter the body of
the function proper. Since the `Hp` has already been incremented,
we can just construct the new heap objects directly:

```
    I32[Hp - 16] = slk_info;
    I32[Hp - 8] = I32[Sp + 0];
    I32[Hp - 4] = base_DataziMaybe_Just_con_info;
    I32[Hp + 0] = Hp - 16;
```

So we get something like this:

```wiki

            |              ...               |
            +--------------------------------+
            |           slk_info             |<-\ Pointer to thunk info table
            +--------------------------------+  |
            |          (undefined)           |  |
            +--------------------------------+  |
            |               x                |  | The "x + 1" thunk's free variable
            +--------------------------------+  |
            | base_DataziMaybe_Just_con_info |  | Pointer to Just info table
  /----\    +--------------------------------+  |
  | Hp |--->|                                |--/ Free variable of Just constructor
  \----/    +--------------------------------+

```

The bottom two words are the allocated `Just` value, and the three
above that correspond to the `x + 1` closure.


### Returning an allocated value to the caller

Now that we have allocated the data we entered the function in
order to construct, we need to return it to the caller. This is
achieved by the following code:

```
    R1 = Hp - 2;
    Sp = Sp + 4;
    jump (I32[I32[Sp + 0]]) ();
```

To return, the STG machine:

1. Sets `R1` to the pointer to the result of evaluation
1. Pops all the arguments to the function from the stack
1. Jumps to the entry code for the continuation. This is always
   found at the top of the STG stack, logically below any arguments
   that were pushed to make the call.

This is indeed exactly what happens here, with two interesting
points: pointer tagging, and the double-deference of the stack
pointer. These will be discussed in the next two subsections.


#### Pointer tagging

One exciting feature is that the code setting `R1`, i.e.
`R1 = Hp - 2`. This is setting `R1` to point to the `Just`, we just
allocated, but simultaneously tagging that pointer with the
value 1. The fact that the tag is non-zero indicates to users of the
pointer that the thing pointed to is already evaluated.
Furthermore, because `Maybe` has only two constructors, we are able
to use the pointer tags to record which constructor it evaluated
to: in this case, the 2 indicates the `Just` constructor.

It is compulsory to tag pointers before jumping to the address of
the continuation entry code: the entry code can and will rely on
those tags being present!

#### `TABLES_NEXT_TO_CODE`

Because I have compiled GHC without `TABLES_NEXT_TO_CODE`, the
entry code for the continuation is found by dereferencing the
pointer to the info table we found at the top of the STG stack -
i.e. a double-dereference.

The layout of heap objects without `TABLES_NEXT_TO_CODE` is as
follows:

```wiki

          Closure             Info table        Entry code
       +--------------+    +--------------+   +------------+
 x --->| Info pointer |--->| Code pointer |-->|    ...     |
       +--------------+    +--------------+
       |  .. FVs ..   |    |     ...      |

```

With `TABLES_NEXT_TO_CODE` on, the situation looks more like this:

```wiki

          Closure          | .. Info table .. |
       +--------------+    +------------------+
 x --->| Info pointer |--->| .. Entry code .. |
       +--------------+    |                  |
       |  .. FVs ..   |

```

The `TABLES_NEXT_TO_CODE` optimisation removes the need for that
second dereference during the return, because the entry code is
always right next to the info table. However, it requires special
support from the backend for ensuring that data (i.e. the info
table) and code are contiguous in memory, so it cannot always be
used.


## Example 7: `case` expressions

Let us now examine how `case` expressions are handled. Compiling
the following Haskell

```wiki
case_scrut :: Maybe Int -> Int
case_scrut x = case x of Just x -> x; Nothing -> 10
```

Produces this C-- code

```
Main_casezuscrut_entry() {
    ccx:
        R1 = I32[Sp + 0];
        I32[Sp + 0] = scj_info;
        if (R1 & 3 != 0) goto ccA;
        jump (I32[I32[R1]]) ();
    ccA:
        jump (I32[scj_info]) ();
}

scj_ret() {
    cct:
        _ccu::I32 = R1 & 3;
        if (_ccu::I32 >= 2) goto ccv;
        R1 = stg_INTLIKE_closure+209;
        Sp = Sp + 4;
        jump (I32[I32[Sp + 0]]) ();
    ccv:
        R1 = I32[R1 + 2];
        Sp = Sp + 4;
        R1 = R1 & (-4);
        jump (I32[I32[R1]]) ();
}
```

```asm
_Main_casezuscrut_entry:
Lccx:
    movl (%ebp),%esi
    movl $_scj_info,(%ebp)
    testl $3,%esi
    jne LccA
    movl (%esi),%eax
    jmp *(%eax)
LccA:
    jmp *_scj_info

_scj_ret:
Lcct:
    movl %esi,%eax
    andl $3,%eax
    cmpl $2,%eax
    jae Lccv
    movl L_stg_INTLIKE_closure$non_lazy_ptr,%eax
    leal 209(%eax),%esi
    addl $4,%ebp
    movl (%ebp),%eax
    jmp *(%eax)
Lccv:
    movl 2(%esi),%esi
    addl $4,%ebp
    andl $-4,%esi
    movl (%esi),%eax
    jmp *(%eax)
```

Notice that GHC has generated *two* functions:
`Main_casezuscrut_entry` and `scj_ret` correspond to the code for
forcing the argument to the `case`, and for the *continuation* of
the `case` respectively. Let's pick them apart and see how they
work!


### Forcing the scrutinee of the `case`

When we first call the `case_scrut` function, its entry code begins
executing:

```
ccx:
    R1 = I32[Sp + 0];
    I32[Sp + 0] = scj_info;
    if (R1 & 3 != 0) goto ccA;
    jump (I32[I32[R1]]) ();
ccA:
    jump (I32[scj_info]) ();
```

This is a function of arity 1 (i.e. with a single argument), so
upon entry the machine state looks like this:

```wiki

  /----\     +--------------------------+
  | R1 |---->| Main_casezuscrut_closure |
  \----/     +--------------------------+

  /----\     +------------+
  | Sp |---->|     x      |
  \----/     +------------+
             |    ...     |

```

Because this is a top level function, the closure is statically
allocated and contains no free variables. However, as discussed
previously, the single argument to the function is guaranteed to be
present at the top of the stack.

The code starts off by saving this argument (the `x`) temporarily
into `R1`:

```
ccx:
    R1 = I32[Sp + 0];
```

The next thing the code does is overwrites this argument on the
stack with a pointer to the info-table of the continuation code.
This is the code that will be invoked after `x` has been evaluated
into WHNF, and which will do the test to decide whether to continue
as the `Nothing` or as the `Just` branch of the case:

```
    I32[Sp + 0] = scj_info;
```

As we saw earlier, any time that the STG machine decides that it
has a value in its hand, it will continue evaluation by
tail-calling the entry code found by dereferencing the info-table
pointer at the top of the stack. So by putting the address of our
continuation in here, we ensure that the entry code for `scj_info`
is executed after `x` becomes a value.

Now, what we need to do is to start the evaluation of `x`. We could
just jump into `x`'s entry code and hope for the best, but thanks
to GHC's pointer tagging we can sometimes avoid doing this indirect
branch.

So, instead, we test to see if the `x` pointer has a tag. If it is
tagged, then we know that it is already evaluated and hence jump
directly to the code for the continuation. If it is not tagged, we
are forced to make the jump into the entry code for `x`. This
choice is embodied by the following code:

```
    if (R1 & 3 != 0) goto ccA;
    jump (I32[I32[R1]]) ();
ccA:
    jump (I32[scj_info]) ();
```

Note the test `R1 & 3 != 0`: this reflects the fact that pointer
tags are stored in the lower 2 bits of the pointer on 32 bit
machines. Another interesting feature is how the `jump`
instructions find the entry code: again, we see a deference of the
info pointer because `TABLES_NEXT_TO_CODE` is turned off.

As we saw, the `case` scrutinisation code ended with one of two
things happening: 1. A direct call into the continuation code
`scj_ret` if the scrutinee was already evaluated 2. A call into the
entry code for the scrutinee, if the scrutinee was not evaluated
(or it *was* evaluated, but the pointer was somehow not tagged
with that information) - Because we pushed `scj_info` onto the STG
stack, control will eventually return to `scj_ret` after the
evaluation of `x` has finished

It is now time to examine the continuation code to see what happens
after `x` becomes a value.


### Dealing with the forced scrutinee

The continuation code is a little more complicated:

```
cct:
    _ccu::I32 = R1 & 3;
    if (_ccu::I32 >= 2) goto ccv;
    R1 = stg_INTLIKE_closure+209;
    Sp = Sp + 4;
    jump (I32[I32[Sp + 0]]) ();
ccv:
    R1 = I32[R1 + 2];
    Sp = Sp + 4;
    R1 = R1 & (-4);
    jump (I32[I32[R1]]) ();
```

Whenever the STG machine evaluates to a value it will return the
value by jumping to the entry point at the top of the stack. In
this case, `R1` is guaranteed to be a (tagged) pointer to the thing
that was just evaluated. Because we are scrutinising a `Maybe` type
(which has fewer than 4 constructors) the code for the `case`
continuation is able to use the tag bits on the returned pointer to
decide which of the two branches to take:

```
cct:
    _ccu::I32 = R1 & 3;
    if (_ccu::I32 >= 2) goto ccv;
```


If we were scrutinising a data type with more constructors, the tag
bits would only tell us that the thing was evaluated, not which
constructor it was evaluated to. In this case, we would have to
read the constructor tag by dereferencing `R1` and testing the
resulting info table pointer against all possibilities.

If the tag was greater than or equal to 2, we go to the `ccv`
branch, which deals with what happens if we had a `Just`. In this
case, we need to continue by forcing the thunk inside the `Just`
and returning that value to our caller, which is what these lines
are doing:

```
ccv:
    R1 = I32[R1 + 2];
    Sp = Sp + 4;
    R1 = R1 & (-4);
    jump (I32[I32[R1]]) ();
```

To access the thing inside the `Just`, the code assumes that the
`R1` pointer is tagged with the 2 that indicates a `Just`
constructor, and hence finds the first free variable (stored 4
bytes into the closure) using `I32[R1 + 2]`, which is then saved
into `R1`. It pops the address of `scj_info` that was pushed onto
the stack in `Main_casezuscrut_entry` by moving `Sp` up 4 bytes
(remember that the STG stack grows downwards) and then untags and
jumps into the entry code for the `R1` thunk, using the same
double-dereference pattern discussed earlier.

There seems to be a small missed opportunity here: the code could
check the pointer tag on `R1`, and then return directly if it is
set. I imagine that this isn't being done in order to reduce
possible code bloat.


## Example 8: thunks and thunk update

You might be wondering how the `x + 1` thunk we saw allocated in a
previous section will behave when it is actually forced. To remind
you, the thunk we saw was constructed by the following Haskell
code:

```haskell
build_data :: Int -> Maybe Int
build_data x = Just (x + 1)
```

So how does the `x + 1` thunk work? An excellent question! Let's
take a look at the C-- for its entry code and find out:

```
slk_entry() {
    cph:
        if (Sp - 12 < SpLim) goto cpj;
        I32[Sp - 8] = stg_upd_frame_info;
        I32[Sp - 4] = R1;
        R1 = I32[R1 + 8];
        I32[Sp - 12] = soN_info;
        Sp = Sp - 12;
        if (R1 & 3 != 0) goto cpk;
        jump (I32[I32[R1]]) ();
    cpj: jump stg_gc_enter_1 ();
    cpk: jump (I32[soN_info]) ();
}

soN_ret() {
    cp7:
        Hp = Hp + 8;
        if (Hp > HpLim) goto cpc;
        _soL::I32 = I32[R1 + 3] + 1;
        I32[Hp - 4] = ghczmprim_GHCziTypes_Izh_con_info;
        I32[Hp + 0] = _soL::I32;
        R1 = Hp - 3;
        Sp = Sp + 4;
        jump (I32[stg_upd_frame_info]) ();
    cpd: jump stg_gc_enter_1 ();
    cpc:
        HpAlloc = 8;
        goto cpd;
}
```

```
_sst_entry:
Lcph:
    leal -12(%ebp),%eax
    cmpl 84(%ebx),%eax
    jb Lcpj
    movl L_stg_upd_frame_info$non_lazy_ptr,%eax
    movl %eax,-8(%ebp)
    movl %esi,-4(%ebp)
    movl 8(%esi),%esi
    movl $_soN_info,-12(%ebp)
    addl $-12,%ebp
    testl $3,%esi
    jne Lcpk
    movl (%esi),%eax
    jmp *(%eax)
Lcpj:
    jmp *-8(%ebx)
Lcpk:
    jmp *_soN_info

_soN_ret:
Lcp7:
    addl $8,%edi
    cmpl 92(%ebx),%edi
    ja Lcpc
    movl 3(%esi),%eax
    incl %eax
    movl $_ghczmprim_GHCziTypes_Izh_con_info,-4(%edi)
    movl %eax,(%edi)
    leal -3(%edi),%esi
    addl $4,%ebp
    movl L_stg_upd_frame_info$non_lazy_ptr,%eax
    jmp *(%eax)
Lcpc:
    movl $8,112(%ebx)
Lcpd:
    jmp *-8(%ebx)
```

The original Haskell code read `x + 1`, but GHC has inlined the
actual code for the addition operation on `Int`s, which looks
something like:

```
plusInt (I# a) (I# b) = I# (a + b)
```

The second pattern match (to get `b`) has been performed statically
by GHC, obtaining the machine literal 1, which shows up directly in
the generated code. Therefore, the code only need to evaluate and
case-decompose the unknown free variable `x` of our closure, to get
the `a` argument to `plusInt`.


### Thunk entry point

This evaluation is what is being done by the thunk entry code
`slk_entry`. Ignoring the stack check, the C-- begins thusly:

```
    I32[Sp - 8] = stg_upd_frame_info;
    I32[Sp - 4] = R1;
    R1 = I32[R1 + 8];
    I32[Sp - 12] = soN_info;
    Sp = Sp - 12;
```

Remembering that upon entry to the thunk entry code, `R1` points to
the thunk's closure, the new stack looks as follows:

```wiki

 /----\    +---------------------+
 | Sp |--->|       soN_info      |
 \----/    +---------------------+
           | stg_upd_frame_info  |
           +---------------------+     +-------------+
           | (thunk closure ptr) |---->|   slk_info  |
           +---------------------+     +-------------+
           |        ...          |     | (undefined) |
                                       +-------------+
                                       |      x      | Free variable of thunk
                                       +-------------+
```

The C-- statement `R1 = I32[R1 + 8]` is pulling out the pointer to
the free variable of the thunk (which was set up in
`Main_buildzudata_entry`) into `R1`.

Finally, the entry code evaluates that free variable (checking the
tag bits of the pointer first, as usual):

```
    if (R1 & 3 != 0) goto cpk;
    jump (I32[I32[R1]]) ();
cpk: jump (I32[soN_info]) ();
```

Because we put `soN_info` at the top of the stack, when evaluation
of `x` is complete the STG machine will continue by executing the
`soN_ret` code.

The most interesting feature of this code is the extra stuff that
has been pushed onto the stack below `soN_ret`: an info pointer
called `stg_upd_frame_info`, and a pointer to the thunk currently
being evaluated.

This is all part of the STG machine's thunk update mechanism. When
the `soN_ret` continuation returns, it will transfer control
*not* to the code forcing the thunk, but to some code which
overwrites the contents of the current thunk closure with a closure
representing an "indirection". The entry code for such an
indirection closure is trivial: it immediately returns a pointer to
the thing that was returned from the `soN_ret` continuation in
`R1`.

These indirections are the mechanism which ensures that the STG
machine never repeats the work of evaluating a thunk more than
once: after the first evaluation, any code forcing the thunk jumps
into the indirection entry code rather than `slk_entry`.

That being said, let us look at how the continuation responsible
for actually finding the value of `x + 1` works:


### Continuation of the thunk

Upon entry to the continuation code, we have the evaluated `x` in
`R1`: it now needs to do the addition and allocate a `I#`
constructor to hold the result of the addition. Because of the
allocation, `soN_ret` begins with a heap check. Ignoring that
check, we have the following code:

```
    _soL::I32 = I32[R1 + 3] + 1;
    I32[Hp - 4] = ghczmprim_GHCziTypes_Izh_con_info;
    I32[Hp + 0] = _soL::I32;
    R1 = Hp - 3;
    Sp = Sp + 4;
    jump (I32[stg_upd_frame_info]) ();
```

This is mostly standard stuff. Because the `R1` pointer is
guaranteed tagged, and there is only one possible constructor, the
tag must be 1 and so the `Int#` value inside the `Int` is pulled
out using `I32[R1 + 3]`. This is then put into a newly
heap-allocated `I#` constructor, which is returned in `R1` after we
pop the `soN_info` pointer from the stack.

The only interesting point is where we return to: rather than
dereference `Sp` to find the info pointer at the top of the STG
stack, GHC has generated code that takes advantage of the fact that
the `Sp` is guaranteed to point to `stg_upd_frame_info`. This
avoids one pointer dereference.


## Conclusion

This document has left much of the detail of how STG is implemented
out: notable omissions include CAFs, and the precise behaviour of
the garbage collector. Nonetheless, my hope is that it has helped
you to gain some more insight into the weird and wonderful way the
Haskell evaluation model is implemented.