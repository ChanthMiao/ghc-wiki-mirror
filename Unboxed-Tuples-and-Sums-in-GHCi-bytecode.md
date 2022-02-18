# More Unboxed Types in GHCi

The GHCi bytecode interpreter doesn't support unboxed tuples and sums except for a few special cases. When GHCi encounters the `UnboxedTuples` language extension, it falls back to object code, which has much slower compile times than bytecode and means that the debugger cannot be used.

This document describes how we can extend GHCi bytecode to support unboxed tuples and sums beyond the special cases.

## Unboxed Types in the Interpreter

GHCi runs programs as a mix of bytecode and object code. Primops and libraries like `base` are typically object code, while the user's program is bytecode. Frequent switches between bytecode and object code are expected.

The bytecode operates exclusively on the stack. It does not have access to the registers that the STG machine for object code uses to pass around function arguments or return values.

GHCi must ensure that the following conditions are met at the time of every switch between bytecode and object code:

* all function arguments or return values are saved on the stack
* the stack is in consistent state, i.e. all values (in particular GC pointers) on the stack are correctly described by the stack frames

### Returning an Unboxed Value

There is a specific bytecode instruction for returning each specific type of unboxed value. For example there is `RETURN_D` for bytecode that returns a `Double#`. The bytecode would first push the value onto the stack (growing down):

```
...
next_frame
double_value <- Sp
```

And then execute a `RETURN_D` instruction. The interpreter would then push an `stg_ret_d_info` header:

```
...
next_frame
double_value
stg_ret_d_info <- Sp
```

If `next_frame` is a bytecode frame (`RET_BCO`) the interpreter executes it immediately (with the `stg_ret_d_info` frame still on the stack). If `next_frame` is object code, the interpreter will yield to the scheduler, which in turn calls `stg_ret_d`.

`stg_ret_d` pops the topmost frame and returns `double_value` to `next_frame` according to GHCs native calling convention (in the `D1` register).

### Receiving Unboxed Value

To receive a `Double#` value, bytecode would push a continuation using `PUSH_ALTS_D`, followed by jumping to the code that produces the `Double#`. The `PUSH_ALTS_D` instruction causes the interpreter to push the continuation and an `stg_ctoi_D1_info` header on the stack:

```
...
cont_free_vars
cont_BCO
stg_ctoi_D1_info <- Sp
```

If the `Double#` value is returned from object code, the `stg_ctoi_D1` procedure is executed. This does the following:

1. take the `Double#` value from `D1` and push it onto the stack
2. push `stg_ret_d_info`
3. yield to the interpreter

When the interpreter is entered, the stack looks as follows:

```
...
cont_free_vars
cont_BCO
stg_ctoi_D1_info
double_value
stg_ret_d_info <- Sp
```

This is exactly what the stack would have looked like if `double_value` was returned within the interpreter. Nice!

## Notes

Here are a couple of items that affect the implementation of support for tuples in bytecode.

### Unarisation

GHC performs an operation known as `UnariseStg` during the `stg2stg` pass. This rewrites unboxed tuples in function arguments and let bindings. We need to perform this operation before generating bytecode if we want to be able to use unboxed tuples in these situations.

### Whatcha Pointin' at?

The `Native` (object code) calling convention uses both registers and the stack for returning tuples. The stack pointer `Sp` points at the topmost value on the stack.

For example when an 8-tuple is returned to `next_frame`, the stack (growing down) might look like this:

```
...
next_frame
tuple_element_7
tuple_element_8 <- Sp
```

with the other tuple elements in registers.

The value of `Sp` makes it more difficult to implement a generic procedure that converts any type of tuple.

### Profiling

The interpreter uses `stg_restore_cccs` stack frames to restore cost centre stack before the continuation is started. `stg_restore_cccs` preserves all values returned in STG machine registers, but it does not work correctly if there's anything returned on the stack.

Since unboxed tuples may have values spilled onto the stack, we do not use `stg_restore_cccs` for tuples. Instead, we store the `CCCS` in the continuation and restore it inside the interpreter itself (before the first instruction of the continuation is executed).

## Extending the Bytecode

GHCi supports each unary (non-tuple) unboxed type explicitly supported by bytecode instructions and helper procedures. For example returning/receiving a `Double#` uses `RETURN_D`, `PUSH_ALTS_D`, `stg_ret_d`, `stg_ctoi_D1`. For a word-sized non-pointer (e.g. `Word#` or `Int#`), the equivalents are `RETURN_N`, `PUSH_ATLS_N`, `stg_ret_n`, `stg_ctoi_n`. The bytecode instructions and helper procedures work together to convert the values between the GHC native calling convention (stack + STG registers) and the GHCi bytecode calling convention (stack only).

The above approach works well if we only have to deal with a limited number of different representation and calling conventions. Tuples come in infinitely many shapes and sizes, so it's not possible to have a specific `stg_ret_XXX`/`stg_ctoi_XXX` for each of them.

Therefore we take a slightly different approach. We introduce a single set of "generic" instructions and prodecures that handle all tuples: `RETURN_TUPLE`, `PUSH_ALTS_TUPLE`, `stg_ret_t`, `stg_ctoi_t`. These instructions can handle all possible tuple shapes by using a description (i.e. which registers and how much stack space are used) of the specific tuple at hand.

The generic tuple instructions have some runtime overhead, therefore we keep using the existing instructions and helpers for unboxed values where possible. For example the tuple `(# Double#, Void#, Void# #)` has the same runtime representation as `Double#`, so we use `RETURN_D`/`stg_ret_d` to return such a tuple, instead of the less efficient `RETURN_TUPLE`.

### Bytecode from STG

We change the bytecode generator to generate bytecode from STG instead of core. This means we can use the result of the unarisation step.

This change is relatively straightforward, and it makes the bytecode generator a bit more clear, thanks to the more explicit structure of STG.

There is one complication: STG loses coercions, and there is no STG equivalent for the Core `exprType` function. The bytecode generator used `exprType` to compute the type of breakpoints.

To solve this, we compute the type of each breakpoint in `CoreToStg` and store it inside the breakpoint itself. This required a bit of refactoring of the related types.

### New Bytecode Operations

We introduce the new bytecode instructions `RETURN_TUPLE` and `PUSH_ALTS_TUPLE` and the new helpers `stg_ret_t` and `stg_ctoi_t` (see `rts/StgMiscClosures.cmm`)

The new instructions use the helper data `tuple_BCO` and `tuple_info`, described below.

#### Tuples on the Stack

Object code uses a mix of registers and stack to return a tuple. Bytecode only has stack. We choose the stack layout for tuples in the interpreter to make conversion to and from the GHC native calling convention as easy as possible:

* everything that object code returns on the stack remains on the stack in the same place.
* everything that is returned in registers will be saved onto the stack. The order of the elements on the stack is determined by the registers in which they were returned (which may not be the same as the order of the tuple elements in the Haskell source code)

Since the native calling convention expands every element to a full word width, we continue doing the same. For example an `Int8#` will always use a full register or a full word on the stack.

#### Returning a tuple

Bytecode that returns a tuple first pushes all the tuple fields followed by the appropriate `tuple_info` and `tuple_BCO` onto the stack. It then executes the `RETURN_TUPLE` instruction, which causes the interpreter to push `stg_ret_t_info` to the top of the stack. The stack then looks as follows:

```
...
next_frame
tuple_field_1
tuple_field_2
...
tuple_field_n
tuple_info
tuple_BCO
stg_ret_t_info <- Sp
```

If `next_frame` is bytecode, the interpreter will start executing it. If it's object code, the interpreter jumps back to the scheduler, which in turn jumps to `stg_ret_t`. `stg_ret_t` converts the tuple to the native calling convention using the description in `tuple_info`, and then jumps to `next_frame`.

#### Receiving a tuple

Bytecode that receives a tuple uses the `PUSH_ALTS_TUPLE` instruction to push a continuation, followed by jumping to the code that produces the tuple. The `PUSH_ALTS_TUPLE` instuction contains three pieces of data:

* `cont_BCO`: the continuation that receives the tuple
* `tuple_info`: see below
* `tuple_BCO`: see below

The interpreter pushes these onto the stack when the `PUSH_ALTS_TUPLE` instruction is executed, followed by `stg_ctoi_tN_info`, with `N` depending on the number of stack words used by the tuple in the GHC native calling convention. `N` is derived from `tuple_info`.

For example if we expect a tuple with three words on the stack, the stack looks as follows after `PUSH_ALTS_TUPLE`:

```
...
next_frame
cont_free_var_1
cont_free_var_2
...
cont_free_var_n
tuple_info
tuple_BCO
cont_BCO
stg_ctoi_t3_info <- Sp
```

When the tuple is returned from object code, `stg_ctoi_t3` is called with the stack looking as follows (some of the tuple elements are in registers):

```
...
next_frame
cont_free_var_1
cont_free_var_2
...
cont_free_var_n
tuple_info
tuple_BCO
cont_BCO
stg_ctoi_t3_info
tuple_element_stack_1
tuple_element_stack_2
tuple_element_stack_3 <- Sp
```

`stg_ctoi_t3` adjusts the stack pointer `Sp` to point to the start of the continuation frame and then jumps to `stg_ctoi_t`:

```
...
next_frame
cont_free_var_1
cont_free_var_2
...
cont_free_var_n
tuple_info
tuple_BCO
cont_BCO
stg_ctoi_t3_info <- Sp
tuple_element_stack_1
tuple_element_stack_2
tuple_element_stack_3
```

`stg_ctoi_t` then uses the data in `tuple_info` (which is now at a fixed offset from `Sp`) to copy the tuple data from registers onto the stack, and pushes a tuple return frame. It duplicates the `tuple_info` and `tuple_BCO` items into this new frame:

```
...
next_frame
cont_free_var_1
cont_free_var_2
...
cont_free_var_n
tuple_info
tuple_BCO
cont_BCO
stg_ctoi_t3_info
tuple_element_stack_1
tuple_element_stack_2
tuple_element_stack_3
tuple_element_reg_1
tuple_element_reg_2
tuple_element_reg_3
tuple_element_reg_4
tuple_info
tuple_BCO
stg_ret_t_info <- Sp
```

At this point `stg_ctoi_t` switches to interpreted mode and yields to the scheduler. 

#### The `tuple_BCO`

The `tuple_BCO` is a helper bytecode object. Its main purpose is describing the contents of the stack frame containing the tuple for the storage manager. It contains only instructions to immediately return the tuple that is already on the stack.

#### The `tuple_info` word

The `tuple_info` word describes the stack and STG register (e.g. `R1`..`R6`, `D1`..`D6`) usage for the tuple. `tuple_info` contains enough information to convert the tuple between the stack-only bytecode and stack+registers GHC native calling conventions.

See `rts/StgMiscClosures.cmm` for more details of how the data is packed in a single word.

(the `tuple_info` word is not an infotable, my apologies for the confusing name)

#### Maximum Tuple Size

The GHC native calling convention makes it necessary to have an `stg_ctoi_tN` "adjustor" procedure for a tuple that has `N` words on the stack. In the current implementation, we provide `stg_ctoi_t1`..`stg_ctoi_t62`, so all tuples that have up to and including 62 words on the stack (with the GHC native calling convention) are supported.

It's straightforward to add more `stg_ctoi_tN` helpers for bigger tuples, but it requires recompiling GHC and the RTS. A fully general solution that supports all tuple sizes will likely require a change in the GHC calling convention.

