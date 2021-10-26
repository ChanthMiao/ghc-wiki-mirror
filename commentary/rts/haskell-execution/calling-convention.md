# Calling Convention


Entry conventions are very conventional: the first N arguments in registers and the rest on the stack.

You can generally find the mapping of STG to machine registers in `MachRegs.h`.

For convenience here are the x86-64 mappings for the gp registers:

```
#define REG_Base  r13
#define REG_Sp    rbp
#define REG_Hp    r12
#define REG_R1    rbx
#define REG_R2    r14
#define REG_R3    rsi
#define REG_R4    rdi
#define REG_R5    r8
#define REG_R6    r9
#define REG_SpLim r15
#define REG_MachSp  rsp
```

# Return Convention



All returns are now *direct*; that is, a return is made by jumping to the code associated with the [info table](commentary/rts/storage/heap-objects#) of the topmost [stack frame](commentary/rts/storage/stack).



GHC used to have a more complex return convention called vectored returns in which some stack frames pointed to vectors of return addresses; this was dropped in GHC 6.8 after measurements that showed it was not (any longer) worthwhile.
