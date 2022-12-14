# Native Code Generator (NCG)


For other information related to this page, see:

- [BackEndNotes](back-end-notes) for optimisation ideas regarding the current NCG
- [The Cmm language](commentary/compiler/cmm-type) (the NCG code works from Haskell's implementation of C-- and many optimisations in the NCG relate to Cmm)
- [The register allocator](commentary/compiler/backends/ncg/register-allocator).


On some platforms (currently x86 and x86_64, with possibly bitrotted support for PowerPC and Sparc), GHC can generate assembly code directly. The NCG is enabled by default on supported platforms.


The NCG has always been something of a second-class citizen inside GHC, an unloved child, rather. This means that its integration into the compiler as a whole is rather clumsy, which brings some problems described below. That apart, the NCG proper is fairly cleanly designed, as target-independent as it reasonably can be, and so should not be difficult to retarget.


NOTE! The native code generator was largely rewritten as part of the C-- backend changes, around May 2004. Unfortunately the rest of this document still refers to the old version, and was written with relation to the CVS head as of end-Jan 2002. Some of it is relevant, some of it isn't.

### Files, Parts


After GHC has produced [Cmm](commentary/compiler/cmm-type) (use -ddump-cmm or -ddump-opt-cmm to view), the Native Code Generator (NCG) transforms Cmm into architecture-specific assembly code.  The NCG is located in [compiler/GHC/CmmToAsm](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm) and is separated into eight modules:

- [compiler/GHC/CmmToAsm.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm.hs)

  top-level module for the NCG, imported by [compiler/GHC/Driver/CodeOutput.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Driver/CodeOutput.hs); also defines the Monad for optimising generic Cmm code, `CmmOptM`
- [compiler/nativeGen/MachCodeGen.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/nativeGen/MachCodeGen.hs)

  generates architecture-specific instructions (a Haskell-representation of assembler) from Cmm code
- [compiler/nativeGen/MachInstrs.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/nativeGen/MachInstrs.hs)

  contains data definitions and some functions (comparison, size, simple conversions) for machine instructions, mostly carried out through the `Instr` data type, defined here
- [compiler/GHC/CmmToAsm/Monad.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/Monad.hs)

  defines the main monad in the NCG: the Native code Machine instruction Monad, `NatM`, and related functions.  *Note: the NCG switches between two monads at times, especially in `AsmCodeGen`: `NatM` and the `UniqSM` Monad used throughout the compiler.*
- [compiler/GHC/CmmToAsm/PIC.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/CmmToAsm/PIC.hs)

  handles generation of position independent code and issues related to dynamic linking in the NCG; related to many other modules outside the NCG that handle symbol import, export and references, including `CLabel`, `Cmm`, `codeGen` and the RTS, and the Mangler
- [compiler/nativeGen/PprMach.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/nativeGen/PprMach.hs)

  Pretty prints machine instructions (`Instr`) to assembler code (currently readable by GNU's `as`), with some small modifications, especially for comparing and adding floating point numbers on x86 architectures
- [compiler/nativeGen/RegAllocInfo.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/nativeGen/RegAllocInfo.hs)

  defines the main register information function, `regUsage`, which takes a set of real and virtual registers and returns the actual registers used by a particular `Instr`; register allocation is in AT&T syntax order (source, destination), in an internal function, `usage`; defines the `RegUsage` data type
- [compiler/nativeGen/RegisterAlloc.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/nativeGen/RegisterAlloc.hs)

  one of the most complicated modules in the NCG, `RegisterAlloc` manages the allocation of registers for each *basic block* of Haskell-abstracted assembler code: management involves *liveness* analysis, allocation or deletion of temporary registers, *spilling* temporary values to the *spill stack* (memory) and many optimisations.  *See [The Cmm language](commentary/compiler/cmm-type) for the definition of a *basic block* (in Haskell, *`type CmmBasicBlock =  GenBasicBlock CmmStmt`*).*


and one header file:

- [compiler/nativeGen/NCG.h](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/nativeGen/NCG.h)

  defines macros used to separate architecture-specific code in the Haskell NCG files; since GHC currently only generates machine code for the architecture on which it was compiled (GHC is not currently a cross-compiler), the Haskell NCG files become considerably smaller after preprocessing; ideally all architecture-specific code would reside in separate files and GHC would have them available to support cross-compiler capabilities.


The NCG has **machine-independent**  and **machine-dependent** parts.  


The **machine-independent** parts relate to generic operations, especially optimisations, on Cmm code.  The main machine-independent parts begin with *Cmm blocks.*  (A *Cmm block* is a compilation unit of Cmm code, a file.  See [The Cmm language](commentary/compiler/cmm-type) for a discussion of what a *Cmm block* is but note that *Cmm* is a type synonym for `GenCmmTop CmmStatic CmmStmt`.)  A machine-specific (assembler) instruction is represented as a `Instr`.  The machine-independent NCG parts:

1. optimise each Cmm block by reordering its basic blocks from the original order (the `Instr` order from the `Cmm`) to minimise the number of branches between basic blocks, in other words, by maximising fallthrough of execution from one basic block to the next.
1. lazily convert each Cmm block to abstract machine instructions (`Instr`) operating on an infinite number of registers--since the NCG Haskell files only contain instructions for the host computer on which GHC was compiled, these `Instr` are machine-specific; and,
1. lazily allocate real registers for each basic block, based on the number of available registers on the target (currently, only the host) machine; for example, 32 integer and 32 floating-point registers on the PowerPC architecture.  The NCG does not currently have support for SIMD registers such as the vector registers for Altivec or any variation of SSE.
  *Note*: if a basic block simultaneously requires more registers than are available on the target machine and the temporary variable needs to be used (would sill be *live*) after the current instruction, it will be moved (*spilled*) into memory.


The **machine-dependent** parts:

1. define the abstract (Haskell) assembler `Instr` for the target (host) machine and convert every Cmm block into it;
1. define, manage and allocate the real registers available on the target system;
1. pretty-print the Haskell-assembler to GNU AS (GAS) assembler code

## Overview


The top-level code generator function is

```wiki
absCtoNat :: AbstractC -> UniqSM (SDoc, Pretty.Doc)
```


The returned `SDoc` is for debugging, so is empty unless you specify `-ddump-stix`. The `Pretty.Doc` bit is the final assembly code. Translation involves three main phases, the first and third of which are target-independent.

#### Translation into the Stix representation


Stix is a simple tree-like RTL-style language, in which you can mention:

- An infinite number of temporary, virtual registers.
- The STG "magic" registers (`MagicId`), such as the heap and stack pointers.
- Literals and low-level machine ops (`MachOp`).
- Simple address computations.
- Reads and writes of: memory, virtual regs, and various STG regs.
- Labels and `if ... goto ...` style control-flow. 


Stix has two main associated types:

- `StixStmt` -- trees executed for their side effects: assignments, control transfers, and auxiliary junk such as segment changes and literal data.
- `StixExpr` -- trees which denote a value. 


Translation into Stix is almost completely target-independent. Needed dependencies are knowledge of word size and endianness, used when generating code to do deal with half-word fields in info tables. This could be abstracted out easily enough. Also, the Stix translation needs to know which `MagicId`s map to registers on the given target, and which are stored in offsets from `BaseReg`.


After initial Stix generation, the trees are cleaned up with constant-folding and a little copy-propagation ("Stix inlining", as the code misleadingly calls it). We take the opportunity to translate `MagicId`s which are stored in memory on the given target, into suitable memory references. Those which are stored in registers are left alone. There is also a half-hearted attempt to lift literal strings to the top level in cases where nested strings have been observed to give incorrect code in the past.


Primitive machine-level operations will already be phrased in terms of `MachOp`s in the presented Abstract C, and these are passed through unchanged. We comment only that the `MachOp`s have been chosen so as to be easy to implement on all targets, and their meaning is intended to be unambiguous, and the same on all targets, regardless of word size or endianness.

**A note on `MagicId`s**. Those which are assigned to registers on the current target are left unmodified. Those which are not are stored in memory as offsets from `BaseReg` (which is assumed to permanently have the value (`&MainCapability.r`)), so the constant folder calculates the offsets and inserts suitable loads/stores. One complication is that not all archs have `BaseReg` itself in a register, so for those (sparc), we instead generate the address as an offset from the static symbol `MainCapability`, since the register table lives in there.


Finally, `BaseReg` does occasionally itself get mentioned in Stix expression trees, and in this case what is denoted is precisely (`&MainCapability.r`), not, as in all other cases, the value of memory at some offset from the start of the register table. Since what it denotes is an r-value and not an l-value, assigning `BaseReg` is meaningless, so the machinery checks to ensure this never happens. All these details are taken into account by the constant folder.

#### Instruction selection


This is the only majorly target-specific phase. It turns Stix statements and expressions into sequences of `Instr`, a data type which is different for each architecture. Instr, unsurprisingly, has various supporting types, such as `Reg`, `Operand`, `Imm`, etc. The generated instructions may refer to specific machine registers, or to arbitrary virtual registers, either those created within the instruction selector, or those mentioned in the Stix passed to it.


The instruction selectors live in `MachCode.lhs`. The core functions, for each target, are:

```wiki
getAmode :: StixExpr -> NatM Amode
getRegister :: StixExpr -> NatM Register
assignMem_IntCode :: PrimRep -> StixExpr -> StixExpr -> NatM InstrBlock
assignReg_IntCode :: PrimRep -> StixReg -> StixExpr -> NatM InstrBlock
```


The insn selectors use the "maximal munch" algorithm. The bizarrely-misnamed `getRegister` translates expressions. A simplified version of its type is:

```wiki
getRegister :: StixExpr -> NatM (OrdList Instr, Reg)
```


That is: it (monadically) turns a StixExpr into a sequence of instructions, and a register, with the meaning that after executing the (possibly empty) sequence of instructions, the (possibly virtual) register will hold the resulting value. The real situation is complicated by the presence of fixed registers, and is detailed below.


Maximal munch is a greedy algorithm and is known not to give globally optimal code sequences, but it is good enough, and fast and simple. Early incarnations of the NCG used something more sophisticated, but that is long gone now.


Similarly, `getAmode` translates a value, intended to denote an address, into a sequence of insns leading up to a (processor-specific) addressing mode. This stuff could be done using the general `getRegister` selector, but would necessarily generate poorer code, because the calculated address would be forced into a register, which might be unnecessary if it could partially or wholly be calculated using an addressing mode.


Finally, `assignMem_IntCode` and `assignReg_IntCode` create instruction sequences to calculate a value and store it in the given register, or at the given address. Because these guys translate a statement, not a value, they just return a sequence of insns and no associated register. Floating-point and 64-bit integer assignments have analogous selectors.


Apart from the complexities of fixed vs floating registers, discussed below, the instruction selector is as simple as it can be. It looks long and scary but detailed examination reveals it to be fairly straightforward.

#### Register allocation


The register allocator, `AsmRegAlloc.lhs` takes sequences of Instrs which mention a mixture of real and virtual registers, and returns a modified sequence referring only to real ones. It is gloriously and entirely target-independent. Well, not exactly true. Instead it regards `Instr` (instructions) and `Reg` (virtual and real registers) as abstract types, to which it has the following interface:

```wiki
insnFuture :: Instr -> InsnFuture
regUsage :: Instr -> RegUsage
patchRegs :: Instr -> (Reg -> Reg) -> Instr
```

`insnFuture` is used to (re)construct the graph of all possible control transfers between the insns to be allocated. `regUsage` returns the sets of registers read and written by an instruction. And `patchRegs` is used to apply the allocator's final decision on virtual-to-real reg mapping to an instruction.


Clearly these 3 fns have to be written anew for each architecture. They are defined in `RegAllocInfo.lhs`. Think twice, no, thrice, before modifying them: making false claims about insn behaviour will lead to hard-to-find register allocation errors.

`AsmRegAlloc.lhs` contains detailed comments about how the allocator works. Here is a summary. The head honcho

```wiki
allocUsingTheseRegs :: [Instr] -> [Reg] -> (Bool, [Instr])
```


takes a list of instructions and a list of real registers available for allocation, and maps as many of the virtual regs in the input into real ones as it can. The returned `Bool` indicates whether or not it was successful. If so, that's the end of it. If not, the caller of `allocUsingTheseRegs` will attempt spilling. More of that later. What `allocUsingTheseRegs` does is:

- Implicitly number each instruction by its position in the input list.
- Using `insnFuture`, create the set of all flow edges -- possible control transfers -- within this set of insns.
- Using `regUsage` and iterating around the flow graph from the previous step, calculate, for each virtual register, the set of flow edges on which it is live.
- Make a real-register commitment map, which gives the set of edges for which each real register is committed (in use). These sets are initially empty. For each virtual register, attempt to find a real register whose current commitment does not intersect that of the virtual register -- ie, is uncommitted on all edges that the virtual reg is live. If successful, this means the vreg can be assigned to the realreg, so add the vreg's set to the realreg's commitment.
- If all the vregs were assigned to a realreg, use `patchInstr` to apply the mapping to the insns themselves. 

### Spilling


If `allocUsingTheseRegs` fails, a baroque mechanism comes into play. We now know that much simpler schemes are available to do the same thing and give better results. Anyways:


The logic above `allocUsingTheseRegs`, in `doGeneralAlloc` and `runRegAllocate`, observe that allocation has failed with some set R of real registers. So they apply `runRegAllocate` a second time to the code, but remove (typically) two registers from R before doing so. This naturally fails too, but returns a partially-allocated sequence. `doGeneralAlloc` then inserts spill code into the sequence, and finally re-runs `allocUsingTheseRegs`, but supplying the original, unadulterated R. This is guaranteed to succeed since the two registers previously removed from R are sufficient to allocate all the spill/restore instructions added.


Because x86 is very short of registers, and in the worst case needs three removed from R, a softly-softly approach is used. `doGeneralAlloc` first tries with zero regs removed from R, then if that fails one, then two, etc. This means `allocUsingTheseRegs` may get run several times before a successful arrangement is arrived at. `findReservedRegs` cooks up the sets of spill registers to try with.


The resulting machinery is complicated and the generated spill code is appalling. The saving grace is that spills are very rare so it doesn't matter much. I did not invent this -- I inherited it.

### Dealing with common cases fast


The entire reg-alloc mechanism described so far is general and correct, but expensive overkill for many simple code blocks. So to begin with we use `doSimpleAlloc`, which attempts to do something simple. It exploits the observation that if the total number of virtual registers does not exceed the number of real ones available, we can simply dole out a new realreg each time we see mention of a new vreg, with no regard for control flow. `doSimpleAlloc` therefore attempts this in a single pass over the code. It gives up if it runs out of real regs or sees any condition which renders the above observation invalid (fixed reg uses, for example).


This clever hack handles the majority of code blocks quickly. It was copied from the previous reg-allocator (the Mattson/Partain/Marlow/Gill one). 

## Complications, observations, and possible improvements

### Real vs virtual registers in the instruction selectors


The instruction selectors for expression trees, namely `getRegister`, are complicated by the fact that some expressions can only be computed into a specific register, whereas the majority can be computed into any register. We take x86 as an example, but the problem applies to all archs.


Terminology: `rreg` means real register, a real machine register. `vreg` means one of an infinite set of virtual registers. The type `Reg` is the sum of `rreg` and `vreg`. The instruction selector generates sequences with unconstrained use of vregs, leaving the register allocator to map them all into rregs.


Now, where was I ? Oh yes. We return to the type of `getRegister`, which despite its name, selects instructions to compute the value of an expression tree.

```wiki
getRegister :: StixExpr -> NatM Register

data Register
  = Fixed   PrimRep Reg InstrBlock
  | Any     PrimRep (Reg -> InstrBlock)

type InstrBlock -- sequence of instructions
```


At first this looks eminently reasonable (apart from the stupid name). `getRegister`, and nobody else, knows whether or not a given expression has to be computed into a fixed rreg or can be computed into any rreg or vreg. In the first case, it returns `Fixed` and indicates which rreg the result is in. In the second case it defers committing to any specific target register by returning a function from `Reg` to `InstrBlock`, and the caller can specify the target reg as it sees fit.


Unfortunately, that forces `getRegister`'s callers (usually itself) to use a clumsy and confusing idiom in the common case where they do not care what register the result winds up in. The reason is that although a value might be computed into a fixed rreg, we are forbidden (on pain of segmentation fault :) from subsequently modifying the fixed reg. This and other rules are record in "Rules of the game" inside `MachCode.lhs`.


Why can't fixed registers be modified post-hoc? Consider a simple expression like `Hp+1`. Since the heap pointer `Hp` is definitely in a fixed register, call it R, `getRegister` on subterm `Hp` will simply return Fixed with an empty sequence and R. But we can't just emit an increment instruction for R, because that trashes `Hp`; instead we first have to copy it into a fresh vreg and increment that.


With all that in mind, consider now writing a `getRegister` clause for terms of the form `(1 + E)`. Contrived, yes, but illustrates the matter. First we do `getRegister` on `E`. Now we are forced to examine what comes back.

```wiki
getRegister (OnePlus e)
   = getRegister e           `thenNat`   \ e_result ->
     case e_result of
        Fixed e_code e_fixed 
           -> returnNat (Any IntRep (\dst -> e_code ++ [MOV e_fixed dst, INC dst]))
        Any e_any 
           -> Any (\dst -> e_any dst ++ [INC dst])
```


This seems unreasonably cumbersome, yet the instruction selector is full of such idioms. A good example of the complexities induced by this scheme is shown by `trivialCode` for x86 in `MachCode.lhs`. This deals with general integer dyadic operations on x86 and has numerous cases. It was difficult to get right.


An alternative suggestion is to simplify the type of `getRegister` to this:

```wiki
getRegister :: StixExpr -> NatM (InstrBloc, VReg)
type VReg = .... a vreg ...
```


and then we could safely write

```wiki
getRegister (OnePlus e)
   = getRegister e        `thenNat`  \ (e_code, e_vreg) ->
     returnNat (e_code ++ [INC e_vreg], e_vreg)
```


which is about as straightforward as you could hope for. Unfortunately, it requires `getRegister` to insert moves of values which naturally compute into an rreg, into a vreg. Consider:

```wiki
1 + ccall some-C-fn
```


On x86 the ccall result is returned in rreg `%eax`. The resulting sequence, prior to register allocation, would be:

```wiki
# push args
call some-C-fn
# move %esp to nuke args
movl   %eax, %vreg
incl   %vreg
```


If, as is likely, `%eax` is not held live beyond this point for any other purpose, the move into a fresh register is pointless; we'd have been better off leaving the value in `%eax` as long as possible.


The simplified `getRegister` story is attractive. It would clean up the instruction selectors significantly and make it simpler to write new ones. The only drawback is that it generates redundant register moves. I suggest that eliminating these should be the job of the register allocator. Indeed:

- There has been some work on this already ("Iterated register coalescing" ?), so this isn't a new idea.

- You could argue that the existing scheme inappropriately blurs the boundary between the instruction selector and the register allocator. The instruction selector should .. well .. just select instructions, without having to futz around worrying about what kind of registers subtrees get generated into. Register allocation should be *entirely* the domain of the register allocator, with the proviso that it should endeavour to allocate registers so as to minimise the number of non-redundant reg-reg moves in the final output. 

## Selecting insns for 64-bit values/loads/stores on 32-bit platforms


Note that this stuff doesn't apply on 64-bit archs, since the `getRegister` mechanism applies there. The relevant functions are:

```wiki
assignMem_I64Code :: StixExpr -> StixExpr -> NatM InstrBlock
assignReg_I64Code :: StixReg  -> StixExpr -> NatM InstrBlock
iselExpr64        :: StixExpr -> NatM ChildCode64

data ChildCode64     -- a.k.a "Register64"
   = ChildCode64 
        InstrBlock   -- code
        VRegUnique   -- unique for the lower 32-bit temporary
```

`iselExpr64` is the 64-bit, plausibly-named analogue of `getRegister`, and `ChildCode64` is the analogue of `Register`. The aim here was to generate working 64 bit code as simply as possible. To this end, I used the simplified `getRegister` scheme described above, in which iselExpr64generates its results into two vregs which can always safely be modified afterwards.


Virtual registers are, unsurprisingly, distinguished by their `Unique`s. There is a small difficulty in how to know what the vreg for the upper 32 bits of a value is, given the vreg for the lower 32 bits. The simple solution adopted is to say that any low-32 vreg may also have a hi-32 counterpart which shares the same unique, but is otherwise regarded as a separate entity. `getHiVRegFromLo` gets one from the other.

```wiki
data VRegUnique
   = VRegUniqueLo Unique          -- lower part of a split quantity
   | VRegUniqueHi Unique          -- upper part thereof
```


Apart from that, 64-bit code generation is really simple. The sparc and x86 versions are almost copy-n-pastes of each other, with minor adjustments for endianness. The generated code isn't wonderful but is certainly acceptable, and it works.

## Shortcomings and inefficiencies in the register allocator

### Redundant reconstruction of the control flow graph


The allocator goes to considerable computational expense to construct all the flow edges in the group of instructions it's allocating for, by using the `insnFuture` function in the `Instr` pseudo-abstract type.


This is really silly, because all that information is present at the abstract C stage, but is thrown away in the translation to Stix. So a good thing to do is to modify that translation to produce a directed graph of Stix straight-line code blocks, and to preserve that structure through the insn selector, so the allocator can see it.


This would eliminate the fragile, hacky, arch-specific `insnFuture` mechanism, and probably make the whole compiler run measurably faster. Register allocation is a fair chunk of the time of non-optimising compilation (10% or more), and reconstructing the flow graph is an expensive part of reg-alloc. It would probably accelerate the vreg liveness computation too.

### Really ridiculous method for doing spilling


This is a more ambitious suggestion, but ... reg-alloc should be reimplemented, using the scheme described in "Quality and speed in linear-scan register allocation." (Traub?) For straight-line code blocks, this gives an elegant one-pass algorithm for assigning registers and creating the minimal necessary spill code, without the need for reserving spill registers ahead of time.


I tried it in Rigr, replacing the previous spiller which used the current GHC scheme described above, and it cut the number of spill loads and stores by a factor of eight. Not to mention being simpler, easier to understand and very fast.


The Traub paper also describes how to extend their method to multiple basic blocks, which will be needed for GHC. It comes down to reconciling multiple vreg-to-rreg mappings at points where control flow merges.

### Redundant-move support for revised instruction selector suggestion


As mentioned above, simplifying the instruction selector will require the register allocator to try and allocate source and destination vregs to the same rreg in reg-reg moves, so as to make as many as possible go away. Without that, the revised insn selector would generate worse code than at present. I know this stuff has been done but know nothing about it. The Linear-scan reg-alloc paper mentioned above does indeed mention a bit about it in the context of single basic blocks, but I don't know if that's sufficient.

## x86 arcana that you should know about


The main difficulty with x86 is that many instructions have fixed register constraints, which can occasionally make reg-alloc fail completely. And the FPU doesn't have the flat register model which the reg-alloc abstraction (implicitly) assumes.


Our strategy is: do a good job for the common small subset, that is integer loads, stores, address calculations, basic ALU ops (+, -, and, or, xor), and jumps. That covers the vast majority of executed insns. And indeed we do do a good job, with a loss of less than 2% compared with gcc.


Initially we tried to handle integer instructions with awkward register constraints (mul, div, shifts by non-constant amounts) via various jigglings of the spiller et al. This never worked robustly, and putting platform-specific tweaks in the generic infrastructure is a big No-No. (Not quite true; shifts by a non-constant amount are still done by a giant kludge, and should be moved into this new framework.)


Fortunately, all such insns are rare. So the current scheme is to pretend that they don't have any such constraints. This fiction is carried all the way through the register allocator. When the insn finally comes to be printed, we emit a sequence which copies the operands through memory (`%esp`-relative), satisfying the constraints of the real instruction. This localises the gruesomeness to just one place. Here, for example, is the code generated for integer divison of `%esi` by `%ecx`:

```wiki
# BEGIN IQUOT %ecx, %esi
pushl $0
pushl %eax  
pushl %edx
pushl %ecx
movl  %esi,% eax
cltd
idivl 0(%esp)
movl %eax, 12(%esp)
popl %edx  
popl %edx
popl %eax
popl %esi
# END   IQUOT %ecx, %esi
```


This is not quite as appalling as it seems, if you consider that the division itself typically takes 16+ cycles, whereas the rest of the insns probably go through in about 1 cycle each.


This trick is taken to extremes for FP operations.


All notions of the x86 FP stack and its insns have been removed. Instead, we pretend, to the instruction selector and register allocator, that x86 has six floating point registers, `%fake0` .. `%fake5`, which can be used in the usual flat manner. We further claim that x86 has floating point instructions very similar to SPARC and Alpha, that is, a simple 3-operand register-register arrangement. Code generation and register allocation proceed on this basis.


When we come to print out the final assembly, our convenient fiction is converted to dismal reality. Each fake instruction is independently converted to a series of real x86 instructions. `%fake0` .. `%fake5` are mapped to `%st(0)` .. `%st(5)`. To do reg-reg arithmetic operations, the two operands are pushed onto the top of the FP stack, the operation done, and the result copied back into the relevant register. When one of the operands is also the destination, we emit a slightly less scummy translation. There are only six `%fake` registers because 2 are needed for the translation, and x86 has 8 in total.


The translation is inefficient but is simple and it works. A cleverer translation would handle a sequence of insns, simulating the FP stack contents, would not impose a fixed mapping from `%fake` to `%st` regs, and hopefully could avoid most of the redundant reg-reg moves of the current translation.


There are, however, two unforeseen bad side effects:

- This doesn't work properly, because it doesn't observe the normal conventions for x86 FP code generation. It turns out that each of the 8 elements in the x86 FP register stack has a tag bit which indicates whether or not that register is notionally in use or not. If you do a FPU operation which happens to read a tagged-as-empty register, you get an x87 FPU (stack invalid) exception, which is normally handled by the FPU without passing it to the OS: the program keeps going, but the resulting FP values are garbage. The OS can ask for the FPU to pass it FP stack-invalid exceptions, but it usually doesn't.

  Anyways: inside NCG created x86 FP code this all works fine. However, the NCG's fiction of a flat register set does not operate the x87 register stack in the required stack-like way. When control returns to a gcc-generated world, the stack tag bits soon cause stack exceptions, and thus garbage results.

  The only fix I could think of -- and it is horrible -- is to clear all the tag bits just before the next STG-level entry, in chunks of code which use FP insns. `i386_insert_ffrees` inserts the relevant `ffree` insns into such code blocks. It depends critically on `is_G_instr` to detect such blocks.

- It's very difficult to read the generated assembly and reason about it when debugging, because there's so much clutter. We print the fake insns as comments in the output, and that helps a bit. 

## Generating code for ccalls


For reasons I don't really understand, the instruction selectors for generating calls to C (genCCall) have proven surprisingly difficult to get right, and soaked up a lot of debugging time. As a result, I have once again opted for schemes which are simple and not too difficult to argue as correct, even if they don't generate excellent code.


The sparc ccall generator in particular forces all arguments into temporary virtual registers before moving them to the final out-registers (`%o0` .. `%o5`). This creates some unnecessary reg-reg moves. The reason is explained in a comment in the code.

## Duplicate implementation for many STG macros


This has been discussed at length already. It has caused a couple of nasty bugs due to subtle untracked divergence in the macro translations. The macro-expander really should be pushed up into the Abstract C phase, so the problem can't happen.


Doing so would have the added benefit that the NCG could be used to compile more "ways" -- well, at least the 'p' profiling way.

## How to debug the NCG without losing your sanity/hair/cool


Last, but definitely not least ...


The usual syndrome is that some program, when compiled via C, works, but not when compiled via the NCG. Usually the problem is fairly simple to fix, once you find the specific code block which has been mistranslated. But the latter can be nearly impossible, since most modules generate at least hundreds and often thousands of them.


My solution: cheat.


Because the via-C and native routes diverge only late in the day, it is not difficult to construct a 1-1 correspondence between basic blocks on the two routes. So, if the program works via C but not on the NCG, do the following:

- Recompile `AsmCodeGen.lhs` in the afflicted compiler with `-DDEBUG_NCG`, so that it inserts `___ncg_debug_markers` into the assembly it emits.
- Using a binary search on modules, find the module which is causing the problem.
- Compile that module to assembly code, with identical flags, twice, once via C and once via NCG. Call the outputs `ModuleName.s-gcc` and `ModuleName.s-nat`. Check that the latter does indeed have `___ncg_debug_markers` in it; otherwise the next steps fail.
- Build (with a working compiler) the program `utils/debugNCG/diff_gcc_nat`.
- Run: `diff_gcc_nat ModuleName.s`. This will construct the 1-1 correspondence, and emits on stdout a cppable assembly output. Place this in a file -- I always call it synth.S. Note, the capital S is important; otherwise it won't get cpp'd. You can feed this file directly to ghc and it will automatically get cpp'd; you don't have to do so yourself.
- By messing with the `#define`s at the top of `synth.S`, do a binary search to find the incorrect block. Keep a careful record of where you are in the search; it is easy to get confused. Remember also that multiple blocks may be wrong, which also confuses matters. Finally, I usually start off by re-checking that I can build the executable with all the `#define`s set to 0 and then all to 1. This ensures you won't get halfway through the search and then get stuck due to some snafu with gcc-specific literals. Usually I set `UNMATCHED_GCC` to 1 all the time, and this bit should contain only literal data. `UNMATCHED_NAT` should be empty. 

`diff_gcc_nat` was known to work correctly last time I used it, in December 01, for both x86 and sparc. If it doesn't work, due to changes in assembly syntax, or whatever, make it work. The investment is well worth it. Searching for the incorrect block(s) any other way is a total time waster. 
