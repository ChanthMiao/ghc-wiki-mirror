# LLVM Back-end Design


The current design tries to fit into GHC's pipeline stages as an alternative to the C and NCG back-ends as seamlessly as possible. This allows for quicker development and focus on the core task of LLVM code generation.


The LLVM pipeline works as follows:

- New path for LLVM generation, separate from C and NCG. (path forks at compiler/GHC/Driver/CodeOutput.hs, same place where C and NCG fork).
- LLVM code generation will output LLVM assembly code.
- The LLVM assembly code is translated to an object file as follows

  - The LLVM optimizer is run which is a series of bitcode to bitcode optimization passes (using the `llc` tool).
  - Finally an object file is created from the LLVM bitcode (using the `llc` tool)
- This brings the LLVM path back to the other back-ends.
- The final state is the Link stage, which uses the system linker as with the other back-ends.


Here is a diagram of the pipeline:

```wiki
Cmm -> (codeOutput) --->(ncg) Assembler      -->(mangler, splitter) --> ('As' phase) -----> Object Code --> (link) --> executable
                        \---> (llvm) LLVM IR --> LLVM Optimizer     --> ('llc' phase) -----/
```


This approach was the easiest and thus quickest way to initially implement the LLVM back-end. Now that it is working, there is some room for additional optimisations. A potential optimisation would be to add a new linker phase for LLVM. Instead of each module just being compiled to native object code ASAP, it would be better to keep them in the LLVM bitcode format and link all the modules together using the LLVM linker. This enable all of LLVM's link time optimisations. All the user program LLVM bitcode will then be compiled to a native object file and linked with the runtime using the native system linker.

# Implementation

## Framework

- New **-fllvm** code generation pipeline, involved modifying:

  - `main/CodeOutput.lhs` - Selects appropriate back-end for code generation (C, NCG, LLVM).
  - `main/DynFlags.hs` - Stores GHC configuration (command line options, compile time options... ect). Added `HscLlvm` target type.
  - `ghc.cabal.in` - Stores modules/files to compile for ghc. Added new LLVM files and directory stored under `llvmGen`, and new CPP flag to enable the LLVM code generator (`-DLLVM`).
  - `ghc.mk` - Added new `GhcWithLlvmCodeGen` option which can be set in `build.mk` to `YES` to enable the LLVM code generator.
  - `main/DriverPhases.hs` - Added `LlvmAs` phase to invoke the compilation of LLVM bitcode/IR to an object file. After this phase linking can occur.
  - `main/DriverPipeline.hs` - Added code for new `LlvmAs`, `LlvmOpt` and `LlvmLlc` phases.

    - `LlvmAs` - Invokes `llvm-as` tool to compile a llvm assembly file ('.ll') to a bitcode file (`.bc`).
    - `LlvmOpt` - Invokes the llvm `opt` tool to optimise the module. Just use the llvm standard optimisation groups of `O1`, `O2`, `O3`, depending on the optimisation level passed to 'ghc' by the user.
    - `LlvmLlc` - Invokes the llvm `llc` tool to generate the machine code ('.s' file) from the optimised bitcode. 'As' stage runs next, part of existing 'ghc' pipeline.
  - `SysTools.lhs` - Stores the path and default settings of the system tools needed, so for LLVM back-end this is `llvm-as`, `opt` and `llc`.


The LLVM pipeline works as specified above. Code generation phase occurs, using the `dflags` option data the appropriate generator is selected (which is the Llvm back-end is `-fllvm` has been specified on the command line). After code generation, the next phase is determined, this is done from the `HscLlvm` target data constructor which is selected at ghc startup by `DynFlags.hs`. The next phase is `LlvmAs` which will compile the text IR to an LLVM bitcode file (equivalent to `llvm-as` tool). After this the `LlvmLlc` phase is run, which produces a native object file from the llvm bitcode file (equivalanet to the `llc` tool). At this stage, the output from all three back-ends should be 'equivalent'. After this phase, the `StopLn`, or linking phase occurs which should result in the end result. Compiling some Haskell code with the c-backend and some with the llvm-backend and linking them together is supported.

## LLVM Code Generation


For LLVM code generation we need a method for representing and generating LLVM code. The [LLVM FAQ](http://llvm.org/docs/FAQ.html#langirgen) suggest the following possible approaches: 

- Call into LLVM Libraries using FFI (can probably use [Haskell LLVM Bindings](http://hackage.haskell.org/package/llvm)) 
- Emit LLVM Assembly (approach taken by [EHC's](http://www.cs.uu.nl/wiki/Ehc/WebHome) LLVM Back-end, can use the [ module](https://subversion.cs.uu.nl/repos/project.UHC.pub/trunk/EHC/src/ehc/LLVM.cag) developed by them for this) 
- Emit LLVM Bitcode (can't see any reason to do this)


The approach taken was to use the LLVM module from [EHC](http://www.cs.uu.nl/wiki/Ehc/WebHome). This module contains an abstract syntax representation of LLVM Assembly and the ability to pretty print it. It has been heavily modified to increase its language coverage as it was missing several LLVM constructs which were needed. Ideally we would like to add a second pretty printer which calls into the LLVM C++ API to generate LLVM Bitcode. This should hopefully decrease the compile times and make the back-end more resilient to future changes to LLVM Assembly. The LLVM Haskell binding (first option) wasn't used as it represents LLVM at a very high level, which isn't appropriate for the back-end.

## Register Pinning


The new back-end supports a custom calling convention to place the STG virtual registers into specific hardware registers. The current approach taken by the C back-end and NCG of having a fixed assignment of STG virtual registers to hardware registers for performance gains is not implemented in the LLVM back-end. Instead, it uses a custom calling convention to support something semantically equivalent to register pinning. The custom calling convention passes the first N variables in specific hardware registers, thus guaranteeing on all function entries that the STG virtual registers can be found in the expected hardware registers. This approach is believed to provide better performance than the register pinning used by NCG/C back-ends as it keeps the STG virtual registers mostly in hardware registers but allows the register allocator more flexibility and access to all machine registers.


For some more information about the use of a custom calling convention see [here (Discussion between Chris Lattner and David Terei)](http://www.nondot.org/sabre/LLVMNotes/GlobalRegisterVariables.txt)

## Code Generation


Code generation consists of translating a list of `GenCmmTop` data types to LLVM code. `GenCmmTop` has the following form:

```wiki
data GenCmmTop d h g
  = CmmProc          -- Function
        h            -- Extra header such as the info table
        CLabel       -- Procedure name
        CmmFormals   -- Argument locals live on entry
        g            -- Control flow graph
 
 | CmmData           -- Static data
        Section      -- Type
        [d]          -- Data

data BlockId = BlockId Unique
data GenBasicBlock i = BasicBlock BlockId [i]
type CmmBasicBlock = GenBasicBlock CmmStmt

newtype ListGraph i = ListGraph [[GenBasicBlock i]

type RawCmmTop = GenCmmmTop CmmStatic [CmmStatic] (ListGraph CmmStmt)
-- new type RawCmm = Cmm [RawCmmTop] : A list version of RawCmmTop, actual code is different, but its effectively this.
```


That is, it consists of two types, static data and functions. Each can largely be handled separately. Just enough information is needed such that pointers can be constructed to them and in many cases this information can be gathered from assumptions and constraints on Cmm.


After all the polymorphic types are bound we get this:

```wiki
RawCmm = [
    CmmProc [CmmSatic] CLabel [LocalReg] [BlockId [CmmStmt]]
  | CmmData Section [CmmStatic]
]

data Section = Text | Data | ReadOnlyData | RelocatableReadOnlyData | UninitialisedData | ReadOnlyData16 | OtherSection String
```


The code generator lives in `llvmGen` with the driver being `llvmGen/LlvmCodeGen.lhs`.


A large part of the code generation is keeping track of defined variables/functions and their type. An `LlvmEnv` construct is used for this. It is simply a dictionary storing function/variable names with their corresponding type information. This is used to create correct references/pointers between variables and functions.

### Unregisterised Vs. Registerised


Code generation can take place in two general modes, `unregisterised` and `registerised`. There are two major differences from a back-end code generation point of view. Firstly, in unregisterised mode a optimisation feature called `TABLES_NEXT_TO_CODE` is disabled. This means that the `h` field of `CmmProc` is empty. In registerised mode it instead contains the `CmmStatic` data for the procedures info table which must be placed just before the procedure in the generated code so that both the info table and procedure can be accessed through one pointer. This optimisation can be disabled separately though in `registerised` mode.


The other major change is the use of pinned global registers. The `Cmm` language includes a concept called registers. These are used like machine registers or variables in C to store the result of expressions. Unlike `LLVM` they are mutable. `Cmm` includes two types of registers as you can see below: 

```wiki
data CmmReg 
  = CmmLocal  LocalReg
  | CmmGlobal GlobalReg
  deriving( Eq, Ord )

data LocalReg = LocalReg Unique CmmType
```


A `LocalReg` is a temporary general purpose register used in a procedure with scope of a single procedure. A `GlobalReg` on the other hand has global scope and a specific use. They are used just like machine registers, with a Stack Pointer and Heap Pointer registers creating a virtual machine (`STG`). `GlobalReg` is of the form:

```wiki
data GlobalReg
  -- Argument and return registers
  = VanillaReg			-- pointers, unboxed ints and chars
	{-# UNPACK #-} !Int	-- its number
 	VGcPtr

  | FloatReg		-- single-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | DoubleReg		-- double-precision floating-point registers
	{-# UNPACK #-} !Int	-- its number

  | LongReg	        -- long int registers (64-bit, really)
	{-# UNPACK #-} !Int	-- its number

  -- STG registers
  | Sp			-- Stack ptr; points to last occupied stack location.
  | SpLim		-- Stack limit
  | Hp			-- Heap ptr; points to last occupied heap location.
  | HpLim		-- Heap limit register
  | CurrentTSO		-- pointer to current thread's TSO
  | CurrentNursery	-- pointer to allocation area
  | HpAlloc		-- allocation count for heap check failure

		-- We keep the address of some commonly-called 
		-- functions in the register table, to keep code
		-- size down:
  | EagerBlackholeInfo  -- stg_EAGER_BLACKHOLE_info
  | GCEnter1		-- stg_gc_enter_1
  | GCFun		-- stg_gc_fun

  -- Base offset for the register table, used for accessing registers
  -- which do not have real registers assigned to them.  This register
  -- will only appear after we have expanded GlobalReg into memory accesses
  -- (where necessary) in the native code generator.
  | BaseReg

  -- Base Register for PIC (position-independent code) calculations
  -- Only used inside the native code generator. It's exact meaning differs
  -- from platform to platform (see module PositionIndependentCode).
  | PicBaseReg

  deriving( Show )
```


In unregisterised mode these global registers are all just stored in memory in the heap. A specific pass operating on Cmm that takes place just before code generation thus transforms code such as:

```wiki
__stginit_ZCMain() {
        { update_frame: <none>
        }
    cfF:
        Sp = Sp + 4;
        jump (I32[Sp - 4]) ();
}
```


into the following unregisterised form for code generation:

```wiki
__stginit_main::Main() {
        { []
        }
    crF:
        I32[MainCapability+92] = I32[MainCapability+92] + 4;
        jump (I32[I32[MainCapability+92] - 4]) ();
}
```


Where `MainCapability` is a label to the start of a RTS defined structure storing all the global registers.


In registerised mode as many of these global registers are assigned permanently to fixed hardware registers. This is done as it greatly improves performance. As these registers are accessed very frequently needing to load and store to memory for accessing adds a great cost. So for example on `x86` the following map between `Cmm` global registers and `x86` hardware registers exists:

```wiki
Base -> %EBX
Sp   -> %EBP
Hp   -> %EDI
R1   -> %ESI
```


These are all the available `callee save` registers on x86. `callee save` are used as in ghc generated code now saving and restoring of these registers are needed due to there new special use and because GHC uses continuation passing style, so a `'ret'` statement is never actually generated. And since they are `callee save`, foreign code can also be called without any need to handle the `Cmm` registers.

## CmmData

`CmmData` takes the following form:

```wiki
newtype CmmData = CmmData Section [CmmStatic]

data CmmStatic
  = CmmStaticLit      CmmLit  -- static value
  | CmmUninitialised  Int     -- n bytes of uninitialised data
  | CmmAlign          Int     -- align to next N byte boundary
  | CmmDataLabel      CLabel  -- label current position in code
  | CmmString         [Word8] -- string of 8 bit values

data CmmLit
  = CmmInt            Integer  Width    -- 2 compliments, truncated int
  | CmmFloat          Rational Width    -- float
  | CmmLabel          CLabel            -- &l1
  | CmmLabelOff       CLabel Int        -- &l1 + offset
  | CmmLabelDiffOff   CLabel CLabel Int -- &l1 - &l2 + offset
  | CmmBlock          BlockId           -- address of code label
  | CmmHighStackMark                    -- max stack space used during a procedure

data Width = W8 | W16 | W32 | W64 | W80 | W128
```


Code generation takes place mainly in `llvmGen/LlvmCodeGen/Data.hs`, driven by the main Llvm compiler driver, {{llvmGen/LlvmCodeGen.lhs}}}.


The code generation for data occurs in two phases, firstly the types and all data is generated except for address values. Then the address values are resolved. This two step method is used as in the first pass, we don't know if a address refers to an external address or a procedure/data structure in the current LLVM module. We also need the type information in LLVM to create a pointer.

### 1st Pass : Generation


All `CmmStatic` is translated to LLVM structures.

## CmmStaticLit


These are translated when possible as follows:

- `CmmInt` -\> Reduced to Int and then an appropriate `LMInt` of correct size is created. As LLVM supports any bit size, this is very straight forward.
- `CmmFloat` -\> Translated to a double, detecting NAN and INFINITY correctly. Then correct LLVM type (`float`, `double`, `float80`, `float128`) is selected.
- `CmmLabel` -\> Left untranslated at first, later resolved once we have determined types. As pointers are cast to word size ints, we can still determine types.
- `CmmLabelOff` -\> As above.
- `CmmLabelDiffOff` -\> As above.
- `CmmBlock` -\> `BlockId` is changed to a `CLabel` and then treated as a `CmmLabel` static type.
- `CmmHighStackMark` -\> Panic occurs if this type is encountered.

#### CmmUninitialised


For this, a zeroed array of `8bit` values is created of correct size.

#### CmmAlign & CmmDataLabel


The LLVM back-end can't handle `CmmAlign` or `CmmDataLabel`. A panic occurs if either is encountered. A `CmmDataLabel` is expected at the very start of each list of `CmmStatic`. It is removed and used as the name for the structure and constant instance.

#### CmmString


This is translated into a LLVM string. Ascii characters are used when they are printable, escaped hex values otherwise. A null termination is added.

### 2nd Pass : Resolution


After the first pass, all types have been determined and all data translated except for address values (CLabel's). All generated llvm data is added to a Map of string to `LlvmType`, string being the data structure name. All `CmmProc's` are added to the map as well, they don't need to be properly passed though, just their names retrieved as they have a constant type of void return and no parameters.


Now appropriate pointers can be generated using the type information from the map and LLVM's `getelementptr` instruction. These are then all passed to int's to allow the types of structures to be determined in advance. If a pointer doesn't have a match in the Map, it is assumed to refer to an external (outside of this module) address. An external reference is declared for this address as:

```wiki
@label = external global [0 * i32]
```


Where i32 is the pointer size. (i64 if on 64 bit).

## CmmProc


A Cmm procedure is made up of a list of basic blocks, with each basic block being comprised of a list of CmmStmt???s.


Code generation takes place mainly in `llvmGen/LlvmCodeGen/CodeGen.hs`, driven by the main Llvm compiler driver, {{llvmGen/LlvmCodeGen.lhs}}}.


While Cmm procedures include a specification for arguments and a return type there is in fact only one type used, that is a procedure which takes no arguments and returns void. The reason for this is that the STG registers are instead used for the purpose of argument passing and the returning of results.Another detail of the Cmm code produced by GHC is that
it doesn???t contain any return statements. Instead a style of code called continuation passing is used in which the control is explicitly passed in the form of a continuation, and all Cmm procedures produced by GHC are instead terminated by tail calls.


Below is the Haskell definition for Cmm statements and expressions.

```wiki
data CmmStmt
  = CmmNop                         
  | CmmComment    FastString
  | CmmAssign     CmmReg CmmExpr
  | CmmStore      CmmExpr CmmExpr
  | CmmCall       CmmCallTarget HintedCmmFormals HintedCmmActuals CmmSaftey CmmReturnInfo
  | CmmBranch     BlockId
  | CmmCondBranch CmmExpr BlockId
  | CmmSwitch     CmmExpr [Maybe BlockId]
  | CmmJump       CmmExpr HintedCmmActuals

data CmmExpr,
  = CmmLit       CmmLit
  | CmmLoad      CmmExpr CmmType
  | CmmReg       CmmReg
  | CmmMachOp    MachOp [CmmExpr]
  | CmmStackSlot Area Int
  | CmmRegOff    CmmReg Int

type CmmFormals = [CmmFormal]
type CmmFormal  = LocalReg
```

### CmmExpr


CmmExpr???s are handled in a relatively straight-forward manner. The most interesting aspect of their compilation to LLVM is the return type of functions in the LLVM back-end which
compile CmmExpr???s. This gives an idea of the compilation process, as while each expression must be handled differently, they all return the same type when compiled to LLVM code by
the back-end.

```wiki
-- Return type of LLVM fucntions that compile CmmExpr's
type ExprData = (LlvmEnv , LlvmVar , LlvmStatements , [LlvmCmmTop] )
```

- **LlvmEnv**: During code generation for an expression, an external Cmm Label may be encountered for the first time. An external reference for it will be created and return as part of the \[LlvmCmmTop\] list. It is also added to the current environment.
- **LlvmVar**: All expressions share the property that there execution results in a single value which can be stored in a variable. This LLVM local variable holds the result of the CmmExpr. This allows for statements to very easily use and access the result of an expression.
- **LlvmStatements**: A CmmExpr may require several LLVM statements to implement, they are returned in this list and must be executed before the LlvmVar is accessed.
- **\[LlvmCmmTop\]**: An externally declared Cmm Label can be encountered at any point as Cmm requires no external declaration. LLVM though requires that these labels do have an external declaration and in this list such declarations are returned. They add new global variables to the LLVM module.

### CmmStmt


Statements are also handled in a fairly straight-forward manner process involved can be detailed most simply by studying the return type of functions in the LLVM back-end which deal with compiling CmmStmt???s. Statements just as expressions also all return the same basic type when compiled to LLVM code by the back-end. This type is shown below.

```wiki
type StmtData = (LlvmEnv , [LlvmStatement ] , [LlvmCmmTop ] )
```

- **LlvmEnv**: As compiling a Cmm statement usually involves also compiling a Cmm expression, this LLVM Environment performs the same purpose of returning an updated environment if new external Cmm Label???s have been encountered. This first case updates the environments global map, as a new global variable has been created. In the case of a CmmStore statement though, a Cmm local register may be encountered for the first time. It will be allocated on the stack and added to the local map of the environment.
- **LlvmStatements**: A CmmStatment is compiled to a list of LLVM Statements.
- **\[LlvmCmmTop\]:** Serves the same purpose as it does for Cmm expression code generation.

### Handling LLVM's SSA Form


Handling LLVM???s SSA Form One of the main difference between Cmm and LLVM Assembly is the requirement that LLVM Assembly be in single static assignment form. Thankfully, this is actually quite easy to handle. LLVM allows for data to be explicitly allocated on the stack, using its alloca instruction. This instruction provides an alternative to producing SSA formed code. If a mutable variable is needed, then it is allocated on the stack with alloca. The value returned from this instruction is a pointer to the stack memory and this memory location can be read from and written to just like any other memory location in LLVM by using the load and store instructions respectively. While this initially allocates all these variables on the stack and doesn???t use any registers, LLVM includes an optimisation pass called mem2reg which is designed to correct this, changing explicit stack allocation into SSA form instead which can use machine registers when compiled to native code. This approach to handling LLVM???s SSA form is in fact the method that the LLVM developers themselves recommend.

### Handling Registered Code


Handling registerised Cmm Code involves handling the pinning of the STG virtual registers and the TABLES_NEXT_TO_CODE optimisation.


To handle the TABLES_NEXT_TO_CODE optimisation, the LLVM backend uses a gnu as feature called subsections.

[http://sourceware.org/binutils/docs-2.20/as/Sub_002dSections.html\#Sub_002dSections](http://sourceware.org/binutils/docs-2.20/as/Sub_002dSections.html#Sub_002dSections)


The way this works is that you can put stuff into a section like '.text 2', where 2 is a subsection of .text When run, 'as' orders the subsections. So all you need to do is arrange for the info-table to be in section '.text n' and the code in section '.text n+1'. Each info-table and its code goes in its own subsection. The nice thing is, this is purely a gnu as feature. When it compiles the assembly to object code, the subsections aren't present in the object code, so you don't get 100's of sections that take up space and slow down linking.


This subsection feature is only supported by the GNU assembler. On Linux and Windows this is the assembler used so the above trick is used. On Mac OSX we don't use the GNU assembler, but the Mac OS X BSD based assembler which has no support for subsections. So on OSX we actually use a mangler to fix up the code to support TNTC.


To handle the pinning of the STG registers the LLVM back-end uses a custom calling convention that passes the first n arguments of a function call in the specific registers that the STG registers should be pinned to. Then, whenever there is function call, then LLVM back-end generates a call with the correct STG virtual registers as the first n arguments to that call. Why does this work? It works as it guarantees that on the entrance to any function, the STG registers are currently stored in the correct hardware registers. It also guarantees this on a function exit since all Cmm functions that GHC generates are exited by tail calls. In the function itself, the STG registers can be treated just like normal variables, read and written to at will.


The new calling convention was included by the LLVM developers in LLVM 2.7. It uses calling convention number 10. At the moment it supports x86-32/64.

## After Code Generation


After code generation there are three more stages, they are simply calls to the LLVM tools though:

- **LLVM Optimisation**: In this section a range of LLVM???s optimisations are applied to the llvm file, resulting in a new optimised bitcode file. This is done by simply invoking the LLVM opt tool on the stage input file. The optimisations are selected using the standard optimisation groups of '-mem2reg' (O0), '-O1', '-O2' provided by opt, depending on the level of optimisation requested by the user when they invoked GHC. LLVM also supports '-O3' but that needs to be set by the user through -opt-lo-O3.
- **LLVM Compiler**: This is the final stage in which the input LLVM bitcode file is compiled to native assembly for the target machine. This is done by simply invoking the LLVM llc tool on the stage input file.
