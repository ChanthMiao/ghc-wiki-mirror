
As of 7.10, GHC has basic functionality to generate DWARF-compliant
debugging information with its binaries. This opens up a completely
new way of debugging or profiling Haskell programs: Instead of
instrumenting the program or the runtime system, we teach
external debugging tools to make sense of a running Haskell program.
This means that we gain debugging and profiling capabilities in
situations where existing profiling approaches could not help us, such
as for crashes or code that we cannot meaningfully instrument.


There are a few caveats to this method. GHC optimisations can
be very aggressive in reorganizing the code and avoiding redundancies
at runtime. This means that even with good heuristics, there will be
situations where information is lost. DWARF-based
debugging tools also make assumptions about the code that are more
geared towards C-like languages, and get confused for Haskell programs.


While the infrastructure should be perfectly stable and
safe to use, inspecting Haskell programs like this is still very much
"wild west". Having a good working knowledge of low-level Haskell
execution is definitely a good idea. We hope that some experience will
help us improve the situation.

## Features


There main components to GHC's DWARF support are:

- The generation of debugging symbols in generated object code when `-g` is passed to GHC

  - This enables seeing and using them in tools like `gdb`, `valgrind`, `perf` etc.
- "stack unwinding" support: The GHC runtime being able to print stack traces:

  - when it crashes
  - when requested by sending a signal to the RTS
  - via the `GHC.ExecutionStack` API


You can read more about them on [DWARF/Status](dwarf/status).


On using these features:

- Stack unwinding support is enabled by passing `--enable-dwarf-unwind` at GHC build-configuration time.
- Debugging symbols are generated when you compile Haskell modules to object files by passing `-g`.
- `--enable-dwarf-unwind` and -`g` are technically independent (`-g` can be useful for other tools even when GHC's own stack unwinding support is disabled), but stack unwinding support in GHC when `-g` was not given to compiled code is quite useless because there won't be much content in printed stack traces (details on that below).
- The debugging symbols can be stripped (removed) after the build using e.g. the Unix `strip` tool.
- When using `Cabal`, `-g` passed to GHC is controlled by the `--enable-debug-info` flag, and stripping of debugging symbols controlled by `--enable-library-stripping` and `--enable-executable-stripping`.

## Basic Set-Up


To keep things simple for the moment, let's say we compile the
tried-and-true `fib`:

```wiki
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
main :: IO ()
main = print $ fib 20
```


Just like with GCC, the command line flag to generate DWARF sections is `-g`:

```wiki
ghc -O -g -rtsopts fib.hs
```


For Mac Os, debug information is kept separate from the
binary, so we first have to package it using `dsymutil`:

```wiki
dsymutil fib
```


We can now check that DWARF information was generated
correctly using `objdump` or `dwarfdump` respectively:

```wiki
[Linux] objdump -Wi fib
[MacOs] dwarfdump fib.dSYM
```


Either should show us something of the form:

```wiki
TAG_compile_unit [1] *
 AT_name( "fib.hs" )
 AT_producer( "The Glorious Glasgow Haskell Compilation System 7.9.20141217" )
 AT_language( DW_LANG_Haskell )
```


This tells us that we have DWARF information about the compilation
unit `fib.hs`. Note that the Haskell language ID `0x18` is not
recognized by all tools yet, so you might see an "Unknown" there
instead.


It is important to realize that - in contrast to instrumentation -
adding DWARF debug information does not change the code
sections of the executable. In fact, we can strip them without
changing the performance characteristics of the program at all:

```wiki
$ cp fib fib_stripped
$ strip fib_stripped
$ time ./fib
1134903170

real	0m13.171s
user	0m13.160s
sys	0m0.001s
$ time ./fib_stripped
1134903170

real	0m13.179s
user	0m13.168s
sys	0m0.000s
$ 
```


This is a very good property in certain situations - DWARF information
is simply painless to add and throw away. Furthermore, we can now
profile code and always be sure that the measurements actually reflect
real-life performance.

## Debugging


At this point, we can simply invoke `gdb` and set breakpoints (TODO -
doesn't quite work on Mac yet):

```wiki
gdb -q fib
Reading symbols fib...done.
(gdb) break fib.hs:3
Breakpoint 1 at 0x403d98: file fib.hs, line 3.
(gdb) run +RTS -V0
Starting program: fib +RTS -V0

Breakpoint 1, Main_zdwfib_info () at fib.hs:3
3	fib 1 = 1
(gdb) 
```


At this point we have stopped the Haskell program at the
given line. We can now step a bit further to see the program working
its magic:

```wiki
(gdb) s
Main_zdwfib_info () at fib.hs:4
4	fib n = fib (n-1) + fib (n-2)
(gdb) s
Main_zdwfib_info () at fib.hs:2
2	fib 0 = 0
(gdb) s
c3Vy_info () at fib.hs:4
4	fib n = fib (n-1) + fib (n-2)
(gdb) s
Main_zdwfib_info () at fib.hs:2
2	fib 0 = 0
(gdb) s

Breakpoint 1, Main_zdwfib_info () at fib.hs:3
3	fib 1 = 1
(gdb) s
c3Vy_info () at fib.hs:4
4	fib n = fib (n-1) + fib (n-2)
(gdb)
```


Note that passing `+RTS -V0` as program command line parameter is
necessary for this to work, as otherwise we would end up stepping into
the RTS timer.


Furthermore, we can back-trace to get an idea of where we were coming
from:

```wiki
(gdb) bt
#0  c3Vy_info () at fib.hs:4
#1  0x0000000000403de0 in Main_zdwfib_info () at fib.hs:2
          [...]
#17 0x0000000000403de0 in Main_zdwfib_info () at fib.hs:2
#18 0x0000000000403e98 in Main_main2_info () at libraries/base/GHC/Show.hs:145
#19 0x0000000000481aa0 in ccNI_info () at libraries/base/GHC/IO/Handle/Text.hs:603
#20 0x000000000067d968 in ?? () at rts/Exception.cmm:332
#21 0x0000000000682530 in ?? ()
Backtrace stopped: previous frame identical to this frame (corrupt stack?)
(gdb) info symbol 0x000000000067d968
stg_catch_frame_info in section .text of /home/cserv1_a/soc_pg/scpmw/leeds/tests/haskell/fib/fib
(gdb) info symbol 0x0000000000682530
stg_stop_thread_info in section .text of /home/cserv1_a/soc_pg/scpmw/leeds/tests/haskell/fib/fib
```


The stack trace you get at this point might look a bit different - to
get the above result you will need to compile both libraries and the
runtime system with debugging support (`GhcRtsHcOpts += -g` and
`GhcLibHcOpts += -g`).


Let us have a closer look at the stack:

- First we have a number of return closures to our `fib`
  function. That the stack is so "clean" means that we have no lazy
  evaluation going on, which is a result of compiling
  with optimisations above (`-O`). Perhaps unintuitively, simple
  optimisations often improve stack trace clarity (the following flags,
  all part of `-O`, seem particularly helpful:
  `-fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fenable-rewrite-rules`).

- On the other hand, `main` does not appear in the stack any more. In
  fact, where it should be we only see `show` and `putStrLn`
  respectively, which is what `print` unfolds to. Here GHC figured out
  that it can tail-call, causing a "hole" in our stack trace.

- Finally, the two last frames are RTS frames, corresponding to
  `stg_catch_frame` (the top-level exception handler) and
  `stg_stop_thread`, which is the stack frame handling returning the
  thread to the RTS. This is where the debugger runs out of guidance
  from the DWARF information and complains with the "corrupt stack?"
  error message.

## Debugging while bootstrapping GHC


Imagine you have a crashing GHC *stage2* compiler, but *stage1* works.
It is a rather frustrating experience to identify where the crash is coming
from. With DWARF backtraces the situation improves a bit, as you get
source locations for the backtrace, which can give valuable input.



To do this, you can compile the second stage with `-g`, like


```
make V=1 GhcStage2HcOpts="-O1 -g"
```


(or equivalent), assuming the miscompilation is triggered by an optimisation.



Then modify the GHC wrapper `inplace/bin/ghc-stage2` to end with:


```
rm -f /tmp/${USER}-gdb
echo run -B"$topdir" ${1+"$@"} > /tmp/${USER}-gdb
exec gdb --tui "$executablename" -x /tmp/${USER}-gdb
```


This teaches the wrapper to run GHC under GDB's supervision
with the passed command-line arguments.


Then simply invoke `make` again, and you should find yourself at the
crash site.

## Profiling


Another application of DWARF information is that we can use low-level
profiling to map performance data to source code. On Linux, we can use
the `perf` utility to gather and annotate such data, for example:

```wiki
$ perf record ./fib
1134903170
[perf record: Woken up 9 times to write data ]
[perf record: Captured and wrote 2.027 MB perf.data (~88582 samples) ]
$ perf annotate
```


This yields us a profiling view that looks as follows:

```wiki
       ???    0000000000403de0 <c3Vl_info>:
       ???    fib :: Int -> Int
       ???    fib 0 = 0
       ???    fib 1 = 1
       ???    fib n = fib (n-1) + fib (n-2)
 37.96 ???      movq   $0x403e10,0x0(%rbp)
 43.54 ???      mov    0x8(%rbp),%rax
  5.09 ???      lea    -0x2(%rax),%r14
  2.38 ???      mov    %rbx,0x8(%rbp)
 11.03 ???    ??? jmpq   ffffffffffffffff
       ???      nop
       ???      add    %al,(%r8) 
       ???      add    %al,(%rax)
```


DWARF information allows `perf` to locate the source code that belongs
to the "hot" assembler code. Note that the fairly nonsensical `add %al,(%r8)` instructions is info table data, which `perf` is
interpreting as code.

## lldb

`lldb` can interpret the DWARF data produced by GHC, and in some respects it can do it better than `gdb`.

```wiki
$ lldb ./fib
Current executable set to './fib' (x86_64).
(lldb) break set -f fib.hs -l 3
Breakpoint 1: where = fib`rnf_info + 207 [inlined] c2rw_entry, address = 0x00000000004056f7
(lldb) r
Process 1901610 launched: './fib' (x86_64)
Process 1901610 stopped
* thread #1: tid = 1901610, 0x00000000004056f7 fib`rnf_info [inlined] c2rw_entry at fib.hs:3, name = 'fib', stop reason = breakpoint 1.1
    frame #0: 0x00000000004056f7 fib`rnf_info [inlined] c2rw_entry at fib.hs:3
   1    fib :: Int -> Int
   2    fib 0 = 0
-> 3    fib 1 = 1
   4    fib n = fib (n-1) + fib (n-2)
   5    main :: IO ()
   6    main = print $ sum [fib i | i <- [3..20]]
(lldb) image lookup -a 0x00000000004056f7 -v
      Address: fib[0x00000000004056f7] (fib..text + 10967)
      Summary: fib`rnf_info + 207 [inlined] c2rw_entry
               fib`rnf_info + 207 [inlined] c2rr_entry
               fib`rnf_info + 207 at
       Module: file = "./fib", arch = "x86_64"
  CompileUnit: id = {0x00000000}, file = "/data/users/bnitka/tmp/fib.hs", language = "Language(language = 0x0018)"
     Function: id = {0x000003b1}, name = "fib", range = [0x0000000000405628-0x0000000000405707)
     FuncType: id = {0x000003b1}, clang_type = "void (void)"
       Blocks: id = {0x000003b1}, range = [0x00405628-0x00405707)
               id = {0x00000495}, ranges = [0x00405675-0x0040567f)[0x0040567f-0x00405685)[0x004056e2-0x004056e7)[0x004056d2-0x004056e2)[0x00405685-0x004056c9)[0x004056f7-0x00405707)[0x004056e7-0x004056f7), name = "c2rr_entry"
               id = {0x00000539}, range = [0x004056f7-0x00405707), name = "c2rw_entry"
    LineEntry: [0x00000000004056f7-0x0000000000405720): /data/users/bnitka/tmp/fib.hs:3:9
       Symbol: id = {0x00000057}, range = [0x0000000000405628-0x0000000000405707), name="rnf_info"
```


Note that we get line number + column number, AFAICT there's no way to do that in gdb.

## Open Issues


The example should give an idea of the potential we have
here. Unfortunately, it is still quite easy to run into
show-stoppers. The following list shows the most pressing issues on
the way to truly "supporting" DWARF-based debugging:


Debugger problems - these could go away by either patching the used
debugging tools or replacing them with our own:

- Note that both `perf` as well as `gdb` refer to blocks using their
  backend names (e.g. `c3Vl_info`). This is not GHC's fault - we
  correctly set the `DW_AT_name` for `DW_TAG_subprogram` records. The
  current state of investigation is that debugging tools
  prefer to use language-dependent "unmangling" of symbol names. We
  probably will have to patch these programs to do something sensible
  for Haskell code.

- Furthermore, the symbol names in `gdb`'s backtrace are
  wrong -- notice the `??` entries. On the other hand, note that \`info
  symbol\` still gives us the right answer. What is happening
  here is that `gdb` looks up the symbol with an offset of 1, as that
  makes sense for C-like programs (see `[Note: Info Offset]` in
  `compiler/nativeGen/Dwarf/Types.hs` for details). This is another
  thing we probably have to patch out.

- While it is possible to use `perf`, it seems to cope rather badly
  with the fragmented nature of Haskell code. Furthermore, the
  fine-grained performance data it produces do not always sync up well
  with high-level Haskell code. It might be a better idea to map
  performance to Core instead - or maybe we could, again, contribute
  to `perf` so it learns to represent Haskell programs better.

- Currently debugging seems to not work at all on Mac Os - both `gdb`
  and `lldb` seem to simply crash once you set breakpoints. Possibly
  something overwriting info tables? Investigation necessary.


Runtime problems - can probably be fixed by using Haskell-specific
tools and/or improving infrastructure further:

- When trying to look at stack traces, there are two characteristics
  of Haskell code leading to problems. The first one is that
  tail-calls are very common, and not entirely straight-forward to
  prevent. This is significant, because sometimes we positively want
  to know whether a certain function is on the stack frame or not.
  Arash Rouhani has been working on a way to implement
  `forceStackFrame`, hopefully we'll have something like that
  eventually.

- The second issue is lazy evaluation. A thunk update will be
  represented as a rather non-descript `update_frame` stack
  closure. There has also been experimental work at retaining enough
  information to make this more useful.

- Furthermore, we have the conceptual problem that with optimisations
  it becomes hard to pin-point "the" source tick to associate with a
  code location. Because of the prevalence of in-lining in Haskell
  code, using the most "low-level" source link would make the system
  almost entirely useless - we would essentially be looking at
  variants of `$`, `.` and `>>=` all the time. Instead, we look for
  the most specific source note that belongs to the currently compiled
  compilation unit. This heuristic works well, but it is not
  hard to demonstrate situations where it goes wrong.


Architectural problems - might need us to re-think our infrastructure
a bit:

- While this sort of debugging does not \*require\* all libraries and
  RTS to be built with `-g`, it certainly is a very good idea. After
  all, currently stack unwinding stops at the first closure that
  doesn't have DWARF information associated with it. As Johan Tibell
  suggested, the best approach might be to look at options to
  implement `-gsplit-dwarf`, which would allow us to ship a DWARF
  package separately.

>
>
> (Also note that tools with more knowledge about the Haskell stack
> can work around this issue somewhat by using info tables to traverse
> the Haskell stack. Explaining this strategy to DWARF readers might
> also be possible, Nathan Howell has done some experiments on this if
> I remember correctly.)
>
>

- Windows situation completely unclear.


Performance problems:

- Potential slowdown bug: "Using -g causes differences in generated core" - #15960

## Design decisions


Currently we use `libdw` (part of `elf-utils`) for stack unwinding and DWARF reading. While `libdw` has the advantage of being comprehensive () and widely used (used by `perf`, among others), it only targets ELF platforms. There are a few alternatives that were considered,

- `libdwarf` ([https://www.prevanders.net/dwarf.html](https://www.prevanders.net/dwarf.html)): not widely available; questionable upstream maintenance practices
- `libdw` ([https://sourceware.org/elfutils/](https://sourceware.org/elfutils/)): widely available and used, offering both unwinding and ELF/DWARF parsing, reasonably active upstream, only supports ELF, poor documentation
- `libbacktrace` ([https://github.com/gcc-mirror/gcc/tree/master/libbacktrace](https://github.com/gcc-mirror/gcc/tree/master/libbacktrace)): Supports ELF and XCOFF, not widely available
- `libunwind` ([http://www.nongnu.org/libunwind/](http://www.nongnu.org/libunwind/)): well documented, also no MachO support
