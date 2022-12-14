# Ticky-ticky profiling


Ticky-ticky profiling adds counters to every STG function.  It's very low-level, but it really tells you what is going on:

- Add the `-ticky` flag when compiling a Haskell module to enable "ticky-ticky" profiling of that module.  This makes GHC emit performance-counting instructions in every STG function.

- Add `-ticky` to the command line when linking, so that you link against a version of the runtime system that allows you to display the results.  In fact, in the link phase `-ticky` implies `-debug` (see #3439), so you get the debug version of the runtime system too.

- Add `+RTS -rfoo.ticky` to the run-time command line, to put the ticky-ticky profile in the file `foo.ticky`.


You need to use `-ddump-prep` or `-ddump-stg` when compiling the source files to see the functions that correspond to the performance counter report.


It's very low level stuff, but in exchange:

- It's guaranteed that adding `-ticky` doesn't affect optimisation or transformation.  It just adds the overhead of performance counters to the final code. (Any chance these affect the Cmm optimisations?)

- You can mix modules compiled with `-ticky` and modules compiled without.

To *really* see everything you need to compile all the libraries with `-ticky`.  To do that in a standard build tree, here are some flag settings in `build.mk` that work:

```wiki
# Build all libraries with -ticky
GhcLibHcOpts += -ticky -ticky-allocd -ticky-dyn-thunk

# Also build Stage-2 with -ticky
GhcStage2HcOpts += -ticky -ticky-allocd -ticky-dyn-thunk

# Currently ticky is incompatible with threading
GhcThreaded = NO
```

For hadrian you should compile with the `ticky_ghc` flavour transformer:

```
./hadrian/build --flavour=default+ticky_ghc
```

## Ticky-ticky quick start


Suppose you are doing peformance debugging on a new GHC Core-to-Core optimisation
pass.  You have a program `prog` that allocates a bit more with the
optimisation on than when it is off.  But *why* is it allocating
more?  Using full-bore profiling is too big a hammer, because it changes the way optimisation works.  Ticky-ticky is just the thing.


Here is what I do:

- Two build trees, one for the compiler (and libraries, etc) without the change, one for the compiler with the change

- In both build trees, edit `build.mk` as described above.  That makes the libraries generate ticky-ticky information.

- Run `nofib` in both trees and use `nofib-analyse` to compare, as described in [Building/RunningNoFib](building/running-nofib).

- Find one where the performance changes in the wrong direction, say `x2n1`. It is often best, at least at first, to use allocation as a proxy for execution time; allocation has the huge merit of being repeatable and measurable at a fine grain.

- Compile `x2n1` in both trees with `-ticky`, and also generating Core and STG, thus:

  ```wiki
  $ cd x2n1
  $ make clean
  $ make boot
  $ make EXTRA_HC_OPTS="-ticky -ddump-simpl -ddump-stg" >& x2n1.dump }}}
  ```

- Run `x2n1` in both trees, with `+RTS -rx2n1.ticky` to dump ticky output into that file

  ```wiki
  $ x2n1 +RTS -rx2n1.ticky
  ```

  Some `nofib` programs need command-line arguments to run; look in `Makefile` to see.  Others require input on stdin; look for `*.stdin`.

- Compare the two ticky files.  They each have a section looking like this

  ```wiki
      Entries      Alloc    Alloc'd  Non-void Arguments      STG Name
  --------------------------------------------------------------------------------
           92       2448          0   1 i                    unpack{v s72} (ghc-prim:GHC.CString) in 0k
           16       1112          0  11 +++.LEEMSMM          base:GHC.IO.Handle.Internals.mkDuplexHandle5{v r6vW}
            6        432          0  12 pMEiiipMEiii         base:GHC.IO.Encoding.UTF8.$wa{v r44d}
           10        400          0   4 LM>p                 base:GHC.IO.Handle.Internals.$wa2{v r6w5}
           22        352          0   1 L                    base:GHC.IO.Encoding.getFileSystemEncoding_go{v r2BF}
           19        336          0   2 LL                   base:GHC.Base.++{v 03}
  ```

  The column to focus on initially is `Alloc`, which gives the bytes allocated by the code fragment named under `STG Name`.  I generally sort both files by the `Alloc` column (using Unix command `sort -k2 -nr` on that region).

  bgamari/ghc-utils> contains a [utility](https://gitlab.haskell.org/bgamari/ghc-utils/tree/master/compare-ticks) for comparing ticky output from multiple runs.

- If your change is small, the two tables will look pretty similar, so you can just run your eye down until you find a difference.  Then go look in `x2n1.dump` for the unique (e.g `r6vW`) in the `STG Name`.  Look initially in the `STG Syntax` dump, but having found the right place I generally back up to the `Tidy Core` section which is far more readable.

- Now you can compare the two sections of Core code.


The good thing is that ticky-ticky is guaranteed to be non-invasive. It generates a bit of extra memory traffic for the instrumentation, but that's all.

## Ticky-ticky overview


(This is a high-level overview, see the following section for details.)


It is possible to compile Haskell programs so that they will count several kinds of interesting things, e.g., number of updates, number of data constructors entered, etc.  We call this "ticky-ticky" profiling because that's the sound a CPU makes when it is running up all those counters (*slowly*).


Ticky-ticky profiling is mainly intended for implementors; it is quite separate from the main "cost-centre" profiling system, intended for all users everywhere.


You don't need to build GHC, the libraries, or the RTS a special way in order to use ticky-ticky profiling.  You can decide on a module-by-module basis which parts of a program have the counters compiled in, using the compile-time `-ticky` option.  Those modules that were not compiled with `-ticky` won't contribute to the ticky-ticky profiling results. That will normally include all the pre-compiled packages that your program links with.



There are currently two coarse classes of ticky-ticky counters: name-specific counters and global counters.


- name-specific counters

>
> >
> >
> > Each "name-specific counter" is associated with a name that is defined in the result of CorePrep. For each such name, there are three possible counters: entries, heap allocation by the named thing, and heap used to allocate that named thing.
> >
> >
>

- global counters

>
> >
> >
> > Each "global counter" describes some aspect of the entire program execution. For example, one global counter tracks total heap allocation; another tracks allocation for PAPs.
> >
> >
>

## Flags: ticky-ticky and its extensions

<table><tr><th> flag </th>
<th> effect 
</th></tr>
<tr><th> <tt>-ticky</tt> </th>
<th> count entries and allocation ticky-ticky (both global and name-specific counters) 
</th></tr>
<tr><th> <tt>-ticky-dyn-thunk</tt> </th>
<th> also use name-specific counters for dynamic thunks 
</th></tr>
<tr><th> <tt>-ticky-LNE</tt> </th>
<th> also use name-specific counters for let-no-escapes 
</th></tr>
<tr><th> <tt>-ticky-allocd</tt> </th>
<th> also track allocation <i>of</i> each named thing in addition to allocation <i>by</i> that thing 
</th></tr></table>


Ticky-ticky counters are enabled in two ways.

- A module compiled with `-ticky` will maintain name-specific counters for the names defined in that module's CorePrep output.

- A program linked with the `-debug` RTS will include the RTS's effect on the global ticky-ticky counters. At link-time, `-ticky` implies `-debug`.


Some global counters are synthetized from multiple other counters, including both name-specific as well as other global counters. For example, the `ALLOC_HEAP_tot` counter accumulates the total of all heap allocations that were tracked by ticky-ticky; it is influenced both by the name-specific counters for allocation as well as by the global counters for heap allocation by the RTS.

*By default*, the name-specific counters are only tracked for functions. In particular, both let-no-escape (LNE) names and thunks are not tracked. Allocation by each is included in the nearest lexically enclosing ticky counter. Entries in to each are not tracked at all.


Two flags enable LNE and dynamically allocated thunks to be tracked by name-specific ticky counters: `-ticky-LNE` and `-ticky-dyn-thunk`. Note well that these flags, especially for dynamic thunks, incur much higher instrumentation overhead and much larger ticky reports.

*By default*, the name-specific counters track only entries-into and allocation-by the named thing. A flag `-ticky-allocd` additionally tracks the heap used to allocate that thing. Again, this flag increases the instrumentation overhead.

## Generating the ticky report


Any GHC executable linked with `-rtsopts` will generate a ticky-ticky profiling report if provided the `-r` RTS option.  Thus

```wiki
$ ghc -ticky -rtsopts Main.hs -o main
$ ./main +RTS -rmain.ticky
```


This report includes all global counters as well as the name-specific counters for those names with at least one interesting counter value. If a named thing was never allocated and (hence) never entered, its counters will not be in the ticky report.


Below is an excerpt from a ticky report. The executable was compiled with all of the extensions above.

- **Entries:** the number of times this closure was entered

- **Allocs:** the number of bytes allocated by the code for this closure.

- **Alloc'd:** the number of bytes allocated that have this closure's info pointer.  You need `-ticky-allocd` to activate this column.  (Otherwise it shows up as all zeros)

- **Non-void arguments:** gives a short summary of the named things's non-void arguments: how many there are and a terse description of each, according to the following table.

<table><tr><th> Classification </th>
<th> Description 
</th></tr>
<tr><th> <tt>+</tt> </th>
<th> dictionary 
</th></tr>
<tr><th> <tt>></tt> </th>
<th> function 
</th></tr>
<tr><th> <tt>{C,I,F,D,W}</tt> </th>
<th>  char, int, float, double, word 
</th></tr>
<tr><th> <tt>{c,i,f,d,w}</tt> </th>
<th> unboxed ditto 
</th></tr>
<tr><th> <tt>T</tt> </th>
<th> tuple 
</th></tr>
<tr><th> <tt>P</tt> </th>
<th> other primitive type 
</th></tr>
<tr><th> <tt>p</tt> </th>
<th> unboxed ditto 
</th></tr>
<tr><th> <tt>L</tt> </th>
<th> list 
</th></tr>
<tr><th> <tt>E</tt> </th>
<th> enumeration type 
</th></tr>
<tr><th> <tt>S</tt> </th>
<th> other single-constructor type 
</th></tr>
<tr><th> <tt>M</tt> </th>
<th> other multi-constructor data-con type 
</th></tr>
<tr><th> <tt>.</tt> </th>
<th> other type 
</th></tr>
<tr><th> <tt>-</tt> </th>
<th> reserved for others to mark as &quot;uninteresting&quot; 
</th></tr></table>


- **CorePrep/STG name:** the name to which the counters in this row refer. Each entry in this column uses an encoding that differentiates between exported names (`main:Main.puzzle`) and non-exported names (`go1{v r2Hj} (main:Main)`). Some non-exported names indicate that they are let-no-escape (`(LNE)`) or a dynamically allocated thunk (`(thk)`). All let-bound names also specify the unique of the parent (`in s2T4`). The "parent", here, is the innermost enclosing definition that has a ticky counter; the parent is thus affected by `-ticky-LNE` and `-ticky-dyn-thunk`.

```wiki
$ ghc ... -ticky -ticky-LNE -ticky-dyn-thunk -ticky-allocd ... -o foo
$ ./foo +RTS -rticky -RTS
$ cat ticky # I'm just showing an excerpt
...
**************************************************

    Entries      Alloc    Alloc'd  Non-void Arguments      STG Name
--------------------------------------------------------------------------------
         10        240        240   0                      sat_s2T2{v} (main:Main) (thk) in s2T4
         10        240        240   0                      sat_s2T4{v} (main:Main) (thk) in r2Hj
         10          0        240   0                      sat_s4eb{v} (main:Main) (thk) in r2Hj
         11        960          0   1 L                    go1{v r2Hj} (main:Main)
          6         96        192   0                      sat_s4hz{v} (main:Main) (thk) in s2Ua
          7        528         16   1 L                    go3{v s2Ua} (main:Main) in rj2
         49          0         16   2 iL                   $wlgo{v s2TW} (main:Main) in rj2
          0          0         24   0                      sat_s4ck{v} (main:Main) (thk) in r2Hi
          1         48          0   1 L                    go{v r2Hi} (main:Main)
          1         48          0   1 S                    a{v r2H9} (main:Main)
       1070      11264      17232   1 L                    go5{v s2Sg} (main:Main) in s2Sm
        718      17232      28920   0                      sat_s2Sm{v} (main:Main) (thk) in s2Pf
       8465      78688     144168   1 L                    go5{v s2RO} (main:Main) in s2RV
       6007     144168     240280   0                      sat_s2RV{v} (main:Main) (thk) in s2Pf
       1035      33120     269200   3 TSL                  $sgo1{v s2R0} (main:Main) in s2Pf
       1029          0     215360   1 L                    go4{v s2QX} (main:Main) in s2Pf
       1035      16560     161520   0                      a2{v s2QO} (main:Main) (thk) in s2Pf
       6730     107680     215360   0                      sat_s2Ov{v} (main:Main) (thk) in s2Oy
      77223    4199520     590064   1 L                    go3{v s2NH} (main:Main) in s2NP
      12644    1011520     303456   3 TSL                  $sgo1{v s2JG} (main:Main) in s2K4
      12634          0     202304   1 L                    go4{v s2JD} (main:Main) in s2K4
       4130      99120     404608   0                      sat_s2Kt{v} (main:Main) (thk) in s2Kw
      16765    1416128      66096   1 L                    go5{v s2Kw} (main:Main) in s2KG
      16765    1011520      66096   1 L                    go4{v s2Kg} (main:Main) in s2KG
      16765    1820736      99144   1 L                    go3{v s2K4} (main:Main) in s2KG
       4131     363528          0   3 SSL                  $sgo{v s2KG} (main:Main) (LNE) in r2H6
       4121          0      66096   1 L                    go2{v s2KE} (main:Main) in r2H6
       4131      66096          0   2 CS                   main:Main.permute1{v r2H6}
      37907    2834272          0   3 SSL                  $sgo{v s2MG} (main:Main) (LNE) in r2H7
      37865          0     606512   1 L                    go2{v s2MA} (main:Main) in r2H7
      37907     606512          0   2 CS                   main:Main.select1{v r2H7}
      68020    1770192     498832   1 L                    go2{v s2NP} (main:Main) in r2Ha
      37907     821872          0   2 LS                   a1{v r2Ha} (main:Main)
       7752    1130640      32896   1 L                    go3{v s2Pf} (main:Main) in r2H8
        453          0      24672   0                      lvl7{v s2OF} (main:Main) (thk) in r2H8
       7752     753760      16448   1 L                    go2{v s2Oy} (main:Main) in r2H8
       1036      74016          0   4 LLIS                 main:Main.solve1{v r2H8}
          1          0         24   0                      sat_s4hu{v} (main:Main) (thk) in rj2
          7        192         16   1 L                    go2{v s2Tj} (main:Main) in rj2
          1         72          0   2 LL                   main:Main.puzzle{v rj2}
          1          0          0   0                      main:Main.main1{v r2GU}
          1          0          0   0                      main:Main.main12{v r2H5}
          1          0          0   0                      main::Main.main{v 01C}

**************************************************
     293586 ALLOC_HEAP_ctr
   18391320 ALLOC_HEAP_tot
         26 ALLOC_RTS_ctr
        672 ALLOC_RTS_tot
     170019 ALLOC_FUN_ctr
    3193952 ALLOC_FUN_gds
...
```


The omitted information above the first row of asterisks is just human-readable summaries; its content and format should not be relied upon.  Below the first row of asterisks the name-specific counters are dumped in a fixed format. Below the second row of asterisks, *all global counters* are dumped, in a format intended to be machine-readable: zero or more spaces, an integer, a space, the counter name, and a newline.


In fact, not *all* counters are necessarily dumped; compile- or run-time flags can render certain counters invalid.  In this case, either the counter will simply not appear, or it will appear with a modified counter name, possibly along with an explanation for the omission. Software analysing this output should always check that it has the counters it expects.  Also, beware: some of the counters can have very large values.

## History


Ticky has been around a long time, and its glory days seem to have
passed: there are several bitrotted counters, now totally inactive or
at least of dubious accuracy.


As of March 2013, nfrisby added the extension flags and resurrected
some global counters (including those in the RTS, esp the slow call
counters), and standardized the enabled allocation counters on bytes.



Prior history:


- tickets: #607 #2455 #3439

- there used to be a `t` "way", but it was folded into `debug`

- Tim's notes [Commentary/Profiling/TickyNotes](commentary/profiling/ticky-notes) [Commentary/Compiler/StrictnessAnalysis/KirstenNotes](commentary/compiler/strictness-analysis/kirsten-notes)


If some of the counters are zero when they shouldn't be, that means they're not implemented yet. If you want them to be, complain on a mailing list.

## Implementation notes


When compiling with `-ticky`, the back-end emits the name-specific counters in the data section and generates instructions that tick them directly.


The global counters are *always* statically allocated by the RTS [includes/stg/Ticky.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/stg/Ticky.h) regardless of the compilation way.


Some `TICK_...` C-- macros are sprinkled throughout the RTS. In the debug way (cf `ways.mk`) `TICKY_TICKY` is `defined`, so these macros expand to code that ticks the counters. The relevant compiler code is mostly in [compiler/GHC/StgToCmm/Ticky.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/StgToCmm/Ticky.hs). NB that some of these macros are expanded by [compiler/GHC/Cmm/Parser.y](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Cmm/Parser.y)!


The C-- macros are defined in [includes/Cmm.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/Cmm.h). Most of them (probably all of them, at the moment) just increment counters (variables in C) that are declared in [includes/stg/Ticky.h](https://gitlab.haskell.org/ghc/ghc/blob/master/includes/stg/Ticky.h). The latter file is likely to get out of sync with the former, so it really should be automatically generated.


The code in the RTS that prints out the ticky report is in [rts/Ticky.c](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/Ticky.c).

## Ticky-Ticky counters

TODO document each active counter, especially regarding its accuracy.


These are the counters present in GHC 7.10.1
If you are interested in these, you may want to look at the comments in `StgCmmTicky.hs`.


Allocations numbers are all in bytes.

#### Naming convention


You will sometimes see sets of counters with names of the form `$NAME_$suffix`. These suffixes typically describe which aspect of `$NAME` the counter measures,

- `_tot`: a running total (e.g. total bytes allocated over the life of the program).
- `_ctr`: number of events (e.g. number of times we allocated)
- `_adm`: administrative overhead (e.g. the size of headers)
- `_gds`: the size of the goods themselves (e.g. the size of the "meat" of an allocated object)
- `_slp`: the "slop" of the block actually allocated (e.g. allocated block size - `gds` - `adm`)

#### Counters

<table><tr><th> Counter                       </th>
<th> Status      </th>
<th> Meaning                                     
</th></tr>
<tr><th> <b>RTS statistics</b> 
</th>
<th></th>
<th></th></tr>
<tr><th> <tt>HEAP_CHK_ctr</tt>                  </th>
<th> Works         </th>
<th> Bumped whenever a heap-check is performed     
</th></tr>
<tr><th> <tt>STK_CHK_ctr</tt>                   </th>
<th> Works         </th>
<th> Bumped whenever a stack-check is performed    
</th></tr>
<tr><th> <b>Allocation statistics</b> 
</th>
<th></th>
<th></th></tr>
<tr><th> <tt>ALLOC_HEAP_tot</tt>                </th>
<th> ?             </th>
<th> Total size of global heap allocations         
</th></tr>
<tr><th> <tt>ALLOC_HEAP_ctr</tt>                </th>
<th> ?             </th>
<th> Total number of global heap allocations       
</th></tr>
<tr><th> <tt>ALLOC_PRIM_ctr</tt>                </th>
<th> Number of allocations done by primops (anything emitted by GHC.StgToCmm.Prim) </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_PRIM_adm</tt>                </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_PRIM_gds</tt>                </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_PRIM_slp</tt>                </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_UP_THK_ctr</tt>              </th>
<th> ?             </th>
<th> Number of UP? thunk allocations               
</th></tr>
<tr><th> <tt>ALLOC_THK_adm</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_THK_gds</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_THK_slp</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_PAP_ctr</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_PAP_gds</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_PAP_slp</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_CON_ctr</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_CON_gds</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_FUN_ctr</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ALLOC_FUN_gds</tt>                 </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <b>Call statistics</b> 
</th>
<th></th>
<th></th></tr>
<tr><th> <tt>KNOWN_CALL_ctr</tt>                </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>KNOWN_CALL_TOO_FEW_ARGS_ctr</tt>   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>KNOWN_CALL_EXTRA_ARGS_ctr</tt>     </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>UNKNOWN_CALL_ctr</tt>              </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>VERY_SLOW_CALL_ctr</tt>            </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <b>Closure entry statistics</b> 
</th>
<th></th>
<th></th></tr>
<tr><th> <tt>ENT_DYN_CON_ctr</tt>               </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_STATIC_CON_ctr</tt>            </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_VIA_NODE_ctr</tt>              </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_STATIC_THK_SINGLE_ctr</tt>     </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_STATIC_THK_MANY_ctr</tt>       </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_DYN_THK_SINGLE_ctr</tt>        </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_DYN_THK_MANY_ctr</tt>          </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>UPD_BH_SINGLE_ENTRY_ctr</tt>       </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>UPD_BH_UPDATABLE_ctr</tt>          </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>UPD_CAF_BH_UPDATABLE_ctr</tt>      </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>UPD_CAF_BH_SINGLE_ENTRY_ctr</tt>   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>ENT_STATIC_FUN_DIRECT_ctr</tt>     </th>
<th> ?             </th>
<th> Static (e.g. top-level) function entered 
</th></tr>
<tr><th> <tt>ENT_DYN_FUN_DIRECT_ctr</tt>        </th>
<th> ?             </th>
<th> Dynamic function entered 
</th></tr>
<tr><th> <tt>ENT_LNE_ctr</tt>                   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <b>Return statistics</b> 
</th>
<th></th>
<th></th></tr>
<tr><th> <tt>RET_OLD_ctr</tt>                   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>RET_OLD_hst</tt>                   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>RET_NEW_ctr</tt>                   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>RET_NEW_hst</tt>                   </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>RET_UNBOXED_TUP_ctr</tt>           </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>RET_UNBOXED_TUP_hst</tt>           </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>VEC_RETURN_ctr</tt>                </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <tt>RET_VEC_RETURN_hsr</tt>            </th>
<th> ?             </th>
<th> 
</th></tr>
<tr><th> <b>Update frame statistics?</b> 
</th>
<th></th>
<th></th></tr>
<tr><th> <tt>UPDF_PUSHED_ctr</tt>               </th>
<th> ?             </th>
<th> Update frame pushed 
</th></tr>
<tr><th> <tt>UPDF_OMITTED_ctr</tt>              </th>
<th> ?             </th>
<th> Update frame omitted 
</th></tr></table>


