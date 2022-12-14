**Note: further work is being done on the project. Ideas/progress/etc is on [EventLog/LiveMonitoring](event-log/live-monitoring)**

## Summary


EventLog is a fast, extensible event logging framework in the GHC run-time system (RTS) to support profiling of GHC run-time events. The [GHC User's Guide](http://www.haskell.org/ghc/docs/6.12.2/html/users_guide/runtime-control.html#rts-eventlog) describes how to enable event logging, after the program is linked with `eventlog`.


An example:

```wiki
$ ghc -O -eventlog -rtsopts A.hs --make
$ ./A +RTS -l

.. produces A.eventlog ...

$ ghc-events show A.eventlog 
Event Types:
   0: Create thread (size 4)
   1: Run thread (size 4)
   2: Stop thread (size 6)
   3: Thread runnable (size 4)
   4: Migrate thread (size 6)
...
```


Log events in binary format to the file program.eventlog, where flags is a sequence of zero or more characters indicating which kinds of events to log. Currently there is only one type supported: -ls, for scheduler events.


The format of the log file is described by the header EventLogFormat.h that comes with GHC, and it can be parsed in Haskell using the [ghc-events](http://hackage.haskell.org/package/ghc-events) library. To dump the contents of a .eventlog file as text, use the tool `ghc-events` that comes with the ghc-events package.

## Limitations


Event logging can produce "observer effects". In particular, programs can pause while writing to the `.eventlog` file. These display in Threadscope as periods of 100% CPU use with no obvious cause. This effect is exacerbated by heavy use of debug tools which expand the eventlog file, such as `Trace.traceMarkerIO`. (This effect was at least seen on OS X. In an extreme case, a program doing nothing but writing 200,000 trace markers, which took 120 ms to run, showed a single 10ms pause in the middle to write them to the `.eventlog` file. These periods were shown to be due to writing the `.eventlog` file by using `dtruss -a`, which shows long periods of `write_nocancel` syscalls, writing to the `.eventlog` file opened earlier with `open_nocancel`.)

## Design

- EventLog framework is located in ghc_root/rts/eventlog/
- A [ghc-events](http://hackage.haskell.org/package/ghc-events) library to parse EventLog files for any visualizer, which relies upon the Data.Binary library.
- Visualizer: [ThreadScope](https://wiki.haskell.org/ThreadScope)

## Code repository

- Available in GHC 6.10.x in ghc_root/rts/eventlog
- [ghc-events: EventLog binary file parser for profilers](http://code.haskell.org/ghc-events)
- [ThreadScope: thread-level profiler for GHC EventLog events](http://code.haskell.org/ThreadScope/)

## Publications

- [Parallel Performance Tuning for Haskell (Don Jones Jr., Simon Marlow, Satnam Singh) Haskell '09: Proceedings of the second ACM SIGPLAN symposium on Haskell, Edinburgh, Scotland, ACM, 2009](http://research.microsoft.com/pubs/80976/ghc-parallel-tuning2.pdf)

## Contributors

- Satnam Singh
- Simon Marlow
- Donnie Jones \<donnie@???\>
