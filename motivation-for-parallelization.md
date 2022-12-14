
Back to the [GarbageCollectorNotes](garbage-collector-notes)

# Motivation For Parallelization


The essential idea is best described here:
[Parallel Garbage Collection for Shared Memory Multiprocessors (2001)](http://citeseer.ist.psu.edu/flood01parallel.html)


It is helpful to be aware of copy collection and mark-compact collection before you read the above paper. The Richard Jones and Raphael Lins text on Garbage Collection is a recommended resource. 


For our garbage collector, we are yet to work out the details of how Gen0 should be compacted. The best idea for later generations is some variant of the work stealing approach proposed in the above paper. This of course might change in the course of the project. 

## Measurement of Block Distance while Scavenging


Here are some plots of block distance against the collection number and the average block distance and the collection number. Block distance is defined to be the number of links one has to follow from the scan_bd to reach the hp_bd in a step during garbage collection. If the block distance in 2 then it means that there is atleast one independent block in between the pointers that can be taken up by another thread.


The essential idea behind work stealing is that free threads can steal work from busy threads. The work is essentially the work of scavenging live objects. hp_bd points to the top of the to-space where the next free object can go. scan_bd points to the block where the next object to be scanned is. All objects between scan_bd and hp_bd  are objects that are yet to be scanned. A free thread essentially steal a block of objects in this range and can scan them, essentially reducing the load of the busy thread. 

[http://www.cs.indiana.edu/\~rpjames//HaskellGC/ds/st-scanning-3.jpg](http://www.cs.indiana.edu/~rpjames//HaskellGC/ds/st-scanning-3.jpg)


The following program was used to generate some the graphs below. Changing the treeDepth and the nTrees values below one can get the program to have different memory profiles. 

```wiki
import System

treeDepth = 17
nTrees = 40

makeList 0 d = []
makeList n d = d : (makeList (n-1) d)

main :: IO ()
main = if (recVal (makeList nTrees treeDepth)) < 10
       then print "###"
       else print "##"


data Tree a = L a
	    | B (Tree a) (Tree a)

makeTree 0 = L 1
makeTree n = B (makeTree (n-1)) (makeTree (n-1))

sumTree (L x) = x
sumTree (B t1 t2) = 1 + (sumTree t1) + (sumTree t2)

treeVal n rest = let tr1 = makeTree n in
		     sumTree(tr1) + sumTree(makeTree n) + (recVal rest) + sumTree(tr1)

recVal [] = 0
recVal (x:xs) = treeVal x xs
```


Here are some plots:



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g0-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g0-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g0-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g0-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g0-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g0-live_objs-time.png)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g1-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g1-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g1-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g1-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g1-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g1-live_objs-time.png)



Here is how to interpret the graphs. The label ???\#??? on an axis indicates that it is time where each tick is a garbage collection. The label ???live_objs??? indicates the total number of live objects encountered during this collection. This is not the total number of live objects in the system but only those in the generations currently collected. The value ???block_dist??? indicates the maximum block distance encountered during a collection. 


The value \`avg_block_dist??? indicates the average block distance encountered during a collection. If you think about the block distance a bit you realize that it essentially starts from zero increases and decreases during the duration of scavenging and finally becomes zero when the scan point catches up with the heap pointer. We wanted to measure approximately the area under this region as a indication of the average chance of parallelization. Further to make the measurement a little less fine grained, it was taken only when a new block was allocated to the to-space. This value can be considered indicative of how much parallelization is possible on average during that GC run. \[At least I hope so\]


Here are similar plots of some programs in the nofib test suite that is available in the GHC source tree. 



Plots of real/fulsom (with input 8)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g0-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g0-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g0-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g0-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g0-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g0-live_objs-time.png)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g1-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g1-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g1-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g1-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g1-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g1-live_objs-time.png)



Plots of real/pic (with input 20000)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g0-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g0-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g0-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g0-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g0-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g0-live_objs-time.png)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g1-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g1-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g1-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g1-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g1-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g1-live_objs-time.png)



Plots of real/fem (with fem.stdin)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fem-g0-avg_block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fem-g0-avg_block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fem-g0-block_dist-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fem-g0-block_dist-time.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fem-g0-live_objs-time.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fem-g0-live_objs-time.png)



fem did not do any G1 collections.


Here are some slightly differet plots. The following show how number of live objects and block distance are related. One might expect a linear relationship as long as the application consists primarily of objects of the shape. If there are objects that have only one pointer as opposed to ones that have sevral then the block distance would be gretly affected by this. 



Plots of memtest (the tree application)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g0-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g0-avg_block_dist-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-memtest-g1-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-memtest-g1-avg_block_dist-live_objs.png)



Plots of real/fulsom (with input 8)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g0-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g0-avg_block_dist-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fulsom-g1-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fulsom-g1-avg_block_dist-live_objs.png)



Plots of real/pic (with input 20000)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g0-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g0-avg_block_dist-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-pic-g1-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-pic-g1-avg_block_dist-live_objs.png)



Plots of real/fem (with fem.stdin)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fem-g0-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fem-g0-avg_block_dist-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G2/plot-fem-g1-avg_block_dist-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G2/plot-fem-g1-avg_block_dist-live_objs.png)



Finally here are some plots of memtest run with 3 generations instead of 2. 



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g0-time-avg_block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g0-time-avg_block_dist.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g0-time-block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g0-time-block_dist.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g0-time-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g0-time-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g0-live_objs-avg_block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g0-live_objs-avg_block_dist.png)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g1-time-avg_block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g1-time-avg_block_dist.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g1-time-block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g1-time-block_dist.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g1-time-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g1-time-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g1-live_objs-avg_block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g1-live_objs-avg_block_dist.png)



[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g2-time-avg_block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g2-time-avg_block_dist.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g2-time-block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g2-time-block_dist.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g2-time-live_objs.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g2-time-live_objs.png)
[http://www.cs.indiana.edu/\~rpjames/HaskellGC/G3/plot-memtest-g2-live_objs-avg_block_dist.png](http://www.cs.indiana.edu/~rpjames/HaskellGC/G3/plot-memtest-g2-live_objs-avg_block_dist.png)



Roshan James (rpjames \[at\] cs \[dot\] indiana \[dot\] edu)
