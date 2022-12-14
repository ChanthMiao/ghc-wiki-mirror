
This page is intended for practical notes on why list functions and rules are written as they are, why they're not written other ways, ideas about what will/won't fuse properly and why, and descriptions of issues affecting fusion. It will also have open questions of various sorts. It's currently a bit disorganized; hopefully someone will find a way to set it in order.


Q: I know that `foldr/build` fusion is [not always safe](http://www.haskell.org/haskellwiki/Correctness_of_short_cut_fusion#In_the_presence_of_seq) in the presence of `seq` (and `pseq`, strict fields, bang patterns, etc.). What exactly do I need to know about this when writing functions I want GHC to fuse safely? \[There are other contexts it can be used in program construction, but those are not relevant here\]


A: The type checker will take care of a lot of things for you; your only job is to be careful about how the function you pass to `build` (or `augment`) uses its arguments or things constructed from its arguments.

`build` forces its argument to have type `forall b . (a -> b -> b) -> b -> b`. For the purposes of this explanation, assume this is its exact type signature.


Zeroeth approximation: The function you pass to `build` should not, directly or indirectly, use `seq` (or similar). This is certainly safe, but much too restrictive. As a trivial example,

```
f x = build (\c n -> x `seq` x `c` n)
```


violates this rule, but is perfectly safe.


First approximation: The function you pass to `build` should not, directly or indirectly, use `seq` to force anything whose type contains `b`. Again, this is safe, but overly restrictive. **It is, however, good enough for most purposes.** Virtually every safe use will obey this rule.



Some examples of functions that (may) use `seq` but obey this rule and are therefore safe to fuse:


```
strictifyHeads xs = build (\c n -> foldr (\x r -> x `seq` (x `c` r)) n xs)
unfoldr f q0 = build $ \c n ->
  let go q = case f q of
               Nothing -> n
               Just (a, new_q) -> a `c` go new_q
```


Note in the case of `unfoldr` that the `f` and `q0` passed in may each use `seq`. This is okay, however, because neither of them is passed `c`, `n`, or anything built from them.



The first approximation still isn't perfect, because it has trouble with this silly function (I'm not thinking of good examples right now, if there are any):


```
f x = build (\c n -> let q = if x then Left n else Right n in q `seq` (5 `c` n) )
```


So we come to the


Final approximation (this may even be precise; I don't know, but it's probably good enough for anything sane): Don't use `seq` or similar to force anything whose type contains `b` unless you could accomplish the same thing without using `seq` to force anything whose type contains `b`.


Q: Why are functions written back to other forms when they don't fuse?


A1: Sometimes, the fancy fusing version is somehow worse than the simpler one if fusion doesn't happen.


A2: Functions must be inlined in order to fuse. When fusion doesn't happen, this creates duplicate code, often with no benefit. In fact, it may create multiple copies of the same function at the top level.


Q: Why are functions written back to recursive forms when they don't fuse, rather than ones that use higher-order functions?


Guess: by the time the writing-back happens, it may be too late in simplification for the compiler to optimize the form using higher-order functions properly. However, ti may be possible to get around this sometimes by using a NOINLINE form rather than an INLINE one. If you write back to a NOINLINE form, I would guess that you should get an already-optimized version, whereas if you write back to an INLINE form, you will get that form as it is, too late to optimize. So there may be some room to improve this in some cases.


Q: Why are functions rewritten to forms using weird-looking functions like `mapFB`?


A: If the function doesn't fuse and it needs to be rewritten back to something like its original form, we need to have enough to match on. One of the more reliable ways to do this is to use some `NOINLINE` functions in the rewritten form that we can then match on.


Q: Why isn't `map` written to inline when given one argument like `foldr` is written to inline when given two?


Guess: it may be that there isn't enough opportunity for inlining to do anything useful in that case, since `map` isn't doing anything with the results of the calls except wrapping them up in conses.


Q: Why does `repeat` fuse, but not `cycle`?


A: See #9398. This seems almost to work out, but then it doesn't quite???things that are unboxed when the current implementation is used aren't, and it can be very bad.


Q: Why does making one thing fuse sometimes make something else not fuse?


A: Because the whole system is built around inlining, and no one really knows how to make that Do The Right Thing every time. Also, no one knows a better way to avoid basing it on inlining.


Q: How can full laziness interfere with fusion?


A: Full laziness can pull a piece of an expression up to the top level, away from its context. A `build` form that's been pulled to the top level currently will not be seen by the RULES engine when it's inspecting a `foldr` form containing its (automatically generated) name. The first (partial) full laziness pass happens before any inlining, and the simplifier does not run after specialization until after full laziness, and therefore full laziness runs before a great many fusion opportunities have been revealed. The specialization issue affects `enumFromTo` and related functions, while the inlining one causes general difficulty. One workaround for the latter is to use `RULES` to "manually" inline a function; this is what many of the "translate to" rules effectively do, but many things aren't covered. For example, `($)` and `(.)` aren't inlined before full laziness tries to rip expressions using them apart.


Q: What can we do to mitigate problems caused by full laziness?


A: One thing to watch out for is the `($)` operator. For now, it seems best to avoid using it on the RHS of any fusion rules. For example, `foldr c n (build $ g)` actually looks, in the "gentle" phase, like `foldr c n (($) build g)`, which will not match the `fold/build` rule. We can't really stop anyone from writing `foldr c n $ build g`, which causes similar problems in some cases.


Q: What can we do to the compiler to keep full laziness from goofing up fusion, without having bad effects in many cases?


Idea 1 (by Joachim Breitner): Let the `RULES` engine see through the introduced bindings so it can fuse things that have been separated a little. Some care may be required to keep track of `NOINLINE` annotations.


Guess 2 (by David Feuer): Introduce the notion of something being "inlined early", specifically allowing inlining before any full laziness happens. Something that's inlinable, and that uses something that's inlined early becomes inlined early. This seems messier than Idea 1, but I thought I'd put it on the table.


Guess 3 (perhaps just implemented by Simon Peyton-Jones?): restrict constant floating to try to prevent some of these problems.


Q: Which NoFib benchmarks seem to be particularly sensitive to additional fusion rules?


A (incomplete, and poorly remembered): `fft2` tends to get significant allocation reduction, around 20%, in general. `wang` gets a 50% allocation reduction with either `foldr/cons` or `cons/build`, but only if `-fsimple-list-literals` is enabled. `constraints` tends to do a little worse, with around +4% allocation. `cacheprof` has mixed results (nothing huge). Adding a (highly invasive) simplifier run with inlining and rules has all sorts of wild effects, many bad, but reduces allocation in `fannkuch-redux` by a whopping 100%.


Q: What kinds of things should I avoid in `RULES`?


A1: When something appears as the outermost name in a rule LHS, the inliner will consider it "interesting", which makes it appear cheaper to inline expressions using it. This is not always a good thing. In particular, it turns out that a rule looking like

```
{-# RULES
"badrule" forall ... . e1 : e2 = e3
 #-}
```


makes the `(:)` constructor look "interesting", and this leads to poor performance all over the place. It may be possible to modify the compiler to fix this problem, but until then all such rules must be avoided.


Q: Why is fusing a numeric range like `[1..10000] :: [Int]` sometimes a bad idea?


A: (adapted from explanations by Simon Peyton-Jones and others): If the list is used multiple times, and the numbers *don't* get unboxed, then fusion prevents those boxes from being shared. So instead of allocating one list and one set of boxes, the program will allocate zero lists and many sets of boxes. If the list is of `Integer`s, it's much more likely that sharing the list is a good idea.
