This is the root page for examples and issues about functional dependencies in GHC.

## Main sub-pages

**IMPORTANT** This list of sub-pages have most of the content!

* [Background and terminology](Functional-dependencies-in-GHC/background-and-terminology)
* [Key examples](Functional-dependencies-in-GHC/key-examples)
* [Single-choice inference](Functional-dependencies-in-GHC/Single-choice-inference)
* [Wiggly arrows](Functional-dependencies-in-GHC/Wiggly-arrows)
* [AntC's proposal](Functional-dependencies-in-GHC/AntC-proposal)
* [Why (most) FunDeps are tantamount to instance selection](Functional-dependencies-in-GHC/Background-in-database-theory)

The rest this page gathers other useful links.

## Related tickets

Here is the list of [tickets related to functional dependencies](https://gitlab.haskell.org/ghc/ghc/-/issues/?label_name%5B%5D=FunctionalDependencies)

## Key papers

* [Type classes with functional dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html), Mark Jones, ESOP 2000.  The original fundep paper.
* [Understanding functional dependencies via constraint handling rules](https://www.microsoft.com/en-us/research/publication/understanding-functional-dependencies-via-constraint-handling-rules/), Sulzmann et al, JFP 2006.  This journal paper has a lot of examples and we cite it frequently below as "JFP-paper".
* [Language and Program Design for Functional Dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps-design.pdf), Jones and Diatchki, Haskell Symposium 2008.
* [Elaboration on functional dependencies](https://people.cs.kuleuven.be/~tom.schrijvers/portfolio/haskell2017a.html), Karachalias and Schrijvers, Haskell Symposium 2017.  This paper shows how to translate functional dependencies into type families. See also [Evidenced functional dependencies](Functional-dependencies-in-GHC/Evidenced-Functional-Dependencies) a wiki page developing the Karachalias and Schrijvers paper further. Crucially, K&S do not consider overlapping instances -- which gives no way forward for writing a type-level type equality test, nor a type-level `AddNat` function with three-way FunDeps.
* [Strongly typed heterogeneous collections](https://dl.acm.org/doi/10.1145/1017472.1017488), Kiselyov, Lämmel, Schupke 2003 is more or less a cheat sheet for sophisticated usage of FunDeps, including advice on compiler limitations, 'gotchas' and work-rounds at the time -- most of which are still gotchas.


## Key proposals

* Coverage condition: [Per-instance DYSFUNCTIONAL pragma](https://github.com/ghc-proposals/ghc-proposals/pull/374).
* Instance consistency condition: [Explore ways to weaken or abandon the Instance Consistency Condition](https://github.com/ghc-proposals/ghc-proposals/issues/391).

## GHC today

GHC today does this:

* By default: GHC uses the strict coverage condition and imposes the Paterson conditions.

* If `UndecidableInstances` is on, GHC
  * uses the liberal (not strict) coverage condition
  * lifts the Paterson conditions

* Always: GHC implements liberal instance consistency unconditionally.  See `Note [Bogus consistency check]` in `GHC.Tc.Instance.Fundeps`.  (GHC presumably does this because SICC is incompatible with LCC. [AntC] Hmm? Because chasing through all the constraints is computationally expensive at compile time; and potentially non-terminating (with `UndecidableInstances`). Also because not possible with separate compilation: instances in non-visible modules. Hugs supports LCC but does impose SICC (but doesn't support separate compilation); and indeed chases through instance constraints and superclass constraints/instances; and sometimes just gives up with a message to the effect 'this is too hard' -- or of course a stack depth check.)
