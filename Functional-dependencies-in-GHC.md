This is the root page for examples and issues about functional dependencies in GHC.

## Main sub-pages

**IMPORTANT** This list of sub-pages have most of the content!

* [Background and terminology](Functional-dependencies-in-GHC/background-and-terminology)
* [Key examples](Functional-dependencies-in-GHC/key-examples)
* [Single-choice inference](Functional-dependencies-in-GHC/Single-choice-inference)
* [Wiggly arrows](Functional-dependencies-in-GHC/Wiggly-arrows)

The rest this page gathers other useful links.

## Related tickets

Here is the list of [tickets related to functional dependencies](https://gitlab.haskell.org/ghc/ghc/-/issues/?label_name%5B%5D=FunctionalDependencies)

## Key papers

* [Type classes with functional dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html), Mark Jones, ESOP 2000.  The original fundep paper.
* [Understanding functional dependencies via constraint handling rules](https://www.microsoft.com/en-us/research/publication/understanding-functional-dependencies-via-constraint-handling-rules/), Sulzmann et al, JFP 2006.  This journal paper has a lot of examples and we cite it frequently below as "JFP-paper".
* [Language and Program Design for Functional Dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps-design.pdf), Jones and Diatchki, Haskell Symposium 2008.
* [Elaboration on functional dependencies](https://people.cs.kuleuven.be/~tom.schrijvers/portfolio/haskell2017a.html), Karachalias and Schrijvers, Haskell Symposium 2017.  This paper shows how to translate functional dependencies into type families. See also [Evidenced functional dependencies](Functional-dependencies-in-GHC/Evidenced-Functional-Dependencies) a wiki page developing the Karachalias and Schrijvers paper further.


## Key proposals

* Coverage condition: [Per-instance DYSFUNCTIONAL pragma](https://github.com/ghc-proposals/ghc-proposals/pull/374).
* Instance consistency condition: [Explore ways to weaken or abandon the Instance Consistency Condition](https://github.com/ghc-proposals/ghc-proposals/issues/391).

## GHC today

GHC today does this:

* By default: GHC uses the strict coverage condition and imposes the Paterson conditions.

* If `UndecidableInstances` is on, GHC
  * uses the liberal (not strict) coverage condition
  * lifts the Paterson conditions

* Always: GHC implements liberal instance consistency unconditionally.  See `Note [Bogus consistency check]` in `GHC.Tc.Instance.Fundeps`.  (GHC presumably does this because SICC is incompatible with LCC.)
