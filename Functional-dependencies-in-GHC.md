This is the root page for examples and issues about functional dependencies in GHC.

## Main sub-pages

These pages have most of the content!

* [Background and terminology](Functional-dependencies-in-GHC/background-and-terminology)
* [Key examples](Functional-dependencies-in-GHC/key-examples)
* [Design choices](Functional-dependencies-in-GHC/design-choices)

## Key papers

* [Type classes with functional dependencies](https://web.cecs.pdx.edu/~mpj/pubs/fundeps.html), Mark Jones, ESOP 2000.  The original fundep paper.
* [Understanding functional dependencies via constraint handling rules](https://www.microsoft.com/en-us/research/publication/understanding-functional-dependencies-via-constraint-handling-rules/), Sulzmann et al, JFP 2006.  This journal paper has a lot of examples and we cite it frequently below as "JFP-paper".
* [Elaboration on functional dependencies](https://people.cs.kuleuven.be/~tom.schrijvers/portfolio/haskell2017a.html), Karachalias and Schrijvers, Haskell Symposium 2017.  This paper shows how to translate functional dependencies into type families.
* [Evidenced functional dependencies](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies/Evidenced-Functional-Dependencies) a wiki page developing the Karachalias and Schrijvers paper further.

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
