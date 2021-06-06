# Visualizing module dependency graphs

The image below shows a slice of a graph of the import dependencies of `GHC.Parser`.
<br/><br/>
![parser-deps-slice](uploads/6b8b90fc722e0cdf22d6795bae4bfba2/parser-deps-slice.png)

Using the program `count-deps` from [`ghc/utils`](https://gitlab.haskell.org/ghc/ghc/-/tree/master/utils/),  it's possible to generate graphs like these for any module in the `ghc` package.

It's easy! This [README](https://gitlab.haskell.org/ghc/ghc/-/blob/master/utils/count-deps/README.md) ought to tell you all you need to know.