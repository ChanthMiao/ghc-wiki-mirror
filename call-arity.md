# Call Arity


Call Arity analysis and transformation eta-expands definitions if we know that it is always being called with a certain number of arguments. This looks at the *uses* of a function, which is different from the code in CoreArity, which looks at the *definition* of a function.


This pages does **not** document Call Arity as implemented; that documentation should be found and maintained with the code, at [source:compiler/GHC/Core/Opt/CallArity.hs](https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/GHC/Core/Opt/CallArity.hs).


This page used to discuss possible changes to the analysis, but these are implemented now (\[cb8a63c\]), so I removed the obsolete notes from here.


More information on the analysis can be found in the [TFP 2013 paper](http://www.joachim-breitner.de/publications/CallArity-TFP.pdf). A proof of safety can be found in a paper [ presented at Haskell’15](http://www.joachim-breitner.de/publications/CallArity-Haskell15.pdf) and in the [Isabelle formalization of Call Arity](http://afp.sourceforge.net/entries/Call_Arity.shtml).


