# GHC Commentary: What the hell is a `.cmm` file?


A `.cmm` file is rather like C--.  The syntax is almost C-- (a few constructs are missing), and it is augmented with some macros that are expanded by GHC's code generator (eg. `INFO_TABLE()`).  A `.cmm` file is compiled by GHC itself: the syntax is parsed by [compiler/GHC/Cmm/Parser.y](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Cmm/Parser.y) and [compiler/GHC/Cmm/Lexer.x](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Cmm/Lexer.x) into the [Cmm](commentary/compiler/cmm-type) data type, where it is then passed through one of the [back-ends](commentary/compiler/backends).


We use the C preprocessor on `.cmm` files, making extensive use of macros to make writing this low-level code a bit less tedious and error-prone.  Most of our C-- macros are in [rts/include/Cmm.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/Cmm.h). One useful fact about the macros is `P_` is an alias for `gcptr`, and you should not use it for non-garbage-collected pointers.

## Reading references


Reading material for learning Cmm is somewhat scattered, so I (Arash) have created a list of useful links. Since the Cmm language is changing as GHC changes, I have prioritized resources that are not too old. (*Feel free to add/remove/modify this list! :)*)

- An overview of Cmm is given in [David Terei's bachelor thesis](https://llvm.org/pubs/2009-10-TereiThesis.pdf) (chapter 2.4.3).
- The comments in the beginning of [compiler/GHC/Cmm/Parser.y](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Cmm/Parser.y) is super-useful and kept up to date. The rest of the file contains the *grammar* of the language. Afraid of grammars? Edward Yang wrote this fantastic [blog post](http://blog.ezyang.com/2013/07/no-grammar-no-problem/) on how to understand the constructs of Cmm by using the grammar.  
- Cmm has a preprocessor like the one in C and many of the macros are defined in [rts/include/Cmm.h](https://gitlab.haskell.org/ghc/ghc/blob/master/rts/include/Cmm.h). 
- In 2012, Simon Marlow extended the Cmm language by adding a new high-level syntax which can be used when you don't need low-level access (like registers). The [commit](https://github.com/ghc/ghc/commit/a7c0387d20c1c9994d1100b14fbb8fb4e28a259e) explains the details.
- Cmm is also described [on this wiki](commentary/compiler/cmm-type), but it is written before the new syntax was introduced.
- See also [Cmm syntax](commentary/compiler/cmm-syntax) for Cmm new syntax. 
- Stack frame types are created using `INFO_TABLE_RET`, the syntax can be confusing since there are both *arguments* and *fields*, I (Arash) have not seen anything like it in other programming languages. I tried to explain it in my [master thesis](http://arashrouhani.com/papers/master-thesis.pdf) (sections 4.2 and 4.2.1).

## Other information


It can take time to learn Cmm. One unintuitive thing to watch out for is that there are no function calls in low-level cmm code. The new syntax from 2012 allows function calls but you should know that they are kind of magical.


We say that **Cmm** is GHC's implementation of **C--**. This naming scheme is not done consistently everywhere, unfortunately. If you are interested in C-- (which have diverged from Cmm), you can check out its [specification](http://www.cs.tufts.edu/~nr/c--/extern/man2.pdf).