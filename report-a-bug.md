How to report a bug in the GHC system

Glasgow Haskell is a changing system so there are sure to be bugs in it. Please report them to GHC's Gitlab instance (https://gitlab.haskell.org/ghc/ghc)! (However, please check the earlier part of this section to be sure it's not a known not-really-a problem.)

The name of the bug-reporting game is: facts, facts, facts. Don't omit them because “Oh, they won't be interested…”

1. What kind of machine are you running on, and exactly what version of the operating system are you using? (**uname -a** or **cat /etc/motd** will show the desired information.)  
2. What version of GCC are you using? **gcc -v** will tell you.  
3. Run the sequence of compiles/runs that caused the offending behaviour, capturing all the input/output in a “script” (a UNIX command) or in an Emacs shell window. We'd prefer to see the whole thing. 
4. Be sure any Haskell compilations are run with a -v (verbose) flag, so we can see exactly what was run, what versions of things you have, etc. 
5. What is the program behaviour that is wrong, in your opinion? 
6. If practical, please send enough source files for us to duplicate the problem. 
7. If you are a Hero and track down the problem in the compilation-system sources, please send us patches relative to a known released version of GHC, or whole files if you prefer. 