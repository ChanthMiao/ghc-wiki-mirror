I'm trying to follow along with the Plutus Pioneer lectures, and I'm getting this error 
My system:

Ubuntu Linux Docker image running on MacOS. 
I installed the Haskell package on the image from here: https://www.haskell.org/platform/linux.html
 
using this line in my Dockerfile: `RUN apt-get install -y haskell-platform`

This seems to install Cabal version 2.4.

When I clone the code repository for the lectures: https://github.com/input-output-hk/plutus-pioneer-program and then go to the week01 directory and try `cabal build` like in the lecture, I get dependencies not found errors. The first missing dependency is 'aeson', which seems to install if I run `cabal install aeson`. 

The subsequent build attempts fail on dependency 'base' being the wrong version. 
```
cabal: Encountered missing dependencies:
base >=4.14.1.0 && <4.15,
```

Then I thought maybe if I update Cabal to version 3.4 it might help, so I tried `cabal install cabal-install`, but this also has errors:
```Cabal-3.4.0.0-Ffni3rZRsi0BS61YYIZOfd failed during the building phase. 
Theexception was:ExitFailure (-9)

This may be due to an out-of-memory condition.
```
So I googled some more and tried this command line: `cabal install --ghc-options="+RTS -M600M" -j1 cabal-install` from here https://stackoverflow.com/a/46148345/52236

This seems to get further, but now I'm at this error: 
```
ghc: panic! (the 'impossible' happened)  
(GHC version 8.6.5 for x86_64-unknown-linux):	heap overflow
```
If anyone has any idea how to fix this it would be appreciated. Do I need to add more RAM to my Docker Ubuntu image? It currently has 1.9GB of memory and 1.7GB free. Do I need to add more memory to the RTS parameter?

thanks,
m

*Update*
I solved this by adding more memory to the RTS param:
`cabal install --ghc-options="+RTS -M1000M" -j1 cabal-install`

Now I have this issue, where 3.4 installs but I'm still stuck using 2.4:
```
Warning: could not create a symlink in /root/.cabal/bin for cabal because the

file exists there already but is not managed by cabal. You can create a

symlink for this executable manually if you wish. The executable file has been

installed at /root/.cabal/bin/cabal
```