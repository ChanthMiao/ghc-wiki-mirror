
BEWARE: this page is still under construction and should be considered work in progress.

# Template Haskell under Cross Compilation


Template Haskell does not properly work with cross compilers so far.  So why does Template Haskell not work with cross compilation?  The
main reason is that (thanks to rwbarton) Template Haskell needs to be able to evaluate arbitrary expression.  These expressions could for
example link with other libraries that are only available on the target architecture (ffi), in this case the host can not evaluate the splice.


Another example is (thanks to merijn) the splice "maxBound :: Int", which would be architecture dependent on the Int type. Which the compiler
on the host does not have.


rwbarton shared a few more points that require TH to run on the target platform:

- Because TH can do arbitrary IO, which one might want to have running on the target platform, it would require ghc to completely emulate the target to produce the identical results.
- Some detect endianness through pointer/peek, which would also require the target to be emulated.


sairheit also shared a nice explanation as well: 

- the point of cross compilation is not to run code on the host, but TH can do it. You can't force the host to behave like the target. (e.g. what libc would you use?)

# But Template Haskell is Terrible altogether


TH, being a GHC extension and potentially unsafe, and also allowing arbitrary IO during compile time (launch missiles anyone?). There are quite a few discussions if TH should be embraced or not.
See for example [SO: Why does Template Haskell allow arbitrary IO operations during compilation?](http://stackoverflow.com/questions/13785753/why-does-template-haskell-allow-arbitrary-io-operations-during-compilation) and [ SO: What's so bad about Template Haskell?](http://stackoverflow.com/questions/10857030/whats-so-bad-about-template-haskell)


Here is a blog entry that takes this critique into account with a [Major proposed revision of Template Haskell](https://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges), which is the evolution of [ New directions for Template Haskell](https://ghc.haskell.org/trac/ghc/blog/Template%20Haskell%20Proposal).


Yesod, is probably one of the more prominent larger users of TH and Michael Snoyman has laid out the reasoning here: [http://www.yesodweb.com/blog/2011/04/yesod-template-haskell](http://www.yesodweb.com/blog/2011/04/yesod-template-haskell)


To me, ultimately looking for example at [the list of packages depending on template haskell](http://packdeps.haskellers.com/reverse/template-haskell) through a reverse search, gives the list of
packages that can not be used without TH support.

# Possible Solutions


There are a few solutions, which mostly consist of dumping the splices and running them through a separate pass, (e.g. evil Splicer).


And there is also the solution which is uses in ghcjs, which is that there is a runner (think slave compiler) running on the target architecture,
to which the TH splices are handed from the ghc running on the host to compile the splice on the target, which in return produces the
correctly evaluated splice and ships it back to the ghc on the host. I am in the process of porting this mechanism from ghcjs to
ghc.

# Open Questions


This is not an exhaustive list, but should highlight all the known questions so far

- Given the TH splice being evaluated on the target, and it would perform arbitrary IO (e.g. include a file) which is dependent on the hosts environment, this will work under no-cross compilation, but fail under cross-compilation with a slave process on the target, as the target might not have the same environment. Example: [git-embed](https://hackage.haskell.org/package/git-embed), which runs git to obtain the branch and commit hash information. The repository would be available to the host, but not the runner (slave), nor is guaranteed that the slave will have git available (or that any other programm for that matter is available on the target architecture)
