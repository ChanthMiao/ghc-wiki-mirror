# GHC Status Report, May 2014


In early April 2014, GHC 7.8 was finally released, after nearly 18 months of development. This was one of the longest development periods in recent memory, and there was a lot of grumbling near the end. Ultimately, the reason for this was scope creep - we kept getting bugs dripping in here and there, and fixing them, and putting things in.


Meanwhile, HEAD steams onward, with some preliminary work for the 7.10 milestone laid down. We've already got some plans as to what we'll be doing - and if you want something done, you should join in as well!

## GHC 7.8


We released GHC 7.8.1 in early April, and immediately discovered a disastrous bug (#8978) that had slipped in between the release candidates.  That led to an immediate follow-up release of 7.8.2, which seems pretty stable. We will continue to fix bugs on the 7.8 branch, and release 7.8.3 later this year, when (and if) pressure builds up from users to get the fixes into the field.


However, now that 7.8 is out, there is a lot there for users to play with: the release was one of the most feature-packed ones we've done, with a lot of changes touching almost every part of the compiler. To recap a few of them:

- **Dynamic GHC** - GHC and GHCi are now dynamically linked - this means any time you ask them to load object code (for example, loading a library in GHCi, or using `TemplateHaskell` when you compile something) GHC will now use the system linker. The upshot of this is that a lot of nasty bugs in our own linker have been fixed - there are a few catches for users however. To that end, we've put together a GHC 7.8 FAQ\[GHC78FAQ\] to help people who might experience problems, dynamic GHC being one of them.

- **New and improved I/O manager** - Earlier this year, Andreas Voellmy and Kazu Yamamoto worked on a host of improvements to our I/O manager, making it scale significantly better on multicore machines. Since then, it's seen some other performance tweaks, and many bugfixes. As a result, the new I/O manager should scale linearly up to about 40 cores. Andreas reports their McNettle Software-defined-network (SDN) implementation can now achieve over *twenty million connections per second*, making it the fastest SDN implementation around - an incredible feat! \[McNettle\]

- **MINIMAL pragma**.  Twan van Laarhoven implemented a new pragma, `{-# MINIMAL #-}`, allowing you to explicitly declare the minimal complete definition of a class [\[Minimal](http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/pragmas.html#minimal-pragma)\].

- **Typed Holes**. Thijs Alkemade, with some help from Simon PJ, implemented typed holes.  These make it possible to tell GHC there is a 'hole' in a program, and have the compiler spit out an error stating what types are in scope. As a trivial example

  ```wiki
  Prelude> let f :: a -> a; f x = _

  <interactive>:6:24:
      Found hole ???_??? with type: a
      Where: ???a??? is a rigid type variable bound by
                 the type signature for f :: a -> a at <interactive>:6:10
      Relevant bindings include
        x :: a (bound at <interactive>:6:20)
        f :: a -> a (bound at <interactive>:6:18)
      In the expression: _
      In an equation for ???f???: f x = _
  ```

  GHC now tells us that the term `f` has a hole of type `a`, and there is a term `x :: a` in scope. So the definition is clear: `f x = x`. Holes are originally a concept borrowed from [Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php), and we hope they will be useful to Haskell programmers too!

- **Pattern synonyms** - Gerg?? ??rdi worked on an implementation of pattern synonyms for GHC, and it actually landed in the 7.8 release. While there's still more work to do, it represents a real improvement in GHC's support for abstraction.  More detail on the wiki page [\[PatSyn](pattern-synonyms)\].

- **New Template Haskell**.  Geoff Mainland did the heavy lifting to implement the new Template Haskell story, more or less as described in Simon's blog post [\[THBlog](template-haskell/blog-post-changes)\].  Template Haskell now has two flavours, which can inter-operate.  **Typed TH** is fully typed in the style of Meta ML, but works for expressions only.  **Untyped TH** is much more expressive, allowing splices in patterns, types, and declarations, as well as expressions, but is completely untyped.  Gergely Risko added support for creating and reifying annotations from Template Haskell.  The API for this feature may change in 7.10, but not drastically, probably only will be extended.  The overview of the feature with examples is detailed on the [TemplateHaskell/Annotations](template-haskell/annotations) page.

- **Closed type families** are a major extension to the type-family feature, implemented by Richard Eisenberg.  A closed type family allows you to declare all the equations for a type family in one place, with top-to-bottom matching; for example

  ```wiki
  type family Or a b where
    Or False False = False
    Or a     b     = True
  ```

  We thought this was going to be fairly easy, but it turned out to be much more interesting than we expected, and led to a POPL paper [\[ClosedFam](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/)\].

- **Safe coercions** extend the power of newtypes, one of Haskell's main data-abstraction features. For example, given

  ```wiki
  newtype Age = MkAge Int
  ```

  you can convert betwen `Age` and `Int` by using the `MkAge` constructor, knowing that the conversion is free at runtime.  But to convert betwen `Maybe Age` and `Maybe Int` you have to write code that unpacks and packs the `Maybe` type, and GHC cannot reasonably eliminate the cost.  Safe coercions let you do just that.  But (and this is not obvious) to be type-safe, in the presence of type families, we have to extend the type system with so-called *type roles*.  Moreover, using roles finally solves the notorious, seven-year-old Generalised Newtype Deriving bug (#1496).  Safe conversions were implemented by Joachim Breitner with help from Richard Eisenberg; there is a full description in our ICFP submission [\[SafeCo](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/)\].

- **New code generator** - As previously reported, the New Code Generator is live and switched on by default. There have been a host of bugfixes and stability improvements, meaning it should be solid for the 7.8 release.

- **Parallel --make** - as part of the haskell.org 2013 GSoC, Patrick Palka implemented a new parallel compilation driver, a long-requested feature. This allows GHC to build multiple modules in parallel when using `--make` by adding a `-j` flag, while having almost no overhead in the single-threaded case.

- **iOS support** - After many years of work by Ian, Stephen Blackheath, Gabor Greif and friends Luke Iannini and Maxwell Swadling, GHC now has full support for iOS cross-compilation. As of GHC 7.8, you'll really be able to write iOS apps in your favorite programming language!


That's just a fraction of what we did in the 7.8 timeline - there were at least a dozen other significant improvements, as you can see from the release notes [\[ReleaseNotes](http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/release-7-8-1.html)\]

# Future plans


There's still a lot planned for GHC 7.10, however. While we haven't quite decided when we'll release it, it will very likely be a short release cycle compared to the last one - which was the longest one we've had!

## Libraries, source language, type system

- **Applicative-Monad** - GHC 7.10 will (finally) make `Applicative` a superclass of `Monad`. This is an API-breaking change for `base`, and users are encouraged to begin fixing their code now. To that end, GHC 7.8 now emits warnings for code that would violate the Applicative-Monad proposal \[AMP\].

- **[ApplicativeDo](applicative-do)** - Now that `Applicative` is a superclass of `Monad`, Simon Marlow has plans to implement a new extension for GHC, which will allow `do` notation to be used in the context of `Applicative`, not just `Monad`.

- **Overloaded record fields** - In 2013, Adam Gundry implemented the new `-XOverloadedRecordFields` extension for GHC, described on the wiki \[ORF\]. This will finally be available in GHC 7.10.

- **Kinds without Data** - Trevor Elliott, Eric Mertens, and Iavor Diatchki have began implementing support for "data kind" declarations, described in more detail on the GHC wiki \[KD\]. The idea is to allow a new form of declaration that introduces a new kind, whose members are described by the (type) constructors in the declaration. This is similar to promoting data declarations, except that no new value-level-constructors are declared, and it also allows the constructors to mention other kinds that do not have corresponding type-level representation (e.g., \*). 

- **Explicit type application** - Stephanie Weirich, Richard Eisenberg and Hamidhasan Ahmed have been working on adding explicit type applications to GHC. This allows the programmer to specify the types that should be instantiated for arguments to a function application, where normally they would be inferred. While this capability already exists in GHC's internal language, System FC -- indeed, every FC-pro program has function application with explicitly applied types -- it has not been available in Haskell itself. While a lot of the syntax and design is not quite final, there are some details about the design available on the wiki \[TA\].

- **Using an SMT Solver in the type-checker** - Iavor Diatchki is working on utilizing an off-the-shelf SMT solver in GHC's constraint solver. Currently, the main focus for this is improved support for reasoning with type-level natural numbers, but it opens the doors to other interesting functionality, such as supported for lifted (i.e., type-level) `(&&)`, and `(||)`, type-level bit-vectors (perhaps this could be used to implement type-level sets of fixed size), and others.   This work is happening on branch `wip/ext-solver`.

- **Kind equality and kind coercions** - Richard Eisenberg (with support from Simon PJ and Stephanie Weirich, among others) is implementing a change to the Core language, as described in a recent paper \[FC\]. When this work is complete, *all* types will be promotable to kinds, and *all* data constructors will be promotable to types. This will include promoting type synonyms and type families. As the details come together, there may be other source language effects, such as the ability to make kind variables explicit. It is not expected for this to be a breaking change -- the change should allow strictly more programs to be accepted.

- **Partial type signatures** - Thomas Winant and Dominique Devriese are working on partial type signatures for GHC. A partial type signature is a type signature that can contain *wildcards*, written as underscores. These wildcards can be types unknown to the programmer or types he doesn't care to annotate. The type checker will use the annotated parts of the partial type signature to type check the program, and infer the types for the wildcards. A wildcard can also occur at the end of the constraints part of a type signature, which indicates that an arbitrary number of extra constraints may be inferred. Whereas `-XTypedHoles` allow holes in your terms, `-XPartialTypeSignatures` allow holes in your types. The design as well as a working implementation are currently being simplified \[PTS\].

## Back-end and runtime system

- **Dynamic space limits** - Edward has been working on dynamic space limits for Haskell, whereby you can run some code in a container with a maximum space limit associated with it.  There's working code \[RLIMITS\] but there are some barriers to getting it deployable in GHC (it requires a new compilation mode ala prof, and it doesn't yet work with GHCi or 32-bit). We're not yet sure if this will make it for 7.10, but look out!

- **CPU-specific optimizations** - Austin is currently investigating the implementation of CPU-specific optimisations for GHC, including new `-march` and `-mcpu` flags to adjust tuning for a particular processor. Right now, there is some preliminary work towards optimizing copies on later Intel machines. There's interest in expanding this further as well.

- **Changes to static closures for faster garbage collection** - Edward is working on an overhaul of how static closures represented at runtime to eliminate some expensive memory dereferences in the GC hotpath. The initial results are encouraging: these changes can result in an up to 8% in the runtime of some GC heavy benchmarks \[HEAPALLOCED\].

- **Coverity** - Austin & friends have began running the Coverity static analyzer over the GHC runtime system in an attempt to weed out bugs \[Coverity\]. This has luckily reported several very useful issues to us, and identified some possible cleanup. These fixes are also going into the 7.8 branch, and GHC and its associated code will be scanned by Coverity continuously in the future.

- **New, smaller array type** - Johan Tibell has recently added a new array type, `SmallArray#`, which uses less memory (2 words) than the `Array#` type, at the cost of being more expensive to garbage collect for array sizes large than 128 elements.

- **DWARF-based stack tracing** - Peter Wortmann and Arash Rouhani (with support from the Simons) are working on enabling GHC to generate and use DWARF debugging information. This should allow us to obtain stack traces and do profiling without the need for instrumentation.

## Frontend, build-system, and miscellaneous changes

- **Repo reorganization** One big thing that Herbert Valerio Riedel has been tackling has been the problematic situation with GHC's current usage of git submodules and `./sync-all`. This is one of our most common complaints from newcomers and people attempting to help with development (with good reason), and we're hoping within the 7.10 timeframe, GHC will be far easier to clone and work on.

>
>
> To this end, we've already done some massive simplification - in HEAD, the repositories for `base`, `testsuite`, `template-haskell`, `ghc-prim`, `integer-gmp` and `integer-simple` are now part of GHC's repository itself. These repositories are commonly developed in lockstep with GHC, and it greatly helps in many workflows, including bisection of bugs.
>
>

>
>
> Moreover, the remaining packages officially maintained by the core library committee that are currently managed via GHC's Trac will be relocated to the Haskell GitHub organization in order to have GHC Trac focus on developing GHC proper as well as reduce the overhead for casual contributors to file issues and submit simple fixes for those packages.
>
>

- **Continuous integration improvements** - Work on new CI systems for GHC has been slow, but thanks to the work of **Joachim Breitner** and **G??bor P??li**, GHC is now built on [http://travis-ci.org](http://travis-ci.org) \[TravisCI\] as well as nightly builders of a variety of flavors and machines \[Builders\]. We're also hoping to investigate using a Continuous Integration system to help build against a stable set of selected Hackage packages, to help find issues with the releases more easily.

- **Debian builds of GHC** - Thanks to **Joachim Breitner** and **Herbert Valerio Riedel**, GHC now has greatly improved support for Debian packaging - there is now an official Ubuntu PPA for GHC \[PPA\], as well as a dedicated Debian repository for GHC nightly builds \[DEB\].

# References



\[GHC78FAQ\] [https://ghc.haskell.org/trac/ghc/wiki/GHC-7.8-FAQ](https://ghc.haskell.org/trac/ghc/wiki/GHC-7.8-FAQ) 

\[ClosedFam\] Closed type families with overlapping equations, POPL 2014 [http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/) 

\[Minimal\] MINIMAL pragma [http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/pragmas.html\#minimal-pragma](http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/pragmas.html#minimal-pragma) 

\[PatSyn\] Pattern synonyms [http://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms](http://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms) 

\[ReleaseNotes\] GHC 7.8.1 release notes [http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/release-7-8-1.html](http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/release-7-8-1.html) 

\[SafeCo\] Safe Coercions, submitted to ICFP 2014 [http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/](http://research.microsoft.com/en-us/um/people/simonpj/papers/ext-f/) 

\[THBlog\] Major revision of Template Haskell [https://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges](https://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/BlogPostChanges) 

\[AMP\] [https://github.com/quchen/articles/blob/master/applicative_monad.md](https://github.com/quchen/articles/blob/master/applicative_monad.md) 
 
\[KD\] Kinds without Data - [http://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData](http://ghc.haskell.org/trac/ghc/wiki/GhcKinds/KindsWithoutData)  
 
\[ORF\] [https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields](https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields) 

\[TA\] Explicit type application - [http://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication](http://ghc.haskell.org/trac/ghc/wiki/ExplicitTypeApplication) 

\[FC\] System FC with Explicit Kind Equality - [http://www.seas.upenn.edu/\~eir/papers/2013/fckinds/fckinds-extended.pdf](http://www.seas.upenn.edu/~eir/papers/2013/fckinds/fckinds-extended.pdf) 

\[PTS\] [https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures](https://ghc.haskell.org/trac/ghc/wiki/PartialTypeSignatures) 

\[Coverity\] [https://scan.coverity.com](https://scan.coverity.com) 
 
\[McNettle\] [http://haskell.cs.yale.edu/?post_type=publication&p=821](http://haskell.cs.yale.edu/?post_type=publication&p=821) 
 
\[PPA\] [https://launchpad.net/\~hvr/+archive/ghc/](https://launchpad.net/~hvr/+archive/ghc/) 
 
\[DEB\] [http://deb.haskell.org](http://deb.haskell.org) 
 
\[TravisCI\] [https://github.com/nomeata/ghc-complete](https://github.com/nomeata/ghc-complete) 
 
\[Builders\] [https://ghc.haskell.org/trac/ghc/wiki/Builder](https://ghc.haskell.org/trac/ghc/wiki/Builder) 

\[HEAPALLOCED\] [https://ghc.haskell.org/trac/ghc/ticket/8199](https://ghc.haskell.org/trac/ghc/ticket/8199) 
 
\[RLIMITS\] [http://ezyang.com/rlimits.html](http://ezyang.com/rlimits.html) 

\[ReleaseNotes\] [http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/release-7-8-1.html](http://www.haskell.org/ghc/docs/7.8.1/html/users_guide/release-7-8-1.html) 


