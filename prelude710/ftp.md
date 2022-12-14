# The 7.10 Prelude should be generalized


As per [Prelude710](prelude710), this page attempts to address concerns about the [Foldable/Traversable Proposal](https://wiki.haskell.org/Foldable_Traversable_In_Prelude).


A brief summary of both this plan and the alternative being considered at this point is available at [Prelude710](prelude710), while the details of the **Plan List** counter-proposal are available at [Prelude710/List](prelude710/list). 


In 2013, two proposals passed through the libraries@ process. Unlike most proposals before them, these proposals affect types in the Prelude. These are the [Applicative/Monad Proposal (AMP)](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal) and the [ Foldable/Traversable Proposal (FTP)](https://wiki.haskell.org/Foldable_Traversable_In_Prelude) (also sometimes referred to as the "Burning Bridges Proposal" based on the title of the original thread).


You can try out both of these solutions today simply by downloading GHC 7.10RC2.


It has recently been highlighted that as these changes affect the `Prelude`, and thus affect what users of Haskell see out of the box, they should be held to a higher bar than the usual libraries@ traffic. In particular, there was concern that while the [Applicative/Monad Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal) was warned about extensively in GHC 7.8, the [ Foldable/Traversable Proposal](https://wiki.haskell.org/Foldable_Traversable_In_Prelude) was not nearly as well broadcast.



However, there are many good reasons to do both the AMP and FTP generalizations at this time.


- **`Foldable` and `Traversable` have seen long use in the Haskell community**, predating even the existence of `Applicative`, dating back into the early 2000s. We know and have tested these abstractions, and they have deep explanatory power.

-  `Traversable` in particular has given us insight into the nature of finitary traversals and have been the subject of many papers since Jeremy Gibbons wrote [The Essence of the Iterator Pattern](http://www.cs.ox.ac.uk/jeremy.gibbons/publications/iterator.pdf) each of which has managed to narrow the set of laws until we're left with only the **obvious generalization of the `Functor` laws** that allows for `Applicative` effects. [ An Investigation of the Laws of Traversals](http://arxiv.org/pdf/1202.2919v1.pdf) is one such paper, providing the common sense reading of the `Traversable` laws.

- However, `Traversable` is too strong a requirement for many of the operations it enables. `foldMap` is what you get when you limit the power of `traverse` to just consumption, so `Foldable` has a role in this hierarchy. It is particularly telling that almost all the interesting combinators enabled by `Traversable` are actually enabled by merely `Foldable` constraints, including some at-first-surprising results such as `traverse_`. **`Foldable` shows us a fairly deep connection between 33+ seemingly unrelated operations** from the `Prelude`. `Traversable` adds connections between several more to that mix.

- Despite this broad explanatory power and the ability to derive connections between a large chunk of the combinators that came before these abstractions were even named, they remain relegated to modules off to the side and are **harder to use for the simple reason that using them requires qualified imports or massive amounts of manual hiding**.

- **By generalizing the `Prelude`, the `Prelude` will no longer cause name collisions anywhere within `base`**. This is a very simple rule to state; it is a very simple rule to understand. This is no panacea, other packages can and do still export combinators that collide with `Prelude`, but it means that simple things like explaining how `traverse` relates to `mapM` in `ghci` will no longer invite a comedy of errors and name conflicts. While don't _quite_ fully achieve this aim through GHC 7.10, as `Control.Category` still collides with `id` and `(.)`, the FTP gets us to within spitting distance.

- Nothing in `Foldable` or `Traversable` ever changes the "shape" of the container it is given. This is actually more information for the user than seeing a combinator accepts any list and returns any list. While we give up the knowledge that the thing we're walking over is a list, we gain information as well. **The more general signatures can often tell the user more** about what a combinator can do behind the scenes. e.g. we can know that `forM` will not change the number of elements in the container by leaning on the `Traversable` laws alone and the lack of any other way to construct the result value. We're not just able to rely the fact that it gives back a list, but we can rely on the specific shape of the structure we get back.

- At the time of the "Burning Bridges Proposal" thread on the libraries mailing list back in 2013, this question was widely polled, and there was an **overwhelmingly strong call from the community for generalization**. Admittedly, this was from the self-selecting subset of the community that is active on the [libraries@ mailing list](https://www.haskell.org/mailman/listinfo/libraries).

- One thing that we were very careful to avoid during this entire wave of generalization is an impact to performance on either asymptotic or constant factor grounds. The **continuous performance testing ** that is done on GHC has done a lot to keep us honest in this regard, but has definitely complicated development.

- For better or worse, **several hundred packages have already been adapted** be compatible with GHC 7.10RC2 expecting that it would be a reasonable approximation of the final, shipping version of GHC 7.10. Package authors who have been actively working and who have attempted to build completely without warnings will have changes to partially undo. Keep in mind that they need to keep the AMP changes, and just roll back the FTP related ones, if the [Prelude710/List](prelude710/list) plan was followed. Rolling these changes back would mean that we're going to ask our most proactive users to re-release the majority of these packages, and to use the ability to change .cabal files for historic versions to make their old versions uninstallable or risk them being selected by cabal in the build plan. That is a messy solution at best. We're better as a community at "getting ahead" and making changes early in a GHC release cycle. Far more code is "GHC 7.10 ready" at this point in the release cycle than was at a comparable point in the GHC 7.8 cycle, which exacerbates this problem.

- **Stretching out these sorts of changes over many releases increases the complexity** of CPP and other tricks that authors have to use to maintain their packages **over long support windows**, so there is a case to be made for a fairly "sharp shock" to get this out of the way at the same time as the `Applicative`-`Monad` Proposal. Results so far have shown that the AMP changes break far more code than generalizing these signatures.

- Finally, once we make `Applicative` a superclass of `Monad`, **`mapM` is basically never the right combinator** for, well, anything. In particular requires far too tight a constraint (`Monad` rather than `Applicative`). Generalizing `mapM` to `Applicative`, runs afoul of several other issues stated below in the FAQ. On the other hand, going the 'wrong' direction and adding a monomorphized `traverse` breaks far more code, and makes the scoping problems even worse. Once you bring in a generalized `traverse`, it seems a very arbitrary choice to avoid bringing in the context necessary to define instances of the abstraction it uses.

## Details


Bringing `Foldable` and `Traversable` into the `Prelude` attempts to balance a number of competing concerns.


With the advent of the AMP, bringing `traverse` into scope is rather important. `mapM` is restricted to `Monad`.


The obvious paths to get there that don't require hard to explain excuses have the side benefits of letting us clean up the library by removing duplication and generalize things, but any such cleanup has to be tempered by pragmatic considerations:

- **Avoid incurring a major performance penalty.** Going in, this was actually a pressing concern. Would generalizing the Prelude to Foldable compromise foldr/build fusion? We went in expecting some performance loss, but for the most part haven't seen any at all and have actually seen some speedups.

- **Avoid introducing massive breakage.** -- see the issues surrounding `Data.List` in the FAQ below.

- **Avoid requiring a future Haskell Report to lean on a much larger language standard** or veering off into uncharted territory. This pushes us away from designs such as `mono-traversable` or `IsList`.


The details of the current implementation are available on the [Foldable/Traversable in Prelude](https://wiki.haskell.org/Foldable_Traversable_In_Prelude) HaskellWiki page. That article, however, remains written from a more neutral point of view.

## FAQ

# `Foldable` has lots of class members. Why is it so complicated?



The first variant of `Foldable` that people tend to propose is to reduce it to something like


```
class Foldable f where
  toList :: f a -> [a]
```


But this requires us to be able to fully re-associate all of the elements of the structure `f` to the right to make a list out of them.



To repair that we need to switch to something like


```
class Foldable f where
  foldMap :: Monoid m => (a -> m) -> f a -> m
```


This gives us a version where folding over the container relies strictly on the monoid laws of `m`, but doesn't require us to use the ability to reassociate an infinite number of times.


So, why isn't `Foldable` just this?


It turns out that `foldr` implemented in terms of `foldMap` builds up a gigantic function chain before invoking it. This can leak space, and runs slower, and some variants of this can even blow the stack. Allowing versions of these combinators to be supplied for specific containers can avoid such overhead, so several years ago when the class was first concocted a number of specialized folds were added to address such concerns.


Some combinators in `Foldable` compute their answers by different means than their Prelude counterparts, even just by exploiting the `Monoid` laws. e.g. `sum = getSum . foldMap Sum`, for some containers can compute in exponentially faster time than the traditional `Prelude`'s `sum = foldl (+) 0` definition.



There are at least 3 different camps out there for what the proper definition of `sum` should be:


```
sum = foldl (+) 0
sum = getSum . foldMap Sum
sum = foldl' (+) 0
```


The first group is those who believe sum should follow the existing `Prelude` behavior at all costs.


The second group is concerned with ensuring the correct asymptotics for the operation.


The third group notes that the `foldl` definition is sadly na??ve, and requires stack frames for every entry in the list, especially if the numeric type involved isn't known and so the compiler can't optimize away to something closer to the third representation based on strictness information for `(+)`.


By adding some of these methods where the `Prelude` behavior and `Data.Foldable` behavior to the class, we are able to ensure that the existing semantics hold for existing programs that were using the `Prelude` combinators without compromising on the asymptotic behavior of code that is written with `Foldable` today.


At the same time, an unintended but interesting consequence is that members of that third group can wire up their container types to get vastly improved performance over the Prelude version as well.


Finally, in a proposal back in September, David Feuer and Reid Barton proposed adding several more members to `Foldable` to enable specific containers to offer asymptotically even more efficient versions of many combinators.

`Foldable` is nearly as small as it can be without either changing the semantics of existing programs on their authors, or compromising the asymptotic performance of `Foldable` operations.

# `Traversable` contains both `Monad` and `Applicative` variants for each function, and following the `Applicative`-`Monad` proposal, the `Monad` variants (`mapM` and `sequence`) are now redundant.


We cannot remove `mapM` from the `Traversable` class without a deprecation cycle. Users have defined \*many\* instances of this class in the wild that happen to define their `mapM` member manually rather than rely on the default newtype-wrapper-based implementation.


But there is another technical concern: it turns out that there exists a form of container that you can write for which `traverse` will blow the stack when `mapM` will not! If we are able to eventually resolve this conflict, then by all means, a longer term goal would be to deprecate the redefinition of the members of `Traversable` other than `traverse`, move the remainder to top level definitions outside of the class, and generalize their signatures. However, we don't currently see how to get there from here and it remains not only possible, but probable that this issue cannot be resolved.

# Given `Foldable` and `Traversable` may benefit from further refinement, dragging them into `Prelude` seems premature.


The combinators will remain, on the other hand whether we go through a smooth deprecation cycle to remove some from the class and move them out to top level definitions when and if we can find ways to implement them without suffering an asymptotic or large constant factor hit is the major concern.


We are proactively seeking ways to resolve this issue. Ticket #10071 explores adding the ability to deprecate class member redefinition. This gives us the ability to move things out of the class over a pair of release cycles, should we find something we can improve in this manner.

# `Data.List` now has many functions that don't mention list in their type signature. Having such functions in the `Data.List` module is awkward from a naming perspective.


From a data-driven perspective, the vast majority of users of `Data.List` import it unqualified to get at other combinators, such as `sort`, which aren't mentioned in the `Prelude`. Let's call this "group A". Having such an import break dozens of unrelated combinators seems like a bad idea.


By exporting generalized versions rather than removing them, we are able to support group A, but also the second most common scenario, where users of `Data.List` follow the qualified import pattern of `Data.Map`, let us call this "group B".


However, it is an ugly intermediate state at best.


There are two clear paths for how to evolve `Data.List` from here.


1.) Deprecate the re-export of the methods from the `Prelude` in GHC 7.12 and to remove them entirely in GHC 7.14. This ensures that group A never feels any pain at all, and that group B gets a deprecation window of warnings notifying them that they don't have to use the combinators qualified any more. The cost of this approach is that we'd have no place in `base` to house monomorphic versions of these combinators. Ticket #4879 addresses the need for deprecated re-exports, which are useful for many things and a patch is now available that can enable this functionality.


2.) Concoct some form of `{-# WEAK #-} ` pragma or enable users to export type restricted versions of another combinator and then apply this pragma to these members of `Data.List`. This could revert those combinators to monomorphic form, but requires a more controversial language extension that has some potentially thorny implementation issues to work through, and we've elected not to presume they can be resolved.


By adopting this admittedly awkward intermediate state we enable the maximal amount of existing code to continue to work, without committing to either one of these plans at this time without broader community discussion.

# There are lots of functions that could be generalized further, but are not. For example, `mapM`, `forM` and `sequence` could all be expressed in terms of `Applicative` instead of `Monad`.


We can't remove `mapM` for the reasons mentioned above at this time: It is an existing member of the class and can't be removed without a deprecation cycle, but it also has the concrete counter-example where `mapM` can avoid blowing the stack where `traverse` must. `sequence` is similarly an existing member of the class, and can't be removed without a deprecation cycle, and can't be generalized without breaking the existing instances and since it is implemented through `mapM id` suffers the same concern as `mapM`.

# Similarly things like `length` could be generalized to `Num`, making `length` and `genericLength` equivalent.

`length` is left ungeneralized with regards to the numeric type primarily because `genericLength` has absolutely abysmal performance. One of the pragmatic guidelines we followed is that we can't make code slower.


Reply by dfeuer: The abysmal performance of `genericLength` is not inherent to its type signature, but rather to its definition, which makes it good for producing certain varieties of lazy natural number while making it terrible for anything else. We could give `length` a sensible default by applying `fromIntegral` to a `Word`, or simply by using a strict left fold. The biggest potential problem, I believe, is that code that uses `length` and does not give the result an explicit signature will get an `Integer` by default, which seems rather rude.

# While the `Prelude` operations (e.g. `foldr`) will now work on containers such as `Vector`, they still won't work on things like `ByteString` or `Text`, which in some code is used far more than other non-list containers.


The same could be said for `Functor`.


However, we derive a lot of benefit from having polymorphism on hand to help enforce one of the `Functor` laws. We have two `Functor` laws:

```wiki
fmap id = id
fmap f . fmap g = fmap (f . g)
```


However, given parametricity, once you have proven the first one, the second follows via a free theorem. This ensures that Functor instances are easy to write, and hard to screw up. Moreover, the existing `Functor` class can be defined entirely within Haskell 98/2010.


The trend of API duplication for monomorphic containers cannot be entirely reversed without accepting a lot of limitations, moving to a library that lies outside of what is standardizable, and simultaneously giving up a lot of the nice theoretical properties that motivate `Foldable` and `Traversable`.


Packages such as `mono-traversable` attempt to address this more general need. However, they rely on many language extensions. Inviting an API that is necessarily based on type families or MPTCs + fundeps into standards discussion would be a huge move, but then there are pragmatic concerns around polymorphic recursion and the details of all intermediate types used during a `Traversal` leaking into the signature of your methods, so the `mono-traversable` path is not a clear win. 


Finally, the `mono-traversable` path is also incapable of handling type-changing assignments. Variants which are capable of type-changing assignment, such as `Each` in the `lens` package introduce a number of type inference concerns in practice.

# Some functions in `Data.List` could be generalized to `Foldable`, but have not been. For example, `isPrefixOf` and `isInfixOf` can be generalised. More generally, anything with a list in an argument position can be generalized.


There are two lists involved in the signatures of `isPrefixOf` and `isInfixOf`. Which gets generalized? Both? Adding `toList` to the class to avoid destroying sharing on the list case with `foldr (:) []` enables us to match the existing performance of these operations even with generalized signatures. However, they aren't in the `Prelude`, and they aren't historically in `Data.Foldable`. There isn't a strong argument _against_ generalizing them except for the fact that the generalized forms would have no good place to live. The changes we've made enable such general code to be written efficiently by users who want them.

# Some functions in `Data.List` could be generalized to `Traversable`, but have not been. For example, `sort` and `reverse` can be generalized. However, such generalizations are likely to add a performance penalty.


The runs afoul of the "first do no harm" principle from the standpoint of performance. 


Also it turns out that none of members we've added in this current wave of generalization were added strictly for _constant_ performance factors, merely asymptotic ones, or due to increased stack utilization causing asymptotically more space usage.

# Given that lots of functions could be generalized, it seems we should either generalize everything, or have a good story for where to stop. For example, `isPrefixOf` can be generalized, but the related function `stripPrefix` can only be partly generalized, so should `isPrefixOf` be generalized?


A generalized `isPrefixOf` lacks a good home, so fortunately we're spared being hoist on the horns of this particular dilemma.

# The `IsList` class is an alternative generalization that could be made for some functions, and would work for `ByteString` and `Text`. Neither `Foldable` nor `IsList` is strictly more general, so both are potential alternatives.


It also gives up a number of existing inhabitants, or gives up the ability to reason polymorphically about the existing inhabitants such as `Data.Map`. There are _many_ `Foldable` containers that contain more than just their elements. `IsList` requires both the ability to consume the elements of a structure but the ability to construct the structure as well.

# It should also be noted that `Traversable` can be added to `Prelude` without adding `Foldable`.  Today `Foldable` is a superclass of `Traversable`, but there is no real need for that.  (E.g., building the lens package and all its dependencies only requires 9 trivial changes when `Foldable` is no longer a superclass.)


Removing `Foldable` as a superclass of `Traversable` destroys a relationship between the `Traversable` class and 33+ operations with well known and well understood semantics.


The same argument has historically been applied to argue against adding `Applicative` as a superclass of `Monad`. 


It can also be applied equally well to say that `Ord` doesn't need `Eq` as a superclass, despite the fact that `Ord` provides us obvious laws giving us guidance for how the corresponding `Eq` should work.


It would seem remarkably backwards to finally unify all the work on `Applicative` and `Monad` and simultaneously cleave apart all the work on `Foldable` and `Traversable` creating a variant on the `ap` vs. `(<*>)`, `fmap` vs. `liftM` problem but now spanning dozens of combinators.

# The existing corpus of books, tutorials, syllabi, and the like usually have a significant portion of the text dedicated to these very `Prelude` functions - and they would all need significant revision.

[Real World Haskell](http://book.realworldhaskell.org/read/io.html#x_TE) dispels this same sort of concern around the generality of the `Monad` operations with a quick aside:


Tip: These functions actually work for more than just I/O; they work for any `Monad`. For now, wherever you see `m`, just think `IO`.



Moreover, at least two books aimed at beginners (the very popular [Learn You a Haskell](http://learnyouahaskell.com/functors-applicative-functors-and-monoids) as well as "Beginning Haskell") already teach and promote the `Foldable` and `Traversable` abstractions with words like


>
>
> Because there are so many data structures that work nicely with folds,
> the `Foldable` type class was introduced. Much like `Functor` is for
> things that can be mapped over, `Foldable` is for things that can be
> folded up!
>
>


or


>
>
> As you saw in the previous chapter, lots of different algorithms can be
> expressed using folds. The module Data.Foldable includes most of them,
> like maximum or elem. One easy way to make your functions more general
> is hiding the functions with those names from the Prelude and importing
> the ones using Foldable.
>
>


...and go on to promote their use with the words


>
>
> But now that you know about them, you should aim for the largest
> degree of abstraction that you can achieve.
>
>


So, generalizations such as `Foldable`/`Traversable` are in fact taught (and recommended) to beginners via existing books. Its use is rather going to increase the more newcomers read those books, so we should rather make sure they're properly integrated into the standard libraries than sending mixed signals by making them awkward to use.

# Teaching beginners what the new types mean in their full generality is going to be a challenge.


This somehow has never been a problem for Python. They don't even have the types to help guide them!

# We could support restricting type signatures in export lists, so that when both a specific and general version are imported they do not clash.


Such an extension could be used as a blunt instrument to make it so that imports of `Data.Foldable` and `Data.Traversable` combinators would automatically shadow the `Prelude` versions by having the `Prelude` versions be type-restricted versions of whatever `Data.Foldable` and `Data.Traversable` supply, but the end result of this is that all users still have to use those imports, and those concepts remain demoted to fringes.


However, this does nothing to address the desire for a more general replacement for `mapM` suitable for `Applicative` to be in scope, which comes about as a first-order consequence of the AMP.


There also remain questions of what happens when two such restricted imports come into scope, as well as type checker concerns, and as such an extension starts affecting the behavior of the Prelude out of the box, it becomes a language extension that we now have to standardize.


All of these issues conspire to make this alternative far less palatable than it appears at first glance.


When and if such a proposal was implemented we could definitely use this to resolve the intermediate `Data.List` ugliness, yes! 


Of course, this proposal isn't unique in that regard. We could also use a `WEAK` pragma that made it so an export was only used in the absence of a normal export, or a `QUALIFIED` pragma that said we only brought an identifier into scope when it was used explicitly qualified or via an explicit import list, or one of several other such proposals to allow the combinators we've generalized in `Data.List` to revert to a more palatable, simpler state.


To do any of these, however, we'd still have to accept a report-affecting language extension, which is a much bigger proposal than generalizing a few signatures in a way that does not affect the semantics of existing programs.

# A language pragma could be used select alternative `Prelude` options.


This fails on many grounds. 

- `Control.Monad` re-exports `mapM`, `mapM_`, `sequence`, `sequence_`, `forM` and `forM_`. Which version do users get?

- `Data.List` re-exports large numbers of combinators from the Prelude as well.

- The `mtl` and many other libraries based around monads and monad transformers re-exports `Control.Monad`, so you get a whole ecosystem where where folks some times have 15+ imports at the top of a module, and every one of them would have to agree on which variant of the `Prelude` they were re-exporting or you'd start getting conflicts.


If we removed these re-exports, then in the future we'd be able to employ a strategy like this much more easily, but it isn't an option today.

# A module with only the non-`Foldable` overlapping bits of `Data.List` could be created, allowing users who wanted `Foldable` plus some list functions to avoid name clashes.


This requires a proactive change on the behalf of a rather large segment of users and is a path we for which we can't offer a nice deprecation cycle.

# The functions in `Data.Foldable` and `Data.Traversable` could be renamed not to clash. It might be reasonable to rename its `foldr` something like `ffoldr`.


This proposal ignores the fact that a very large segment of the community has already been using those combinators for several years, and that they have been actively taught in books for almost four years.

# How may I express my support or disapproval?


We actively are seeking feedback through a [survey](https://goo.gl/forms/XP1W2JdfpX) through February 21st. Once the results are in Simon Peyton Jones and Simon Marlow have agreed to weigh the matter and decide the direction we will take in GHC 7.10.

# I have a question/concern not addressed here... ?


There's an IRC channel `#haskell-ftp` (on freenode) where we're happy to try to answer additional questions you may have!
