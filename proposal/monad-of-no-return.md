
This proposal has been revised based on the feedback gathered from the [proposal discussion](proposal/monad-of-no-return#discussion). The original revision of this proposal can be found [here](https://mail.haskell.org/pipermail/libraries/2015-September/026121.html). See wiki page [History](proposal/monad-of-no-return?action=history) for changes relative to that first revision.

# Monad of no `return`/`>>` Proposal (MRP) <sup>*2e*</sup>

## Abstract


To complete the `Monad`-hierarchy refactoring started with AMP (& MFP) and unify `return`/`pure` & `>>`/`*>`, move `Monad(return)` and `Monad((>>))` methods out of the `Monad` class into top-level bindings aliasing `Applicative(pure)` and `Applicative((*>))` respectively.


The original proposal didn't include `(>>)` yet. But in the interest of bundling related changes, taking care of `(>>)` has been added to this proposal.

## Current Situation


With the implementation of Functor-Applicative-Monad Proposal (AMP)\[1\] and
(at some point) the MonadFail proposal (MFP)\[2\] the AMP class hierarchy
becomes


```
class  Functor f  where
    fmap    :: (a -> b) -> f a -> f b

class  Functor f => Applicative f  where
    pure    :: a -> f a
    (<*>)   :: f (a -> b) -> f a -> f b
 
    (*>)    :: f a -> f b -> f b
    u *> v  = …
 
    (<*)    :: f a -> f b -> f a
    u <* v  = …

class  Applicative m => Monad m  where
    (>>=)   :: m a -> (a -> m b) -> m b

    return  :: a -> m a
    return  =  pure

    (>>)    :: m a -> m b -> m b
    m >> k  =  m >>= \_ -> k

class  Monad m => MonadFail m  where
    fail    :: String -> m a
```


Consequently, the `Monad` class is left with a now redundant `return`
method as a historic artifact, as there's no compelling reason to
have `pure` and `return` implemented differently. 



More to the point, this redundancy violates the
"**making illegal states unrepresentable**" idiom: Due to the default 
implementation of `return` **this redundancy leads to
error-prone situations** which aren't caught by the compiler; for instance, when
`return` is removed while the `Applicative` instance is left with a
`pure = return` definition, this leads to a cyclic definition which
can be quite tedious to debug as it only manifests at runtime by a
hanging process.


Traditionally, `return` is often used where `pure` would suffice
today, **forcing a `Monad` constraint even if a weaker `Applicative`
would have sufficed**. As a result, language extensions like `ApplicativeDo`\[3\] have to
rewrite `return` to weaken its `Monad m =>` constraint to
`Applicative m =>` in order to benefit existing code at the cost
of introducing magic behavior at the type level.


An additional (somewhat minor) benefit results from having **smaller class dictionaries**, as well as **avoiding the additional indirection** through the `Monad` class dictionary (when the class dictionary can't be resolved at compile time).


For `(>>)`, in addition to arguments applying to `return`, the
status quo is optimising `(>>)` and forgetting about `(*>)`, resulting
in **unexpected performance regressions** when code is generalised from `Monad` to `Applicative`.
This unfortunate situation also **blocks us from being able to remove
the post-AMP method redundancy in the `Foldable`/`Traversable` classes**.


Finally, this redundancy becomes even more significant when viewed in
light of the renewed Haskell standardisation process\[7\]: The next
Haskell Report will almost certainly incorporate the AMP (and MFP)
changes, and **there is no justification for the next Haskell Report to retain
`return` nor `(>>)` as methods of `Monad`**. A good reason would have been to
retain backward compatibility with Haskell 2010. However, as the AMP
superclass hierarchy requires `Monad` instances to be accompanied by
`Applicative` instances (which aren't part of Haskell 2010, c.f. \[6\]),
backward compatibility with Haskell 2010 goes out the window when it
comes to defining `Monad` instances (unless via use of `-XCPP` or
similar). Consequently, meeting the high bar for a formal document
such as the Haskell Report demands that `Monad` shall not carry a
redundant `return` method that serves no purpose anymore. Moreover,
getting `return` out of the way is desirable to facilitate
standardising potential candidates such as the earlier mentioned
`ApplicativeDo` in the future and avoids the technical debt incurred
by keeping around this language wart.


When considered out of context, the enumerated reason above could be considered 
weak on their own and would maybe not be enough to carry this proposal
individually. But put together, those smaller benefits form one bigger
composite benefit, and taken in the context of the recent AMP, and the
upcoming MFP, the costs are comparatively low (especially with the
reduced-breakage-transition-strategy), and it makes sense to settle this technical debt soon
while it's still relatively cheap. 


It's easy to underestimate the infinite accrued cost of
retaining language warts which persist in the language indefinitely.

## Proposed Change


Remove `return` and `(>>)` as methods from the `Monad` class and in its place
define top-level bindings with the weaker `Applicative` typeclass
constraint:


```
-- | Alias for 'pure' 
return :: Applicative f => a -> f a
return = pure

-- | Alias for `(*>)`
(>>) :: Applicative f => f a -> f b -> f b
(>>) = (*>)
```


This allows existing code using `return` to benefit from a weaker
typeclass constraint as well as cleaning the `Monad` class from
redundant methods in the post-AMP world.


A possible migration strategy is described further below.

## Compatibility Considerations


Generalizing the type signature of a function from a `Monad`
constraint to its superclass `Applicative` doesn't cause new
type-errors in existing code.


However, moving a method to a top-level binding obviously breaks code
that assumes e.g. `return` to be a class method. Foremost, code that
defines `Monad` instances is at risk:

### Instance Definitions


Code defining `return` (or `(>>)`) as part of an instance definition
breaks. However, `(>>)` has a default implementation in Haskell 98/2010 in terms of `(>>=)`, and
we had the foresight to provide a default implementation in `base-4.8` for `return` so that the following
represents a proper minimal instance definition post-AMP:


```
instance Functor Foo where
    fmap g foo  = …

instance Applicative Foo where
    pure x      = …
    a1 <*> a2   = …

instance Monad Foo where
    m >>= f     = …

    -- NB: No mention of `return` nor `(>>)`
```


Consequently, it is possible to write forward-compatible instances omitting `return`
that are valid starting with GHC 7.10/`base-4.8`.


Heuristically `grep`ing through Hackage source-code reveals a
non-negligible number of packages defining `Monad` instances with
explicit `return` definitions\[4\]. This has a comparable impact to the
AMP, and similarly will require a transition scheme aided by compiler
warnings.


As large code bases are reported to not have been updated to GHC 7.10 yet, it's more economical to follow-up with MRP warnings closely in the wake of AMP/MFP to have all required `Monad`-related changes applied in one sweep when upgrading.

### Module Import/Export Specifications


A second source of incompatibility may be due to
`import`s. Specifically module import that assert `return` to be a
method of `Monad`, e.g.:


```
import Control.Monad  (Monad ((>>=), return))
```


or


```
import Prelude hiding (Monad(..))
import Control.Monad  (Monad(..)) as Monad

f = Monad.return ()
```


The dual situation can occur when re-exporting `return` or `(>>)` via module
export specifications.


However, given that `return` and `(>>)` are (re)exported by `Prelude` and the examples
above are rather artificial, we don't expect this to be a major source
of breakage in the case of `return`. In fact, a heuristic grep\[5\] over
Hackage source-code revealed only 21 packages affected.

### Tool for (Semi)Automatic Refactoring


There is ongoing work in the
[Hs2010To201x](https://github.com/hvr/Hs2010To201x) project to
provide automatic refactoring assistance for migrating pre-AMP
code-bases to AMP+MFP+MRP. Tooling of this sort can dramatically
reduce the maintenance cost incurred by the recent `Monad`
restructuring changes.

### Example for writing future-proof code

**GHC extension** to reduce code-breakage: 


When `(>>)` and `return` are moved out of the `Monad` class, GHC would still tolerate (as a NO-OP) the lawful definitions for `(>>)` and `return` as used in the example above (and otherwise emit an error).



This way, code can be made forward compatible the desired semantics *without* the use of `-XCPP`.


```
instance Functor Foo where
    fmap f x    = …

instance Applicative Foo where
    pure x      = …
    a1 <*> a2   = …
    a1 *>  a2   = …  -- only needed when
                     -- optimised version possible

instance Monad Foo where
    m >>= f     = …

    -- see note for GHC extension ignoring the two 
    -- lawful definitions:
    (>>)   = (*>)
    return = pure  -- only needed for compatibility
                   -- with base < 4.8
```

## Migration Strategy

### Original Simple Variant


This transition scheme is **not** proposed anymore; see [new strategy below](proposal/monad-of-no-return#reduced-breakage-variant)


In this transition scheme, the time when **Phase 2** starts is determined by the amount of packages already converted at that time. "GHC 8.2" is only the earliest *theoretical* time to begin **Phase 2**, but a more realistic time would be "GHC 8.6" or even later.



The migration strategy is straightforward:


<table><tr><th>Phase 1 <i>(GHC 8.0)</i></th>
<td>Implement new warning in GHC which gets
triggered when <tt>Monad</tt> instances explicitly override the
default <tt>return</tt>/<tt>(>>)</tt> methods implementation.
</td></tr></table>


<table><tr><th>Phase 2 <i>(GHC 8.2 OR LATER)</i></th>
<td>When we&apos;re confident that the
majority of Hackage has reacted to the warning (with the help of
Stackage actively pursuing maintainers to update their packages) we
turn the <tt>return</tt> and <tt>(>>)</tt> methods into a top-level binding and remove the
warning implemented in <b>Phase 1</b> from GHC again.
</td></tr></table>


### Reduced Breakage Variant



**Based on the feedback from the proposal discussion this revised transition scheme is expected to address all concerns raised**: This scheme aims to avoid breakage while allowing code to be written in a way working across a large range of GHC versions with the same semantics. Specifically, this allows a 3-year-compatibility window, avoidance of `-XCPP`, as well as the ability to write code in such a way to avoid any warnings.


<table><tr><th>Phase 1 <i>(starting with GHC 8.0)</i></th>
<td>Implement new warning in GHC which gets
triggered when <tt>Monad</tt> instances explicitly override the
default <tt>return</tt> and <tt>(>>)</tt> method implementations with non-lawful 
definitions (see compatible instance definition example in previous section).


The warning was implemented in GHC 8.0 and is called <b><tt>-Wnoncanonical-monad-instances</tt></b> (there are variants of this warning flag for <tt>Monoid</tt> and <tt>Fail</tt>) is included in the default warning set since GHC 9.2.


</td></tr></table>


>
>
> This warning can be controlled via the new flag ~~`-fwarn-mrp-compat`~~ `-Wnoncanonical-monad-instances`, and becomes part of the default warning-set (#3174). 
>
>

<table><tr><th>Phase 2 <i>(GHC 8.4 or even later)</i></th>
<td>When we&apos;re confident that the
majority of Hackage has reacted to the warning (with the help of
Stackage actively pursuing maintainers to update their packages) we
turn the <tt>return</tt> and <tt>(>>)</tt> methods into a top-level binding and let GHC ignore lawful method definitions of <tt>return</tt> and <tt>(>>)</tt>. 
</td></tr></table>


>
>
> Non-lawful definitions (which were warned about in **Phase 1**) will now result in a compile error, while
> lawful definitions will be ignored and not be warned about (not even with `-Wall`).
>
>


   


<table><tr><th>Phase 3 <i>(very distant future)</i></th>
<td>Start warning about lawful <tt>return</tt>/<tt>>></tt> method overrides (in order to prepare for <b>Phase 4</b>)
</td></tr></table>


<table><tr><th>Phase 4 <i>(even more distant future)</i></th>
<td>Remove support in GHC for ignoring lawful <tt>return</tt>/<tt>>></tt> overrides, turning any method override of <tt>return</tt> and <tt>(>>)</tt> into a compile error.
</td></tr></table>


## Discussion


### Discussion Period


A discussion period of three weeks (until 2015-10-15) should be enough
to allow everyone to chime in as well as leave enough time to make the
required preparations for GHC 8.0 should this proposal pass as we hope.

### Discussion Summary


See [MonadOfNoReturn/Discussion](proposal/monad-of-no-return/discussion)

---

- \[1\]: [https://wiki.haskell.org/Functor-Applicative-Monad_Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal)
- \[2\]: [https://gitlab.haskell.org/haskell/prime/-/wikis/libraries/proposals/monad-fail](https://gitlab.haskell.org/haskell/prime/-/wikis/libraries/proposals/monad-fail)
- \[3\]: [https://gitlab.haskell.org/trac/ghc/wiki/ApplicativeDo](https://gitlab.haskell.org/trac/ghc/wiki/ApplicativeDo)
- \[4\]: [https://gist.github.com/hvr/b0e34463d85b58f169d9](https://gist.github.com/hvr/b0e34463d85b58f169d9)
- \[5\]: [https://gist.github.com/hvr/afcd040783d980594883](https://gist.github.com/hvr/afcd040783d980594883)
- \[6\]: [https://ghc.haskell.org/trac/ghc/ticket/9590](https://ghc.haskell.org/trac/ghc/ticket/9590)
- \[7\]: [https://mail.haskell.org/pipermail/haskell-prime/2015-September/003936.html](https://mail.haskell.org/pipermail/haskell-prime/2015-September/003936.html)
