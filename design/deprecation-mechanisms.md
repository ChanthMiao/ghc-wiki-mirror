# Interface deprecation mechanisms


This page serves as a place to collect proposals, tickets, open problems, and ideas regarding mechanisms for deprecating program interfaces.


For motivation for these mechanisms one can look to a number of recently-considered proposals for reworking the core libraries, for instance the [Applicative-Monad Proposal](https://wiki.haskell.org/Functor-Applicative-Monad_Proposal#Future-proofing_current_code).

## Warning on import of `DEPRECATED` definitions


GHC has for a long time allowed users to attach `DEPRECATED` pragmas to definitions, defining a warning message which will be shown to clients when a use-site of the marked definition is encountered.


Sadly, this design doesn't account for instances where a deprecated definition is explicitly imported but not actually *used*. #2119 argues that GHC should issue a warning when an explicit import of a deprecated definition is found.

**SimonM** If you import a `DEPRECATED` identifier, then there are two cases: either it is mentioned somewhere, in which case the deprecation warning will be shown, or it is not mentioned, in which case there will be an unused import warning (provided it is enabled), so this seems less useful than the other suggestions on this page. **End SimonM**

**John Ericson**: A related question is whether two overlapping explicit imports (not of the same underlying thing) counts as an error or not. I think they both shouldn't be *silent*, even this is an warning rather than error.

In conclusion, I feel unused explicit imports are "semi uses", somewhat akin to dead code. We don't have to treat them as onerousness as "real" uses, but they do indicate more direct intent than wildcards so we should hold them to a higher standard.

TODO Write formal specification.

## Deprecating exports


GHC's existing `DEPRECATED` pragma only covers definitions, not their visibility. #4879 argues that GHC should allow `DEPRECATED` pragmas to be attached to names in export lists, meaning that the definition itself isn't deprecated (allowing warning-free uses within the defining module), only its role as an outward-facing interface.

TODO Write specification

## Class method deprecation


As of GHC 7.10 there is no effective way to demote a typeclass method to a normal top-level binding (see #10071). This would be useful in cases such as

- removing the `return` method from `Monad` as proposed by the [Monad of No Return proposal](https://mail.haskell.org/pipermail/libraries/2015-September/026121.html)
- removing the `mappend` method from `Monoid` as proposed in [Proposal/SemigroupMonoid](proposal/semigroup-monoid)


A concrete proposal for allowing `DEPRECATED` pragmas to be attached to class methods can be found at [Design/DeprecationMechanisms/TypeClassMethods](design/deprecation-mechanisms/type-class-methods).

## `REMOVED` Pragma


See also #10933.


Once a method or function is finally removed, we just get a compiler error which doesn't give much guidance what to use instead. A suggestion is to have a `REMOVED` pragma into which a `DEPRECATED` pragma can be rewritten into, providing additional guidance for a missing symbol that was once there.


Possible extension for removed class methods: When defining a no-more existent `method` as part of an instance definition, ignore definitions matching a set of specified implementation bodies. E.g. for AMP defining `return = pure` even though `return` has been `REMOVED` could be tolerated. This would allow for a CPP-less transition.
**Richard:** Interesting extension. But this should surely cause a warning even if it is tolerated. **End Richard**

TODO Write specification
