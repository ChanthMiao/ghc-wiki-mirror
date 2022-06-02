The Overloaded Record Fields family of extensions for GHC allow multiple record datatypes to share the same field names, and make it possible for type information to disambiguate fields. There is no single `OverloadedRecordFields` extension, but rather a family of related extensions:
 * `DisambiguateRecordFields`: makes use of constructor names to disambiguate fields in record construction or pattern matching.
 * `DuplicateRecordFields` (GHC 8.0.1): permits a module to define the same field name in multiple datatypes.
 * `OverloadedLabels` (GHC 8.0.1): provides `#field` syntax for an identifier whose meaning is determined by typeclass instance resolution. Intended for use with the `HasField` magic type class (GHC 8.2.1).
 * `NoFieldSelectors` (GHC 9.2.1): prevents fields being in scope as selector functions.
 * `RecordDotSyntax` (GHC 9.2.1): permits `expression.field` syntax for record projection.

For user-facing documentation, see the GHC user's guide:

 - [DisambiguateRecordFields extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/disambiguate_record_fields.html)
 - [DuplicateRecordFields extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/duplicate_record_fields.html)
 - [OverloadedLabels extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/overloaded_labels.html)
 - [Record field selector polymorphism (HasField class)](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/hasfield.html)

For implementation status, see #18598, the tracking ticket for most recent work related to overloaded record fields, and the ~OverloadedRecordFields label.

For design discussion, see the GHC proposals:

  - [Adding HasField class, changes to OverloadedLabels](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst) (Implemented in GHC 8.2 without `IsLabel x (r -> a)` instance)
  - [Adding setField to HasField](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-record-set-field.rst) (Being implemented, see !3257)
  - [NoFieldSelectors](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst) (Being implemented for GHC 9.2, see !4743)
  - [RecordDotSyntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst)
  - [DuplicateRecordFields without ambiguous field access](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst)

Content previously on this page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.

## Code

- [Prototype implementation of the magic typeclasses](https://github.com/adamgundry/records-prototype)
- [Phab:D761](https://phabricator.haskell.org/D761), [ Phab:D1391](https://phabricator.haskell.org/D1391), [ Phab:D1486](https://phabricator.haskell.org/D1486), [ Phab:D1586](https://phabricator.haskell.org/D1586), [ Phab:D1600](https://phabricator.haskell.org/D1600): `DuplicateRecordFields` extension
- [Phab:D1331](https://phabricator.haskell.org/D1331), [ Phab:D1623](https://phabricator.haskell.org/D1623): `OverloadedLabels` extension
- [Phab:D1687](https://phabricator.haskell.org/D1687), [ Phab:D2708](https://phabricator.haskell.org/D2708): magic classes
- !3257: extension of `HasField` class to support updates
- !4532: `RecordDotSyntax`
- !4743: `NoFieldSelectors` and liberalisation of `DuplicateRecordFields`

## History

The extension was initially implemented in 2013 as a Google Summer of Code project, by Adam Gundry under the mentorship of Simon Peyton Jones.

- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Discussion of the problem and possible solutions](records)
- [Original design of the extension](records/overloaded-record-fields/design) (2013)
- [Redesigned variant involving three extensions](records/overloaded-record-fields/redesign) (2015)
  - Part 1: [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields) (in GHC 8.0)
  - Part 2: [OverloadedLabels](records/overloaded-record-fields/overloaded-labels) (in GHC 8.0)
  - Part 3: [Magic type classes](records/overloaded-record-fields/magic-classes) (partly in GHC 8.2)
  - [Adam Gundry's blog post](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)
