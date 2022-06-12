The Overloaded Record Fields family of extensions for GHC allow multiple record datatypes to share the same field names, and make it possible for type information to disambiguate fields. There is no single `OverloadedRecordFields` extension, but rather a family of related extensions:
 * `DisambiguateRecordFields`: makes use of constructor names to disambiguate fields in record construction or pattern matching.
 * `DuplicateRecordFields` (GHC 8.0.1): permits a module to define the same field name in multiple datatypes.
 * `OverloadedLabels` (GHC 8.0.1): provides `#field` syntax for an identifier whose meaning is determined by typeclass instance resolution. Intended for use with the `HasField` magic type class (GHC 8.2.1).
 * `NoFieldSelectors` (GHC 9.2.1): prevents fields being in scope as selector functions.
 * `OverloadedRecordDot` (GHC 9.2.1): permits `expression.field` syntax for record projection, using `getField` from `HasField`.
 * `OverloadedRecordUpdate` (partially in GHC 9.2.1): generalises `r { f = x }` to use `setField` (yet to be added to `HasField`), and additionally permits `r { f1.f2 = x }`.

For user-facing documentation, see the [GHC user's guide](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/records.html).

For implementation status, see #18598, the tracking ticket for most recent work related to overloaded record fields, and the ~OverloadedRecordFields, ~DuplicateRecordFields and ~OverloadedLabels labels.

## GHC proposals and design discussions

Accepted GHC proposals:

  - [Adding HasField class, changes to OverloadedLabels](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst) (Implemented in GHC 8.2 without `IsLabel x (r -> a)` instance)
  - [NoFieldSelectors](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0160-no-toplevel-field-selectors.rst) (Implemented in GHC 9.2, see !4743)
  - [RecordDotSyntax](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0282-record-dot-syntax.rst) (amended by [ghc-proposals#405](https://github.com/ghc-proposals/ghc-proposals/pull/405) and implemented in GHC 9.2)
  - [DuplicateRecordFields without ambiguous field access](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0366-no-ambiguous-field-access.rst) (implemented for fields in GHC 9.2, not yet implemented for updates, see #19461)
  - [Adding setField to HasField](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0042-record-set-field.rst) (Pending redesign and implementation, see #16232 and !3257)
  - [Unrestricted Overloaded Labels](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0170-unrestricted-overloadedlabels.rst) (Draft implementation needs work, see #11671)

Other related GHC proposals (not yet accepted):

 - [Separate HasField into GetField and SetField](https://github.com/tysonzero/ghc-proposals/blob/patch-3/proposals/separate-get-set-field.md)
 - [Add GHC.Variants module to mirror GHC.Records](https://github.com/tysonzero/ghc-proposals/blob/ghc-variants/proposals/ghc-variants.md)
 - [`NoFieldSelectors` as a datatype annotation](https://github.com/parsonsmatt/ghc-proposals/blob/matt/field-selectors-scoped/proposals/0512-nofieldselectors-per-datatype.md)
 - [Disambiguate Record Update](https://github.com/Ericson2314/ghc-proposals/blob/disambiguate-record-update/proposals/0000-disambiguate-record-update.rst) and [related discussion](https://github.com/ghc-proposals/ghc-proposals/discussions/506)
 - [Local modules](https://github.com/goldfirere/ghc-proposals/blob/local-modules/proposals/0000-local-modules.rst)
 - [Add Row Polymorphism to Haskell](https://github.com/jvanbruegge/ghc-proposals/blob/row-polymorphism/proposals/0000-row-polymorphism.rst)
 - [Relaxing HasField constraints](https://github.com/ocharles/ghc-proposals/blob/hasfield/proposals/0000-hasfield-incoherence.rst)

Planned/draft GHC proposals:

 - [HasField redesign](https://github.com/adamgundry/ghc-proposals/blob/hasfield-redesign/proposals/0000-hasfield-redesign.rst)
 - Datatype names as module qualifiers in updates (no proposal yet, see [discussion](https://github.com/ghc-proposals/ghc-proposals/discussions/506#discussioncomment-2868700))

## Task list

 - Finish and submit GHC proposal for `HasField` redesign (see [comment](https://github.com/ghc-proposals/ghc-proposals/pull/510#issuecomment-1137887333))
 - Change `OverloadedRecordDot` parser to permit pseudo-keywords (#21226)
 - Implement unrestricted `OverloadedLabels` (#11671)
 - Write a proposal for datatype names as module qualifiers
 - Improve error messages related to records (see [tickets](https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=updated_desc&state=opened&label_name%5B%5D=error%20messages&label_name%5B%5D=records))
 - Improve interaction between `TemplateHaskell` and `DuplicateRecordFields` (see [tickets](https://gitlab.haskell.org/ghc/ghc/-/issues/?sort=updated_desc&state=opened&label_name%5B%5D=OverloadedRecordFields&label_name%5B%5D=TemplateHaskell))

## Documentation

 - [`DisambiguateRecordFields` extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/disambiguate_record_fields.html)
 - [`DuplicateRecordFields` extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/duplicate_record_fields.html)
 - [`OverloadedLabels` extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/overloaded_labels.html)
 - [Record field selector polymorphism (`HasField` class)](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/hasfield.html)
 - [`(No)FieldSelectors` extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/field_selectors.html)
 - [`OverloadedRecordDot` extension](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/overloaded_record_dot.html)


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

- [Discussion of the problem](records)
- [Simple Overloaded Record Fields (SORF)](records/overloaded-record-fields/sorf), Simon PJ's original proposal
- [Declared Overloaded Record Fields (DORF)](records/declared-overloaded-record-fields), a counterpoint proposal by Anthony Clayden
- [Discussion of the problem and possible solutions](records)
- [Original design of the extension](records/overloaded-record-fields/design) (2013)
- [Redesigned variant involving three extensions](records/overloaded-record-fields/redesign) (2015)
  - Part 1: [DuplicateRecordFields](records/overloaded-record-fields/duplicate-record-fields) (in GHC 8.0)
  - Part 2: [OverloadedLabels](records/overloaded-record-fields/overloaded-labels) (in GHC 8.0)
  - Part 3: [Magic type classes](records/overloaded-record-fields/magic-classes) (partly in GHC 8.2)
  - [Adam Gundry's blog post](http://www.well-typed.com/blog/2015/03/overloadedrecordfields-revived/)

Content previously on this wiki page has been moved to the [SORF](records/overloaded-record-fields/sorf) page.