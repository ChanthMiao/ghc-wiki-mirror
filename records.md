# Records in Haskell


This [Yesod blog post](http://www.yesodweb.com/blog/2011/09/limitations-of-haskell), and accompanying [ Reddit discussion](http://www.reddit.com/r/haskell/comments/k4lc4/yesod_the_limitations_of_haskell/) brought to the surface again the thorny issue of records in Haskell.


There are two rather different sets of issues:

- The narrow issue: **namespacing for record field names**. Currently in Haskell two records in the same module can't share a field name.  This is sometimes extremely painful.  This page is about the narrow issue.

- The broad issue: **first class record types**.  In Haskell there is no "record type" per se. Rather, you can simply give names to the fields of a constructor.  Records are not extensible and there is no polymorphism on records. 


This page focuses exclusively on the first, narrow issue of disambiguating record field names.  We have a separate Wiki page, [ExtensibleRecords](extensible-records), on the broad issue of first class record types.

**This page summarises the problem, and discusses alternative possible designs. See the [Overloaded Record Fields](records/overloaded-record-fields) page for more up to date status of related GHC extensions.**

## The problem: record name spacing


(Quoting the Yesod blog.)  Consider

```wiki
data Record = Record { a :: String }
data RecordClash = RecordClash { a :: String }
```


Compiling this file results in:

```wiki
record.hs:2:34:
    Multiple declarations of `Main.a'
    Declared at: record.hs:1:24
                 record.hs:2:34
```


In the Persistent data store library, Yesod works around the issue by having the standard of prefixing every record field with the record name (`recordA` and `recordClashA`). But besides being extremely verbose, it also limits us from experimenting with more advanced features like a partial record projection or an unsaved and saved record type.


The verbose name-spacing required is an in-your-face, glaring weakness telling you there is something wrong with Haskell. This issue has been solved in almost every modern programming languages, and there are plenty of possible solutions available to Haskell.


Never mind experimental/advanced features, it gets in the way of doing utterly dull things like:

- look up an entity by name (such as a customer or dictionary entry)
- get its identifier xxx_id
- link that same field name xxx_id in other record types to create/read/update/delete,
- without having to copy the darn value to xyz_id, pqr_id, ...


And inhibits doing relatively low-level generic/polymorphic stuff like standard print-formatting for any records with lastName and firstName fields.
-- added by AntC 21-Feb-2012

## Solutions


So we have decided to avoid the extensible record debate, but how can we have multiple record field selectors in scope and correctly resolve the type of the record?  There are two main mechanisms on offer:

- **Plan A**: Namespacing.  This uses qualified names to disambiguate record field names.
- **Plan B**: Types.  This uses types to disambiguate record field names.

1. **[Simple Overloaded Record Fields](records/overloaded-record-fields/sorf) (SORF)**.  Pure (Plan B).
1. **[Type Directed Name Resolution](http://hackage.haskell.org/trac/haskell-prime/wiki/TypeDirectedNameResolution) (TDNR)**.  Pure (Plan B), but without abstraction over fields of the same name.
1. **[Agda-derived Records](records/name-spacing) (ADR)** Pure (Plan A). Explained on below FDR page.
1. **[Frege-derived Records](records/name-spacing) (FDR)**. Uses (Plan A) + (Plan B).
1. **[Declared Overloaded Record Fields](records/declared-overloaded-record-fields) (DORF)**. Tweak to SORF. (Plan B)
1. **[Syntax Directed Name Resolution](records/syntax-directed-name-resolution)**. Alternative that relies on syntactic rewriting and lenses.
1. **[Type Indexed Records](records/type-indexed-records)**. (Plan B)
1. **[Type-Punning Declared Overloaded Record Fields](records/type-punning-declared-overloaded-record-fields) (TPDORF)**. In the DORF stable. (Plan B)
1. **[Explicit Classy Records](records/explicit-classy-records)**
1. **polymorphic extensible records with scoped labels** by Daan Leijen, [(implemented in Elm)](http://elm-lang.org/blog/announce/version-0.7.elm) and in the DSL [ WaveScript](http://www.cs.indiana.edu/~rrnewton/wavescope/WaveScope_+_WaveScript/)

1. **[type-level strings for field access](http://nikita-volkov.github.io/record/)**  `data Record2 (n1 :: Symbol) v1 (n2 :: Symbol) v2 = Record2 v1 v2`
1. **Are there any other approaches?**


The [OverloadedRecordFields](records/overloaded-record-fields) extension eventually implemented was based on SORF, but with some modifications based on feedback. Of course, this may not be the end of the story...

### Similarities


Some records solutions are planning on using the dot operator for normal record field selection. We need to consider the [future usage of the dot, particularly as a function composition operator](records/dot-operator).


Except:


DORF doesn't insist on dot notation: it's to be syntactic sugar for reverse function application.


Syntax directed name resolution doesn't use dots at all, the dot remains function composition as always.

### Comparisons


The DORF proposal is a variant of SORF with similar goals. However, it only solves the narrow name-spacing issue within a module. If a record is imported into another module, it must either share labels with it (automatic abstraction over fields) or use qualified module names.


DORF and SORF abstract over fields. The benefit of abstracting over field names is being able to write code that works against any Record with a given field. So I can have a function:

```wiki
getA = r.a
```


and that can work for both `Record` and `RecordClash` if they are defined in the same module because they both have a field `a`.
With other approaches (including TDNR) this will fail to type check unless the compiler can determine the type of r is either `Record` or `RecordClash`. Note that we already can accomplish this on an opt-in basis with Type Classes: making this automatic is not required and could give the unwary user weakly-typed code.


The advantage of Namespacing is that the implementation is clear, straightforward, and has already been done in Agda and Frege. We can either stop with name-spacing (Agda) or continue on with automatically resolving the field when the dot operator is used. Overloading has seen downsides in practice. In the words of the Frege author, who abandoned Overloading with abstraction over field names:

- only very inefficient code could be generated, if you have to access or update a field of some unknown record. In the end, every record type was basically a map.
- it turned out that errors stemming from mistyping a field name often could not be diagnosed at the point where they were committed, but led to inferred types with crazy signatures and an incomprehensible type error at the use side of the function that contained the error.
- the extra constraints complicated the type checker and did not play well with higher-kinded type variables (at least in the code I had then, I do not claim that this is necessarily so).


SORF without abstraction over fields may be able to avoid some of these potential downsides, and a judicious (no virtual fields, etc) implementation of either could look very similar to the programmer.


SORF has also been recognized as an approach to internal type resolution, whereas namespacing would require an internal SORF-like step or some other approach to avoid the need for lots of annotations.

### Type directed name resolution


One particular way of integrating this idea into Haskell is called (TDNR). All of the name-space mechanisms require some level of user-supplied disambiguation: if there are two fields `a` in scope, you must use a qualified name to disambiguate them.  What is tantalising about this is that the *type* of the argument immediately specifies which one you mean. There is really no ambiguity at all, so it is frustrating to have to type qualified names to redundantly specify that information.  Object-oriented languages take for granted this form of type-directed disambiguation. Proposed a couple of years ago, the Haskell community didn't like it much.  (But I still do; SLPJ.)


The discussion has many similarities with the original Type directed name resolution proposal: the question seems to be largely about nailing down a concrete implementation. The original TDNR proposal had internal Overloading in mind, but Namespacing ends up having similarities.


Haskell already has a (tried and tested) mechanism to disambiguate where "the *type* of the argument immediately specifies which one you mean" -- namely class/method/instance resolution. The DORF proposal uses this mechanism (and this mechanism alone: no funny-hand-shake syntax) -- AntC 21-Feb-2012

### Other (functional, and especially typeful) languages


If you know of other relevant language implementations, please add them!


The DDC language (very similar to Haskell) puts forth a similar solution to Frege. See the
[thesis](http://www.cse.unsw.edu.au/~benl/papers/thesis/lippmeier-impure-world.pdf) section 2.7 - 2.7.4 pages 115 - 119


The Opa language (functional, focused on web development) states that its modules are a special case of records.


The Agda language [generates a module (name space) for each record and also allows a record, like any module to be placed into the global scope by the programmer (opened in Agada terms)](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.Records).


SML\# supports [abstraction over fields](http://www.pllab.riec.tohoku.ac.jp/smlsharp/?FeatureRecordPolymorphism) as per the overloaded records implementation.

[Roy](http://roy.brianmckenna.org/), a functional language that targets only Javascript, also has structural typing which prevents clashes and allows abstraction over fields.


Ur/Web has a [very advanced records system](http://www.impredicative.com/ur/tutorial/tlc.html). It is explained in [ the Ur implementation paper](http://adam.chlipala.net/papers/UrPLDI10/UrPLDI10.pdf) and in Edward Z. Yang's [ explanation for Haskellers](http://blog.ezyang.com/2012/04/how-urweb-records-work-and-what-it-might-mean-for-haskell/).


Other FP languages where I looked for a record implementation but it appeared they have no solution for records with the same fields (my information could be wrong/out-dated) OCaml, Oz. However, the O in OCaml is for objects, and objects have structural typing that supports abstraction over fields.


I couldn't find great specific information on record implementation ML variants other than SML\#. Best I can tell, SML does not allow records in the same module with the same field. Records from other modules require name-spacing or must be opened up similar to Agda. 

### Problems with using the current module namespace mechanism


Suppose I have 112 hand-crafted data types in my project (e.g. see attachment 51369.txt), this creates a lot of conflicts in field names and constructor names. For example:

```wiki
data Comment = Comment {
      commentId           :: CommentId
    , commentContent      :: Content
    , commentReviewId     :: ReviewId
    , commentSubmissionId :: SubmissionId
    , commentConferenceId :: ConferenceId
    , commentDate         :: ISODate
    , commentReviewerNumber :: Int
  } deriving (Show)
```


This is a real type in my project. It has fields like ???id???, ???content???, ???reviewId???, ???submissionId???, ???date???. There are seven other data types that have a field name ???submissionId???. There are 15 with ???conferenceId???. There are 7 with ???content???. And so on. This is just to demonstrate that field clashes can occur frequently.


I tried putting all 112 types in separate modules, and around the 20 type mark and it was, apart from being very slow at compiling, very tedious to work with. Creating and editing these modules was a distracting and pointless chore. It also demonstrated, to me, that qualified imports are horrible when used on a large scale. It happened all the time, that'd I'd import, say, 10 different data types all qualified. Typing `map (Foo.id . BarMu.thisField)` and `foo Bar.Zot{x=1,y=2}` becomes tedious and distracting, especially having to add every type module when I want to use a type. And when records use other types in other modules, you have a lot of redundancy. With the prefixing paradigm I'd write `fooId` and `barMuThisField`, which is about as tedious but there is at least less `.` confusion and no need to make a load of modules and import lines. Perhaps local modules would solve half of this problem.


I also have 21 Enum types which often conflict. I end up having to include the name of the type in the constructor, or rewording it awkwardly.

#### counterpoint to the above


I (elaforge) have a project with 314 .hs files, containing 224 `^data .*=` lines, of which 104 define named record fields. They do tend to collect in large central modules: the largest one has 800 lines and 13 records defined in it. I have often wished for shorter record names, but I have never wished for two records in the same module with the same field name. Probably this is a result of the specific application, the example above looks like a database-style app that has lots of IDs instead of directly containing e.g. a Submission or a Conference.


I also use qualified imports exclusively, and my conclusion is the opposite: on a large scale they are more important than ever.  Importing is not tedious for me because I use a program to manage imports automatically, which was easy to write only because of qualified imports.
