Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

## Why (most) FunDeps are tantamount to instance selection


[AntC: in response to crossed wires with SPJ]

At the outset we should say Haskell's FunDeps are not exactly database theory's Functional Dependencies -- more that there's a 'family resemblance':

* Haskell's FunDeps (instances) typically contain type schemas -- that is structures of type constructors and variables, typically sharing variables amongst different parameters; then
* FunDep-driven type improvement is more like firing rewrite rules.
* Whereas database Functional Dependencies are an analysis tool to arrive at a table structure where the determining positions/LHS are an index into the rows in the table.
* Then there's no 'rules' to fire: merely **select** a row that matches the wanted values in the determining positions.
* That's why I was presuming everybody sees FunDep determining/LHS positions as selecting an instance.

**Example: person database**

(Sorry to introduce humdrum data-processing.)

We wish to record `{FamilyName, GivenName, DoB, Mobile, Email}` for a bunch of people. Let's imagine the halcyon days of the 1990's when people could afford only one mobile, and had only one email address. So

* `Mobile` uniquely determines a record;
* `Email` uniquely determines a record;
* `FamilyName`? No: there might be many 'Jones'; `GivenName`? No: there might be many 'Simon's; `FamilyName+GivenName`? No: often first-born inherits a parents names. Then `FamilyName+GivenName+DoB` uniquely determines a record.

Capture that analysis as Functional Dependencies:
* `{FamilyName, GivenName, DoB} -> {Mobile, Email}`
* `{Mobile} -> {FamilyName, GivenName, DoB, Email}`
* `{Email} -> {FamilyName, GivenName, DoB, Mobile}`

Slap that lot in a database table definition; each Functional Dependency's LHS gives an index to look up the table -- that is, select a row.

Note those Functional Dependencies are 'Full' -- that is, mention all of the columns. This is a Good Thing: it means the design is 'normalised'.

Advance to the 2000's when people started owning more than one Mobile. And a lazy systems maintainer who tries to persevere making minimal changes to the structure. Then revise the Functional Dependencies:
* `{FamilyName, GivenName, DoB} -> {Email}` -- had to remove `-> Mobile`
* `{Mobile} -> {FamilyName, GivenName, DoB, Email}`
* `{Email} -> {FamilyName, GivenName, DoB}` -- no `-> Mobile`

Now _not all_ Functional Dependencies' LHS give an index to look up the table: `Email` will find multiple rows, with same Names/DoB, but each a different `Mobile`. And the systems maintainer must write a load of validation code to keep multiple rows in synch. Because (gasp) database technology doesn't support Functional Dependencies directly, it only supports unique indexes.

How do we know we no longer have a Good Thing? Because some of those Functional Dependencies are not Full.

**Remedy**

**Normalise** the table structure by '**vertical partitioning**'. (We'll anticipate people getting multiple `Email`s while we're at it.)

```haskell
class MobileFor mobile familyName givenName doB  | mobile -> familyName givenName doB

class EmailFor email familyName givenName doB  | email -> familyName givenName doB

class ( MobileFor mobile familyName givenName doB
      , EmailFor email familyName givenName doB)
      => Person familyName givenName doB mobile email   -- | note FunDeps optional here
                                                        -- the compiler can infer them
                                                        -- (or at least Hugs does)
```

This 'vertical partitioning' is exactly what the JFP-paper recommends for non-Full FunDeps [§ 6.2 create an auxiliary class]. Now we're back to all Full FunDeps. That technique can always be applied -- at cost of extra classes and their instances. If I were Haskell FunDep dictator for a day, I'd ban non-Full FunDeps.

**Dysfunctional dependencies/Wiggly arrows**

There's a hangover in our personnel database: although `FamilyName+GivenName+DoB` no longer uniquely determines `Mobile`, it does severely limit how many `Mobile`s belong to a person. A non-unique dependency.

Does database theory have something to offer for this case? Yes you've already seen it:
* FunDep `mobile -> familyName givenName doB` on the `MobileFor` class; plus
* `MobileFor` being a superclass of `Person`;
* The database terminology for this is 'Foreign Key';

is saying exactly that. And database engines have smarts to present a 'join view' that 'denormalises' a structure across Foreign Keys.