Go up to [Functional dependencies in GHC](https://gitlab.haskell.org/ghc/ghc/-/wikis/Functional-dependencies-in-GHC)

## Background in database theory

[AntC: in response to crossed wires with SPJ]

At the outset we should say Haskell's FunDeps are not exactly database theory's Functional Dependencies -- more that there's a 'family resemblance':

* Haskell's FunDeps (instances) typically contain type schemas -- that is structures of type constructors and variables, typically sharing variables amongst different parameters; then
* FunDep-driven type improvement is more like firing rewrite rules.
* Whereas database Functional Dependencies are an analysis tool to arrive at a table structure where the determining positions/LHS are an index into the rows in the table.
* Then there's no 'rules' to fire: merely select a row that matches the wanted values in the determining positions.