# Type Checker Plugins

- See [Phab:D489](https://phabricator.haskell.org/D489) for the original implementation.

- See [Phab:D841](https://phabricator.haskell.org/D841) for the addition of empty closed type families.

- See [Phab:D909](https://phabricator.haskell.org/D909) for extensions to the TcPluginM API for creating constraints.

- See [discussion of most recent proposed changes below](plugins/type-checker#).

- See [Nicolas Frisby's notes about plugins](https://gitlab.haskell.org/ghc/ghc/wikis/plugins/type-checker/notes), esp how the constraint solver works.

## Issues

See the ~"typechecker plugins" label.


## Motivation


There is much interest at present in various extensions to GHC Haskell
type checking:

- Type-level natural numbers, with an SMT solver... (Iavor Diatchki)

- ...or integer ring unification (Christiaan Baaij)

- Units of measure, with a solver for abelian group unification (Adam Gundry)

- Type-level sets and maps, e.g. for effect tracking


All of these share a common pattern: they introduce extensions to the
language of constraints or the equational theory of types, and
corresponding extensions to the constraint solving algorithm X that
underlies GHC's type checking algorithm OutsideIn(X).  In principle,
OutsideIn is parametric in the constraint solver, but in practice GHC
provides only one solver, which supports type families and GADT
equality constraints.


Type families can be used to encode some of the desired extensions,
but they do not provide exactly the desired equational theory, and
this leads to worse type inference behaviour and worse error messages
than we might expect for a native implementation.


The aim of this proposal is to make it easier to experiment with
alternative constraint solvers, by making it possible to supply them
in normal Haskell libraries and dynamically loading them at compile
time, rather than requiring implementation inside GHC itself.  This is
much like the situation for [Core plugins](plugins), which allow
experiments with transformations and optimizations of the intermediate
language.  The fact that plugins can be developed without recompiling
GHC is crucial, as it reduces barriers to entry and allows the
resulting constraint solvers to be used by non-developers.

## Design

### Creating a plugin


A type checker plugin, like a Core plugin, consists of a normal Haskell
module that exports an identifier `plugin :: Plugin`.  We extend the
`Plugin` type (moved to a new module `Plugins`) with an additional field:

```haskell
data Plugin = Plugin
  { installCoreToDos :: ... -- as at present
  , tcPlugin         :: [CommandLineOption] -> Maybe TcPlugin
  }
```


The `TcPlugin` type and related pieces are defined in `TcRnTypes`:

```haskell
data TcPlugin = forall s . TcPlugin
  { tcPluginInit  :: TcPluginM s
  , tcPluginSolve :: s -> TcPluginSolver
  , tcPluginStop  :: s -> TcPluginM ()
  }

type TcPluginSolver = [Ct]    -- given
                   -> [Ct]    -- derived
                   -> [Ct]    -- wanted
                   -> TcPluginM TcPluginResult

data TcPluginResult
  = TcPluginContradiction [Ct]
  | TcPluginOk [(EvTerm,Ct)] [Ct]
```


The basic idea is as follows:

- When type checking a module, GHC calls `tcPluginInit` once before constraint solving starts.  This allows the plugin to look things up in the context, initialise mutable state or open a connection to an external process (e.g. an external SMT solver).  The plugin can return a result of any type it likes, and the result will be passed to the other two fields.

- During constraint solving, GHC repeatedly calls `tcPluginSolve`.  Given lists of Given, Derived and Wanted constraints, this function should attempt to simplify them and return a `TcPluginResult` that indicates whether a contradiction was found or progress was made.  If the plugin solver makes progress, GHC will re-start the constraint solving pipeline, looping until a fixed point is reached.

- Finally, GHC calls `tcPluginStop` after constraint solving is finished, allowing the plugin to dispose of any resources it has allocated (e.g. terminating the SMT solver process).


The `Ct` type, representing constraints, is defined in [TcRnTypes](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Tc/Types.hs#L932). A constraint is essentially a triple of a type (of kind `Constraint`, e.g. an equality or fully applied typeclass), an evidence term of that type (which will be a metavariable, for wanted constraints) and an original source location.


Plugin code runs in the `TcPluginM` monad defined in `TcRnTypes` as a wrapper around `TcM` (and hence around `IO`). Eventually the `TcPluginM` monad will supply wrappers for `TcM` functions that are appropriate for use in a plugin. Initially, plugins will need to rely on `unsafeTcPluginTcM :: TcM a -> TcPluginM a` if a wrapper is not available.


Note that `TcPluginM` can perform arbitrary IO via `tcPluginIO :: IO a -> TcPluginM a`, although some care must be taken with side effects (particularly in `tcPluginSolve`).  In general, it is up to the plugin author to make sure that any IO they do is safe.

### Calling plugins from the typechecker


Typechecker plugins will be invoked at two points in the constraint solving process: after simplification of given constraints, and after unflattening of wanted constraints.  They can be distinguished because the deriveds/wanteds will be empty in the first case.


During simplification of givens:

- The deriveds and wanteds lists will be empty.

- The givens will be flat, un-zonked and inert.

- If the plugin finds a contradiction amongst the givens, it should return `TcPluginContradiction` containing the contradictory constraints.  These will turn into inaccessible code errors.

- Otherwise, the plugin should return `TcPluginOk` with lists of "solved" givens and new givens.  "Solved" givens (i.e. those that are uninformative, such as `x * y ~ y * x` in a plugin for arithmetic) must be exactly as supplied to the plugin and will simply be dropped; the evidence term is ignored.  If there are any new givens, the main constraint solver will be re-invoked in case it can make progress, then the plugin will be invoked again.

- If the plugin cannot make any progress, it should return `TcPluginOk [] []`.


During solving of wanteds:

- The givens and deriveds will be flat, un-zonked and inert.

- The wanteds will be unflattened and zonked.

- If the plugin finds a contradiction amongst the wanteds, it should return `TcPluginContradiction` containing the contradictory constraints.  These will turn into unsolved constraint errors.

- Otherwise, the plugin should return `TcPluginOk` with lists of solved wanteds and new work.  Solved wanteds must be exactly as supplied to the plugin and must have a corresponding evidence term of the correct type.  If there are any new constraints, the main constraint solver will be re-invoked in case it can make progress, then the plugin will be invoked again.

- If the plugin cannot make any progress, it should return `TcPluginOk [] []`.


Plugins are provided with all available constraints, but it is easy for them to discard those that are not relevant to their domain, because they need return only those constraints for which they have made progress (either by solving or contradicting them).

### Using a plugin


Just as at present, a module that uses a plugin must request it with a
new GHC command-line option `-fplugin=<module>` and command line
options may be supplied via `-fplugin-opt=<module>:<args>`.


This means that a user should always know which plugins are affecting
the type checking of a module.  It does mean that a library that relies
on a special constraint domain (e.g. for units of measure), and
exposes types involving these constraints, may need its users to
explicitly activate a plugin for their programs to type check.  This is
probably desirable, since type checker plugins may cause unexpected
type checker behaviour (even performing arbitrary IO).


If multiple type checker plugins are specified, they will be
initialised, executed and closed in the order given on the command
line.  This makes it possible to use plugins that work on disjoint
constraint domains (e.g. a units of measure plugin and a type-level
numbers plugin), or even experiment with combining plugins for the
same constraint domains.

### Evidence


The interface sketched above expects type checker plugins to produce
evidence terms `EvTerm` for constraints they have simplified.
Different plugins may take different approaches generating this
evidence.  The simplest approach is to use "proof by blatant
assertion": essentially this amounts to providing an axiom `forall s t . s ~ t`
and trusting that the constraint solver uses it in a sound
way.  However, in some cases (such as an abelian group unifier used
for units of measure) it should be possible for the solver to encode
the axioms of the equational theory and build proofs from them.

## Post-7.10 changes to TcPluginM API


In the light of experience with GHC 7.10.1, we are considering some changes to the typechecker plugins API for the next release.

### Implemented: Empty closed type families


Plugins that define type families often need to ensure that those type families have no normal instances, to avoid inconsistency, but empty closed type families were previously rejected (#9840). They are now permitted in HEAD. See [Phab:D841](https://phabricator.haskell.org/D841).

### Implemented: Creating constraints


The existing API does not offer a very direct way for plugins to create new constraints. In particular, creating new givens is problematic now that [givens contain EvVars rather than EvTerms](https://github.com/ghc/ghc/commit/fa46c597db9939de1de4bc9b917c8dc1d9e2093a). I propose that we add new functions to `TcPluginM`:

```haskell
newWanted  :: CtLoc -> PredType -> TcPluginM CtEvidence
newDerived :: CtLoc -> PredType -> TcPluginM CtEvidence
newGiven   :: CtLoc -> PredType -> EvTerm -> TcPluginM CtEvidence
```


The implementation of `newGiven` will require `TcPluginM` to pass around an `EvBindsVar`, so that it can bind a new evidence variable. This is not available in `tcPluginInit` and `tcPluginStop`, so using `newGiven` there will result in a crash. (I previously considered making `TcPluginM` wrap `TcS` directly, but that turns out to require a lot of rearrangement and probably hs-boot files.)


These are now in HEAD, along with `newUnique :: TcPluginM Unique`. See [Phab:D909](https://phabricator.haskell.org/D909).

### Under discussion: Defining type families


Defining type families in plugins is more work than it needs to be, because the current interface forces the plugin to search the unsolved constraints for the type family in question (which might be anywhere within the types), then emit a new given constraint to reduce the type family. Instead, it should be possible to plug in to `matchFam` directly.


I propose to extend the `TcPlugin` data type with a new field

```haskell
tcPluginMatchFam :: s -> TyCon -> [Type] -> TcPluginM (Maybe (TcCoercion, TcType))
```


This is similar to `sfMatchFam`, which gives the definition of built-in type families like `(+)`. However, it would be invoked only in `matchFam`, rather than `reduceTyFamApp_maybe`, to save having to load plugins at all the call sites of the latter. Thus plugin-defined type families would reduce in the constraint solver, but not necessarily elsewhere.

**Richard:** This makes me uncomfortable, in exactly the same way that I was made to feel uncomfortable in the comments starting with [comment:4:ticket:9840](https://gitlab.haskell.org/ghc/ghc/issues/9840). The fact that the new, (what I will call) *external* type families will behave differently than internal type families is further evidence that something is amiss. (The difference in behavior I'm referring to is the difference between `matchFam` and `reduceTyFamApp_maybe`.) This, of course, ties into #9636 as well and to some of the more esoteric issues that cropped up while working on #6018/[Phab:D202](https://phabricator.haskell.org/D202) and perhaps even #10327. I would love to approach this problem with the idea of making broader changes instead of looking for a minimal change just to support typechecker plugins better. **End Richard**

### Implemented: Embedding CoreExpr in EvTerm


At the moment, the `EvTerm` type used to represent evidence for constraints is quite restricted. In particular, it permits a selection of special cases (e.g. `EvLit`, `EvCallStack`, `EvTypeable`) but does not permit general `CoreExpr`s. This makes it difficult to constraint evidence for typeclass constraints, because they must use `EvDFunApp` with an existing dfun, rather than generating a dictionary directly. See ["EvTerms and how they are used" on ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2015-February/008414.html) for discussion of this.


The plan is to add a constructor `EvCoreExpr CoreExpr` to `EvTerm`, with

```haskell
dsEvTerm :: EvTerm -> DsM CoreExpr
dsEvTerm (EvCoreExpr e) = return e
...
```


I'm not very clear on whether we need to extend zonking to work on `CoreExpr`? Or should `EvCoreExpr` take a pre-zonked expression?


This is now implemented; see #14691.

### Under discussion: Evidence for axioms


At present, plugins can produce blatant assertions using a `UnivCo` inside a `TcCoercion`. GHC has limited support for theory-specific axioms in the form of `CoAxiomRule`, but this is limited to built-in axioms relating to type literals. A plugin that creates its own `CoAxiomRule` may at first appear to work fine, but if such an axiom is exposed in an interface file (e.g. via an unfolding) then GHC will crash with a `tcIfaceCoAxiomRule` panic when importing it. See ["Serialising evidence generated by typechecker plugins" on ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2014-December/007626.html) for discussion of the problem and a potential solution, namely making plugins able to create their own `CoAxiomRule`s and serialise them in interface files. 

**Richard:** While we're at it, we should probably merge `CoAxiomRule` with `CoAxiom` somehow. I believe `CoAxiomRule` is just a generalization of `CoAxiom`. **End Richard**

**Adam:** I'm not convinced this is true. It seems to me that `CoAxiom` and `CoAxiomRule` encode two different forms of axiom families:

- `CoAxiom` describes a finite collection of branches for a closed type family, for example `forall a . Equal a a ~ True` and `forall a b . a # b => Equal a b ~ False` (where I pretend we have a built-in notion of apartness written `#`). We sometimes need to observe the branch structure, for example when reifying the type family for TH.

- `CoAxiomRule` describes a potentially-infinite family of axioms that have the form `forall as . cs => coaxrProves(as, cs)` for some arbitrary metalevel function `coaxrProves` that generates an equation. For example, the definition for (+) generates `2 + 2 ~ 4` and infinitely many other axioms. `CoAxiomRule` doesn't give a way to describe apartness requirements.


It may be possible to generalize both of these into a single form of evidence, but it'll take a bit of thought. I wonder if the real distinction should be between single axioms (with a type that can be expressed in FC) and infinite families of axioms (which are not directly expressible in FC, e.g. arithmetic operators). In particular, I think it may be worth representing apartness evidence more explicitly, so that a `CoAxiom` can correspond directly to a set of single axioms. **End Adam**


Iavor notes out that it would be useful to be able to express evidence for functional dependencies using axioms like `forall a b c. (C a b, C a c) => (b ~ c)`.

[Christiaan points out](https://github.com/clash-lang/ghc-typelits-natnormalise/issues/1#issuecomment-106748739) that the evidence we can generate constrains the way multiple plugins can interact sensibly. 

## Outstanding issues

- The current API does not make it possible to express the fact that a plugin solved some constraints but discovered others were impossible to solve. The only option is to solve the valid constraints and leave the others as unsolved, rather than identifying the contradiction.

- For units of measure, it would be nice to be able to extend the
  pretty-printer for types (so we could get a nicely formatted
  inferred type like `m/s^2` rather than a nested type family
  application).  This ought to be possible using a plugin approach,
  provided we can thread the required information via the
  `SDocContext`.

- It would be nice for plugins to be able to manipulate the error
  messages that result from type checking, along the lines of error
  reflection in Idris.

- Would it be useful if plugins could generate code on a per-module basis, depending on the contents of the module (e.g. generating instances of a class like `Typeable` uniformly for every datatype)?

- At the moment there is not a very clear relationship between plugins and [hooks](ghc/hooks).  It might be nice to unify the two approaches, but they have quite different design goals.

## Applications


Known plugins:

- [type-nat-solver](https://github.com/yav/type-nat-solver) is a plugin for solving numeric constraints using an SMT solver

- [ghc-typelits-natnormalise](https://github.com/christiaanb/ghc-typelits-natnormalise) is another plugin for solving numeric constraints, using normalisation instead of an external SMT solver

- [uom-plugin](https://github.com/adamgundry/uom-plugin) is an implementation of units of measure that uses a typechecker plugin to implement the equational theory of abelian groups


Other possible applications:

- making particular type families [injective](injective-type-families) (cf. #6018)

- extra improvement for closed type families (cf. #10227)

- adding functional dependency style behaviour to particular typeclasses

- eta-expansion of type-level products (#7259)


Feel free to extend these lists.

## Reactions from community

**N.B.** These comments refer to an earlier version of this page, prior to the implementation of the proposal.

**Richard:** I really like the use of an existential type variable.

- Should `solve` have the ability to modify the custom state? (I don't know -- just thinking about it.)
- Should the plugin specify some domain of interest, so that it isn't barraged with irrelevant constraints? (Perhaps not -- it's easy enough for the plugin to do its own filtering.)
- I think the interface you describe subsumes type family / type class lookup, as both of these take the form of constraints to be solved.
- I don't like forcing all users of a library to have to specify the plugin on the command line. It's very anti-modular. I *do* like requiring all users to opt into using plugins at all, say with `-XCustomConstraintSolvers`, but then the module that needs the custom solver should specify the details. There should also be a mechanism whereby importing (even transitively) a module that needs a custom solver can warn users to enable `-XCustomConstraintSolvers`.


Have you tried out this interface? Does it work?
**End Richard**



**Adam:** 


- At the moment, `solve` doesn't explicitly modify the custom state because I suspect that threading the returned values through the `TcS` pipeline might be tricky, and that plugins wanting this can use `IORef`s instead. Actually, I wonder if instead of using an existential type variable, `tcPlugin` should return subsume `init` and return a `TcM (Maybe TcPlugin)`; then any state can be stored in a closure.
- I expect most plugins to work on equality constraints of a particular kind, or typeclass constraints for a fixed typeclass, but I don't see an obvious way to specify such domains. As you say, I think it should be easy for the plugin to filter constraints itself.
- I can see the appeal of a language option to enable plugins, but the advantages of specifying them on the command line are that it fits perfectly with the existing plugins feature, and that it provides the order in which to run multiple plugins.
- I haven't tried this yet, but I'll work on implementing it and let you know how I get on. The interface is essentially based on Iavor's ext-solver work, and I'm pretty sure my units solver can be adapted to fit.

**End Adam**

**Christiaan**:
The interface looks fine by me, I do have some questions/remarks:

- How does the solver architecture check/enforce that the SolveResults match up with the Wanted constraints?
  Perhaps return tuples of wanted constraints and correspondig SolveResults?
- Do we consider a result with a list of all Stuck SolveResults, but with new constraints, as progress?

**End Christiaan**

**Adam:**

- We certainly could return `(Ct, SolveResult)` tuples, and perhaps that will be simpler - I'll try it when I'm working on the implementation. Some of the details around exactly how `solve` fits into the constraint solving pipeline are still a bit hazy...
- Yes, generating new constraints is a form of progress: some plugins might simply add new constraints that are implied by the existing ones, which might lead to additional metavariables being solved by the main solver. (This is basically how functional dependencies work.)

**End Adam**

## FAQ

### Error: `Module imports form a cycle`


If you use `-fplugin=MyPlugin` on the command-line and compile in `--make` mode, you may get the error

```wiki
Module imports form a cycle:
  module ???MyPlugin??? (./MyPlugin.hs) imports itself
```


This occurs because `-fplugin=MyPlugin` essentially imports `MyPlugin` in every module being compiled. When you do `ghc -fplugin=MyPlugin --make M` the dependencies of `M` are compiled with the `-fplugin=MyPlugin` command-line flag, and hence you end up compiling `MyPlugin` with a dependency on itself.


If you're defining and using a plugin in a single package, use `OPTIONS_GHC` pragmas in the modules that rely on the plugin, rather than supplying `-fplugin` on the command line. Alternatively, you can define the plugin in one package and use it in another, then it is easy to compile the first package without `-fplugin`, and you can use it globally in the second package.


See #10077 for more information.

### Error: `cannot find normal object file`


When compiling with a plugin, if you receive the error

```wiki
<no location info>:
    cannot find normal object file ???./MyPlugin.dyn_o???
    while linking an interpreted expression
```


you need to compile `MyPlugin` with one of the `-dynamic` or `-dynamic-too` options.
