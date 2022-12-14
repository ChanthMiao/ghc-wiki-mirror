# Recompilation Avoidance

## What is recompilation avoidance?


When GHC is compiling a module, it tries to determine early on whether

- The results of compilations exist from a previous compilation. This includes [interface files](commentary/compiler/iface-files), hie files, object files and dynamic object files. 
- Recompilation is sure to produce exactly the same results, so it
  is not necessary.


If both of these hold, GHC stops compilation early, because the
existing object code and interface are still valid.  In GHCi and
`--make`, we must generate the `ModDetails` from the `ModIface`, but
this is easily done by calling `MkIface.typecheckIface`.

The recompilation check is **entirely** implemented in [`GHC.Iface.Recomp`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Iface/Recomp.hs). This is the **only** place in the compiler where we make any decision about recompilation.

## Example


Let's use a running example to demonstrate the issues.  We'll have
four modules with dependencies like this:

```wiki
      A
     / \
    B   C
     \ /
      D
```

`A.hs`:

```wiki
module A where
import B
import C

a = print (f 2)
```

`B.hs`:

```wiki
module B (f) where
import D
```

`C.hs`:

```wiki
module C where
import D
```

`D.hs`:

```wiki
module D (T, f, h) where

data T a b = C1 a | C2 b

f :: Int -> Int
f x = h x

h :: Int -> Int
h x = x + 3
```

## Why do we need recompilation avoidance?

### GHCi and `--make`


The simple fact is that when you make a small change to a large
program, it is often not necessary to recompile every module that
depends directly or indirectly on something that changed.  In GHCi and
`--make`, GHC considers every module in the program in dependency
order, and decides whether it needs to be recompiled, or whether the
existing object code and interface will do.



## How does it work?


We use [fingerprints](http://en.wikipedia.org/wiki/Fingerprint_%28computing%29) to uniquely identify the interface exposed by a module,
and to detect when it changes.  In particular, we currently use
128-bit hashes produced by the MD5 algorithm (see
[compiler/GHC/Utils/Fingerprint.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Utils/Fingerprint.hs)).


An [interface file](commentary/compiler/iface-files) contains:

- Various fingerprints:

  - The *interface hash*, which depends on the entire contents of the
    interface file.  This is used to detect whether we should
    update the interface on disk after recompiling the module.  If the
    interface didn't change at all, then we don't want to touch the
    on-disk version because that would cause `make` to perform more
    compilations.
  - The *source hash*, which is a hash of the input file. This is used to determine
    whether a file has been modified or not. In the past we used to use the modification time
    of files but using the source hash is more robust to poor resolution timers and modern build
systems.
  - The *ABI hash* (Application Binary Interface), which depends on everything that the module
    exposes about its implementation: think of this as a hash of
    *export-list hash* and *decls*.
  - The *export-list hash*, which depends on

    - The export list itself.  The export-list hash only depends on the *names* of the exports for the modules. The *types* of these exports are ignored in calculating the hash. Only a change of name or removal or addition of an export will change the hash. Not a type change of definition change.
    - the *orphan hash*, which depends on all the orphan instances/rules in the, and the orphan hashes of all orphan modules below this module in the dependency tree (see [Orphans](commentary/compiler/recompilation-avoidance#orphans)).
    - the **direct** package dependencies (see [Package Version Changes](commentary/compiler/recompilation-avoidance#package-version-changes)).
- *exports*: what the module exports
- *dependencies*: modules and packages that this module **directly** depends on
- *usages*: what specific entities the module depends on
- *decls*: what the module defines
- various other stuff, but the above are the important bits


To look at the contents of an interface, use `ghc --show-iface`.  For
example, here's the output of `ghc --show-iface D.hi` for the module
`D` in our example:

```wiki
interface main:D 6090
  interface hash: 413dacc4c360257e9fb06ad0c13d9fc9
  ABI hash: 0c5278c6f22844f996006259c9f551c8
  export-list hash: cb9dd0d414976d16451bdfe51a021d7d
  orphan hash: 693e9af84d3dfcc71e640e005bdc5e2e
  where
export main:D T f h
module dependencies:
package dependencies: base integer ghc-prim
orphans: base:GHC.Base base:GHC.Num
family instance modules:
import base:GHC.Num 7a6f0c12ee2413f4c07165103924bd61
import base:Prelude ae91aa1798ed1ac514cde3dc4c921717
f4be8645a3e4099b4be0cfc42e976ed7
  data T a b
      RecFlag NonRecursive
      Generics: no
      {- abstract -}
      FamilyInstance: none
0bf838776ef3bc5671e369aec15c3b16
  f :: GHC.Base.Int -> GHC.Base.Int
aa9b8cdef43b6852bcc6f61f6ad6c584
  h :: GHC.Base.Int -> GHC.Base.Int
```


Lines beginning `import` are the *usages*, and after the usages are
the decls.

### Deciding whether to recompile


If we already have all the compilation results for a module, we
might not have to recompile it, if we can be sure the results will be
the same as last time.

- If the hash of the source file has changed then we must recompile it.

- If anything else has changed in a way that would affect the results
  of compiling this module, we must recompile.


In order to determine the second point, we look at the
*dependencies* and *usages* fields of the old interface file.  The
dependencies contains:

- *dep_direct_mods*: Direct dependencies of home-package modules that are
  imported by this module.

- *dep_direct_pkgs*: Direct packages depended on by this module.

- other less important stuff.


First, the direct imports of the current module are resolved to
`Module`s using `Finder.findModule` (a `Module` contains a module name and a
package identifier).  If any of those `Module`s are not listed amongst
the dependencies of the old interface file, then either:

- an exposed package has been upgraded
- we are compiling with different package flags
- a home module that was shadowing a package module has been removed
- a new home module has been added that shadows a package module


and we must recompile.


Second, the *usages* of the module are checked.  The usages contains
two types of information:

- for a module that was imported, the export-list fingerprint of the
  imported module is recorded.  If any of the modules we imported now
  has a different export list we must recompile, so we check the
  current export-list fingerprints against those recorded in the
  usages.

- for every external name mentioned in the source code, the
  fingerprint of that name is recorded in the usages.  This is so
  that if we mention for example an external function `M.f`, we'll
  recompile if `M.f`'s type has changed, or anything referred to
  by `M.f`'s type has changed, or `M.f`'s unfolding has changed
  (when -O is on), and so on.


The interface files for everything in the usages are read (they'll
already be in memory if we're doing `--make`), and the current
versions for each of these entities checked against the usages from
the old interface file.  If any of these versions has changed, the
module must be recompiled.

### Interface file invariants

Often the interface file is not touched in order to avoid unnecessary recompilation of external build systems (see [make](make)). As a result, interface files may not change from the previous build, so may contain outdated information. That said, GHC guarantees that:

- Interface files have an up to date ABI hash for the corresponding module
  - When considering whether or not a module???s dependent modules need to be recompiled due to changes in the current module, a changed ABI hash is a necessary but not sufficient condition for recompilation.

### Example

There are some tricky cases to consider.

Assume optimizations are *not* enabled. If we add a new export `D.g` from module `D` and recompile `A`, then `A` will *not* be recompiled and `A.hi` will not be touched. It works like this:

- `D` is recompiled; the exports and hence ABI hash of `D` changes.
- `B` is considered; it imports all of `D`, so gets recompiled, and now its interface has a different interface hash, but its ABI hash has not changed as it doesn't expose any change from the newly imported `D.g`.
- `C` is considered; it imports all of `D`, so gets recompiled, and now its interface has a different interface hash, but its ABI hash has not changed as it doesn't expose any change from the newly imported `D.g`.
- `A` is considered (if we're using make, this is because `B.hi` changed); Though `D`'s ABI has changed, the only import `D.f` (via B) has not changed, so `A` is not recompiled and `A.hi` is not updated

Since `A.hi` was not updated, it now contains an outdated ABI hash for module `D`, but the ABI hash for `A` itself has not changed so is still up to date.

Now assume optimization *are* enabled. Suppose we change the definition of `D.f` in the example, and make it

```haskell
f x = h x + 1
```

Assuming optimizations are disabled, A.hi will not be updated

Now, ultimately we need to recompile `A`, because it might (assuming optimizations are on) be using
an inlined copy of the old `D.f`, which it got via `B`.

It works like this:

- `D` is recompiled; the fingerprint of `D.f` changes
- `B` is considered; it recorded a usage on the old `D.f`, so
  gets recompiled, and now its interface records a usage on the new `D.f`
- `C` is considered; it doesn't need to be recompiled.
- `A` is considered (if we're using make, this is because `B.hi`
  changed); it recorded a usage on the old `D.f`, and so gets
  recompiled.

Now a slightly more tricky case: suppose we add an INLINE pragma to
`D.f` (this is a trick to prevent GHC from inlining `D.h`, so that we
can demonstrate dependencies between unfoldings).  The code for D.hs
is now

```wiki
{-# INLINE f #-}
f :: Int -> Int
f x = h x + 1

h :: Int -> Int
h x = x + 3
```


Looking at the interface file we can see what happened (snipped slightly):

```wiki
$ ghc --show-iface D.hi
interface main:D 6090
  interface hash: 0a7e886588b3799d909cca39be4b9232
  ABI hash: 8d5cfe1723f32a5b53ded43bf9a1e55b
  export-list hash: cb9dd0d414976d16451bdfe51a021d7d
  orphan hash: 693e9af84d3dfcc71e640e005bdc5e2e
  where
export main:D T f h
674f7fa7c2b13b368042f409007b1f29
  data T a b
      RecFlag NonRecursive
      Generics: no
      = C1 :: forall a b. a -> T a b Stricts: _ |
        C2 :: forall a b. b -> T a b Stricts: _
      FamilyInstance: none
790791c346a0d5965feece84894360a6
  f :: GHC.Base.Int -> GHC.Base.Int
    {- Arity: 1 HasNoCafRefs Strictness: U(L)m
       Unfolding: (__inline_me (\ x :: GHC.Base.Int ->
                                GHC.Base.plusInt (D.h x) (GHC.Base.I# 1))) -}
a0a944e487ebec76031e0672e70bc923
  h :: GHC.Base.Int -> GHC.Base.Int
    {- Arity: 1 HasNoCafRefs Strictness: U(L)m
       Unfolding: (\ x :: GHC.Base.Int ->
                   case @ GHC.Base.Int x of wild { GHC.Base.I# x1 ->
                   GHC.Base.I# (GHC.Prim.+# x1 3) }) -}
```


Note that the unfolding of `D.f` mentions `D.h`.


Now, let's modify `D.h`, and look at the interface file again:

```wiki
$ ghc -O --show-iface D.hi
interface main:D 6090
  interface hash: 55385e568aa80955acbd1b7370041890
  ABI hash: accc0413d94e27c90dff8427f4aafe6e
  export-list hash: cb9dd0d414976d16451bdfe51a021d7d
  orphan hash: 693e9af84d3dfcc71e640e005bdc5e2e
  where
export main:D T f h
674f7fa7c2b13b368042f409007b1f29
  data T a b
      RecFlag NonRecursive
      Generics: no
      = C1 :: forall a b. a -> T a b Stricts: _ |
        C2 :: forall a b. b -> T a b Stricts: _
      FamilyInstance: none
7f2bf159cceae5306ce709db96720f4a
  f :: GHC.Base.Int -> GHC.Base.Int
    {- Arity: 1 HasNoCafRefs Strictness: U(L)m
       Unfolding: (__inline_me (\ x :: GHC.Base.Int ->
                                GHC.Base.plusInt (D.h x) (GHC.Base.I# 1))) -}
5c53713aa59b760e51e6d47173f95b4e
  h :: GHC.Base.Int -> GHC.Base.Int
    {- Arity: 1 HasNoCafRefs Strictness: U(L)m
       Unfolding: (\ x :: GHC.Base.Int ->
                   case @ GHC.Base.Int x of wild { GHC.Base.I# x1 ->
                   GHC.Base.I# (GHC.Prim.+# x1 4) }) -}
```


The fingerprint for `D.h` has changed, because we changed its
definition.  The fingerprint for `D.f` has also changed, because it
depends on `D.h`.  And consequently, the ABI hash has changed, and so
has the interface hash (although the export hash and orphan hash are
still the same). Note that it is significant that we used '-O' here.
If we hadn't used '-O' then a change of a definition doesn't change
any of the hashes because of the lack of inlining.


Why did the fingerprint for `D.f` have to change?  This is vital,
because anything that referred to `D.f` must be recompiled, because it
may now see the new unfolding for `D.h`.


So the fingerprint of an entity represents not just the definition of
the entity itself, but also the definitions of all the entities
reachable from it - its transitive closure.  The consequence of this
is that when recording usages we only have to record the fingerprints
of entities that were referred to directly in the source code, because
the transitive nature of the fingerprint means that we'll recompile if
anything reachable from these entities changes.

### How does fingerprinting work?


We calculate fingerprints by serialising the data to be fingerprinted
using the `Binary` module, and then running the md5 algorithm over the
serlialised data.  When the data contains external `Name`s, the
serialiser emits the fingerprint of the `Name`; this is the way that
the fingerprint of a declaration can be made to depend on the
fingerprints of the things it mentions.

### Mutually recursive groups of entities


When fingerprinting a recursive group of entities, we fingerprint the
group as a whole.  If any of the definitions changes, the fingerprint
of every entity in the group changes.

### Fixities


We include the fixity of an entity when computing its fingerprint.

### Instances


Instances are tricky in Haskell, because they aren't imported or
exported explicitly.  Haskell requires that any instance defined in a
module directly or indirectly imported by the current module is
visible.  So how do we track instances for recompilation, such that if
a relevant instance is changed, added, or removed anywhere beneath the
current module we will trigger a recompilation?


Here's how it works.  For each instance we pick a distinguished entity
to attach the instance to - possibly the class itself, or a type
constructor mentioned in the instance.  The entity we pick must be
defined in the current module; if there are none to pick, then the
instance is an orphan (more about those in the section on Orphans,
below).


Having picked the distinguished entity, when fingerprinting that
entity we include the instances.  For example, consider an instance
for class C at type T.  Any module that could use this instance must
depend (directly or indirectly) on both C and T, so it doesn't matter
whether we attach the instance to C or T - either way it will be
included in the fingerprint of something that the module depends on.
In this way we can be sure that if someone adds a new instance, or
removes an existing instance, if the instance is relevant to a module
then it will affect the fingerprint of something that the module
depends on, and hence will trigger recompilation.


In fact, we don't need to include the instance itself when fingerprinting C or T, it is enough to include the DFun (dictionary function) Id, since the type of this Id includes the form of the instance.  Furthermore, we *must* include the DFun anway, because we must have a dependency on the dictionary and its methods, just in case they are inlined in a client module.  A DFun looks something like this:

```wiki
c3e94597bf9a532e094067b08c216493
  $fEqBool :: GHC.Base.Eq GHC.Bool.Bool
    {- HasNoCafRefs Strictness: m
       Unfolding: (GHC.Base.:DEq @ GHC.Bool.Bool GHC.Base.==2
       GHC.Base.$s$dm/=1)
```


Making a type or class depend on its instances can cause a lot of recompilation when an instance changes.  For example:

```wiki
module A (T) where
import B (C)
data T = ...
instance C t where ...
```


now the DFun for the instance `C T` will be attached to `T`, and so `T`'s fingerprint will change when anything about the instance changes, including `C` itself.  So there is now have a dependency of `T` on `C`, which can cause a lot of recompilation whenever `C` changes.  Modules using `T` who do not care about `C` will still be recompiled.


This seems like it would cause a lot of unnecessary recompilation.  Indeed, in GHC 7.0.1 and earlier we tried to optimise this case, by breaking the dependency of `T` on `C` and tracking usages of DFuns directly - whenever a DFun was used, the typechecker would record the fact, and a usage on the DFun would be recorded in the interface file.  Unfortunately, there's a bug in this plan (see #4469).  When we're using `make`, we only recompile a module when any of the interfaces that it directly imports have changed; but a DFun dependency can refer to any module, not just the directly imported ones.  Instead, we have to ensure that if an instance related to a particular type or class has changed, then the fingerprint on either the type or class changes, which is what the current plan does.  It would be nice to optimise this in a safe way, and maybe in the future we will be able to do that.

### Orphans


What if we have no declaration to attach the instance to?  Instances
with no obvious parent are called *orphans*, and GHC must read the
interface for any module that contains orphan instances below the
current module, just in case those instances are relevant when
compiling the current module.


Orphans require special treatment in the recompilation checker.

- Every module has an *orphan hash*, which is a fingerprint of all
  the orphan instances (and rules) in the current module.

- The *export hash* depends on the *orphan hash* of the current
  module, and all modules below the current module in the dependency
  tree.  This models the fact that all instances defined in modules
  below the current module are available to importers of this module.


So if we add, delete, or modify an orphan instance, the orphan hash of
the current module will change, and so will the export hash of the
current module.  This will trigger recompilation of modules that
import the current module, which will cause their export hashes to
change, and so on up the dependency tree.


This means a lot of recompilation, but it is at least safe.  The trick
is to avoid orphan instances as far as possible, which is why GHC has
the warning flag `-fwarn-orphans`.

### Rules


RULEs are treated very much like instances: they are attached to one
particular parent declaration, and if a suitable parent cannot be
found, they become orphans and are handled in the same way as orphan
instances.

### On ordering


When fingerprinting a collection of things, for example the export
list, we must be careful to use a canonical ordering for the
collection.  Otherwise, if we recompile the module without making any
changes, we might get a different fingerprint due to accidental
reordering of the elements.


Why would we get accidental reordering?  GHC relies heavily on
"uniques" internally (see [compiler/GHC/Types/Unique.hs](https://gitlab.haskell.org/ghc/ghc/blob/master/compiler/GHC/Types/Unique.hs)): every
entity has a unique, and uniques are assigned semi-randomly.  Asking
for the contents of a `UniqSet` or `UniqFM` will return the elements in
order of their uniques, which may vary from run to run of the
compiler.


The solution is to sort the elements using a stable ordering, such as
lexicographic ordering.

### Packages


We need to record usage information about package modules too, so that we
can correctly trigger recompilation if we depend on a package that has changed.  But
packages change rarely, so it would be wasteful to record detailed usage information for
every entity that we use from an external package (imagine recording the fingerprints for
`Bool`, `Int`, etc.).  Instead, we simply record the ABI fingerprint for every package
module that was imported by the current module.  That way, if anything about the ABI of
that package module has changed, then we can trigger a recompilation.


(Correctly triggering recompilation when packages change was one of the things we fixed when implementing fingerprints, see #1372).

### Package version changes


If the version of a package is bumped, what forces recompilation of
the things that depend on it?

1. If a module from the package is imported directly, then we will notice that the imported module is not amongst the dependencies of the module when it was compiled last, and force a recompilation (see [Deciding whether to recompile](commentary/compiler/recompilation-avoidance#deciding-whether-to-recompile)).

1. If a module from the old package is imported indirectly, then the old package will be amongst the package dependencies (`dep_pkgs . mi_deps`), so we must recompile otherwise these dependencies will be inconsistent.  The way we handle this case is by including the package dependencies in the *export hash* of a module, so that other modules which import this module will automatically be recompiled when one of the package dependencies changes.  The recompiled module will have new package dependencies, which will force recompilation of its importers, and so on.  Therefore if a package version changes, the change will be propagated throughout the module dependency graph.

## Interface stability


For recompilation avoidance to be really effective, we need to ensure that fingerprints do not change unnecessarily.  That is, if a module is modified, it should be the case that the only fingerprints that change are related to the parts of the module that were modified.  This may seem obvious, but it's surprisingly easy to get wrong.  Here are some of the ways we got it wrong in the past, and some ways we still get it wrong.

- Prior to GHC 6.12, dictionary functions were named something like `M.$f23`, where `M` is the module defining the instance, and the number `23` was generated by simply assigning numbers to the dictionary functions defined by `M` sequentially.  This is a problem for recompilation avoidance, because now removing or adding an instance in `M` will change the numbering, and force recompilation of anything that depends on any instance in `M`.  Worse, the numbers are assigned non-deterministically, so simply recompiling `M` without changing its code could change the fingerprints.  In GHC 6.12 we changed it so that dictionary functions are named after the class and type(s) of the instance, e.g. `M.$fOrdInteger`.

- compiler-generated bindings used to be numbered in the same way, non-deterministically.  The non-determinism arises because Uniques are assigned by the compiler non-deterministically.  Well, they are deterministic but not in a way that you can sensibly control, because it depends on the order in which interface bindings are read, etc.  Internal mappings use Uniques as the key, so asking for the elements of a mapping gives a non-deterministic ordering.  The list of bindings emitted by the simplifier, although in dependency order, can vary non-deterministically within the constraints of the dependencies.  So if we number the compiler-generated bindings sequentially, the result will be a non-deterministic ABI.

  In GHC 6.12 we changed this so that compiler-generated bindings are given names of the form `f_x`, where `f` is the name of the exported Id that refers to the binding.  If there are multiple `f_x`s, then they are disambiguated with an integer suffix, but the numbers are assigned deterministically, by traversing the definition of `f` in depth-first left-to-right order to find references.  See `TidyPgm.chooseExternalIds`.

- There are still some cases where an interface can change without changing the source code.  The ones we know about are listed in #4012

## Template Haskell and Plugins

Template Haskell and Plugins add different kinds of dependencies to normal
module imports. If a module uses a Template Haskell splice then the result of
compiling the module can depend on the implementation of a modules imports rather than just their interface. Therefore, a module's ABI can remain the same but not trigger recompilation which can lead to stale results.

There is a similar problem for plugins. If a module uses a plugin then
depending on the ABI of a plugin is usually not enough. Modules defining
plugins expose one identifier `plugin :: Plugin` and so the ABI does not often
change. However, we still want to recompile a module if the implementation of
the plugin changes as we expect the new modifications to the plugin to effect the module we are currently compiling.

The solution is the same in both cases -- rather than just depending on the
interface file, we also depend on the hash of the resulting object files. If the
hash of the object file changes, then the module is recompiled.

The dependencies are calculated using the `getLinkDeps`, which is also
used to calculate dependencies when loading modules for execution
using the internal or external interpreter.

* Object file creation is non-deterministic, so sometimes the object files hash will change despite the implementation not changing. This will cause slightly more recompilation but is still safe.




## Only storing direct information

It is important to only store direct information in interface files for two reasons:

* The interface file invariants state that, if you store any transitive information
  then if anything in the transtive closure changes then you have to recompile the module.
* Recompilation checking can get very slow in big projects due to checking the whole
  transitive closure to see if it's the same as before.

See #16885 for some examples where more recompilation is performed than necessary.

Removing transitive information from the interface files means that anywhere in
the compiler where we need information about transitive dependencies we need to compute
the transitive closure rather than simply looking at the interface file. It turns out,
there are not so many places which are called so often.

### Computing the transitive closure for home packages

* In `--make` mode, the transitive closure can be computed by traversing the HPT.
For example see `hptSomeThingsBelow`.

* In `-c` mode, the transitive closure has to be computed by recursively loading interface
  files below the current module. Note that this is no worse than before because
  a similar traversal was already necessary in `checkDependencies` to verify the
  information in the `dep_mods` field was up-to-date.
  In the future it would be good to unify `--make` and `-c` by also
  populating the HPT in `-c` mode (#19795)

### Where is the transitive closure of home module imports needed

* Backpack: Computing `mi_free_holes` -- TODO: Need to fix this usage still
* Linker: getLinkDeps - find the object files which we need to link when loading a module into ghci, this function already computed the transitive closure!
* Instances/Rules/Annotations - Just for `--make` mode, computed by `hptSomeThingsBelow` which now performs a traversal.
* `eps_is_boot` field - Used in `-c` mode to determine whether we should be loading a boot interface or a normal interface. This is now prepopulated by doing a traversal which looks at the transitive closure of interface files.

### Where is the transitive closure of package imports needed

* SafeHaskell: `-fpackage-trust` check has some strange rules about transitively I don't understand yet. TODO Test `Check01` and `[Note Tracking Trust Transitively]`
* Linking: We actually only *need* to link against directly depended upon packages.






### `make`

`make` works by checking the timestamps on dependencies and
recompiling things when the dependencies are newer.  Dependency lists
for `make` look like this (generated by `ghc -M`):

```wiki
# DO NOT DELETE: Beginning of Haskell dependencies
D.o : D.hs
B.o : B.hs
B.o : D.hi
C.o : C.hs
C.o : D.hi
A.o : A.hs
A.o : C.hi
A.o : B.hi
# DO NOT DELETE: End of Haskell dependencies
```


Only the `.hi` files of the *direct imports* of a module are listed.
For example, `A.o` depends on `C.hi` and `B.hi`, but not `D.hi`.
Nevertheless, if D is modified, we might need to recompile A.  How
does this happen?

- first, make will recompile D because its source file has changed,
  generating a new `D.o` and `D.hi`.

- If after recompiling D, we notice that its interface is the same
  as before, there is no need to modify the `.hi` file.  If the `.hi`
  file is not modified by the compilation, then `make` will notice
  and not recompile `B` or `C`, or indeed `A`.  This is an important
  optimisation.

- Suppose the change to `D` did cause a change in the interface
  (e.g. the type of `f` changed).  Now, `make` will recompile both
  `B` and `C`.  Suppose that the interfaces to `B` and `C`
  remain the same: B's interface says only that it re-exports `D.f`,
  so the fact that `f` has a new type does not affect `B`'s
  interface.

- Now, `A`'s dependencies are unchanged, so `A` will not be
  recompiled.  But this is wrong: `A` might depend on something from
  `D` that was re-exported via `B` or `C`, and therefore need
  recompiling.


To ensure that `A` is recompiled, we therefore have two options:

1. arrange that make knows about the dependency of A on D.

1. arrange to touch `B.hi` and `C.hi` even if they haven't changed.


GHC currently does (2), more about that in a minute.


Why not do (1)?  Well, then *every* time `D.hi` changed, GHC would be
invoked on `A` again.  But `A` doesn't depend directly on `D`: it
imports `B`, and it might be therefore be insensitive to changes in `D`.
By telling make only about direct dependencies, we gain the ability to
avoid recompiling modules further up the dependency graph, by not touching
interface files when they don't change.


Back to (2).  In addition to correctness (recompile when necessary), we also want to
avoid unnecessary recompilation as far as possible.
Make only knows about very coarse-grained dependencies.  For example,
it doesn't know that changing the type of `D.f` can have no effect on
`C`, so `C` does not in fact need to be recompiled, because to do so
would generate exactly the same `.o` and `.hi` files as last time.
GHC does have enough information to figure this out, so when GHC is
asked to recompile a module it invokes the *recompilation checker*
to determine whether recompilation can be avoided in this case.
