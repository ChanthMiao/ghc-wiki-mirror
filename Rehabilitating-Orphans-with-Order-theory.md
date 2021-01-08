## Motivation

Without orphans, the depenency graph suffers from linearization: the richer class hierarchy we have, the more we approach the a total order where every package is either imported or imports every other. This is catastrophic for productivity. 

The great thing about open source, and open source with Haskell in particular, is how much the product of our labor --- the code --- itself synchronizes the laborers --- us. The type systems allows information that humans could never communicate in a "game of telephone" to losslessly flow from package to package, all throughout the dependency tree. But rather than force all release happy to be tightly coordinately, types and version constraint solving frees us to release libraries fairly independently.

How do we in fact get more asynchronicity out of types, the great synchronizer? I think the answer lies with Order theory. The PVP induces a relation on versions that indicates compatibility. That relation is partial order. The module `import` and package `build-depends` relations are also partial orders, with a nice isomorphism from the former to the latter. All this allows the ecosystem at large---hackage, let's say---to be seen as a giant CRDT.

From that vantage point, the linearization of the dependency graph directly inhibits the ability of the ecosystem to evolve concurrently, and thus for work to be parallelized.

> TODO some theoretical investigation of:
> - How breaking changes bubble up
> - Expected chain length from dependency to a given "goal packages"
> - How linearization relates to dependency bloat

## Background

### World semantics

GHC's focus on checking consistency instance by instance has good performance characteristics, but obscures the theory of the task.

The "world semantics" from Chapter 1, section 4 of [1] clarify the situation immensely. [TODO summarize key points, but really, just go read it.]

The only quibble I might add is that the multi-param type class example of non-orphans gone wrong can be construed as a semi-orphan: because it only occurs when of the type class parameters have instance heads that *wouldn't* pass the orphan checks were they so sole parameter. A stronger orphan check would have required local-type-guarded argument for every parameter for a non-local type class, and that, while super restrictive, would solve the problem.

## Rust

In https://aturon.github.io/tech/2017/04/24/negative-chalk/ Aaron Turon, one of the core rust designers (as in the ones with the PhDs signing off the theory) makes the connection between modal logic and type classes more specific. For a really quick summary:

- In both Rust and Haskell today, while the instances themselves are monotonic, the consistency checks aren't and cannot be.
- Rust prohibits orphans completely, unlike Haskell, but has much more complicated subtle orphan checks, sometimes observing whether the upstream instances (cover as much as they could), to compensate.

I don't want to advocate for the rust solution, but I do want to point out that it's precisely the non-monotonicity of the consistency checks that opens the door to a vast and rich design space. 

## Solution


[1]: Non-Reformist Reform for Haskell Modularity. PhD thesis, Saarland University, 2019. https://people.mpi-sws.org/~skilpat/papers/kilpatrick-thesis-nov-2019-publication.pdf