In Backpack, modules are assembled into **units** (the same notion as `cabal` components). We fully qualify module names using a colon: `p:A` means the module `A` from unit `p`.

The essence of Backpack is that a unit may contain holes to be filled in later. For example, we might have:

```haskell
unit p where
  signature H where { .. }
  module A where { import H; .. }
```
Here, we can think of `A` as a piece of code that depends on a certain collection of things specified in the signature `H`; for instance `H` might specify an interface for container-like structures and `A` is code that works with anything that implements this interface.   

The way this is set up in Backpack, we **should not** think of `H` as a module in unit `p`. Instead, it's an **input** to unit `p`: a hole to be filled in later by the implementor.
To instantiate `p` with an implementation for `H`, say with `Data.IntMap`, we use the following notation: `p[H=Data.IntMap]`. To refer to something in this instantiation, we use `:` as above, e.g. to refer to the version of `A` we obtain under this instantiation, we write `p[H=Data.IntMap]:A`. In this case, we say that `Data.IntMap` is the **semantic module** which implements the signature `H`.
When we are typechecking `p` on its own, we "fill in" `H` with a standalone **hole module** `<H>`, which is not part of unit `p` but instead lives in its own **hole unit**. We write this as `p[H=<H>]`, and we can think of the **signature module** `H` as meaning `p[H=<H>]:H`. (This is sometimes also called the **identity module** for `H`, to distinguish it from semantic modules for `H` which are implementations of `H`).

To recap:
  - signature declarations in a unit indicate holes to be filled later,
  - to typecheck a unit in which not all holes have been filled, we create hole units containing hole modules which stand in for whatever implementation we might get later,
  - to compile a fully instantiated unit, we fill in the holes with actual modules. A module used to fill in a signature is the semantic module for that signature.