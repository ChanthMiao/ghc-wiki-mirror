This document is a living, incremental style guide targeted at contributors to the GHC compiler's documentation. Its first and main purpose is to standardise our current practices when it comes to documentation writing.

This guide is not a language guide on the English language. The writer should refer to the New Oxford Style Manual, the Chicago Manual of Style, or any other professional reference for any concerns pertaining to writing in the English language.

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL
NOT", "SHOULD", "SHOULD NOT", "RECOMMENDED",  "MAY", and
"OPTIONAL" in this document are to be interpreted as described in [RFC 2119](https://www.ietf.org/rfc/rfc2119.txt).

## Table of contents

[[_TOC_]]


## Comments

There are two kinds of comments in source code, comments that describe the interface (i.e. how is this supposed to be used) and comments that describe the implementation (e.g. subtle gotchas). 


### Comments on top-level entities

Every top-level entity should have a Haddock comment that describes what it does and, if needed, why it's there. Example:

```haskell
-- | Returns which registers are read and written by this 
-- instruction, as a (read, written) pair. This info is used
-- by the register allocator.
x86_regUsageOfInstr :: Platform -> Instr -> RegUsage
```

### Comments in the source code

Commenting is good but

- long comments *interleaved with the code* can make the code itself incredibly hard to read, and
- long comments *detached from the code* are easy to miss when you are editing the code itself, and soon become out of date or even misleading.


A consensus was reached on the use of `Note` blocks outside of the code they document, with a reference to them at the appropriate places.  
For example:  

```haskell
prepareRhs :: SimplEnv -> OutExpr -> SimplM (SimplEnv, OutExpr)
-- Adds new floats to the env iff that allows us to return a good RHS
prepareRhs env (Cast rhs co)    -- Note [Float coercions]
  | (ty1, _ty2) <- coercionKind co      -- Do *not* do this if rhs is unlifted 
  , not (isUnLiftedType ty1)            -- see Note [Float coercions (unlifted)]
  = do  { (env', rhs') <- makeTrivial env rhs
        ; return (env', Cast rhs' co) }

        ...more equations for prepareRhs....

{- Note [Float coercions]
   ~~~~~~~~~~~~~~~~~~~~~~
When we find the binding
        x = e `cast` co
we'd like to transform it to
        x' = e
        x = x `cast` co         -- A trivial binding
There's a chance that e will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
and lead to further optimisation.  
        ...more stuff about coercion floating...
-}
```


Notice that

- **Interleaved with the code** is a short link `Note [Float coercions]`. You can't miss it when you are editing the code, but you can still see the code itself.
- **Detached from the code** is the linked comment, starting with the same string `Note [Float coercions]`.  It can be long, and often includes examples.


The standard format "`Note [Float coercions]`" serves like an URL, to point to an out-of-line comment.  Usually the target is in the same module, but not always.  Sometimes we say

```haskell
    -- See Note [Float coercions] in SpecConstr.lhs
```


Please use this technique.  It's robust, and survives successive changes to the same lines of code.  When you are changing code, it draws attention to non-obvious things you might want to bear in mind.  When you encounter the note itself you can search for the string to find the code that implements the thoughts contained in the comment.

### Comments and examples


When writing a comment to explain a subtle point, consider including an example code
snippet that illustrates the point.  For example, the above `Note [Float coercions]` continues thus:

```wiki
There's a chance that be will be a constructor application or function, or something
like that, so moving the coerion to the usage site may well cancel the coersions
and lead to further optimisation.  Example:

     data family T a :: *
     data instance T Int = T Int

     foo :: Int -> Int -> Int
     foo m n = ...
        where
          x = T m
          go 0 = 0
          go n = case x of { T m -> go (n-m) }
                -- This case should optimise
```


These kind of code snippets are extremely helpful to illustrate the point in a
concrete way.  Other ways of making the comment concrete are:

- Cite a particular ticket that this bit of code deals with
- Cite a test case in the test suite that illustrates it

### Longer comments or architectural commentary


Comments with a broad scope, describing the architecture or workings of more than one module, belong in the [GHC Commentary][https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/) rather than in the code.  Put the URL for the relevant commentary page in a comment in the code itself, and also put URLs for all relevant commentary pages in a comment at the top of each module.
