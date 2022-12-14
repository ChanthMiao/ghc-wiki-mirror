# Implicit locations


This is a lightweight proposal to help with error-finding; see the [ExplicitCallStack](explicit-call-stack) summary page.

## Specification of the feature


Suppose we write

```wiki
f :: [a] -> Int
f []     = error ("Failure at " ++ show (?location :: Location))
f (x:xs) = ...
```


The idea is that `(?location :: Location)` expands to the current source location.


It is written in the syntax of [implicit parameters](http://www.haskell.org/ghc/docs/7.8.2/html/users_guide/other-type-extensions.html#implicit-parameters) because the intention is that it can (if you wish) be passed implicitly from the caller, like this:

```wiki
f :: (?location :: Location) => [a] -> Int
f []     = error ("Failure at " ++ show ?location)
f (x:xs) = ...
```


Now the displayed location will be the location of `f`'s *call site*, rather than the location of the call to `show` inside `f`.  In fact `Location` is a value that captures a *stack* of source-locations, so that you get to see both.


So `?location :: Location` is a rather special implicit parameter, but the *only* special-ness is in the typechecker.  Specifically, when solving a "wanted" constraint `w :: (?location :: Location)`:

- If there is an enclosing "given" constraint `g :: (?location :: Location)`, usually from a type signature, the wanted constraint is solved from the given one, by pushing on `w`'s source location, something like

  ```wiki
    w = push <w's srcloc> g
  ```


 


- If there is no enclosing "given" constraint, the wanted constraint is solved with a unit stack:

  ```wiki
    w = push <w's srcloc> <emptyloc>
  ```


So the type checker will never report an insoluble constraint `(?location :: location)`.


The `Location` type is abstract, but has functions to:

- Turn it into a list of `SrcLoc`
- From a `SrcLoc` you can get line number, character position, file name, module name, package name, etc.


(The names are entirely up for grabs here.)

## Discussion

- This design makes it possible to grab the current source location. But it *also* makes it convenient to get the call site location, if that's what you want, in a very similar way to [ExplicitCallStack/FindingTheNeedle](explicit-call-stack/finding-the-needle).  It doesn't do everything, by any means, and is probably best regarded as a slight enhancement to a basic "give me the source location" mechanism.  But it it's extremely simple and cheap.

- The implicit parameter story answers the open questions in the finding-the-needle paper. (You may not like the answer, but the answer is at least clear.)  For example, consider a CAF

  ```wiki
  xs :: [Int]
  xs = ...blah....
  ```

>
>
> Here we will not propagate a stack from `xs`'s useage site.  But if we wrote
>
>
> ```wiki
> xs :: (?location :: Location) => [Int]
> xs = ...blah...
> ```
>
>
> then we would.  Of course, that would mean `xs` was evaluated once per usage site, but at least that fact becomes 100% clear.
>
>

>
>
> Similarly, overloading behaves predictably. Consider
>
>
> ```wiki
> class C a where
>   op :: a -> a
> instance C Int where
>   op 0 = ....crash...
>   op n = .....
> ```
>
>
> Since `op`'s signature does not have `(?location :: Location`), the crash does not have access to `op`'s call site. You would have to change the class signature to get that.  Doing so might be fine if it's your class, awkward if it's a library class, and impossible if it's a Prelude class.
>
>

- There's an open design choice for functions with an inferred type, e.g.

  ```wiki
  f [] = ....(?location :: Location)...
  f (x:xs) = ...
  ```

  Do we infer a constraint `(?location :: Location)`, and thereby inherit information from the call site, but impose a small runtime overhead?  Or do we solve the constraint locally, and refrain from passing call-site information.  I'm inclined to the latter. You pay only if you ask.

- I've been writing `?location :: Location`, but it's the type name that matters. It'd be fine to use any name for the implicit parameter itself, e.g. `?loc :: Location`

- GHC has number of other special cases in the constraint solver: for `Coercible`, `Has` and `Upd` (overloaded record fields), so another special case there is not a big deal.  There are no other implications for the compiler whatsoever.

## Implementation Details


These notes are based on the updated implementation at [https://phabricator.haskell.org/D1422](https://phabricator.haskell.org/D1422). 

- The major difference between the current implementation and the initial proposal is in the handling of bare occurrences of `?location :: Location`. Instead of solving such occurrences for the current source location, we treat them as regular implicit parameters and resolve them to the binding in the context (if there is no binder we default the occurrence to the empty call-stack). Constraints that arise from a function call are solved as above, by pushing the current call-site onto the stack in the context.

- We add a two new datatypes to `base`:

  1. `GHC.Stack.SrcLoc`, a single source location including package/module/file names and a source span, and
  1. `GHC.Stack.CallStack`, a `[(String, SrcLoc)]`s. The `String` always corresponds to the name of the function that was called, and the list is sorted by most recent call.

>
>
> `CallStack` is kept abstract so that we may experiment with extensions to the system, for example the call-stack "grooming" functionality described in the next section.
>
>

- GHC ignores the name of an implicit CallStack parameter, e.g.

  ```wiki
  f = show (?loc :: CallStack)
  ```

  and

  ```wiki
  g = show (?location :: CallStack)
  ```

  are both valid. However, we will only push a new call-site onto an existing stack **if** the names match, e.g. in

  ```wiki
  f :: (?loc :: CallStack) => IO ()
  f = print (?location :: CallStack)
  ```

  the printed call-stack will be empty, it will **not include** `f`s call-site.

- Responding to the open question above, GHC will infer implicit CallStack constraints as for any other implicit parameter.

## Extension: Grooming the CallStack


Joachim Breitner suggests in [https://ghc.haskell.org/trac/ghc/ticket/11049](https://ghc.haskell.org/trac/ghc/ticket/11049) that it would be nice to allow users to hide portions of the CallStack that correspond to internal library modules, presenting only the portions that the author deems "interesting". Following is Joachim's description of the extension.

---


Assume we are given a function

```wiki
readFile :: ?callstack :: CallStack => FilePath -> IO ()
```


that prints a stack trace with every IO error that happens (bar in the description).


Now assume we want to implement a function that uses the above, for example

```wiki
readConfig :: IO Config
readConfig = do
  s <- readFile "config.txt" -- line 42
  return (parseConfig s)
```


and we deliberately do not want that function have a `?_::CallStack` constraint. What will happen when config.txt will be missing? `readFile` will raise an exception that includes a call stack that tells the user that this was raised due to `readFile` being called in line 42. But as the author of the `readConfig` function, I do not want this information (which is not very helpful to the user) to be omitted. This is the first use case.


The second is related. I might now allow `readConfig` to have a `?_::CallStack` constraint, e.g.

```wiki
readConfig :: ?callstack::CallStack => IO Config
readConfig = do
  s <- readFile "config.txt" -- line 42
  return (parseConfig s)
```


But I do want to provide a polished API, and not leak any information to the users of my API about my internals. So I **do** want the `?callstack` to be passed on to readFile and be included in the exception, but I **don???t** want it to mention line 42; instead it should end with the (for the user relevant) information where `readConfig` was called. This is the second use case.


So now to the suggested implementation: In both cases, I want to insert a marker into the callstack that makes the call stack printer ignore anything ???below??? or ???after??? it. This is the suggested `rootCallStack` value, and it allows me to write

```wiki
readConfig :: IO Config
readConfig = do
  s <- let ?callstack = rootCallStack in readFile "config.txt" -- line 42
  return (parseConfig s)
```


resp.

```wiki
readConfig :: ?callstack::CallStack => IO Config
readConfig = do
  s <- let ?callstack = rootCallStack `pushCallStack` ?callstack in readFile "config.txt" -- line 42
  return (parseConfig s)
```


to implement the above.

---


The important thing is the ability to **freeze** a callstack so that future "push" operations are ignored, so I will call this `freezeCallStack` instead of Joachim's `rootCallStack`. `freezeCallStack`s behavior is given by the following equations,

```wiki
  pushCallStack srcLoc (freezeCallStack callStack) = freezeCallStack callStack
  getCallStack         (freezeCallStack callStack) = getCallStack callStack
```


and can be implemented with a rather small change to `base` (no changes necessary in the compiler).

1. Instead of `CallStack` being an alias for `[(String, SrcLoc)]` we define it ourselves, with an extra `FreezeCallStack` constructor

```wiki
data CallStack = EmptyCallStack
               | PushCallStack CallSite CallStack
               | FreezeCallStack CallStack
```

1. We define

```wiki
emptyCallStack  = EmptyCallStack
freezeCallStack = FreezeCallStack
```

>
>
> so we can keep `CallStack` abstract
>
>

1. We define `pushCallStack` to be a no-op on frozen call-stacks

```wiki
pushCallStack cs stk = case stk of
  FreezeCallStack _ -> stk
  _                 -> PushCallStack cs stk
```

1. We define `getCallStack` as expected

```wiki
getCallStack stk = case stk of
  EmptyCallStack        -> []
  PushCallStack cs stk' -> cs : getCallStack stk'
  FreezeCallStack stk'  -> getCallStack stk'
```


Now we can define both of Joachim's sample use-cases

```wiki
readConfig1 :: IO Config
readConfig1 = do
                        -- produces no stack trace as we freeze the empty stack
  s <- let ?callstack = freezeCallStack emptyCallStack in readFile "config.txt" -- line 42
  return (parseConfig s)


readConfig2 :: ?callstack::CallStack => IO Config
readConfig2 = do
                        -- produces a stack trace containing only calls outside of readConfig2
  s <- let ?callstack = freezeCallStack ?callstack in readFile "config.txt" -- line 42
  return (parseConfig s)
```

## Generalizing to `setCallStack`


It would be nice to generalize the rebinding trick above to a function

```wiki
setCallStack :: CallStack -> (HasCallStack => a) -> a
```


that overrides the callstack for the sub-computation.


Unfortunately we can't write this function in Haskell (though we could easily write it in Core).. Why? Let's look at the implementation

```wiki
setCallStack :: CallStack -> (HasCallStack => a) -> a
setCallStack stk do_this =
  let ?callStack = stk in do_this
```


Rebinding `?callStack` works just fine, but the occurrence of `do_this`
causes GHC to push an entry onto the stack, which is less than ideal.


What does this look like in practice? If we evaluate

```wiki
setCallStack foo (error "die")
```


the resulting stack will be

```wiki
  error
  *do_this*
  foo
```


The rebinding trick works with `freezeCallStack` precisely because we
freeze the CallStack, so the push from `do_this` is ignored.

# Alternate Proposal


Richard E asks on [ghc-devs](https://mail.haskell.org/pipermail/ghc-devs/2016-January/011068.html) why we use ImplicitParams instead of a nullary type class. Indeed, the name of the implicit parameter is not strictly necessary, in `base` we use `?callStack` as a convention to ensure the callstacks are propagated correctly. So why use ImplicitParams at all, instead we could write

```wiki
undefined :: AppendsCallStack => a
```


and not have to enable `ImplicitParams` or remember the `?callStack` convention.

### No ImplicitParams


Simon PJ suggests a magic nullary class

```wiki
class AppendsCallStack where
  callStack :: CallStack
```


with the same solver as we currently have for the ImplicitParams implementation. Instead of using `?callStack :: CallStack` to observe the current call-stack, the user would use the class method 

```wiki
callStack :: AppendsCallStack => CallStack
```

#### Eric S comments


If we keep the same solving logic,

```wiki
callStack :: AppendsCallStack => CallStack
```


would actually append the occurrence of `callStack` to the stack, whereas

```wiki
?callStack :: CallStack
```


would simply return the current stack unmodified (it's really **just** an implicit parameter here). So the semantics are a bit different. This means a user would have to write `popCallStack callStack`, which is counter-intuitive, or we would have to change the solver logic (may or may not be an issue).


Furthermore, how would we handle the "Grooming the CallStack" feature in this scenario? We currently have a function

```wiki
withFrozenCallStack :: (?callStack :: CallStack)
                    => ( (?callStack :: CallStack) => a )
                    -> a
withFrozenCallStack do_this =
                   -- we pop the stack before freezing it to remove
                   -- withFrozenCallStack's call-site
  let ?callStack = freezeCallStack (popCallStack ?callStack)
  in do_this
```


that rebinds the current call-stack before passing control to the continuation. How would we implement this without ImplicitParams? It seems like `withFrozenCallStack` would have to be a magic function.

### Constraint Synonym


Alternatively, Joachim B suggests simply defining a constraint synonym

```wiki
type AppendsCallStack = ?callStack :: CallStack
```


This is currently rejected by GHC 8.0 RC1 (#11466), but would achieve the same user-facing goal of hiding the implicit parameter.
