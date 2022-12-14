# Entailment of Type Equalities


Our aim is to derive the entailment judgement

```wiki
  g1, .., gn  |-  w1, .., wm
```


i.e., whether we can derive the wanted equalities `w1` to `wm` from the given equalities `g1`, .., `gn` under a given set of toplevel equality schemas (i.e., equalities involving universally quantified variables).  We permit unification variables in the wanted equalities, and a derivation may include the instantiation of these variables; i.e., may compute a unifier.  However, that unifer must be most general.


The derivation algorithm is complicated by the pragmatic requirement that, even if there is no derivation for the judgement, we would like to compute a unifier by simplifying the set of wanted equalities as far as possible (to get a unifier that is as specific as possible).  The solution, then, consists of this unifer together with the simplified wanted equalities.  This functionality is necessary for type inference.


The following is based on ideas for the new, post-ICFP'08 solving algorithm described in CVS `papers/type-synonym/new-single.tex`.  A revised version of `new-single.tex` that integrates the core ideas from this wiki page is in `papers/type-synonym/normalised_equations_algorithm.tex`.  Most of the code is in the module `TcTyFuns`.


Notes regarding the implementation inn `TcTyFuns`

- The implementation simultaneously normalises class constraints, but does not simplify them.  Currently, equality and class constraint simplification are separate processes and invoked alternating.  We are currently in the process of changing this to realise an [integrated solver.](type-functions/integrated-solver)
- Due to higher-rank types, the given equalities may contain meta type variables.  However, during solving of equalities, we treat these meta variables like skolems; in particular, we won't instantiate them.
- The implementation does not instantiate meta variables in place, but instead returns a set of type variables bindings.  More precisely, the solver will only instantiate meta variables that are created during solving.  All other variables, which may be free in the environment, will appear in the resulting unifier strictly as explicit type variable bindings.  (These may then be executed by the caller.)

## Terminology


<table><tr><th><i>Wanted equality</i></th>
<td>
An equality constraint that we need to derive during type checking.  Failure to derive it leads to rejection of the checked program.
</td></tr>
<tr><th><i>Local equality</i>, <i>given equality</i></th>
<td>
An equality constraint that -in a certain scope- may be used to derive wanted equalities.
</td></tr>
<tr><th><i>Flexible type variable</i>,  <i>unification variable</i>, <i>HM variable</i></th>
<td>
Type variables that may be <b>globally</b> instantiated by unification.  We use Greek letters <tt>alpha, beta,</tt>... as names for these variables.
</td></tr>
<tr><th><i>Rigid type variable</i>, <i>skolem type variable</i></th>
<td>
Type variable that cannot be globally instantiated, but it may be <b>locally</b> refined by a local equality constraint.  We use Roman letters <tt>a, b,</tt>... as names for these variables.
</td></tr></table>


In positions where we can have both flexible and rigid variables, we use `x, y, z`.

## Overall algorithm


The overall structure is as in `new-single.tex`, namely

1. normalise all constraints (both locals and wanteds),
1. solve the wanteds, and
1. finalise.


However, the three phases differ in important ways.  In particular, normalisation includes decompositions & the occurs check, and we don't instantiate any flexible type variables before we finalise (i.e., solving is purely local).

## Normal equalities



Central to the algorithm are **normal equalities**, which can be regarded as a set of rewrite rules.  Normal equalities are carefully oriented and contain synonym families only as the head symbols of left-hand sides.  They assume one of the following two major forms:


1. **Family equality:** `co :: F t1..tn ~ t` or
1. **Variable equality:** `co :: x ~ t`, where we again distinguish two forms:

  1. **Variable-term equality:** `co :: x ~ t`, where `t` is *not* a variable, or
  1. **Variable-variable equality:** `co :: x ~ y`, where `x > y`.


where

- the types `t`, `t1`, ..., `tn` may not contain any occurrences of synonym families,
- the left-hand side of an equality may not occur in the right-hand side, and
- the relation `x > y` is an arbitrary, but total order on type variables.


The second bullet of the where clause is trivially true for equalities of Form (1); it also implies that the left- and right-hand sides are different.


Furthermore, we call a variable equality whose left-hand side is a flexible type variable (aka unification variable) a **flexible variable equality**, and correspondingly, a variable equality whose left-hand side is a rigid type variable (aka skolem variable) a **rigid variable equality.**

### Observations


The following is interesting to note:

- Normal equalities are similar to equalities meeting the Orientation Invariant and Flattening Invariant of new-single, but they are not the same.
- Normal equalities are **never** self-recursive.  They can be mutually recursive.  A mutually recursive group will exclusively contain variable equalities. 

### Coercions


Coercions `co` are either wanteds (represented by a flexible type variable) or givens *aka* locals (represented by a type term of kind CO).  In GHC, they are represented by `TcRnTypes.EqInstCo`, which is defined as

```wiki
type EqInstCo = Either 
                  TcTyVar    -- case for wanteds (variable to be filled with a witness)
		  Coercion   -- case for locals
```


Moreover, `TcTyFuns.RewriteInst` represents normal equalities, emphasising their role as rewrite rules. 

**SLPJ**: I propose that we use a proper data type, not `Either` for this.

## Normalisation


The following function `norm` turns an arbitrary equality into a set of normal equalities.

```wiki
data EqInst         -- arbitrary equalities
data FlatEqInst     -- synonym families may only occur outermost on the lhs
data RewriteInst    -- normal equality

norm :: EqInst -> [RewriteInst]
norm [[co :: F s1..sn ~ t]] = [[co' :: F s1'..sn' ~ t']] : eqs
  where
    (s1', c1, eqs1) = flatten s1
    ..
    (sn', cn, eqsn) = flatten sn
    (t', c, eqt)    = flatten t
    (co', eqs)      = adjust co (F c1..cn) (sym c) (eqs1++..++eqsn++eqt)
norm [[co :: t ~ F s1..sn]] = norm [[co' :: F s1..sn ~ t]] with co = sym co'
norm [[co :: s ~ t]] = check [[co' :: s' ~ t']] ++ eqs
  where
    (s', cs, eqs) = flatten s
    (t', ct, eqt) = flatten t
    (co', eqs)    = adjust co cs (sym ct) (eqs ++ eqt)

check :: FlatEqInst -> [RewriteInst]
-- Does OccursCheck + Decomp + Triv + Swap (of new-single)
check [[co :: t ~ t]] = [] with co = id
check [[co :: x ~ y]] 
  | x > y     = [[co :: x ~ y]]
  | otherwise = [[co' :: y ~ x]] with co = sym co'
check [[co :: x ~ t]] 
  | x `occursIn` t = fail
  | otherwise      = [[co :: x ~ t]]
check [[co :: t ~ x]] 
  | x `occursIn` t = fail
  | otherwise      = [[co' :: x ~ t]] with co = sym co'
check [[co :: s1 s2 ~ t1 t2]]
  = check [[col :: s1 ~ t1]] ++ check [[cor :: s2 ~ t2]] with co = col cor
check [[co :: T ~ S]] = fail

flatten :: Type -> (Type, Coercion, [RewriteInst])
-- Result type has no synonym families whatsoever
flatten [[F t1..tn]]
  = (alpha, [[F c1..cn |> gamma]], [[gamma :: F t1'..tn' ~ alpha]] : eqt1++..++eqtn)
  where
    (t1', c1, eqt1) = flatten t1
    ..
    (tn', cn, eqtn) = flatten tn
    FRESH alpha, gamma
    RECORD alpha := F t1..tn IF local equality
flatten [[t1 t2]] = ([[t1' t2']], [[c1 c2]], eqs++eqt)
  where
    (t1', c1, eqs) = flatten t1
    (t2', c2, eqt) = flatten t2
flatten t = (t, t, [])

adjust :: EqInstCo -> Coercion -> Coercion -> [RewriteInst] -> (EqInstCo, [RewriteInst])
-- Adjust coercions depending on whether we process a local or wanted equality
adjust co co1 co2 eqs@[[co1 :: s1 ~ t1, .., con :: sn ~ tn]]
  | isWanted co = (co_new, eqs) with co = co1 |> co_new |> co2
  | otherwise   = (co, [[id :: s1 ~ t1, .., id :: sn ~ tn]])
```


Notes:

- Perform Rule Triv & Decomp as part of normalisation.
- Whenever an equality of Form (2) or (3) would be recursive, the program can be rejected on the basis of a failed occurs check.  (Immediate rejection is always justified, as right-hand sides do not contain synonym familles; hence, any recursive occurrences of a left-hand side imply that the equality is unsatisfiable.)
- For all local equalities, we `RECORD` substitutions for fresh flexibles introduced by `flatten` by already updating the meta type variable.  However, the update will only have an effect after the Insts have been zonked; i.e., after finalisation.
- We drop all loopy equalities (see Section 8.2 of our ICFP'08 paper).  If we drop a wanted, it is a type error; if we drop a local, we emit a warning (as we only sacrifice completeness).
- We do the essentially same for class constraints; i.e., we use `flatten` to extract all family applications as equality constraints.  However, instead of adjusting the coercion at the flattened constraint, we generate a dictionary binding for the new dictionary (using a cast expression).

## Propagation (aka Solving)


Propagation is based on four rules that transform family and variable equalities.  The first rule transforms a family equality using a toplevel equality schema.  The other three use one equality to transform another.  In the presentation, the transforming equality is always first and the transformed is second, separate by an ampersand (`&`).



Below the rules are two scheme for applying the rules.  The first one naively iterates until the system doesn't change anymore.  The second scheme optimises the iteration somewhat.


### Rules


<table><tr><th><b>Top</b></th>
<td>

```wiki
co :: F t1..tn ~ t
=(Top)=>
co' :: [s1/x1, .., sm/xm]s ~ t with co = g s1..sm |> co'  
```

where <tt>g :: forall x1..xm. F u1..um ~ s</tt> and <tt>[s1/x1, .., sm/xm]u1 == t1</tt>.
</td></tr></table>


>
>
> NB: Afterwards, we need to normalise.  Then, any of the four propagation rules may apply.
>
>

<table><tr><th><b>SubstFam</b></th>
<td>

```wiki
co1 :: F t1..tn ~ t  &  co2 :: F t1..tn ~ s
=(SubstFam)=>
co1 :: F t1..tn ~ t  &  co2' :: t ~ s with co2 = co1 |> co2'
```

where <tt>co1</tt> may be a wanted only if <tt>co2</tt> is a wanted, too.
</td></tr></table>


>
>
> NB: Afterwards, we need to normalise `co2'`.  Then, the SubstVarVar or SubstVarFam rules may apply to the results of normalisation.  Moreover, `co1` may have a SubstFam or a SubstVarFam match with rules other than the results of normalising `co2'`.
>
>

<table><tr><th><b>SubstVarVar</b></th>
<td>

```wiki
co1 :: x ~ t  &  co2 :: x ~ s
=(SubstVarVar)=>
co1 :: x ~ t  &  co2' :: t ~ s with co2 = co1 |> co2'
```

where <tt>co1</tt> may be a wanted only if <tt>co2</tt> is a wanted, too.
</td></tr></table>


>
>
> NB: Afterwards, we need to normalise `co2'`.  Then, the SubstVarVar or SubstVarFam rules may apply to the results of normalisation, but not with `co1`, as `s` and `t` cannot contain `x` -- cf. the definition of normal equalities.  However, `co1` may have another SubstVarVar or SubstVarFam match with rules other than the results of normalising `co2'`.
>
>

<table><tr><th><b>SubstVarFam</b></th>
<td>

```wiki
co1 :: x ~ t  &  co2 :: F s1..sn ~ s
=(SubstVarFam)=>
co1 :: x ~ t  &  co2' :: [t/x](F s1..sn) ~ s with co2 = [co1/x](F s1..sn) |> co2'
```

where <tt>x</tt> occurs in <tt>F s1..sn</tt>, and <tt>co1</tt> may be a wanted only if <tt>co2</tt> is a wanted, too.
</td></tr></table>


>
>
> NB: No normalisation required.  Afterwards, SubstVarVar or SubstVarFam may apply to `co1` and all rules, except SubstVarVar, may apply to `co2'`.  However, SubstVarFam cannot again apply to `co1` and `co2'`, as `t` cannot contain `x` -- cf. the definition of normal equalities.
>
>


NB: In the three substitution rules, it is never necessary to try using `co1` with any of the equalities derived from `co2'`.  Hence, after having considered one equality as `co1` with every other equality, we can immediately put `co1` into the list of residual equalities.

### Rule application: specification


Propagation proceeds by applying any of the four rules until the system does not change anymore.  After application of a rule, the equalities that were modified need to be normalised again:

```wiki
Propagate = fix (Top | SubstFam | SubstVarVar | SubstVarFam)
```

### Rule application: algorithm


The four propagation rules are implemented by the following four functions:

```wiki
-- all four rule functions return Nothing if rule not applicable
applyTop         :: RewriteInst -> Maybe EqInst
applySubstFam    :: RewriteInst -> RewriteInst -> Maybe EqInst    -- return second argument...
applySubstVarVar :: RewriteInst -> RewriteInst -> Maybe EqInst    -- ...if it needs to go into...
applySubstVarFam :: RewriteInst -> RewriteInst -> Maybe EqInst    -- ...the todo list
```


For `applySubstFam`, `applySubstVarVar`, and `applySubstVarFam`, if the rule is not applicable to the arguments in the given order, *but it is applicable in the opposite order,* return the second argument as if it had been modified.  (As a result, it will get onto the todo list, and the equalities eventually be used in the opposite order.)


The following function gets a list with all local and wanted equalities.  It returns a list of residual equalities that permit no further rule application.

```wiki
propagate :: [RewriteInst] -> [RewriteInst]
propagate eqs = prop eqs []
  where
    prop :: [RewriteInst]  -- todo list (still need to try these equalities)
         -> [RewriteInst]  -- residual list (tried all equalities here already pairwise)
         -> [RewriteInst]  -- these permit no further rule application
    prop []       res = res
    prop (eq:eqs) res = apply eq eqs res

    apply eq eqs res 
      | Just eq' <- applyTop eq
      = prop (norm eq' ++ eqs) res
      | otherwise
      = let (new_eqs, unchanged_eqs) = mapAndUnzip (applySubstRules eq) eqs
            (new_res, unchanged_res) = mapAndUnzip (applySubstRules eq) res
        in prop (concat new_eqs ++ concat new_res ++ concat unchanged_eqs)
                (eq : concat unchanged_res)

  applySubstRules eq1 eq2
    | Just eq2' <- applySubstFam eq1 eq2    = (norm eq2', [eq1])
    | Just eq2' <- applySubstVarVar e1 eq2  = (norm eq2', [eq1])
    | Just eq2' <- applySubstVarFam eq1 eq2 = ([eq2'], [eq1])
    | otherwise                             = ([], [eq1, eq2])
```

### Observations

- Only SubstVarFam when replacing a variable in a family equality can  lead to recursion with (Top).
- A significant difference to `new-single` is that solving is a purely local operation.  We never instantiate any flexible variables.
- We cannot discard any variable equalities after substitution since we do not substitute into right-hand sides anymore.  Moreover, in the concrete implementation, variables may also be re-introduced due to simplification of type classes (which currently doesn't happen in the same iteration).

## Finalisation



The finalisation step instantiates as many flexible type variables as possible, but it takes care not to instantiate variables occurring in the global environment with types containing synonym family applications.  This is important to obtain principle types (c.f., Andrew Kennedy's thesis).  We perform finalisation in two steps:


1.  **Substitution:** 

  - **Step A:** For any (local or wanted) variable equality of the form `co :: x ~ t`, we apply the substitution `[t/x]` to the **right-hand side** of all equalities (wanteds only to wanteds).  We also perform the same substitution on class constraints (again, wanteds only to wanteds).
  - **Step B:** We have two cases:

    - *In checking mode,* for any wanted family equality of the form `co :: F t1..tn ~ alpha`, where `alpha` is a skolem flexible, we apply the substitution `[F t1..tn/alpha]` to the right-hand side of all wanted variable equalities and to both sides of all wanted family equalities.
    - *In inference mode,* we proceed as in checking mode, but we do not substitute into variable equalities.
  - **Step C:** Same as Step B, but `alpha` is not a skolem flexible.

>
>
> At this point all variables bound in the next step have disappeared from the constraint set; it is as if the variables have been locally instantiated.
>
>

1. **Instantiation:** For any variable equality of the form `co :: alpha ~ t` or `co :: a ~ alpha`, where `co` is wanted, we instantiate `alpha` with `t` or `a`, respectively, and set `co := id`.  Moreover, we have to do the same for equalities of the form `co :: F t1..tn ~ alpha` unless we are in inference mode and `alpha` appears in the environment or is a local skolem flexible that is propagated into the environment by another binding.


Important points are the following:

- The substitution step can lead to recursive equalities; i.e., we need to apply an occurs check after each substitution.  
- We perform substitutions in two steps due to situations as ` F s ~ alpha, alpha ~ t`.  Here, we want to substitute `alpha ~ t` first as `alpha` may occur in class dictionaries where a rigid type may help to select a class instance.
- We need to substitute all flexibles that arose as skolems during flattening of wanteds *before* we substitute any other flexibles.  Consider `F delta ~ alpha, F alpha ~ delta`, where `alpha` is a skolem and `delta` a free flexible.  We need to produce `F (F delta) ~ delta` (and not `F (F alpha) ~ alpha`).  Otherwise, we may wrongly claim to having performed an improvement, which can lead to non-termination of the combined class-family solver ??? this is the reason for separating Step B and Step C.
- We need to substitute family equalities into both sides of family equalities; consider, `F t1..tn ~ alpha, G s1..sm ~ alpha`.
- We must not substitute family equalities into right-hand sides of variable equalities.  (If the variable equality directly or indirectly instantiates a flexible that is free in the environment, we would instantiate it with a family application, which we set out to avoid.)
- There is no need to substitute family equalities into dictionaries as they cannot influence instance selection.


Note that it is an important property of propagation that we need to substitute variable equalities only into right-hand sides during finalisation.  After finalisation and zonking all flattening of locals is undone (c.f., note below the flattening code above).

## Examples

### Unflattening locals and finalisation


This seems ok:

```wiki
c :: a ~ [F b] |- gamma :: alpha ~ a
=(norm)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma :: alpha ~ a
  with beta := F b
=(final)=>
alpha := a, gamma := id
```


The problem becomes obvious when we orient the wanted equality the other way around:

```wiki
c :: a ~ [F b] |- gamma :: a ~ alpha
=(norm)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma :: a ~ alpha
=(SubstVarVar)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma' :: [beta] ~ alpha
  with gamma := c |> gamma'
=(norm)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma'' :: alpha ~ [beta]
  with gamma' := sym gamma''
=(final)=>
alpha := [F b], gamma'' := id
```


This result is fine, even when considering Andrew Kennedy's concerns, as we are necessarily in checking mode (following the `normalised_equation_algorithm` terminology).


Let's assume one toplevel equality `forall x. g x :: F x = ()`:

```wiki
c :: a ~ [F b] |- gamma :: a ~ alpha
=(norm)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma :: a ~ alpha
  with beta := F b
=(SubstVarVar)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma' :: [beta] ~ alpha
  with gamma := c |> gamma'
=(norm)=>
c :: a ~ [beta], id :: F b ~ beta |- gamma'' :: alpha ~ [beta]
  with gamma' := sym gamma''
=(Top)=>
c :: a ~ [beta], sym (g b) :: () ~ beta |- gamma'' :: alpha ~ [beta]
=(norm)=>
c :: a ~ [beta], g b :: beta ~ () |- gamma'' :: alpha ~ [beta]
=(final)=>
alpha := [()], gamma'' := id
```

**NB:** The algorithm in the `normalisied_equalities_algorithm` paper (as opposed to the on this wiki page) will compute `alpha := [F b]`, which is equivalent, but less normalised.

---

**TODO**  Still need to adapt the following examples to new rule set!

### Substituting wanted family equalities with SubstFam is crucial if the right-hand side contains a flexible type variable

```wiki
Top: F Int ~ [Int]

  |- F delta ~ [delta], F delta ~ [Int]
(SubstFam)
  |- F delta ~ [delta], norm [[[delta] ~ [Int] ]]
==>
  |- F delta ~ [delta], delta ~ Int
(SubstVar)
  |- norm [[F Int ~ [Int] ]], delta ~ Int
==>
  |- F Int ~ [Int], delta ~ Int
(Top)
  |- norm [[[Int] ~ [Int] ]], delta ~ Int
==>
  |- delta ~ Int
QED
```

### Interaction between local and wanted family equalities


Example 4 of Page 9 of the ICFP'09 paper.

```wiki
  F [Int] ~ F (G Int)  |-  G Int ~ [Int], H (F [Int]) ~ Bool
=(norm)=>
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  G Int ~ [Int], H x ~ Bool, F [Int] ~ x
(SubstFam w/ F [Int])
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  G Int ~ [Int], H x ~ Bool, x ~ a
(SubstFam w/ G Int)
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  b ~ [Int], H x ~ Bool, x ~ a
(SubstVar w/ x)
  F [Int] ~ a, F b ~ a, G Int ~ b
  |-
  b ~ [Int], H a ~ Bool, x ~ a
```

**TODO** If we use flexible variables for the flattening of the wanteds, too, the equality corresponding to `x ~ a` above will be oriented the other way around.  That can be a problem because of the asymmetry of the SubstVar and SubstFun rules (i.e., wanted equalities are not substituted into locals).

## Termination

### SkolemOccurs


The Note \[skolemOccurs loop\] in the old code explains that equalities of the form `x ~ t` (where `x` is a flexible type variable) may not be used as rewrite rules, but only be solved by applying Rule Unify.  As Unify carefully avoids cycles, this prevents the use of equalities introduced by the Rule SkolemOccurs as rewrite rules.  For this to work, SkolemOccurs also had to apply to equalities of the form `a ~ t[[a]]`.  This was a somewhat intricate set up that we seek to simplify here.  Whether equalities of the form `x ~ t` are used as rewrite rules or solved by Unify doesn't matter anymore.  Instead, we disallow recursive equalities after normalisation completely (both locals and wanteds).  This is possible as right-hand sides are free of synonym families.


To look at this in more detail, let's consider the following notorious example:

```wiki
E_t: forall x. F [x] ~ [F x]
[F v] ~ v  ||-  [F v] ~ v
```

**New-single**: The following derivation shows how the algorithm in new-single fails to terminate for this example.

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], F v ~ x
a := F v
==> (Local) with v
F [a] ~ a  ||-  [a] ~ [x], F [a] ~ x
==> normalise
F [a] ~ a  ||-  x ~ a, F[a] ~ x
==> 2x (Top) & Unify
[F a] ~ a  ||-  [F a] ~ a
..and so on..
```

**New-single using flexible tyvars to flatten locals, but w/o Rule (Local) for flexible type variables**: With (SkolemOccurs) it is crucial to avoid using Rule (Local) with flexible type variables.  We can achieve a similar effect with new-single if we (a) use flexible type variables to flatten local equalities and (b) at the same time do not use Rule (Local) for variable equalities with flexible type variables.  NB: Point (b) was necessary for the ICFP'08 algorithm, too.

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [x2], F v ~ x2  ||-  v ~ [x1], F v ~ x1
** x2 := F v
==> (Local) with v
F [x2] ~ x2  ||-  [x2] ~ [x1], F [x2] ~ x1
** x2 := F v
==> normalise
F [x2] ~ x2  ||-  x2 ~ x1, F [x2] ~ x1
** x2 := F v
==> 2x (Top) & Unify
[F x1] ~ x1  ||-  [F x1] ~ x1
** x1 := F v
==> normalise
x1 ~ [y2], F x1 ~ y2  ||-  x1 ~ [y1], F x1 ~ y1
** x1 := F v, y2 := F x1
..we stop here if (Local) doesn't apply to flexible tyvars
```


A serious disadvantage of this approach is that we **do** want to use Rule (Local) with flexible type variables as soon as we have rank-n signatures.  In fact, the lack of doing so is responsible for a few failing tests in the testsuite in the GHC implementation of (SkolemOccurs).

**De-prioritise Rule (Local)**: Instead of outright forbidding the use of Rule (Local) with flexible type variables, we can simply require that Local is only used if no other rule is applicable.  (That has the same effect on satisfiable queries, and in particular, the present example.)

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], F v ~ x
a := F v
==> (IdenticalLHS) with v & F v
v ~ [a], F v ~ a  ||- [a] ~ [x], x ~ a
==> normalise
v ~ [a], F v ~ a  ||-  x ~ a, x ~ a
==> (Unify)
v ~ [a], F v ~ a  ||-  a ~ a
==> normalise
v ~ [a], F v ~ a ||-
QED
```


In fact, it is sufficient to de-prioritise Rule (Local) for variable equalities (if it is used for other equalities at all):

```wiki
[F v] ~ v  ||-  [F v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], F v ~ x
a := F v
==> (Local) with F v
v ~ [a], F v ~ a  ||-  v ~ [x], x ~ a
==> (Unify)
v ~ [a], F v ~ a  ||-  v ~ [a]
==> (Local) with v
v ~ [a], F [a] ~ a ||- [a] ~ [a]
==> normalise
v ~ [a], F [a] ~ a ||-
QED
```

**One problems remains**: The algorithm still fails to terminate for unsatisfiable queries.

```wiki
[F v] ~ v  ||-  [G v] ~ v
==> normalise
v ~ [a], F v ~ a  ||-  v ~ [x], G v ~ x
a := F v
==> (Local) with v
F [a] ~ a  ||-  [a] ~ [x], G [a] ~ x
==> normalise
F [a] ~ a  ||-  x ~ a, G [a] ~ x
==> (Unify)
F [a] ~ a  ||-  G [a] ~ a
==> (Top)
[F a] ~ a  ||-  G [a] ~ a
==> normalise
a ~ [b], F a ~ b  ||-  G [a] ~ a
b := F a
..and so on..
```


My guess is that the algorithm terminates for all satisfiable queries.  If that is correct, the entailment problem that the algorithm solves would be  semi-decidable.

**Derivation with latest (and implemented) algorithm**:

```wiki
Top:
  forall x. F x ~ [F x]

F [a] ~ a |-
=(norm)=>
F [a] ~ a |-
=(Top)=>
[F a] ~ a |-
=(norm)=>
a ~ [beta], F a ~ beta |-
  with beta := F a
=(SubtVarFam)=>
a ~ [beta], F [beta] ~ beta |-
...and so on...
```


The only solution seems to be to give up on completeness and throw away *loopy equalities* as proposed in the ICFP'08 paper.
