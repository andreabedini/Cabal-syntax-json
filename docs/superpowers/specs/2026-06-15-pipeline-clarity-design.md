# Pipeline clarity refactor + correctness cleanup

**Date:** 2026-06-15
**Status:** design — awaiting approval
**Scope:** internal clarity + documentation. **Output is frozen**: every existing golden test must stay green, byte-for-byte, as the regression oracle.

## Problem

`cabal2json` transforms a `GenericPackageDescription` into JSON through a sequence of
representations, but that sequence is invisible in the code:

- The semantic core — `convertCondTree → pushConditionals → flattenCondTree → defragment` —
  lives in `process` in `app/Main.hs`, written as nested `fmap`s with hand-annotated
  intermediate bindings (`components1..components4`). It is not in the library, so it
  can't be reused and isn't named.
- Two unrelated functions are both called `simplifyCondTree` (one in `Simplify`, on
  Cabal's `CondTree`; one in `CondTree`, on our `CondTree`), which obscures which is live.
- `--pristine` and the default emit *intentionally* different conditional shapes, but the
  README documents only the default and never mentions `--pristine` or `--debug`.

The author arrived at the current pipeline by intuition, fixing issue over issue, and
wants the intended semantics made evident — and verified.

## Goals

1. Make the pipeline's five representations explicit, named, and documented in the library.
2. Verify each step is correct and necessary; remove cruft.
3. Document the output contract (both modes, and the conditional encoding) in the README.

## Non-goals

- **No output changes.** Both `--pristine` and default output stay byte-identical.
- Not removing `--pristine` (it is a meaningful distinct view — see below).
- Not switching `FieldMap` to `Data.Map`: it would reorder fields alphabetically and break
  the frozen output. `ListMap`'s insertion-order semantics are load-bearing.
- No new schema file, Hackage release, or CI matrix work (tracked separately).

## The five representations

`--pristine` and default are simply *which representation is rendered*. The transform first
turns Cabal's *tree of fields* inside-out into *fields of trees*, then flattens.

| Stage | Type | Shape |
|------:|------|-------|
| 0 | `C.CondTree ConfVar [Dependency] (FieldMap (Fragment Json))` | tree-of-fields, Cabal-native — **`--pristine` renders here** |
| 1 | `CondTree ConfVar (FieldMap (Fragment Json))` | tree-of-fields, our uniform repr (after `convertCondTree`) |
| 2 | `FieldMap (CondTree ConfVar (Fragment Json))` | fields-of-trees (after `pushConditionals`) |
| 3 | `FieldMap (NonEmpty (Guarded ConfVar (Fragment Json)))` | fields of cumulative-guarded values (after `flattenCondTree`) |
| 4 | `FieldMap (Fragment Json)` | merged JSON fragments — **default renders here** (after `defragment`) |

Note stage 0 has whole `FieldMap`s at the leaves with one condition per branch (conjunctions
nest), whereas stage 4 has conditions pushed inside each field with cumulative guards
(`{_if, _then}`, `else` folded into `not`). The shapes differ by design.

## Correctness review (findings)

Verified correct, resting on two load-bearing instances:

- `ListMap`'s `Semigroup` is `unionWith (<>)` (`ListMap.hs:74`), **not** left-biased — so a
  field appearing in several conditional branches is *merged*, not overwritten. Keystone of
  `pushConditionals`.
- `ListMap.align` (`ListMap.hs:55`) is a correct order-preserving align — drives the
  then-only / else-only / both split in `pushConditionals`.
- Cumulative guards use `cAnd`/`cNot` smart constructors, so `Lit True ∧ x = x`; `defragment`
  correctly treats a `Lit True` guard as unconditional.

Every step does real work (per-field trees remain genuinely nested after `pushConditionals`,
so `flattenCondTree` is not a no-op). `convertCondTree`+`pushConditionals` and
`flattenCondTree`+`defragment` *could* be fused, but fusing hides the representations we are
trying to surface, so they stay as named stages.

## Changes

All output-preserving. Golden tests must remain unchanged and green throughout.

### 1. New `Cabal.Syntax.Pipeline` module

Owns the narrative. Provisional names (final names to be settled in the plan):

```haskell
type CabalComponentTree = C.CondTree ConfVar [Dependency] (FieldMap (Fragment Json)) -- stage 0
type ComponentTree      = CondTree ConfVar (FieldMap (Fragment Json))                 -- stage 1
type FieldCondMap       = FieldMap (CondTree ConfVar (Fragment Json))                 -- stage 2
type GuardedFieldMap    = FieldMap (NonEmpty (Guarded ConfVar (Fragment Json)))       -- stage 3
type JsonFieldMap       = FieldMap (Fragment Json)                                    -- stage 4

-- per component; each (.) step has Haddock quoting the matching README sentence
processComponent :: CabalComponentTree -> JsonFieldMap
processComponent =
      fmap defragment       -- stage 4
    . fmap flattenCondTree  -- stage 3
    . pushConditionals      -- stage 2
    . convertCondTree       -- stage 1

-- the two render points, replacing the if/else in Main:
toDefault  :: GPD (Fragment Json) CabalComponentTree -> GPD (Fragment Json) JsonFieldMap
toPristine :: -- render stage 0 directly
```

`process` and the format helpers move out of `app/Main.hs`; `Main` shrinks to argument
parsing + IO calling `toDefault` / `toPristine`.

### 2. Correctness cleanup

- **Delete** the dead `Cabal.Syntax.CondTree.simplifyCondTree` and its export (the live one
  is `Simplify.simplifyCondTree`, a different function — the name collision is removed).
- **Drop** the unused `Semigroup a` constraint on `flattenCondTree` (`CondTree.hs:165`); it is
  never used and misleadingly implies values merge during flatten.
- **Document** that `convertCondTree` intentionally discards `condTreeConstraints` (the
  `[Dependency]` aggregate is redundant, not source data).

### 3. Capture the "grammar emits no duplicate field names" invariant (merge-on-duplicate)

- Strengthen `ListMap.fromListWith` from adjacent-only (`NE.groupBy`) to a **full,
  order-preserving** merge: first occurrence fixes position, all values for a key folded in
  order. `fromListWith` is currently unused, so this is risk-free.
- Switch the two **field-name** boundaries from `fromList` to `fromListWith (<>)`:
  package metadata (`GenericPackageDescription.hs:118`) and each component
  (`:140-152`). A hypothetical duplicate field then merges (consistent with the rest of the
  pipeline) instead of silently keeping the last. Zero duplicates today ⇒ byte-identical output.
- Leave the component-map boundary (`:138`, keyed by component name) unchanged — that is a
  separate invariant.

### 4. Documentation

- README: add a section documenting the **two output modes** (default = stage 4, pristine =
  stage 0), the five representations, and the conditional JSON encoding — `_if` / `_then` /
  `_else`, when `_else` appears, and the condition vocabulary (`and` / `or` / `not`, and the
  `os` / `arch` / `flag` / `impl` variables).
- Haddock on each pipeline stage tying it to the README narrative; a one-line note on the
  `ListMap.fromList` adjacent-dedup behavior.

## Verification

- The golden suite (`cabal run -- golden-tests`) is the oracle: **no `.json` golden may
  change**. Any diff means an accidental output change and is a defect in the refactor.
- Build clean under `-Wall` (removing the unused constraint and dead function should reduce,
  not add, warnings).
- Sanity-check that `--pristine` output is also unchanged (it shares the moved code paths).

## Risks

- **Accidental reordering** at the `fromList → fromListWith` switch. Mitigated: the new merge
  preserves first-occurrence order, and goldens would catch any drift.
- **Name bikeshedding** for the stage aliases — deferred to the implementation plan.
