# Pipeline Clarity Refactor Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the five-stage cabal→JSON transformation an explicit, documented library pipeline, clean up accreted cruft, and capture the field-name uniqueness invariant — all without changing any output.

**Architecture:** Extract the per-component transformation chain out of `app/Main.hs` into a new `Cabal.Syntax.Pipeline` module that names each representation (stages 0–4). Strengthen `ListMap.fromListWith` to a full order-preserving merge and route the two field-name boundaries through it. Remove dead code (`CondTree.simplifyCondTree`) and an unused constraint. The default JSON output is guarded by the existing golden suite; `--pristine`/`--debug` are guarded by captured baselines.

**Tech Stack:** Haskell (GHC2021), `Cabal`/`Cabal-syntax`, `tasty`/`tasty-golden`, `cabal`.

**Branch:** `refactor/pipeline-clarity` (already created; hooks removed by the user, so `git commit` runs clean).

---

## File structure

- **Create** `src/Cabal/Syntax/Pipeline.hs` — owns the five stage-type aliases and the `processComponent`/`process` pipeline.
- **Modify** `src/Cabal/Syntax/ListMap.hs` — strengthen `fromListWith` to a full, order-preserving merge; add internal `insertWith`.
- **Modify** `src/Cabal/Syntax/GenericPackageDescription.hs` — route both field-name boundaries through `fromListWith (<>)`.
- **Modify** `src/Cabal/Syntax/CondTree.hs` — delete dead `simplifyCondTree`; drop unused `Semigroup a` on `flattenCondTree`; document `convertCondTree`'s dropped constraints.
- **Modify** `app/Main.hs` — delete the local `process`; import it from `Pipeline`; trim now-unused imports.
- **Modify** `Cabal-syntax-json.cabal` — add `Cabal.Syntax.Pipeline` to `exposed-modules`.
- **Modify** `tests/Tests.hs` — add a `fromListWith` unit test.
- **Modify** `README.md` — document the two output modes and the conditional JSON encoding.

**Invariant for every task:** `cabal run -- golden-tests` stays fully green and **no `tests/golden/*.json` file changes**. Any golden diff is a defect in the refactor, not an expected update.

---

### Task 1: Capture baselines for `--pristine` and `--debug`

The golden suite only exercises **default** JSON output (see `tests/Tests.hs` — no `--pristine`/`--debug` args). Capture current output for those two modes so later tasks can prove they're unchanged.

**Files:** none (throwaway baselines under `$TMPDIR`).

- [ ] **Step 1: Build the current binary**

Run: `cabal build`
Expected: builds successfully.

- [ ] **Step 2: Capture baselines**

```bash
BIN=$(cabal list-bin cabal2json)
mkdir -p "$TMPDIR/cabal2json-baseline"
for p in text filepath bytestring unix ghc-bignum; do
  "$BIN"            tests/golden/$p.cabal > "$TMPDIR/cabal2json-baseline/$p.default.json"
  "$BIN" --pristine tests/golden/$p.cabal > "$TMPDIR/cabal2json-baseline/$p.pristine.json"
  "$BIN" --debug    tests/golden/$p.cabal > "$TMPDIR/cabal2json-baseline/$p.debug.txt"
done
ls "$TMPDIR/cabal2json-baseline"
```
Expected: 15 files listed (5 packages × 3 modes). No commit — these are throwaway oracles used by Task 9.

---

### Task 2: Strengthen `ListMap.fromListWith` to a full, order-preserving merge

Currently `fromListWith` only merges *adjacent* equal keys (`NE.groupBy`). Make it merge all duplicates regardless of position, preserving the first occurrence's position. It is currently unused, so this is risk-free.

**Files:**
- Modify: `src/Cabal/Syntax/ListMap.hs`
- Test: `tests/Tests.hs`

- [ ] **Step 1: Write the failing test**

In `tests/Tests.hs`, add this test and wire it into `unitTests`:

```haskell
testFromListWith :: TestTree
testFromListWith =
    testCase "fromListWith merges non-adjacent duplicate keys, preserving first-occurrence order" $
        assertEqual
            "assert"
            (ListMap.fromList [("a", [1, 3]), ("b", [2 :: Int])])
            (ListMap.fromListWith (<>) [("a", [1]), ("b", [2]), ("a", [3])])
```

Add `testFromListWith` to the `unitTests` list (e.g. as the last element after the `testAlign` entries):

```haskell
unitTests =
    testGroup
        "unit tests"
        [ -- ... existing testAlign entries unchanged ...
        , testFromListWith
        ]
```

- [ ] **Step 2: Run the test to verify it fails**

Run: `cabal run -- golden-tests -p "fromListWith"`
Expected: FAIL — the current adjacent-only implementation returns
`ListMap [("a",[1]),("b",[2]),("a",[3])]`, not the merged `ListMap [("a",[1,3]),("b",[2])]`.

- [ ] **Step 3: Rewrite `fromListWith` and add `insertWith`**

In `src/Cabal/Syntax/ListMap.hs`, replace the existing `fromListWith` definition:

```haskell
fromListWith :: (Semigroup v, Eq k) => (v -> v -> v) -> [(k, v)] -> ListMap k v
fromListWith f =
    ListMap
        . map (\ne -> (fst (NE.head ne), foldl1' f (NE.map snd ne)))
        . NE.groupBy ((==) `on` fst)
```

with:

```haskell
fromListWith :: Eq k => (v -> v -> v) -> [(k, v)] -> ListMap k v
fromListWith f = foldl' (\acc (k, v) -> insertWith f k v acc) empty

-- | Insert a key/value, merging with any existing value for the key (existing
-- value on the left of @f@) and preserving the position of the key's first
-- occurrence. Appends if the key is absent.
insertWith :: Eq k => (v -> v -> v) -> k -> v -> ListMap k v -> ListMap k v
insertWith f k new (ListMap kvs)
    | any ((k ==) . fst) kvs =
        ListMap [(k', if k' == k then f old new else old) | (k', old) <- kvs]
    | otherwise =
        ListMap (kvs ++ [(k, new)])
```

- [ ] **Step 4: Fix imports in `ListMap.hs`**

`foldl1'` is no longer used; `foldl'` is now needed. Change the import line:

```haskell
import Data.List (partition)
```
to:
```haskell
import Data.List (foldl', partition)
```

and delete this line entirely (it provided `foldl1'`, now unused):
```haskell
import Data.Foldable1 (foldl1')
```

(`on` and `Data.List.NonEmpty qualified as NE` stay — `fromList` still uses them.)

- [ ] **Step 5: Run the test to verify it passes**

Run: `cabal run -- golden-tests -p "fromListWith"`
Expected: PASS.

- [ ] **Step 6: Run the full suite (no golden drift)**

Run: `cabal run -- golden-tests`
Expected: all tests pass; no golden files modified (`git status --short tests/golden` is empty).

- [ ] **Step 7: Commit**

```bash
git add src/Cabal/Syntax/ListMap.hs tests/Tests.hs
git commit -m "refactor: make ListMap.fromListWith a full order-preserving merge"
```

---

### Task 3: Route both field-name boundaries through `fromListWith (<>)`

Capture the "grammar emits no duplicate field names" invariant as merge-on-duplicate. Output stays byte-identical (no duplicates exist today; every value remains a `ScalarFragment` at the metadata boundary).

**Files:**
- Modify: `src/Cabal/Syntax/GenericPackageDescription.hs`

- [ ] **Step 1: Switch the metadata boundary**

Replace the `meta` binding (currently `GenericPackageDescription.hs:115-135`):

```haskell
    meta :: ListMap String (Fragment Json)
    meta =
        fmap ScalarFragment $
            ListMap.fromList $
                mconcat
                    [ fmap (fmap toJSON) $
                        jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
                    , [ ( "custom-setup"
                        , jsonFieldGrammar' v (setupBInfoFieldGrammar False) sbi
                        )
                      | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                      ]
                    , [ ("source-repositories", sourceRepos2json v repos)
                      | let repos = sourceRepos (packageDescription gpd)
                      , not (null repos)
                      ]
                    , [ ("flags", flags2json v flags)
                      | let flags = genPackageFlags gpd
                      , not (null flags)
                      ]
                    ]
```

with (note `ScalarFragment` moved inside each entry, so values are `Fragment Json` before the merge):

```haskell
    meta :: ListMap String (Fragment Json)
    meta =
        ListMap.fromListWith (<>) $
            mconcat
                [ fmap (fmap (ScalarFragment . toJSON)) $
                    jsonFieldGrammar v packageDescriptionFieldGrammar (packageDescription gpd)
                , [ ( "custom-setup"
                    , ScalarFragment (jsonFieldGrammar' v (setupBInfoFieldGrammar False) sbi)
                    )
                  | sbi <- maybeToList (setupBuildInfo (packageDescription gpd))
                  ]
                , [ ("source-repositories", ScalarFragment (sourceRepos2json v repos))
                  | let repos = sourceRepos (packageDescription gpd)
                  , not (null repos)
                  ]
                , [ ("flags", ScalarFragment (flags2json v flags))
                  | let flags = genPackageFlags gpd
                  , not (null flags)
                  ]
                ]
```

- [ ] **Step 2: Switch the per-component boundary**

In the `components` binding (`GenericPackageDescription.hs:140-152`), change each of the five `ListMap.fromList` occurrences that build a `FieldMap` to `ListMap.fromListWith (<>)`. For example:

```haskell
[ (CLibName n, fmap (FieldMap . ListMap.fromList . jsonFieldGrammar v (libraryFieldGrammar n)) c)
| (n, c) <- libs
]
```
becomes:
```haskell
[ (CLibName n, fmap (FieldMap . ListMap.fromListWith (<>) . jsonFieldGrammar v (libraryFieldGrammar n)) c)
| (n, c) <- libs
]
```

Apply the identical `fromList` → `fromListWith (<>)` change to the `CFLibName`, `CExeName`, `CTestName`, and `CBenchName` lines too.

**Do NOT change** the outer `components = ListMap.fromList $ mconcat [...]` (`:138`) — that map is keyed by component name, a separate invariant.

- [ ] **Step 3: Build**

Run: `cabal build`
Expected: builds with no new warnings. (`Fragment (..)` is already imported, so `ScalarFragment` is in scope.)

- [ ] **Step 4: Verify no output drift (default + baselines)**

```bash
cabal run -- golden-tests
BIN=$(cabal list-bin cabal2json)
for p in text filepath bytestring unix ghc-bignum; do
  diff <("$BIN" --pristine tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.pristine.json" && echo "pristine $p OK"
  diff <("$BIN" --debug    tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.debug.txt"  && echo "debug $p OK"
done
git status --short tests/golden
```
Expected: golden suite passes; every `diff` prints nothing (only the `... OK` lines); `git status` shows no golden changes.

- [ ] **Step 5: Commit**

```bash
git add src/Cabal/Syntax/GenericPackageDescription.hs
git commit -m "refactor: route field-name boundaries through fromListWith (<>)"
```

---

### Task 4: Drop the unused `Semigroup a` constraint on `flattenCondTree`

**Files:**
- Modify: `src/Cabal/Syntax/CondTree.hs`

- [ ] **Step 1: Remove the constraint**

In `src/Cabal/Syntax/CondTree.hs`, change the signature (`:163-167`):

```haskell
flattenCondTree
    :: forall v a
     . Semigroup a
    => CondTree v a
    -> NonEmpty (Guarded v a)
```
to:
```haskell
flattenCondTree
    :: forall v a
     . CondTree v a
    -> NonEmpty (Guarded v a)
```

Leave the body unchanged (it only uses `NonEmpty`'s `Semigroup`, never `<>` on `a`).

- [ ] **Step 2: Build and verify no drift**

Run: `cabal build && cabal run -- golden-tests`
Expected: builds (no new warnings), all golden tests pass, no golden files changed.

- [ ] **Step 3: Commit**

```bash
git add src/Cabal/Syntax/CondTree.hs
git commit -m "refactor: drop unused Semigroup constraint from flattenCondTree"
```

---

### Task 5: Delete the dead `CondTree.simplifyCondTree`

This function is exported but never called (the live one is `Simplify.simplifyCondTree`, a different function on Cabal's `CondTree`). Removing the name collision is the point.

**Files:**
- Modify: `src/Cabal/Syntax/CondTree.hs`

- [ ] **Step 1: Remove from the export list**

In the module header, delete the `, simplifyCondTree` line (`:16`) from the `-- ** Transformations` export group.

- [ ] **Step 2: Delete the function**

Delete the entire `simplifyCondTree` definition (`CondTree.hs:109-139`), i.e. the Haddock comment plus:

```haskell
simplifyCondTree
    :: forall v a
     . Monoid a
    => (v -> Either v Bool)
    -> CondTree v a
    -> Maybe (CondTree v a)
simplifyCondTree eval (CondTree nodes) =
    CondTree <$> NE.nonEmpty (foldMap go nodes)
  where
    go :: CondNode v a -> [CondNode v a]
    go (CondNode a) =
        [CondNode a]
    go (CondIfThen c (CondTree ts)) =
        case fst (simplifyCondition c eval) of
            Lit True ->
                NE.toList ts
            Lit False -> []
            c' -> case NE.nonEmpty (foldMap go ts) of
                Nothing -> []
                Just ts' -> [CondIfThen c' (CondTree ts')]
    go (CondIfThenElse c (CondTree ts) (CondTree es)) =
        case fst (simplifyCondition c eval) of
            Lit True ->
                NE.toList ts
            Lit False ->
                NE.toList es
            c' -> case NE.nonEmpty (foldMap go ts <> foldMap go es) of
                Nothing -> []
                Just ts' -> [CondIfThen c' (CondTree ts')]
```

- [ ] **Step 3: Remove the now-unused `simplifyCondition` import**

Change (`:29`):
```haskell
import Distribution.Types.Condition (Condition (..), cAnd, simplifyCondition)
```
to:
```haskell
import Distribution.Types.Condition (Condition (..), cAnd)
```

(`cAnd` and `Condition (..)` are still used by `flattenCondTree`/`defragment`.)

- [ ] **Step 4: Build with `-Wall` and verify no drift**

Run: `cabal build && cabal run -- golden-tests`
Expected: builds with no unused-import or unused-binding warnings; all golden tests pass; no golden files changed.

- [ ] **Step 5: Commit**

```bash
git add src/Cabal/Syntax/CondTree.hs
git commit -m "refactor: remove dead simplifyCondTree from CondTree"
```

---

### Task 6: Document `convertCondTree`'s intentionally-dropped constraints

**Files:**
- Modify: `src/Cabal/Syntax/CondTree.hs`

- [ ] **Step 1: Expand the Haddock**

Replace the existing comment above `convertCondTree` (`:141`):

```haskell
-- | Convert 'Distribution.Types.CondTree.CondTree' from Cabal-syntax into our 'CondTree'.
convertCondTree :: C.CondTree v c a -> CondTree v a
```
with:
```haskell
-- | Convert 'Distribution.Types.CondTree.CondTree' from Cabal-syntax into our 'CondTree'.
--
-- Cabal's node bundles a value, an aggregated constraint set (@c@, the collected
-- @[Dependency]@), and its conditional branches. We deliberately discard the
-- constraint set (the @_@ below): it is a redundant aggregation derivable from the
-- build-depends fields, not source data, and plays no part in the JSON output.
convertCondTree :: C.CondTree v c a -> CondTree v a
```

- [ ] **Step 2: Build**

Run: `cabal build`
Expected: builds, no warnings.

- [ ] **Step 3: Commit**

```bash
git add src/Cabal/Syntax/CondTree.hs
git commit -m "docs: explain dropped condTreeConstraints in convertCondTree"
```

---

### Task 7: Extract the pipeline into `Cabal.Syntax.Pipeline`

Move the transformation out of `app/Main.hs` into a named library module with explicit stage types.

**Files:**
- Create: `src/Cabal/Syntax/Pipeline.hs`
- Modify: `Cabal-syntax-json.cabal`
- Modify: `app/Main.hs`

- [ ] **Step 1: Create the module**

Create `src/Cabal/Syntax/Pipeline.hs` with exactly:

```haskell
-- | The cabal→JSON transformation, expressed as five named representations.
--
-- @--pristine@ renders stage 0 (Cabal's native conditional tree); the default
-- output renders stage 4. The README \"Details\" section narrates these steps.
module Cabal.Syntax.Pipeline
    ( -- * The five representations
      ComponentTreeCabal
    , ComponentTree
    , FieldTrees
    , GuardedFields
    , JsonFields

      -- * The default pipeline
    , processComponent
    , process
    ) where

import Data.List.NonEmpty (NonEmpty)

import Distribution.Types.CondTree qualified as C (CondTree)
import Distribution.Types.ConfVar (ConfVar)
import Distribution.Types.Dependency (Dependency)
import Distribution.Utils.Json (Json)

import Cabal.Syntax.CondTree
    ( CondTree
    , Guarded
    , convertCondTree
    , defragment
    , flattenCondTree
    , pushConditionals
    )
import Cabal.Syntax.GenericPackageDescription (ComponentMap, FieldMap)
import Cabal.Syntax.JsonFieldGrammar (Fragment)

-- | Stage 0 — Cabal's native conditional tree, with a whole 'FieldMap' at each
-- leaf and one condition per branch (conjunctions nest). @--pristine@ renders this.
type ComponentTreeCabal = C.CondTree ConfVar [Dependency] (FieldMap (Fragment Json))

-- | Stage 1 — our uniform conditional tree; still a tree of fields.
type ComponentTree = CondTree ConfVar (FieldMap (Fragment Json))

-- | Stage 2 — fields of trees: the conditional structure pushed inside each field.
type FieldTrees = FieldMap (CondTree ConfVar (Fragment Json))

-- | Stage 3 — each field a flat list of values guarded by their cumulative condition.
type GuardedFields = FieldMap (NonEmpty (Guarded ConfVar (Fragment Json)))

-- | Stage 4 — each field a single merged JSON fragment. The default output renders this.
type JsonFields = FieldMap (Fragment Json)

-- | The default pipeline for one component: stage 0 → stage 4.
--
-- Read bottom-to-top: 'convertCondTree' (0→1) normalises Cabal's tree;
-- 'pushConditionals' (1→2) turns the tree of fields into fields of trees;
-- @fmap 'flattenCondTree'@ (2→3) flattens each field's tree into cumulative guards;
-- @fmap 'defragment'@ (3→4) encodes the guards as @{_if,_then}@ and merges them.
processComponent :: ComponentTreeCabal -> JsonFields
processComponent =
      fmap defragment
    . fmap flattenCondTree
    . pushConditionals
    . convertCondTree

-- | Apply 'processComponent' to every component of a package.
process :: ComponentMap ComponentTreeCabal -> ComponentMap JsonFields
process = fmap processComponent
```

- [ ] **Step 2: Add the module to the cabal file**

In `Cabal-syntax-json.cabal`, add `Cabal.Syntax.Pipeline` to the library's `exposed-modules` (keep alphabetical-ish order, e.g. after `Cabal.Syntax.JsonFieldGrammar`):

```
  exposed-modules:    Cabal.Syntax.Compat
                      Cabal.Syntax.CondTree
                      Cabal.Syntax.GenericPackageDescription
                      Cabal.Syntax.Json
                      Cabal.Syntax.JsonFieldGrammar
                      Cabal.Syntax.Pipeline
                      Cabal.Syntax.ListMap
                      Cabal.Syntax.Pretty
                      Cabal.Syntax.Simplify
                      Cabal.Syntax.Utils
```

- [ ] **Step 3: Delete the local `process` from `app/Main.hs`**

Delete the entire `process` function (`app/Main.hs:216-232`), including its `-- the four-step` type signature and `where` block:

```haskell
process
    :: ComponentMap (C.CondTree ConfVar c (FieldMap (Fragment Json)))
    -> ComponentMap (FieldMap (Fragment Json))
process components0 = components4
  where
    components1 :: ComponentMap (CondTree ConfVar (FieldMap (Fragment Json)))
    components1 = fmap convertCondTree components0

    components2 :: ComponentMap (FieldMap (CondTree ConfVar (Fragment Json)))
    components2 = fmap pushConditionals components1

    components3 :: ComponentMap (FieldMap (NonEmpty (Guarded ConfVar (Fragment Json))))
    components3 = fmap (fmap flattenCondTree) components2

    components4 :: ComponentMap (FieldMap (Fragment Json))
    components4 = fmap (fmap defragment) components3
```

The call site `format $ GPD top (process components0)` in `main'` stays exactly as-is — `process` now comes from the import.

- [ ] **Step 4: Update `app/Main.hs` imports**

Replace the `Cabal.Syntax.*` import block and remove the three imports that only `process` used. Specifically:

Delete this block (`app/Main.hs:36-43`):
```haskell
import Cabal.Syntax.CondTree
    ( CondTree (..)
    , Guarded
    , convertCondTree
    , defragment
    , flattenCondTree
    , pushConditionals
    )
```

Change (`:44-50`):
```haskell
import Cabal.Syntax.GenericPackageDescription
    ( ComponentMap (..)
    , FieldMap (..)
    , GPD (..)
    , renderFields
    , runGenericPackageDescription
    )
```
to:
```haskell
import Cabal.Syntax.GenericPackageDescription
    ( GPD (..)
    , renderFields
    , runGenericPackageDescription
    )
```

Change (`:52`):
```haskell
import Cabal.Syntax.JsonFieldGrammar (Fragment (..))
```
to:
```haskell
import Cabal.Syntax.JsonFieldGrammar (Fragment)
```

Add a new import (next to the other `Cabal.Syntax` imports, alphabetical):
```haskell
import Cabal.Syntax.Pipeline (process)
```

Delete these three now-unused imports (`:7`, `:23`, `:24`):
```haskell
import Data.List.NonEmpty (NonEmpty)
```
```haskell
import Distribution.Types.CondTree qualified as C
```
```haskell
import Distribution.Types.ConfVar (ConfVar (..))
```

- [ ] **Step 5: Build with `-Wall`**

Run: `cabal build`
Expected: library and executable build with **no** warnings (no unused imports). If GHC flags any remaining unused import in `Main.hs`, remove exactly that import and rebuild.

- [ ] **Step 6: Verify no output drift (all three modes)**

```bash
cabal run -- golden-tests
BIN=$(cabal list-bin cabal2json)
for p in text filepath bytestring unix ghc-bignum; do
  diff <("$BIN"            tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.default.json"  && echo "default $p OK"
  diff <("$BIN" --pristine tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.pristine.json" && echo "pristine $p OK"
  diff <("$BIN" --debug    tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.debug.txt"   && echo "debug $p OK"
done
git status --short tests/golden
```
Expected: golden suite passes; all diffs empty (only `... OK` lines); no golden files changed.

- [ ] **Step 7: Commit**

```bash
git add Cabal-syntax-json.cabal src/Cabal/Syntax/Pipeline.hs app/Main.hs
git commit -m "refactor: extract cabal->JSON pipeline into Cabal.Syntax.Pipeline"
```

---

### Task 8: Document the output modes and conditional encoding in the README

**Files:**
- Modify: `README.md`

- [ ] **Step 1: Add an "Output modes" subsection**

In `README.md`, immediately **after** the `## Details` section (after the line describing the flatten step, ~`:364`), add:

````markdown
## Output modes

`cabal2json` can render the package at two points in the pipeline:

- **default** — the fully transformed form described above: conditionals are pushed
  *inside* each field value and flattened so every value carries its cumulative
  condition. This is *fields of trees*.
- **`--pristine`** — Cabal's own conditional structure, untransformed: conditions sit
  *outside* groups of fields, with one condition per branch, so nested conditions nest
  in the output. This is *tree of fields*. Use it to see the package exactly as
  Cabal parses it.

`--debug` is orthogonal to both: it renders the selected form in a cabal-like text
layout instead of JSON, for eyeballing.

## Conditional encoding

Conditions appear as JSON objects. A variable is one of:

```json
{"flag": "name"}     {"os": "linux"}     {"arch": "x86_64"}     {"impl": "ghc", "range": ">=9.4"}
```

and these combine with:

```json
{"not": <cond>}      {"and": [<cond>, <cond>]}      {"or": [<cond>, <cond>]}
```

In the **default** output, a conditional field value is a list that may mix plain values
with guard objects of the form:

```json
{"_if": <cond>, "_then": [<values>]}
```

`else` branches are folded into the guard as `{"not": <cond>}`, so default output never
emits `_else`. In **`--pristine`** output, the untransformed tree is rendered as
`[<value>, {"_if": <cond>, "_then": <subtree>, "_else": <subtree>}]`, where `_else` may
appear and `_then`/`_else` are themselves subtrees.
````

- [ ] **Step 2: Sanity-check the rendered Markdown**

Run: `git diff README.md`
Expected: the new sections appear after `## Details`, fenced code blocks balanced.

- [ ] **Step 3: Commit**

```bash
git add README.md
git commit -m "docs: document output modes and conditional JSON encoding"
```

---

### Task 9: Final full verification

**Files:** none.

- [ ] **Step 1: Clean build with `-Wall`**

Run: `cabal clean && cabal build`
Expected: library, executable, and test suite build with no warnings.

- [ ] **Step 2: Full golden suite**

Run: `cabal run -- golden-tests`
Expected: every test passes (unit + golden).

- [ ] **Step 3: All-modes baseline diff**

```bash
BIN=$(cabal list-bin cabal2json)
for p in text filepath bytestring unix ghc-bignum; do
  diff <("$BIN"            tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.default.json"
  diff <("$BIN" --pristine tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.pristine.json"
  diff <("$BIN" --debug    tests/golden/$p.cabal) "$TMPDIR/cabal2json-baseline/$p.debug.txt"
done
echo "all modes unchanged"
git status --short
```
Expected: no diff output before `all modes unchanged`; `git status` shows only the (committed) source changes, no stray golden modifications.

- [ ] **Step 4: Format**

Run: `fourmolu -i src app tests`
Expected: no changes, or only whitespace the hooks would have made; if it reformats, `git add -u && git commit -m "style: fourmolu"`.

---

## Self-review notes

- **Spec coverage:** five-stage naming → Task 7; correctness cleanups (dead `simplifyCondTree`, unused `Semigroup`, documented dropped constraints) → Tasks 5/4/6; merge-on-duplicate capture → Tasks 2/3; README modes + encoding → Task 8; output-frozen guarantee → golden suite (every task) + baselines (Tasks 1/3/6/9). The spec's metadata boundary is handled (Task 3 Step 1) via the `ScalarFragment`-first rewrite, which the spec assumed switchable; this plan makes that switchability explicit and output-preserving.
- **Type consistency:** `processComponent :: ComponentTreeCabal -> JsonFields` and `process :: ComponentMap ComponentTreeCabal -> ComponentMap JsonFields`; the stage aliases are used consistently; `fromListWith :: Eq k => (v -> v -> v) -> [(k,v)] -> ListMap k v` (Semigroup dropped) matches its call `fromListWith (<>)`.
- **No placeholders:** every code edit shows exact before/after; every run step shows expected output.
