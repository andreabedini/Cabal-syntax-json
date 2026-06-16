# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`Cabal-syntax-json` turns a `.cabal` file into a robust JSON representation, aimed at
build-system developers who need to extract package information programmatically. It is
built on top of `Cabal-syntax`'s own parser and field grammars, so it makes no
assumptions about individual fields. Its distinguishing feature is how it handles
conditionals: it transforms a *tree of fields* (the cabal conditional structure) into
*fields of trees*, pushing `if`/`else` conditions down into each field value and
attaching the cumulative condition as a guard.

The package ships a library plus the `cabal2json` executable. See `README.md` for the
output format, worked examples, and the conceptual description of the conditional
transformation.

## Build / test / format

```sh
cabal build
cabal run -- golden-tests           # run the whole test suite (unit + golden)
cabal run -- golden-tests -p text   # run only tests whose name matches "text" (tasty -p)
fourmolu -i src app tests           # format (config in fourmolu.yaml; runs on pre-commit)
```

Toolchain is pinned via `mise.toml` (cocogitto, lefthook, release-please, fourmolu
0.18.0.0). CI builds on GHC 9.10.1 across Linux/macOS/Windows.

`cabal.project` constrains `bytestring`, `containers`, `Cabal`, `Cabal-syntax`, and
`pretty` to `installed` (the GHC boot versions) — this is deliberate, keeping the runtime
dependency footprint to GHC boot packages only.

### Golden tests

The golden suite (`tests/Tests.hs`) lives in `tests/golden/`. For each `*.cabal` there,
it runs `cabal2json` with no flags, with each package flag toggled `+`/`-`, and with
`--os` set to windows/linux/osx, comparing against committed `.json` files. The runner
shells out to the `cabal2json` binary (declared via `build-tool-depends`), so a stale
build can cause confusing failures — `cabal run -- golden-tests` rebuilds first.

To regenerate goldens after an intentional output change, tasty-golden writes the actual
output to a sibling `.json.out`; review the diff, then `mv` the `.out` over the `.json`.
Adding a new fixture is just dropping a `<name>.cabal` into `tests/golden/` and creating
its goldens the same way.

## Architecture

The transformation runs as a pipeline; `app/Main.hs` (`main'` and `process`) wires the
stages together and is the best entry point for understanding the whole flow. Stages:

1. **Parse** — `readGenericPackageDescription` (via `Cabal.Syntax.Compat`) yields a
   `GenericPackageDescription`.
2. **Simplify** (`Cabal.Syntax.Simplify`) — given an `Env` (os/arch/compiler/flags from
   the CLI), partially evaluates conditionals. Conditions that can't be decided from the
   partial assignment are left intact.
3. **Convert to our GPD** (`Cabal.Syntax.GenericPackageDescription`) —
   `runGenericPackageDescription` walks the package using `Cabal`'s `FieldGrammar`
   machinery, but with our `JSONFieldGrammar` interpretation, producing
   `GPD (FieldMap a) (ComponentMap b)` where each field value is a `Fragment Json`.
4. **Restructure conditionals** (`Cabal.Syntax.CondTree`), only when not `--pristine`.
   `process` in `Main.hs` chains four sub-steps over each component's cond tree:
   `convertCondTree` → `pushConditionals` (push conditions down into field values:
   tree-of-fields becomes fields-of-trees) → `flattenCondTree` (flatten to a list of
   `Guarded` values, each carrying its cumulative `Condition`) → `defragment` (merge the
   guarded fragments back into a single `Fragment Json` per field).
5. **Render** — `formatJson` (default) via the `ToJSON` class in `Cabal.Syntax.Json`, or
   `formatPretty` (`--debug`) via the `PrettyFieldClass` in `Cabal.Syntax.Pretty`, which
   emits a cabal-like intermediate format useful for debugging.

### Key modules

- `Cabal.Syntax.Compat` — shims over `Cabal`/`Cabal-syntax` API differences across the
  supported version range (`SymbolicPath`, `readGenericPackageDescription`, etc.). The
  build supports Cabal `3.10 || 3.12 || 3.14`; version-specific API breakage belongs
  here, behind CPP, not scattered through the codebase.
- `Cabal.Syntax.JsonFieldGrammar` — the `JSONFieldGrammar` `FieldGrammar` instance and
  the `Fragment` type (a field value carrying enough structure to be re-guarded by
  conditionals). This is where cabal fields are mapped to JSON.
- `Cabal.Syntax.CondTree` — our own `CondTree`/`CondNode`/`Guarded` types and the
  conditional-restructuring operations (the core novelty of the package).
- `Cabal.Syntax.Json` — `Json` rendering, `ToJSON` class, and `ViaPretty`/`ViaUnpack`
  deriving helpers.
- `Cabal.Syntax.GenericPackageDescription` — `GPD`, `FieldMap`, `ComponentMap` wrappers
  and the `runGenericPackageDescription` driver.
- `Cabal.Syntax.Simplify` — `Env` and partial conditional evaluation.
- `Cabal.Syntax.ListMap` — an association-list map preserving insertion order (field and
  component ordering is significant in the output).
- `Cabal.Syntax.Utils` — `These`/`align` helpers (covered by the unit tests).
- `Cabal.Syntax.Pretty` — the `--debug` pretty-printer.

## Conventions

- Default language is `GHC2021`; `-Wall` is on for both the library (`cabal.project`) and
  the executable (`.cabal ghc-options`).
- Formatting is enforced by fourmolu (4-space indent, 100 col, leading commas/arrows);
  the lefthook `pre-commit` hook runs it on staged `.hs` files.
- Commits are validated by cocogitto (`cog verify`) on `commit-msg` — use Conventional
  Commits. Versioning/releases are automated via release-please; the version in the
  `.cabal` file is managed between the `x-release-please` markers, so do not bump it by
  hand.
