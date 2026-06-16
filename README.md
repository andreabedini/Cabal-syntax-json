# Cabal-syntax-json

[![CI](https://github.com/andreabedini/Cabal-syntax-json/actions/workflows/haskell.yml/badge.svg)](https://github.com/andreabedini/Cabal-syntax-json/actions/workflows/haskell.yml)

Turn a `.cabal` file into a robust JSON representation, ready to be consumed by other
tools. The package ships a library plus the `cabal2json` executable.

It is not the first package to render cabal files as JSON, but a few things set it apart:

1. **Simple.** Zero non-boot dependencies — all you need is GHC.
2. **Robust.** It is built on `Cabal-syntax`'s own parser and field grammars, so it makes
   no assumptions about individual fields.
3. **Helpful.** It rewrites `if`/`else` conditionals into a shape that is easier to
   consume programmatically.
4. **Flexible.** Those conditionals can be partially evaluated against flags, OS, arch,
   and compiler passed on the command line.

It is aimed at build-system developers who need to extract package information to drive a
build.

## Build

```sh
cabal build
cabal run cabal2json -- path/to/package.cabal
```

The runtime dependency footprint is restricted to GHC boot packages (see
`cabal.project`), so a plain `cabal build` against any supported GHC just works.

## Usage

```text
Usage: cabal2json [OPTION...] files...
  -h           --help               Print this help message
               --out=FILE           Path for output
  -f FLAG      --flag=FLAG          Specialise to flag assignment FLAG
  -c COMPILER  --compiler=COMPILER  Specialise to the compiler COMPILER
  -o OS        --os=OS              Specialise to the operating system OS
  -a ARCH      --arch=ARCH          Specialise to the architecture ARCH
  -p PLATFORM  --platform=PLATFORM  Specialise to the platform PLATFORM
               --debug              Output an intermediate format useful for debugging
               --pristine           Preserve the conditional structure
```

## Example

Run against
[template-haskell-2.19.0.0](https://hackage.haskell.org/package/template-haskell-2.19.0.0/revision/0.cabal),
`cabal2json` produces the JSON below. Note how the `vendor-filepath` conditional has been
pushed *inside* `other-modules`, `default-extensions`, `build-depends`, and
`hs-source-dirs`, each value carrying the condition that guards it:

<details>
<summary><code>cabal2json template-haskell.cabal</code></summary>

```json
{
  "cabal-version": ">=1.10",
  "name": "template-haskell",
  "version": "2.19.0.0",
  "license": "BSD3",
  "license-file": [
    "LICENSE"
  ],
  "license-files": [
    "LICENSE"
  ],
  "maintainer": "libraries@haskell.org",
  "bug-reports": "https://gitlab.haskell.org/ghc/ghc/issues/new",
  "synopsis": "Support library for Template Haskell",
  "description": "This package provides modules containing facilities for manipulating\nHaskell source code using Template Haskell.\n\nSee <http://www.haskell.org/haskellwiki/Template_Haskell> for more\ninformation.",
  "category": "Template Haskell",
  "build-type": "Simple",
  "extra-source-files": [
    "changelog.md"
  ],
  "source-repositories": {
    "head": {
      "type": "git",
      "location": "https://gitlab.haskell.org/ghc/ghc.git",
      "subdir": "libraries/template-haskell"
    }
  },
  "components": {
    "lib": {
      "exposed-modules": [
        "Language.Haskell.TH",
        "Language.Haskell.TH.Lib",
        "Language.Haskell.TH.Ppr",
        "Language.Haskell.TH.PprLib",
        "Language.Haskell.TH.Quote",
        "Language.Haskell.TH.Syntax",
        "Language.Haskell.TH.LanguageExtensions",
        "Language.Haskell.TH.CodeDo",
        "Language.Haskell.TH.Lib.Internal"
      ],
      "other-modules": [
        "Language.Haskell.TH.Lib.Map",
        {
          "_if": {
            "flag": "vendor-filepath"
          },
          "_then": [
            "System.FilePath",
            "System.FilePath.Posix",
            "System.FilePath.Windows"
          ]
        }
      ],
      "default-language": "Haskell2010",
      "default-extensions": [
        "NoImplicitPrelude",
        {
          "_if": {
            "flag": "vendor-filepath"
          },
          "_then": [
            "ImplicitPrelude"
          ]
        }
      ],
      "other-extensions": [
        "BangPatterns",
        "CPP",
        "DefaultSignatures",
        "DeriveDataTypeable",
        "DeriveGeneric",
        "FlexibleInstances",
        "RankNTypes",
        "RoleAnnotations",
        "ScopedTypeVariables"
      ],
      "ghc-options": [
        "-Wall",
        "-this-unit-id",
        "template-haskell"
      ],
      "build-depends": [
        {
          "package": "base",
          "version": ">=4.11 && <4.18",
          "libs": [
            "base"
          ]
        },
        {
          "package": "ghc-boot-th",
          "version": "==9.4.1",
          "libs": [
            "ghc-boot-th"
          ]
        },
        {
          "package": "ghc-prim",
          "version": ">=0",
          "libs": [
            "ghc-prim"
          ]
        },
        {
          "package": "pretty",
          "version": ">=1.1 && <1.2",
          "libs": [
            "pretty"
          ]
        },
        {
          "_if": {
            "not": {
              "flag": "vendor-filepath"
            }
          },
          "_then": [
            {
              "package": "filepath",
              "version": ">=0",
              "libs": [
                "filepath"
              ]
            }
          ]
        }
      ],
      "hs-source-dirs": [
        {
          "_if": {
            "flag": "vendor-filepath"
          },
          "_then": [
            "./vendored-filepath",
            "."
          ]
        },
        {
          "_if": {
            "not": {
              "flag": "vendor-filepath"
            }
          },
          "_then": [
            "."
          ]
        }
      ]
    }
  }
}
```

</details>

Activating the flag with `cabal2json --flag +vendor-filepath` resolves those conditionals
away and emits the `flags` block:

<details>
<summary><code>cabal2json --flag +vendor-filepath template-haskell.cabal</code></summary>

```json
{
  "cabal-version": ">=1.10",
  "name": "template-haskell",
  "version": "2.19.0.0",
  "license": "BSD3",
  "license-file": [
    "LICENSE"
  ],
  "license-files": [
    "LICENSE"
  ],
  "maintainer": "libraries@haskell.org",
  "bug-reports": "https://gitlab.haskell.org/ghc/ghc/issues/new",
  "synopsis": "Support library for Template Haskell",
  "description": "This package provides modules containing facilities for manipulating\nHaskell source code using Template Haskell.\n\nSee <http://www.haskell.org/haskellwiki/Template_Haskell> for more\ninformation.",
  "category": "Template Haskell",
  "build-type": "Simple",
  "extra-source-files": [
    "changelog.md"
  ],
  "source-repositories": {
    "head": {
      "type": "git",
      "location": "https://gitlab.haskell.org/ghc/ghc.git",
      "subdir": "libraries/template-haskell"
    }
  },
  "flags": {
    "vendor-filepath": {
      "description": "Vendor the dependency on filepath",
      "default": false,
      "manual": true
    }
  },
  "components": {
    "lib": {
      "exposed-modules": [
        "Language.Haskell.TH",
        "Language.Haskell.TH.Lib",
        "Language.Haskell.TH.Ppr",
        "Language.Haskell.TH.PprLib",
        "Language.Haskell.TH.Quote",
        "Language.Haskell.TH.Syntax",
        "Language.Haskell.TH.LanguageExtensions",
        "Language.Haskell.TH.CodeDo",
        "Language.Haskell.TH.Lib.Internal"
      ],
      "hs-source-dirs": [
        "./vendored-filepath",
        "."
      ],
      "other-modules": [
        "Language.Haskell.TH.Lib.Map",
        "System.FilePath",
        "System.FilePath.Posix",
        "System.FilePath.Windows"
      ],
      "default-language": "Haskell2010",
      "default-extensions": [
        "NoImplicitPrelude",
        "ImplicitPrelude"
      ],
      "other-extensions": [
        "BangPatterns",
        "CPP",
        "DefaultSignatures",
        "DeriveDataTypeable",
        "DeriveGeneric",
        "FlexibleInstances",
        "RankNTypes",
        "RoleAnnotations",
        "ScopedTypeVariables"
      ],
      "ghc-options": [
        "-Wall",
        "-this-unit-id",
        "template-haskell"
      ],
      "build-depends": [
        {
          "package": "base",
          "version": ">=4.11 && <4.18",
          "libs": [
            "base"
          ]
        },
        {
          "package": "ghc-boot-th",
          "version": "==9.4.1",
          "libs": [
            "ghc-boot-th"
          ]
        },
        {
          "package": "ghc-prim",
          "version": ">=0",
          "libs": [
            "ghc-prim"
          ]
        },
        {
          "package": "pretty",
          "version": ">=1.1 && <1.2",
          "libs": [
            "pretty"
          ]
        }
      ]
    }
  }
}
```

</details>

## How it works

The JSON representation is built by a pipeline of stages (the per-component core lives in
`Cabal.Syntax.Pipeline`):

1. **Parse** the cabal file with `readGenericPackageDescription`.
2. **Simplify** the conditionals against the CLI assignment (flags, OS, arch, compiler).
   Conditions that can't be decided from a partial assignment are left intact.
3. **Push conditionals down** into the field values. A *tree of fields* becomes *fields of
   trees*. For example:

   ```cabal
       if flag(vendor-filepath)
         other-modules:
           System.FilePath
           System.FilePath.Posix
           System.FilePath.Windows
         hs-source-dirs:
           ./vendored-filepath
           .
         default-extensions:
           ImplicitPrelude
       else
         build-depends:
           filepath
         hs-source-dirs:
           .
   ```

   becomes

   ```cabal
       other-modules:
         if flag(vendor-filepath)
           System.FilePath
           System.FilePath.Posix
           System.FilePath.Windows
       hs-source-dirs:
         if flag(vendor-filepath)
           ./vendored-filepath
           .
       default-extensions:
         if flag(vendor-filepath)
           ImplicitPrelude
       build-depends:
         if !flag(vendor-filepath)
           filepath
       hs-source-dirs:
         if !flag(vendor-filepath)
           .
   ```

4. **Flatten** each field's tree into a list whose values are guarded by their *cumulative*
   condition. Nested conditions collapse into a single conjunction:

   ```cabal
     if impl(ghc >= 8.0)
       if flag(os-string)
         build-depends: filepath >= 1.5.0.0, os-string >= 2.0.0
       else
         build-depends: filepath >= 1.4.100.0 && < 1.5.0.0
   ```

   becomes

   ```cabal
     build-depends:
       if impl(ghc >= 8.0) && flag(os-string)
         filepath >= 1.5.0.0
         os-string >= 2.0.0
       if impl(ghc >= 8.0) && !flag(os-string)
         filepath >= 1.4.100.0 && < 1.5.0.0
   ```

5. **Render** the result as JSON.

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

## License

MIT. Authored by Andrea Bedini (andrea@andreabedini.com).
