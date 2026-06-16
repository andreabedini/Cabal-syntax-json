# Cabal-syntax-json

This package provides a robust JSON representation of cabal files. While it is not the first
or the only package to do so, I believe it has some interesting features that make it worth:

1. Simple. Zero non-boot dependency, you only need GHC.
2. Robust. It uses Cabal-syntax own code and makes no assumptions on the fields.
3. Helpful. It transforms if conditions in a format easier to handle.
4. Flexible. It can simplify those conditionals based on flags passed on the cli.

This tool is aimed to build-system developers who need to extract information about a package to drive the build process.

## Usage

```
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

Using [template-haskell-2.19.0.0](https://hackage.haskell.org/package/template-haskell-2.19.0.0/revision/0.cabal), `cabal2json` produces:

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

Activating the flag `vendor-filepath` with `cabal2json --flag +vendor-filepath` produces:

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

## Details

The JSON representation is obtained through the following steps:
- A cabal file is parsed using `readGenericPackageDescription`.
- The cli flags are used to simplify the conditionals.
- The conditionals are pushed down into the field values. E.g.

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
  is transformed into:
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
  In other words, a tree of fields is transformed into fields of trees.
- Lastly the tree is flattened into a list where every field value is guarded by its cumulative conditionals.
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

## Author

Andrea Bedini (andrea@andreabedini.com)
