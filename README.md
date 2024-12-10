# Cabal-syntax-json

An experiment to use `Cabal-syntax`'s [`FieldGrammar`](https://hackage.haskell.org/package/Cabal-syntax-3.8.1.0/docs/Distribution-FieldGrammar.html) to define a JSON syntax for cabal files.

## Example

Rendering of [template-haskell-2.19.0.0](https://hackage.haskell.org/package/template-haskell-2.19.0.0/revision/0.cabal)

```
{
  "bug-reports": "https://gitlab.haskell.org/ghc/ghc/issues/new",
  "build-type": "Simple",
  "cabal-version": ">=1.10",
  "category": "Template Haskell",
  "description": "This package provides modules containing facilities for manipulating\nHaskell source code using Template Haskell.\n\nSee <http://www.haskell.org/haskellwiki/Template_Haskell> for more\ninformation.",
  "extra-source-files": [
    "changelog.md"
  ],
  "flags": {
    "vendor-filepath": {
      "default": false,
      "description": "Vendor the dependency on filepath",
      "manual": true
    }
  },
  "library": {
    "build-depends": [
      {
        "if": {
          "not": {
            "os": "vendor-filepath"
          }
        },
        "then": [
          "filepath"
        ]
      },
      [
        "base >=4.11 && <4.18",
        "ghc-boot-th ==9.4.1",
        "ghc-prim",
        "pretty >=1.1 && <1.2"
      ]
    ],
    "default-extensions": [
      {
        "if": {
          "os": "vendor-filepath"
        },
        "then": [
          "ImplicitPrelude"
        ]
      },
      [
        "NoImplicitPrelude"
      ]
    ],
    "default-language": [
      "Haskell2010"
    ],
    "exposed-modules": [
      [
        "Language.Haskell.TH",
        "Language.Haskell.TH.Lib",
        "Language.Haskell.TH.Ppr",
        "Language.Haskell.TH.PprLib",
        "Language.Haskell.TH.Quote",
        "Language.Haskell.TH.Syntax",
        "Language.Haskell.TH.LanguageExtensions",
        "Language.Haskell.TH.CodeDo",
        "Language.Haskell.TH.Lib.Internal"
      ]
    ],
    "ghc-options": [
      [
        "-Wall",
        "-this-unit-id",
        "template-haskell"
      ]
    ],
    "hs-source-dirs": [
      {
        "if": {
          "not": {
            "os": "vendor-filepath"
          }
        },
        "then": [
          "."
        ]
      },
      {
        "if": {
          "os": "vendor-filepath"
        },
        "then": [
          "./vendored-filepath",
          "."
        ]
      }
    ],
    "other-extensions": [
      [
        "BangPatterns",
        "CPP",
        "DefaultSignatures",
        "DeriveDataTypeable",
        "DeriveGeneric",
        "FlexibleInstances",
        "RankNTypes",
        "RoleAnnotations",
        "ScopedTypeVariables"
      ]
    ],
    "other-modules": [
      {
        "if": {
          "os": "vendor-filepath"
        },
        "then": [
          "System.FilePath",
          "System.FilePath.Posix",
          "System.FilePath.Windows"
        ]
      },
      [
        "Language.Haskell.TH.Lib.Map"
      ]
    ]
  },
  "license": "BSD3",
  "license-file": [
    "LICENSE"
  ],
  "maintainer": "libraries@haskell.org",
  "name": "template-haskell",
  "source-repos": [
    {
      "location": "https://gitlab.haskell.org/ghc/ghc.git",
      "subdir": "libraries/template-haskell",
      "type": {
        "contents": "Git",
        "tag": "KnownRepoType"
      }
    }
  ],
  "synopsis": "Support library for Template Haskell",
  "version": "2.19.0.0"
}
```

## Notes

- Given this uses `Cabal-syntax` machinery, I don't have to care about
  the specific fields. The only concern is the structure.
- I should write the other direction (parsing from json) too, and see if
  they round-trip.
- If you notice the example above, I am doing something with the
  conditionals, pushing them all the way down into the field value. This
  representation felt more natural in json but it might make parsing more
  complicated.
- Of course, I defined a bunch of orphan instances, beware!

## Author

Andrea Bedini (andrea@andreabedini.com)
