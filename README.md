# Cabal-syntax-json

[![CI](https://github.com/andreabedini/Cabal-syntax-json/actions/workflows/ci.yml/badge.svg)](https://github.com/andreabedini/Cabal-syntax-json/actions/workflows/ci.yml)

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

## Install

### Pre-built binaries

Download the archive for your platform from the
[latest release](https://github.com/andreabedini/Cabal-syntax-json/releases/latest) and unpack it. It
contains the `cabal2json` executable. There are archives for x86_64 and aarch64 Linux,
x86_64 and aarch64 macOS, and x86_64 Windows.

- Linux binaries need glibc 2.34 or newer (e.g. Ubuntu 22.04, Debian 12, RHEL 9), libgmp
  and zlib.
- macOS binaries are not notarised. Remove the quarantine attribute before running one:
  `xattr -d com.apple.quarantine cabal2json`.

Each release also has `SHA256SUMS`, and a GitHub attestation for every file. To check an
archive:

```
sha256sum --check --ignore-missing SHA256SUMS
gh attestation verify ARCHIVE --repo andreabedini/Cabal-syntax-json
```

### With mise

[mise](https://mise.jdx.dev/) installs the release archive for your platform with its
[GitHub backend](https://mise.jdx.dev/dev-tools/backends/github.html), and checks its
checksum and attestation:

```
mise use -g github:andreabedini/Cabal-syntax-json
```

Add `@VERSION` to pin a version, for example
`github:andreabedini/Cabal-syntax-json@0.1.0.0`. Without `-g`, the tool is added to the
current project's `mise.toml`.

### From source

Each release has the source distribution of every package, `PACKAGE-VERSION.tar.gz`. With
GHC 9.10 or 9.12 and cabal:

```
tar -xzf Cabal-syntax-json-VERSION.tar.gz
cd Cabal-syntax-json-VERSION
cabal install exe:cabal2json
```

To build from a checkout instead, see [CONTRIBUTING.md](CONTRIBUTING.md#building).

## Documentation

The API documentation of the latest release is at
<https://andreabedini.github.io/Cabal-syntax-json/>.

Each [release](https://github.com/andreabedini/Cabal-syntax-json/releases) also has the
documentation of every package with a library, as
`PACKAGE-VERSION-docs.tar.gz`, in the format Hackage takes.

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

The arguments of the specialising options use Cabal's syntax:

| Argument | Form | Example |
|---|---|---|
| `FLAG` | One or more flag assignments, separated by spaces: `+NAME` sets a flag, `-NAME` clears it. The option can be repeated. | `--flag '+pure-haskell -debug'` |
| `COMPILER` | `FLAVOUR-VERSION` | `--compiler ghc-9.12.2` |
| `OS` | An operating system as `os(...)` names it in a `.cabal` file | `--os linux` |
| `ARCH` | An architecture as `arch(...)` names it | `--arch aarch64` |
| `PLATFORM` | `ARCH-OS`; sets both | `--platform x86_64-linux` |

An unknown compiler, OS or architecture is accepted with a warning on stderr.

The output goes to stdout, or to `FILE` with `--out`. With several input files the output
is one JSON object per file, in the order given, with nothing between them: read it with a
parser for concatenated JSON, such as `jq`. `--out` takes a single input file.

## Example

Run against
[template-haskell-2.19.0.0](https://hackage.haskell.org/package/template-haskell-2.19.0.0/revision/0.cabal),
`cabal2json` produces the JSON below. Note how the `vendor-filepath` conditional has been
pushed *inside* `other-modules`, `default-extensions`, `build-depends`, and
`hs-source-dirs`, each value carrying the condition that guards it, and how the `flags`
block declares the flag with its default:

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
away and drops the flag from the `flags` block, which lists only the flags still left
open (here none, so the block is gone):

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

The JSON representation is built in five steps. Steps 3 and 4 are the core of the
transformation: the Haddock documentation of `Cabal.Syntax.Pipeline` follows a field
through them in detail, as stages 0 to 4.

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
         if !flag(vendor-filepath)
           .
       default-extensions:
         if flag(vendor-filepath)
           ImplicitPrelude
       build-depends:
         if !flag(vendor-filepath)
           filepath
   ```

   Each field appears once, with the values of every branch that sets it.

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

## Output format

This section describes the default JSON output. `--pristine` differs only in the
components, as described at the end.

### Top level

The output is a JSON object. Its keys, in this order, are:

- **The package's fields**, named as in the `.cabal` file (`name`, `version`,
  `synopsis`, `extra-source-files`, …). A single-valued field is a string. A list-valued
  field is an array of strings. `tested-with` is an array of `[compiler, version range]`
  pairs.
  Custom `x-` fields are strings, under their own names.
  `license-file` lists every licence file, whether the `.cabal` file names them with
  `license-file` or `license-files`.
- **`custom-setup`**, if the package has a `custom-setup` section: an object with its
  fields, such as `setup-depends`.
- **`source-repositories`**, if there are any: an object with one key per repository
  kind (`head`, `this`), whose value is an object of that repository's fields.
- **`flags`**, if any flag is still open: an object with one key per flag that `--flag`
  did not assign, whose value is an object with `description`, `default` and `manual`.
- **`components`**: an object with one key per component, in this order: `lib` for the
  main library, `lib:NAME` for each sub-library, then `flib:NAME`, `exe:NAME`,
  `test:NAME` and `bench:NAME`. Each value is an object of the component's fields.

### Component fields

A field that no open condition affects has a plain value: a string for a single-valued
field (`main-is`, `default-language`), `true` or `false` for `buildable`, or an array for
a list-valued field (`exposed-modules`, `ghc-options`, `build-depends`).

A field that some open condition affects is always an array, even for a single-valued
field. Each element is either a plain value, which always applies, or a guard:

```json
{"_if": <condition>, "_then": <value>}
```

`_then` holds what applies when the condition is true: an array for a list-valued field,
or the plain value for a single-valued one. The condition is the *cumulative* condition
of the value: nested `if`s are joined with `and`, and an `else` branch is the negated
condition, so the default output never has `_else`. For example:

```json
"buildable": [{"_if": {"not": {"flag": "ffi"}}, "_then": false}]
```

Dependencies are objects:

| Field | Element |
|---|---|
| `build-depends`, `setup-depends` | `{"package": NAME, "version": RANGE, "libs": [LIB, …]}`; `libs` names the libraries used, with the package name standing for its main library |
| `build-tool-depends` | `{"package": NAME, "version": RANGE, "exe": EXE}` |
| `pkgconfig-depends` | `{"name": NAME, "version": RANGE}` |

Version ranges, and fields such as `mixins`, `reexported-modules` and `build-tools`, are
strings in `.cabal` syntax.

### Conditions

A condition is a variable, a literal or a combination. Variables are:

```json
{"flag": "name"}     {"os": "linux"}     {"arch": "x86_64"}     {"impl": "ghc", "range": ">=9.4"}
```

The literals are `true` and `false`. Conditions combine with:

```json
{"not": <cond>}      {"and": [<cond>, <cond>]}      {"or": [<cond>, <cond>]}
```

Conditions that the specialising options decide are evaluated away, so the output keeps
only the conditions that are still open.

### `--pristine`

With `--pristine`, a component keeps Cabal's conditional structure. A component with no
conditionals is an object of fields. A component with conditionals is an array whose
first element is the object of unconditional fields, followed by one branch per
conditional:

```json
{"_if": <condition>, "_then": <component>, "_else": <component>}
```

`_then` and `_else` have the same form as a component, and `_else` appears only when the
`.cabal` file has an `else`.

## License

MIT, see [LICENSE](LICENSE). Authored by Andrea Bedini (andrea@andreabedini.com).
