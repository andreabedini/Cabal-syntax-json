# Contributing

Bug reports and pull requests are welcome.

## Building

You need GHC 9.10 or 9.12 and a recent cabal (CI uses 3.18.1.0). `cabal.project` pins `Cabal`, `Cabal-syntax` and the other dependencies to the versions that come with GHC.

```
cabal build
```

## Testing

```
cabal run golden-tests
```

This runs the unit tests and the golden tests. For each `.cabal` file in `tests/golden/`, the golden tests run `cabal2json` without flags, with each package flag set to `+` and to `-`, and with `--os` set to windows, linux and osx, and compare the output with the committed `.json` files. If you change the output on purpose, the actual output is in a sibling `.json.out` file: check the diff, then move it over the `.json` file. To add a fixture, add a `.cabal` file to `tests/golden/` and create its golden files the same way.

`tests/run.sh` checks a built binary without a build tree, with the golden files for the default options. CI runs it on the archives that it releases:

```
sh tests/run.sh "$(cabal list-bin cabal2json)"
```

## Code style

Format the code with [fourmolu](https://github.com/fourmolu/fourmolu), which uses the repository's `fourmolu.yaml`:

```
fourmolu -i src app tests
```

There are no commit hooks, so format the code before you commit.

## Commit messages

Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/). For example:

```
fix: keep the order of fields with the same name
feat: add --version
feat!: write the output to stdout unless --out is given
docs: explain --pristine
```

The allowed types are `feat`, `fix`, `perf`, `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` and `revert`. A scope in parentheses is optional. `!` before the colon, or a `BREAKING CHANGE:` footer, marks a breaking change. The `commits` CI job checks every commit of a pull request with [git-cliff](https://git-cliff.org/), so the allowed types are the ones that the commit parsers in `cliff.toml` know. There are no commit hooks, so this check is the only one.

The commit types choose the next version:

| Commits since the last release | Next version |
|---|---|
| At least one breaking change, of any type | `B` + 1 |
| At least one `feat` | `C` + 1 |
| At least one `fix` or `perf` | `D` + 1 |
| Only `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` | No release |

Pull requests are merged with rebase merge. A squash merge would leave only the PR title for the release task to read.

## Versions

Versions follow the [Haskell PVP](https://pvp.haskell.org/) and have four components, `A.B.C.D`, for example `0.1.0.0`. `A.B` is the major version, `C` the minor version and `D` the patch version.

- git-cliff calculates the next version from the commit types, as in the table above. This is SemVer applied to `B.C.D`. There is no special rule for versions below 1.0: a breaking change increments `B` even when it is `0`.
- Incrementing a field sets all fields to its right to `0`.
- `A` changes only by hand, when the maintainer decides that a release deserves a new number. Like a change of `B`, it makes a major release.
- Release tags are `v` followed by the version, for example `v0.1.0.0`. The first release is `0.1.0.0`.

The `version:` field in `Cabal-syntax-json.cabal` is the only source of the version. Only the release task, and the maintainer in a release PR, change it and `CHANGELOG.md`.

## Making a release

The maintainer makes releases.

1. Make sure that `main` is green.
2. Run `mise run release-pr`. It reads the commits since the last release tag, sets the next version in `Cabal-syntax-json.cabal`, regenerates `CHANGELOG.md` with [git-cliff](https://git-cliff.org/), and opens a pull request titled `chore(release): VERSION`. If there are no releasable commits, it says so and does nothing.
3. Review the pull request. To increment `A`, change the version by hand in the `.cabal` file, and change the `CHANGELOG.md` header and the commit description to the same version. Edits to `CHANGELOG.md` are lost at the next release, which regenerates it; wording that must stay goes in `cliff.toml`.
4. When `ci-ok` passes, merge the pull request with rebase merge.
5. CI's `release` job sees the release commit on `main`. It publishes a GitHub release with the archives that CI built and tested, the source distribution (`cabal sdist`) of every package in `cabal.project`, the Haddock documentation of every package with a library in the format Hackage takes, their `SHA256SUMS`, and the notes from `CHANGELOG.md`. Publishing creates the tag. Then the `pages` job publishes the documentation of the release to <https://andreabedini.github.io/Cabal-syntax-json/>.
6. Verify the release:

   ```
   gh release verify vVERSION
   gh attestation verify ASSET --repo andreabedini/Cabal-syntax-json
   ```

To publish a release on Hackage too, upload its source distribution and its documentation, which were built by CI, from the GitHub release:

```
gh release download vVERSION --repo andreabedini/Cabal-syntax-json --pattern 'Cabal-syntax-json-VERSION*.tar.gz'
cabal upload --publish Cabal-syntax-json-VERSION.tar.gz
cabal upload --publish --documentation Cabal-syntax-json-VERSION-docs.tar.gz
```

Releases are immutable: a published version cannot change. If a release is wrong, fix it and make a new release. If the `release` job fails before it publishes, rerun the failed jobs; the job continues the draft release.

### Release artifacts without a release

Every CI run, on a pull request or started by hand, uploads everything that a release would publish as one artifact, `dist`: the same files and `SHA256SUMS`, without attestations. To get them for any branch without releasing:

```
gh workflow run ci.yml --ref BRANCH
gh run list --workflow ci.yml --branch BRANCH --event workflow_dispatch --limit 1
gh run download RUN_ID --name dist
```

The `release` job runs only on a push to `main`, so a manual run never releases. The `github-pages` artifact of a run is the documentation site, as a tar file.

