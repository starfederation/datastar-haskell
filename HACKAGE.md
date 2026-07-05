# Publishing to Hackage

The repository holds several packages. Released to Hackage:

- `datastar-hs` (core, at the repository root)
- `datastar-hs-zlib`
- `datastar-hs-brotli`

Not released: `datastar-hs-zstd` (depends on an unreleased hs-zstd, see
[#3](https://github.com/starfederation/datastar-haskell/issues/3)),
`datastar-hs-examples`, and `datastar-hs-bench` (repo-internal).

Release the core first when its version bumped. Also bump the lower bound
of `datastar-hs` in each sub-package.

## 1. Build the source distributions

```sh
cabal sdist . datastar-hs-zlib datastar-hs-brotli
```

The core must be named as `.` — `cabal sdist datastar-hs` trips over a cabal
target-resolution bug (Cabal-7151, "component library cannot be packaged")
now that sibling packages share the name as a prefix. Don't use
`cabal sdist all`: it also tarballs the unreleased packages and the pinned
hs-zstd checkout.

This produces `dist-newstyle/sdist/<package>-<version>.tar.gz`.

## 2. Upload as a candidate (dry run)

```sh
cabal upload dist-newstyle/sdist/<package>-<version>.tar.gz
```

Review the candidate at `https://hackage.haskell.org/package/<package>-<version>/candidate`.

## 3. Publish for real

```sh
cabal upload --publish dist-newstyle/sdist/<package>-<version>.tar.gz
```

## 4. Upload docs

Build and upload Haddocks (repeat per released package):

```sh
cabal haddock --haddock-for-hackage <package>
cabal upload -d dist-newstyle/<package>-<version>-docs.tar.gz
```

Use `--publish` to push docs to the published package (not the candidate):

```sh
cabal upload -d --publish dist-newstyle/<package>-<version>-docs.tar.gz
```

## Authentication

Either pass `--token=TOKEN` / `--username=USERNAME --password=PASSWORD` on the command line, or let cabal prompt you (it caches credentials after the first time).
