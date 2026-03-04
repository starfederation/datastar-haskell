# Publishing to Hackage

## 1. Build the source distribution

```sh
cabal sdist
```

This produces `dist-newstyle/sdist/datastar-hs-<version>.tar.gz`.

## 2. Upload as a candidate (dry run)

```sh
cabal upload dist-newstyle/sdist/datastar-hs-<version>.tar.gz
```

Review the candidate at `https://hackage.haskell.org/package/datastar-hs-<version>/candidate`.

## 3. Publish for real

```sh
cabal upload --publish dist-newstyle/sdist/datastar-hs-<version>.tar.gz
```

## 4. Upload docs

Build and upload Haddocks:

```sh
cabal haddock --haddock-for-hackage
cabal upload -d dist-newstyle/datastar-hs-<version>-docs.tar.gz
```

Use `--publish` to push docs to the published package (not the candidate):

```sh
cabal upload -d --publish dist-newstyle/datastar-hs-<version>-docs.tar.gz
```

## Authentication

Either pass `--token=TOKEN` / `--username=USERNAME --password=PASSWORD` on the command line, or let cabal prompt you (it caches credentials after the first time).
