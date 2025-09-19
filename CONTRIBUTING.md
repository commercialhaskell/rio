# Contributors Guide

Thank you for considering contributing to the maintenance or development of
`rio`, or otherwise supporting users of `rio`! We hope that the following
information will encourage and assist you.

## Continuous integration (CI)

We use [GitHub Actions](https://docs.github.com/en/actions) to do CI on `rio`.
The configuration of the workflows is in the YAML files in `.github/workflows`.
The current active workflows are:

### Linting - `lint.yml`

This workflow will run if:

* there is a pull request
* commits are pushed to these branches: `master`.

The workflow has one job (`style`). It runs on `ubuntu` only and applies
yamllint and Hlint.

### Testing - `tests.yml`

This workflow will run if:

* there is a pull request
* commits are pushed to these branches: `master`.
* requested

The workflow has one job (`build`).

The `build` job runs on a matrix of operating systems and Stack
project-level configuration files specifying different versions of GHC. It
builds and tests `rio` and `rio-orphans` with the following flags:
`--fast --no-terminal`.

The versions of GHC in the matrix are:

* those specified by the current Stackage Nightly snapshot and the most recent
  Stackage LTS Haskell snapshot, on Windows, macOS and Linux;

* the most recent minor version of all major versions of GHC that have a
  Stackage LTS Haskell snapshot first published within two years of the current
  date, on Windows and Linux (for Unix-like operating systems);

* the most recent minor version of the most recent major version of GHC not
  included above, on Linux only; and

* GHC 8.6.5 (`base-4.12.0.0`), on Linux only.

In that regard:

GHC |LTS|LTS first published
----|---|-------------------
9.10|24 |2025-07-13
9.8 |23 |2024-12-09
9.6 |22 |2023-12-16
9.4 |21 |2023-06-20

Its approach to creating a cache depends on the operating system. Its 'Cache
dependencies on Unix-like OS' step caches the Stack root on Unix-like operating
systems. Its 'Cache dependencies on Windows' step caches the same information
on Windows, but takes into account that a relevant directory is located outside
of the Stack root.
