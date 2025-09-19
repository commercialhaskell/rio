# Changelog for rio

## 0.1.23.0

* Support GHC 9.14
* Re-export type operator `~`, from `base-4.17.0.0`

## 0.1.22.0

* Expose `augmentPathMap'`

## 0.1.21.0

* Fix minor bug in `augmentPathMap` on windows wrt [#234](https://github.com/commercialhaskell/rio/issues/234) not adhering to case-insensitive semantics

## 0.1.20.0

* Export `UnliftIO.QSem` and `UnliftIO.QSemN` in `RIO`

## 0.1.19.0

* Expose `fromLeft` and `fromRight`

## 0.1.18.0

* Add colours to the `LogOption` constructor [#222](https://github.com/commercialhaskell/rio/pull/222)

## 0.1.17.0

* Expose `Bifunctor`, `Bifoldable`, and `Bitraversable`.
* The `first` and `second` functions exported by `RIO` formerly originated from
  `Control.Arrow`. They now come from `Bifunctor`.

## 0.1.16.0

* Expand the number of `microlens` functions exported by the RIO prelude.
* Add new module `RIO.Lens` which provides the rest of `microlens`.

## 0.1.15.1

* Replace `canonicalizePath` with `makeAbsolute` [#217](https://github.com/commercialhaskell/rio/issues/217)

## 0.1.15.0

* Include source in log messages

## 0.1.14.1

* Support `unliftio-core` 0.2

## 0.1.14.0

* Addition of `mkSimpleApp`
* Addition of `lookupEnvFromContext`

## 0.1.13.0

* Add `withLazyFileUtf8`
* Add `mapRIO`
* Add generic logger
* Add `exeExtensions` and improve `findExecutable` on Windows [#205](https://github.com/commercialhaskell/rio/issues/205)

## 0.1.12.0

* Add `logFormat` and `setLogFormat` for `LogOptions`.

## 0.1.11.0

* Replace atomic and durable file writing functions with the ones from `unliftio`, see [#167](https://github.com/commercialhaskell/rio/pull/167)

## 0.1.10.0

* Relax a bunch of `RIO.File` functions from `MonadUnliftIO` to `MonadIO`
* Custom `Monoid` instance for `Utf8Builder` that matches semantics of the
  derived one, but doesn't break list fusion
* Qualified import recommendations for `*.Partial`, `*.Unchecked`, `*.Unsafe`
* Re-export `Data.Ord.Down` from `RIO.Prelude`
* Addition of `RIO.NonEmpty` module
* Addition of `RIO.NonEmpty.Partial` module
* Export `NonEmpty` type and its constructor `(:|)` from RIO.Prelude.Types
* Fix handling of non-ASCII characters in `logSticky`
* Deprecate `withProcess` and `withProcess_`, add `withProcessWait`, `withProcessWait_`, `withProcessTerm`, and `withProcessTerm_`

## 0.1.9.2

* Bring back re-export of `Monad.fail` from `RIO.Prelude`.

## 0.1.9.1

* Remove accidental reexport of `Control.Applicative.empty` introduced in the previous release.
* Functions from `Data.Data.Data` class are brought to the re-export list as well.

## 0.1.9.0

* Add `Prelude.Exit` to export lifted versions of the exit functions from `System.Exit`.
* Re-export the `Control.Monad.State.State` and `Control.Monad.State.StateT` types and related computation functions in `RIO.State`.
* Re-export the `Control.Monad.Writer.Writer` and `Control.Monad.Writer.WriterT` types and related computation functions in `RIO.Writer`.
* Re-export `pred`, `succ` in `RIO.Partial`.
* Add `Semigroup` and `Monoid` instances for `RIO`
* Add the `Deque` double-ended queue data type
* Re-export `Data.Map.Strict.toAscList` and `Data.Map.Strict.toDescList` from `RIO.Map`.
* Re-export `Data.Sequence.Seq` from `RIO`.
* Addition of `RIO.Prelude` module
* Addition of `RIO.Prelude.Types` module
* Re-export `zipWith` and `runST` from `RIO.Prelude`
* Re-export `Exception`, `MonadFail`, `Typeable` and `ST` from `RIO.Prelude.Types`
* Switch to `MonadFail.fail` from `Monad.fail` and re-exported it from `RIO.Prelude`


## 0.1.8.0

* Re-export `Control.Monad.State.modify`, `Control.Monad.State.modify'` and `Control.Monad.State.gets` in `RIO.State`

## 0.1.7.0

* Addition of `textDisplay` to `Display` class.

## 0.1.6.0

* Changed `logUseColor` to default to `False` on Windows, even when verbose and on the terminal
* Add `RIO.File` module which offers a family of file handling functions
  (`withBinaryFileDurable`, `withBinaryFileDurableAtomic`, among others.) with
  better durability and atomicity guarantees

## 0.1.5.0

* Re-export `Numeric.Natural.Natural` [#119](https://github.com/commercialhaskell/rio/issues/119)
* Re-export `Data.Functor.<&>` from GHC 8.4+, falling back local definition for `base < 4.11` [#117](https://github.com/commercialhaskell/rio/issues/117)
* Re-export `Data.Proxy.Proxy(..)`
* Re-export `fromEnum` from RIO, export `toEnum`, `read` and `fromJust` from RIO.Partial
* Add `noLogging` function to skip logging on specific sub-routines
* Re-export `Control.Category.>>>`

## 0.1.4.0

* Add `Const` and `Identity`
* Add `Reader` and `runReader`
* Add instances for `MonadWriter` and `MonadState` to `RIO` via mutable reference [#103](https://github.com/commercialhaskell/rio/issues/103)

## 0.1.3.0

* Add `newLogFunc` function to create `LogFunc` records outside of a callback scope
* Allow dynamic reloading of `logMinLevel` and `logVerboseFormat` for the `LogOptions` record
* Add `foldMapM`
* Add `headMaybe`, `lastMaybe`, `tailMaybe`, `initMaybe`, `maximumMaybe`, `minimumMaybe`,
  `maximumByMaybe`, `minimumByMaybe` functions to `RIO.List` module (issue #82)
* Move non-partial functions `scanr1` and `scanl1` from `RIO.List.Partial` to `RIO.List` (issue #82)
* Add `SimpleApp` and `runSimpleApp`
* Add `asIO`

## 0.1.2.0

* Allow setting usage of code location in the log output

## 0.1.1.0

* Move some accidentally included partial functions

## 0.1.0.0

* Initial stable release

## 0.0

__NOTE__ All releases beginning with 0.0 are considered
experimental. Caveat emptor!
