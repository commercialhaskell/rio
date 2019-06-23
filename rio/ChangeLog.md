# Changelog for rio

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
* Move non partial functions `scanr1` and `scanl1` from `RIO.List.Partial` to `RIO.List` (issue #82)
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
