# Changelog for rio

## 0.1.3.0

* Add `newLogFunc` function to create `LogFunc` records outside of a callback scope
* Add `logGenericCallStack` to be able to register a log entry `CallStack` from a
  different thread
* Add `getLogMinLevel` to gather what is the log min level from `LogOptions`
* Allow dynamic reloading of `logMinLevel` and `logVerboseFormat` for the `LogOptions` record

## 0.1.2.0

* Allow setting usage of code location in the log output

## 0.1.1.0

* Move some accidentally included partial functions

## 0.1.0.0

* Initial stable release

## 0.0

__NOTE__ All releases beginning with 0.0 are considered
experimental. Caveat emptor!
