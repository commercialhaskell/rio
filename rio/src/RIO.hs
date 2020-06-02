module RIO
  (
    -- * Custom @Prelude@

    -- | One of the core features of @rio@ is that it can be used as a @Prelude@
    -- replacement. Therefore it is best to disable the default `Prelude` with:
    -- [NoImplicitPrelude](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NoImplicitPrelude)
    -- pragma:
    --
    -- > {-# LANGUAGE NoImplicitPrelude #-}
    -- > import RIO
    --
    -- Some functions not exported here can be found in "RIO.Partial":
    -- @fromJust@, @read@, @toEnum@, @pred@, @succ@.
    --
    module RIO.Prelude
  , module RIO.Prelude.Types
    -- * The @RIO@ Monad

  , module MonadRIO
    -- ** @SimpleApp@
    -- | If all you need is just some default environment that does basic logging and allows
    -- spawning processes, then you can use `SimpleApp`:
    --
    -- > {-# LANGUAGE OverloadedStrings #-}
    -- > module Main where
    -- >
    -- > main :: IO ()
    -- > main =
    -- >   runSimpleApp $ do
    -- >     logInfo "Hello World!"
    --
    -- Note the
    -- [OverloadedStrings](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedStrings)
    -- extension, which is enabled to simplify logging.
  , module RIO.Prelude.Simple
    -- * @MonadIO@ and @MonadUnliftIO@
  , module Control.Monad.IO.Unlift
    -- * Logger
    -- $logging-intro
  , module RIO.Prelude.Logger
    -- * Display
  , module RIO.Prelude.Display
    -- * Optics
    -- | @microlens@-based Lenses, Traversals, etc.
  , module RIO.Prelude.Lens
    -- * Concurrency
  , UnliftIO.Concurrent.ThreadId
  , UnliftIO.Concurrent.myThreadId
  , UnliftIO.Concurrent.isCurrentThreadBound
  , UnliftIO.Concurrent.threadWaitRead
  , UnliftIO.Concurrent.threadWaitWrite
  , UnliftIO.Concurrent.threadDelay
  , RIO.Prelude.Renames.yieldThread
    -- ** Async
  , module UnliftIO.Async
    -- ** STM
  , module UnliftIO.STM
    -- ** Chan
  , module UnliftIO.Chan
    -- ** Timeout
  , module UnliftIO.Timeout
    -- * Exceptions
  , module UnliftIO.Exception
    -- | Re-exported from "Control.Monad.Catch":
  , Control.Monad.Catch.throwM
    -- * Files and handles
  , module UnliftIO.IO
  , module UnliftIO.Temporary
  , module RIO.Prelude.IO
    -- * Exit
  , module RIO.Prelude.Exit
    -- * Mutable Variables
    -- ** SomeRef
  , module SomeRef
    -- ** URef
  , module RIO.Prelude.URef
    -- ** IORef
  , module UnliftIO.IORef
    -- ** MVar
  , module UnliftIO.MVar
    -- ** Memoize
  , module UnliftIO.Memoize
    -- ** Deque
  , module RIO.Deque

    -- * Debugging
  , module RIO.Prelude.Trace
  ) where

import qualified Control.Monad.Catch (MonadThrow(..))
import RIO.Deque
import RIO.Prelude
import RIO.Prelude.Display
import RIO.Prelude.Exit
import RIO.Prelude.Extra
import RIO.Prelude.IO
import RIO.Prelude.Lens
import RIO.Prelude.Logger
import RIO.Prelude.Renames
import RIO.Prelude.RIO as MonadRIO (RIO(..), liftRIO, runRIO)
import RIO.Prelude.RIO as SomeRef hiding (RIO(..), liftRIO, runRIO)
import RIO.Prelude.Simple
import RIO.Prelude.Text
import RIO.Prelude.Trace
import RIO.Prelude.Types
import RIO.Prelude.URef
import Control.Monad.IO.Unlift
import UnliftIO.Async
import UnliftIO.Chan
import UnliftIO.Exception
import UnliftIO.IO
import UnliftIO.IORef
import UnliftIO.Memoize
import UnliftIO.MVar
import UnliftIO.STM
import UnliftIO.Temporary
import UnliftIO.Timeout
import UnliftIO.Concurrent

--------------------------------------------------------------------------------
-- $logging-intro
--
-- The logging system in RIO is built upon "log functions", which are
-- accessed in RIO's environment via a class like "has log
-- function". There are two provided:
--
-- * In the common case: for logging plain text (via 'Utf8Builder')
--   efficiently, there is 'LogFunc', which can be created via
--   'withLogFunc', and is accessed via 'HasLogFunc'. This provides
--   all the classical logging facilities: timestamped text output
--   with log levels and colors (if terminal-supported) to the
--   terminal. We log output via 'logInfo', 'logDebug', etc.
--
-- * In the advanced case: where logging takes on a more semantic
--   meaning and the logs need to be digested, acted upon, translated
--   or serialized upstream (to e.g. a JSON logging server), we have
--   'GLogFunc' (as in "generic log function"), and is accessed via
--   'HasGLogFunc'. In this case, we log output via 'glog'. See the
--   Type-generic logger section for more information.
