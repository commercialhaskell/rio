module RIO.Prelude.Exit
  ( exitFailure
  , exitSuccess
  , exitWith
  , System.Exit.ExitCode(..)
  ) where

import           Control.Monad.IO.Class
import qualified System.Exit ( ExitCode (..)
                             , exitFailure
                             , exitSuccess
                             , exitWith
                             )

-- | Lifted version of "System.Exit.exitFailure".
--
-- @since 0.1.9.0.
exitFailure :: MonadIO m => m a
exitFailure = liftIO System.Exit.exitFailure

-- | Lifted version of "System.Exit.exitSuccess".
--
-- @since 0.1.9.0.
exitSuccess :: MonadIO m => m a
exitSuccess = liftIO System.Exit.exitSuccess

-- | Lifted version of "System.Exit.exitWith".
--
-- @since 0.1.9.0.
exitWith :: MonadIO m => System.Exit.ExitCode -> m a
exitWith code = liftIO $ System.Exit.exitWith code
