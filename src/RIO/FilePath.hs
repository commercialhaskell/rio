module RIO.FilePath
  ( module System.FilePath
  , getSearchPath
  ) where

import Control.Monad.IO.Class
import System.FilePath hiding(getSearchPath)
import qualified System.FilePath

-- | Lifted version of 'System.FilePath.getSearchPath'
getSearchPath :: MonadIO m => m [FilePath]
getSearchPath = liftIO System.FilePath.getSearchPath
