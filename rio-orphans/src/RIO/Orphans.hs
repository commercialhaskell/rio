{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Orphan instances for the 'RIO' data type.
module RIO.Orphans
  ( HasResourceMap (..)
  , ResourceMap
  , withResourceMap
  ) where

import RIO
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Resource.Internal (MonadResource (..), ReleaseMap, ResourceT (..))
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Trans.Control (MonadBaseControl (..))

import qualified Control.Monad.Logger as LegacyLogger
import Control.Monad.Logger (MonadLogger (..), LogStr)
import System.Log.FastLogger (fromLogStr)

-- | @since 0.1.0.0
deriving instance MonadCatch (RIO env)

-- | @since 0.1.0.0
deriving instance MonadMask (RIO env)

-- | @since 0.1.0.0
deriving instance MonadBase IO (RIO env)

-- | @since 0.1.0.0
instance MonadBaseControl IO (RIO env) where
  type StM (RIO env) a = a

  liftBaseWith = withRunInIO
  restoreM = return

-- | @since 0.1.1.0
instance Display LogStr where
  display = displayBytesUtf8 . fromLogStr

-- | @since 0.1.1.0
instance HasLogFunc env => MonadLogger (RIO env) where
  monadLoggerLog loc source level msg =
      logGeneric source rioLogLevel (display $ LegacyLogger.toLogStr msg)
    where
      rioLogLevel =
        case level of
          LegacyLogger.LevelDebug -> LevelDebug
          LegacyLogger.LevelInfo  -> LevelInfo
          LegacyLogger.LevelWarn  -> LevelWarn
          LegacyLogger.LevelError  -> LevelError
          LegacyLogger.LevelOther name -> LevelOther name

-- | A collection of all of the registered resource cleanup actions.
--
-- @since 0.1.0.0
type ResourceMap = IORef ReleaseMap

-- | Perform an action with a 'ResourceMap'
--
-- @since 0.1.0.0
withResourceMap :: MonadUnliftIO m => (ResourceMap -> m a) -> m a
withResourceMap inner =
  withRunInIO $ \run -> runResourceT $ ResourceT $ run . inner

-- | An environment with a 'ResourceMap'
--
-- @since 0.1.0.0
class HasResourceMap env where
  resourceMapL :: Lens' env ResourceMap
instance HasResourceMap (IORef ReleaseMap) where
  resourceMapL = id
instance HasResourceMap env => MonadResource (RIO env) where
  liftResourceT (ResourceT f) = view resourceMapL >>= liftIO . f
