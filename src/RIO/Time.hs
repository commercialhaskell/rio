module RIO.Time
  ( module Data.Time
  , getCurrentTime
  , getTimeZone
  , getCurrentTimeZone
  , getZonedTime
  , utcToLocalZonedTime
  ) where

import Control.Monad.IO.Class
import Data.Time hiding( getCurrentTime, getTimeZone, getCurrentTimeZone
                       , getZonedTime, utcToLocalZonedTime)
import qualified Data.Time

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

getTimeZone :: MonadIO m => UTCTime -> m TimeZone
getTimeZone = liftIO . Data.Time.getTimeZone

getCurrentTimeZone :: MonadIO m => m TimeZone
getCurrentTimeZone = liftIO Data.Time.getCurrentTimeZone

getZonedTime :: MonadIO m => m ZonedTime
getZonedTime = liftIO Data.Time.getZonedTime

utcToLocalZonedTime :: MonadIO m => UTCTime -> m ZonedTime
utcToLocalZonedTime = liftIO . Data.Time.utcToLocalZonedTime
