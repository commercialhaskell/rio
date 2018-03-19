module RIO.Prelude.Trace where

import qualified Debug.Trace as Trace

import           Control.Monad.IO.Class(MonadIO(..))
import           RIO.Prelude.Display
import           RIO.Text         (Text)
import qualified RIO.Text as Text

{-# WARNING trace "Trace statement left in code" #-}
trace :: Text -> a -> a
trace = Trace.trace . Text.unpack

{-# WARNING traceId "Trace statement left in code" #-}
traceId :: Text -> Text
traceId str = Trace.trace (Text.unpack str) str

{-# WARNING traceUtf8Builder "Trace statement left in code" #-}
traceUtf8Builder :: Utf8Builder -> a -> a
traceUtf8Builder = trace . utf8BuilderToText

{-# WARNING traceShow "Trace statement left in code" #-}
traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

{-# WARNING traceShowId "Trace statement left in code" #-}
traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

{-# WARNING traceDisplay "Trace statement left in code" #-}
traceDisplay :: Display a => a -> b -> b
traceDisplay = traceUtf8Builder . display

{-# WARNING traceDisplayId "Trace statement left in code" #-}
traceDisplayId :: Display a => a -> a
traceDisplayId x = traceDisplay x x

{-# WARNING traceStack "Trace statement left in code" #-}
traceStack :: Text -> a -> a
traceStack = Trace.traceStack . Text.unpack

{-# WARNING traceIO "Trace statement left in code" #-}
-- Does this even need a warning?
traceIO :: MonadIO m => Text -> m ()
traceIO = liftIO . Trace.traceIO . Text.unpack

{-# WARNING traceM "Trace statement left in code" #-}
traceM :: Applicative f => Text -> f ()
traceM = Trace.traceM . Text.unpack

{-# WARNING traceShowM "Trace statement left in code" #-}
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceShowM

{-# WARNING traceEvent "Trace statement left in code" #-}
traceEvent :: Text -> a -> a
traceEvent = Trace.traceEvent . Text.unpack

{-# WARNING traceEventIO "Trace statement left in code" #-}
-- Does this even need a warning?
traceEventIO :: MonadIO m => Text -> m ()
traceEventIO = liftIO . Trace.traceEventIO . Text.unpack

{-# WARNING traceMarker "Trace statement left in code" #-}
traceMarker :: Text -> a -> a
traceMarker = Trace.traceMarker . Text.unpack

{-# WARNING traceMarkerIO "Trace statement left in code" #-}
-- Does this even need a warning?
traceMarkerIO :: MonadIO m => Text -> m ()
traceMarkerIO = liftIO . Trace.traceMarkerIO . Text.unpack
