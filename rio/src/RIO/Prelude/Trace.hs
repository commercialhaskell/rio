module RIO.Prelude.Trace
  ( -- * Text
    trace
  , traceId
  , traceIO
  , traceM
  , traceEvent
  , traceEventIO
  , traceMarker
  , traceMarkerIO
  , traceStack
    -- * Show
  , traceShow
  , traceShowId
  , traceShowIO
  , traceShowM
  , traceShowEvent
  , traceShowEventIO
  , traceShowMarker
  , traceShowMarkerIO
  , traceShowStack
    -- * Display
  , traceDisplay
  , traceDisplayId
  , traceDisplayIO
  , traceDisplayM
  , traceDisplayEvent
  , traceDisplayEventIO
  , traceDisplayMarker
  , traceDisplayMarkerIO
  , traceDisplayStack
  ) where

import qualified Debug.Trace as Trace

import           Control.Monad.IO.Class(MonadIO(..))
import           RIO.Prelude.Display
import           RIO.Text         (Text)
import qualified RIO.Text as Text

----------------------------------------------------
-- Text
----------------------------------------------------

{-# WARNING trace "Trace statement left in code" #-}
-- | @since 0.1.0.0
trace :: Text -> a -> a
trace = Trace.trace . Text.unpack

{-# WARNING traceId "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceId :: Text -> Text
traceId str = Trace.trace (Text.unpack str) str

{-# WARNING traceIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceIO :: MonadIO m => Text -> m ()
traceIO = liftIO . Trace.traceIO . Text.unpack

{-# WARNING traceM "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceM :: Applicative f => Text -> f ()
traceM = Trace.traceM . Text.unpack

{-# WARNING traceEvent "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceEvent :: Text -> a -> a
traceEvent = Trace.traceEvent . Text.unpack

{-# WARNING traceEventIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceEventIO :: MonadIO m => Text -> m ()
traceEventIO = liftIO . Trace.traceEventIO . Text.unpack

{-# WARNING traceMarker "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceMarker :: Text -> a -> a
traceMarker = Trace.traceMarker . Text.unpack

{-# WARNING traceMarkerIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceMarkerIO :: MonadIO m => Text -> m ()
traceMarkerIO = liftIO . Trace.traceMarkerIO . Text.unpack

{-# WARNING traceStack "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceStack :: Text -> a -> a
traceStack = Trace.traceStack . Text.unpack

----------------------------------------------------
-- Show
----------------------------------------------------

{-# WARNING traceShow "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShow :: Show a => a -> b -> b
traceShow = Trace.traceShow

{-# WARNING traceShowId "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowId :: Show a => a -> a
traceShowId = Trace.traceShowId

{-# WARNING traceShowIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowIO :: (Show a, MonadIO m) => a -> m ()
traceShowIO = liftIO . Trace.traceIO . show

{-# WARNING traceShowM "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowM :: (Show a, Applicative f) => a -> f ()
traceShowM = Trace.traceM . show

{-# WARNING traceShowEvent "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowEvent :: Show a => a -> b -> b
traceShowEvent = Trace.traceEvent . show

{-# WARNING traceShowEventIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowEventIO :: (Show a, MonadIO m) => a -> m ()
traceShowEventIO = liftIO . Trace.traceEventIO . show

{-# WARNING traceShowMarker "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowMarker :: Show a => a -> b -> b
traceShowMarker = Trace.traceMarker . show

{-# WARNING traceShowMarkerIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowMarkerIO :: (Show a, MonadIO m) => a -> m ()
traceShowMarkerIO = liftIO . Trace.traceMarkerIO . show

{-# WARNING traceShowStack "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceShowStack :: Show a => a -> b -> b
traceShowStack = Trace.traceStack . show

----------------------------------------------------
-- Display
----------------------------------------------------

{-# WARNING traceDisplay "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplay :: Display a => a -> b -> b
traceDisplay = trace . utf8BuilderToText . display

{-# WARNING traceDisplayId "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayId :: Display a => a -> a
traceDisplayId x = traceDisplay x x

{-# WARNING traceDisplayIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayIO :: (Display a, MonadIO m) => a -> m ()
traceDisplayIO = traceIO . utf8BuilderToText . display

{-# WARNING traceDisplayM "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayM :: (Display a, Applicative f) => a -> f ()
traceDisplayM = traceM . utf8BuilderToText . display

{-# WARNING traceDisplayEvent "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayEvent :: Display a => a -> b -> b
traceDisplayEvent = traceEvent . utf8BuilderToText . display

{-# WARNING traceDisplayEventIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayEventIO :: (Display a, MonadIO m) => a -> m ()
traceDisplayEventIO = traceEventIO . utf8BuilderToText . display

{-# WARNING traceDisplayMarker "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayMarker :: Display a => a -> b -> b
traceDisplayMarker = traceMarker . utf8BuilderToText . display

{-# WARNING traceDisplayMarkerIO "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayMarkerIO :: (Display a, MonadIO m) => a -> m ()
traceDisplayMarkerIO = traceMarkerIO . utf8BuilderToText . display

{-# WARNING traceDisplayStack "Trace statement left in code" #-}
-- | @since 0.1.0.0
traceDisplayStack :: Display a => a -> b -> b
traceDisplayStack = traceStack . utf8BuilderToText . display
