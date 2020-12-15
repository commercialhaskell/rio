{-# LANGUAGE CPP #-}

-- | A contravariant tracer API.
--
-- Talk explaining the concept <https://www.youtube.com/watch?v=qzOQOmmkKEM&t=25m30s>

module RIO.Prelude.Tracer
  ( Tracer
  , contramapTracer
  , trace
  , mkTracer
  ) where

import Data.Functor.Contravariant
import GHC.Stack (HasCallStack, CallStack, SrcLoc (..), getCallStack, callStack)
import RIO.Prelude.Logger (GLogFunc)

-- | A tracer that'll log messages over time.
newtype Tracer m a = Tracer (CallStack -> a -> m ())

#if MIN_VERSION_base(4,12,0)
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Functor-Contravariant.html

-- | Use this instance to wrap sub-loggers.
--
-- The 'Contravariant' class is available in base 4.12.0.
--
-- @since XXX
instance Contravariant (Tracer m) where
  contramap = contramapTracer
  {-# INLINABLE contramap #-}
#endif

-- | Perform both sets of actions per log entry.
--
-- @since XXX
instance Applicative m => Semigroup (Tracer m msg) where
  Tracer f <> Tracer g = Tracer (\a b -> f a b *> g a b)

-- | 'mempty' peforms no logging.
--
-- @since XXX
instance Applicative m => Monoid (Tracer m msg) where
  mempty = mkTracer $ \_ _ -> pure ()
  mappend = (<>)

-- | A contramap. Use this to wrap sub-loggers.
--
-- If you are on base > 4.12.0, you can just use 'contramap'.
--
-- @since XXX
contramapTracer :: (a -> b) -> Tracer m b -> Tracer m a
contramapTracer f (Tracer io) = Tracer (\cs msg -> io cs (f msg))
{-# INLINABLE contramapTracer #-}

-- | Make a custom generic logger. With this you could, for example,
-- write to a database or a log digestion service. For example:
--
-- > mkTracer (\msg -> send (Data.Aeson.encode (JsonLog msg)))
--
-- @since XXX
mkTracer :: (CallStack -> msg -> m ()) -> Tracer m msg
mkTracer = Tracer

-- | Log a value generically.
--
-- @since XXX
trace ::
     HasCallStack
  => Tracer m a
  -> a
  -> m ()
trace (Tracer m) a = do
  m callStack a
{-# INLINABLE trace #-}
