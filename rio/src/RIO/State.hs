-- | Provides reexports of 'MonadState' and related helpers.
--
-- @since 0.1.4.0
module RIO.State
  (
    Control.Monad.State.MonadState (..)
  , Control.Monad.State.gets
  , Control.Monad.State.modify
  , Control.Monad.State.modify'
  ) where

import qualified Control.Monad.State
