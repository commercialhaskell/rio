module RIO.Prelude.Lens
  ( view
  , Lens.Micro.ASetter
  , Lens.Micro.ASetter'
  , Lens.Micro.Getting
  , Lens.Micro.Lens
  , Lens.Micro.Lens'
  , Lens.Micro.SimpleGetter
  , Lens.Micro.lens
  , Lens.Micro.over
  , Lens.Micro.set
  , Lens.Micro.sets
  , Lens.Micro.to
  , (Lens.Micro.^.)
  ) where

import Lens.Micro
import Control.Monad.Reader (MonadReader, asks)
import           Lens.Micro.Internal      (( #. ))
import           Control.Applicative      (Const (..))

view :: MonadReader s m => Getting a s a -> m a
view l = asks (getConst #. l Const)
