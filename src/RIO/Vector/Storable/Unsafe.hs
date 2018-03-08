module RIO.Vector.Storable.Unsafe
  (
  -- * Accessors
  -- ** Indexing
    Data.Vector.Storable.unsafeIndex
  , Data.Vector.Storable.unsafeHead
  , Data.Vector.Storable.unsafeLast

  -- ** Monadic indexing
  , Data.Vector.Storable.unsafeIndexM
  , Data.Vector.Storable.unsafeHeadM
  , Data.Vector.Storable.unsafeLastM

  -- ** Extracting subvectors
  , Data.Vector.Storable.unsafeSlice
  , Data.Vector.Storable.unsafeInit
  , Data.Vector.Storable.unsafeTail
  , Data.Vector.Storable.unsafeTake
  , Data.Vector.Storable.unsafeDrop

  -- * Modifying vectors
  -- ** Bulk updates
  , Data.Vector.Storable.unsafeUpd
  , Data.Vector.Storable.unsafeUpdate_

  -- ** Accumulations
  , Data.Vector.Storable.unsafeAccum
  , Data.Vector.Storable.unsafeAccumulate_

  -- ** Permutations
  , Data.Vector.Storable.unsafeBackpermute

  -- * Conversions
  -- ** Mutable vectors
  , Data.Vector.Storable.unsafeFreeze
  , Data.Vector.Storable.unsafeThaw
  , Data.Vector.Storable.unsafeCopy

  -- * Raw pointers
  , Data.Vector.Storable.unsafeFromForeignPtr
  , Data.Vector.Storable.unsafeFromForeignPtr0
  , Data.Vector.Storable.unsafeToForeignPtr
  , Data.Vector.Storable.unsafeToForeignPtr0
  , unsafeWith
  ) where

import Data.Vector.Storable(Storable, Vector)
import qualified Data.Vector.Storable
import Foreign.Ptr(Ptr)
import UnliftIO

-- | Lifted version of 'Data.Vector.Storable.unsafeWith'
unsafeWith :: (MonadUnliftIO m, Storable a) => Vector a -> (Ptr a -> m b) -> m b
unsafeWith vec action = withRunInIO $ \unlifter -> Data.Vector.Storable.unsafeWith vec (unlifter . action)
