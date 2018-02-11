module RIO.Vector.Boxed.Unsafe
  (
  -- * Accessors
  -- ** Indexing
    Data.Vector.unsafeIndex
  , Data.Vector.unsafeHead
  , Data.Vector.unsafeLast

  -- ** Monadic indexing
  , Data.Vector.unsafeIndexM
  , Data.Vector.unsafeHeadM
  , Data.Vector.unsafeLastM

  -- ** Extracting subvectors
  , Data.Vector.unsafeSlice
  , Data.Vector.unsafeInit
  , Data.Vector.unsafeTail
  , Data.Vector.unsafeTake
  , Data.Vector.unsafeDrop

  -- * Modifying vectors
  -- ** Bulk updates
  , Data.Vector.unsafeUpd
  , Data.Vector.unsafeUpdate
  , Data.Vector.unsafeUpdate_

  -- ** Accumulations
  , Data.Vector.unsafeAccum
  , Data.Vector.unsafeAccumulate
  , Data.Vector.unsafeAccumulate_

  -- ** Permutations
  , Data.Vector.unsafeBackpermute

  -- * Conversions
  -- ** Mutable vectors
  , Data.Vector.unsafeFreeze
  , Data.Vector.unsafeThaw
  , Data.Vector.unsafeCopy
  ) where

import qualified Data.Vector
