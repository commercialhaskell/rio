module RIO.Vector.Unboxed.Unsafe
  (
  -- * Accessors
  -- ** Indexing
    Data.Vector.Unboxed.unsafeIndex
  , Data.Vector.Unboxed.unsafeHead
  , Data.Vector.Unboxed.unsafeLast

  -- ** Monadic indexing
  , Data.Vector.Unboxed.unsafeIndexM
  , Data.Vector.Unboxed.unsafeHeadM
  , Data.Vector.Unboxed.unsafeLastM

  -- ** Extracting subvectors
  , Data.Vector.Unboxed.unsafeSlice
  , Data.Vector.Unboxed.unsafeInit
  , Data.Vector.Unboxed.unsafeTail
  , Data.Vector.Unboxed.unsafeTake
  , Data.Vector.Unboxed.unsafeDrop

  -- * Modifying vectors
  -- ** Bulk updates
  , Data.Vector.Unboxed.unsafeUpd
  , Data.Vector.Unboxed.unsafeUpdate
  , Data.Vector.Unboxed.unsafeUpdate_

  -- ** Accumulations
  , Data.Vector.Unboxed.unsafeAccum
  , Data.Vector.Unboxed.unsafeAccumulate
  , Data.Vector.Unboxed.unsafeAccumulate_

  -- ** Permutations
  , Data.Vector.Unboxed.unsafeBackpermute

  -- * Conversions
  -- ** Mutable vectors
  , Data.Vector.Unboxed.unsafeFreeze
  , Data.Vector.Unboxed.unsafeThaw
  , Data.Vector.Unboxed.unsafeCopy
  ) where

import qualified Data.Vector.Unboxed
