module RIO.Vector.Unsafe
  (
  -- * Immutable vectors
    Data.Vector.Generic.Vector(..)

  -- * Accessors
  -- ** Indexing
  , Data.Vector.Generic.unsafeIndex
  , Data.Vector.Generic.unsafeHead
  , Data.Vector.Generic.unsafeLast

  -- ** Monadic indexing
  , Data.Vector.Generic.unsafeIndexM
  , Data.Vector.Generic.unsafeHeadM
  , Data.Vector.Generic.unsafeLastM

  -- ** Extracting subvectors
  , Data.Vector.Generic.unsafeSlice
  , Data.Vector.Generic.unsafeInit
  , Data.Vector.Generic.unsafeTail
  , Data.Vector.Generic.unsafeTake
  , Data.Vector.Generic.unsafeDrop

  -- * Modifying vectors
  -- ** Bulk updates
  , Data.Vector.Generic.unsafeUpd
  , Data.Vector.Generic.unsafeUpdate
  , Data.Vector.Generic.unsafeUpdate_

  -- ** Accumulations
  , Data.Vector.Generic.unsafeAccum
  , Data.Vector.Generic.unsafeAccumulate
  , Data.Vector.Generic.unsafeAccumulate_

  -- ** Permutations
  , Data.Vector.Generic.unsafeBackpermute

  -- * Conversions
  -- ** Mutable vectors
  , Data.Vector.Generic.unsafeFreeze
  , Data.Vector.Generic.unsafeThaw
  , Data.Vector.Generic.unsafeCopy
  ) where

import qualified Data.Vector.Generic
