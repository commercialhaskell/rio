-- | Unboxed @Vector@ partial functions. Import as:
--
-- > import qualified RIO.Vector.Unboxed.Partial as VU'
module RIO.Vector.Unboxed.Partial
  (
  -- * Accessors
  -- ** Indexing
    (Data.Vector.Unboxed.!)
  , Data.Vector.Unboxed.head
  , Data.Vector.Unboxed.last

  -- ** Monadic indexing
  , Data.Vector.Unboxed.indexM
  , Data.Vector.Unboxed.headM
  , Data.Vector.Unboxed.lastM

  -- ** Extracting subvectors
  , Data.Vector.Unboxed.init
  , Data.Vector.Unboxed.tail
  , Data.Vector.Unboxed.slice

  -- * Modifying vectors
  -- ** Bulk updates
  , (Data.Vector.Unboxed.//)
  , Data.Vector.Unboxed.update
  , Data.Vector.Unboxed.update_

  -- ** Accumulations
  , Data.Vector.Unboxed.accum
  , Data.Vector.Unboxed.accumulate
  , Data.Vector.Unboxed.accumulate_

  -- ** Permutations
  , Data.Vector.Unboxed.backpermute

  -- * Folding
  , Data.Vector.Unboxed.foldl1
  , Data.Vector.Unboxed.foldl1'
  , Data.Vector.Unboxed.foldr1
  , Data.Vector.Unboxed.foldr1'

  -- ** Specialised folds
  , Data.Vector.Unboxed.maximum
  , Data.Vector.Unboxed.maximumBy
  , Data.Vector.Unboxed.minimum
  , Data.Vector.Unboxed.minimumBy
  , Data.Vector.Unboxed.minIndex
  , Data.Vector.Unboxed.minIndexBy
  , Data.Vector.Unboxed.maxIndex
  , Data.Vector.Unboxed.maxIndexBy

  -- ** Monadic folds
  , Data.Vector.Unboxed.fold1M
  , Data.Vector.Unboxed.fold1M'
  , Data.Vector.Unboxed.fold1M_
  , Data.Vector.Unboxed.fold1M'_

  -- * Prefix sums (scans)
  , Data.Vector.Unboxed.scanl1
  , Data.Vector.Unboxed.scanl1'
  , Data.Vector.Unboxed.scanr1
  , Data.Vector.Unboxed.scanr1'
  ) where

import qualified Data.Vector.Unboxed
