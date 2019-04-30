-- | Generic @Vector@ interface partial functions. Import as:
--
-- > import qualified RIO.Vector.Partial as V'
module RIO.Vector.Partial
  (
  -- * Accessors
  -- ** Indexing
    (Data.Vector.Generic.!)
  , Data.Vector.Generic.head
  , Data.Vector.Generic.last

  -- ** Monadic indexing
  , Data.Vector.Generic.indexM
  , Data.Vector.Generic.headM
  , Data.Vector.Generic.lastM

  -- ** Extracting subvectors
  , Data.Vector.Generic.init
  , Data.Vector.Generic.tail

  -- * Modifying vectors
  -- ** Bulk updates
  , (Data.Vector.Generic.//)
  , Data.Vector.Generic.update
  , Data.Vector.Generic.update_

  -- ** Accumulations
  , Data.Vector.Generic.accum
  , Data.Vector.Generic.accumulate
  , Data.Vector.Generic.accumulate_

  -- ** Permutations
  , Data.Vector.Generic.backpermute

  -- * Folding
  , Data.Vector.Generic.foldl1
  , Data.Vector.Generic.foldl1'
  , Data.Vector.Generic.foldr1
  , Data.Vector.Generic.foldr1'

  -- ** Specialised folds
  , Data.Vector.Generic.maximum
  , Data.Vector.Generic.maximumBy
  , Data.Vector.Generic.minimum
  , Data.Vector.Generic.minimumBy
  , Data.Vector.Generic.minIndex
  , Data.Vector.Generic.minIndexBy
  , Data.Vector.Generic.maxIndex
  , Data.Vector.Generic.maxIndexBy

  -- ** Monadic folds
  , Data.Vector.Generic.fold1M
  , Data.Vector.Generic.fold1M'
  , Data.Vector.Generic.fold1M_
  , Data.Vector.Generic.fold1M'_

  -- * Prefix sums (scans)
  , Data.Vector.Generic.scanl1
  , Data.Vector.Generic.scanl1'
  , Data.Vector.Generic.scanr1
  , Data.Vector.Generic.scanr1'
  ) where

import qualified Data.Vector.Generic
