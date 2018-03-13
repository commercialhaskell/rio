module RIO.Vector.Boxed.Partial
  (
  -- * Accessors
  -- ** Indexing
    (Data.Vector.!)
  , Data.Vector.head
  , Data.Vector.last

  -- ** Monadic indexing
  , Data.Vector.indexM
  , Data.Vector.headM
  , Data.Vector.lastM

  -- ** Extracting subvectors
  , Data.Vector.init
  , Data.Vector.tail

  -- * Modifying vectors
  -- ** Bulk updates
  , (Data.Vector.//)
  , Data.Vector.update
  , Data.Vector.update_

  -- ** Accumulations
  , Data.Vector.accum
  , Data.Vector.accumulate
  , Data.Vector.accumulate_

  -- ** Permutations
  , Data.Vector.backpermute

  -- * Folding
  , Data.Vector.foldl1
  , Data.Vector.foldl1'
  , Data.Vector.foldr1
  , Data.Vector.foldr1'

  -- ** Specialised folds
  , Data.Vector.maximum
  , Data.Vector.maximumBy
  , Data.Vector.minimum
  , Data.Vector.minimumBy
  , Data.Vector.minIndex
  , Data.Vector.minIndexBy
  , Data.Vector.maxIndex
  , Data.Vector.maxIndexBy

  -- ** Monadic folds
  , Data.Vector.fold1M
  , Data.Vector.fold1M'
  , Data.Vector.fold1M_
  , Data.Vector.fold1M'_

  -- * Prefix sums (scans)
  , Data.Vector.scanl1
  , Data.Vector.scanl1'
  , Data.Vector.scanr1
  , Data.Vector.scanr1'
  ) where

import qualified Data.Vector
