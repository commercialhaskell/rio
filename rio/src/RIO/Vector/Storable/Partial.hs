-- | Storable @Vector@ partial functions. Import as:
--
-- > import qualified RIO.Vector.Storable.Partial as VS'
module RIO.Vector.Storable.Partial
  (
  -- * Accessors
  -- ** Indexing
    (Data.Vector.Storable.!)
  , Data.Vector.Storable.head
  , Data.Vector.Storable.last

  -- ** Monadic indexing
  , Data.Vector.Storable.indexM
  , Data.Vector.Storable.headM
  , Data.Vector.Storable.lastM

  -- ** Extracting subvectors
  , Data.Vector.Storable.init
  , Data.Vector.Storable.tail
  , RIO.Vector.Partial.slice  -- Pending <https://gitlab.haskell.org/ghc/ghc/issues/17233>

  -- * Modifying vectors
  -- ** Bulk updates
  , (Data.Vector.Storable.//)
  , Data.Vector.Storable.update_

  -- ** Accumulations
  , Data.Vector.Storable.accum
  , Data.Vector.Storable.accumulate_

  -- ** Permutations
  , Data.Vector.Storable.backpermute

  -- * Folding
  , Data.Vector.Storable.foldl1
  , Data.Vector.Storable.foldl1'
  , Data.Vector.Storable.foldr1
  , Data.Vector.Storable.foldr1'

  -- ** Specialised folds
  , Data.Vector.Storable.maximum
  , Data.Vector.Storable.maximumBy
  , Data.Vector.Storable.minimum
  , Data.Vector.Storable.minimumBy
  , Data.Vector.Storable.minIndex
  , Data.Vector.Storable.minIndexBy
  , Data.Vector.Storable.maxIndex
  , Data.Vector.Storable.maxIndexBy

  -- ** Monadic folds
  , Data.Vector.Storable.fold1M
  , Data.Vector.Storable.fold1M'
  , Data.Vector.Storable.fold1M_
  , Data.Vector.Storable.fold1M'_

  -- * Prefix sums (scans)
  , Data.Vector.Storable.scanl1
  , Data.Vector.Storable.scanl1'
  , Data.Vector.Storable.scanr1
  , Data.Vector.Storable.scanr1'
  ) where

import qualified Data.Vector.Storable
import qualified RIO.Vector.Partial
