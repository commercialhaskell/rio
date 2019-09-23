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
  , slice  -- Pending <https://gitlab.haskell.org/ghc/ghc/issues/17233>

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

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Data.Vector.Generic.Vector v a
  => Int   -- ^ @i@ starting index
  -> Int   -- ^ @n@ length
  -> v a
  -> v a
slice i n v = if i > 0 && n > 0 && i + n < 0  -- `i+n` overflows
  -- Special case handling for cases when `i+n` overflows. This is
  -- required due to <https://gitlab.haskell.org/ghc/ghc/issues/17233>.
  -- Once that GHC issue is closed this function can be replaced by
  -- `Data.Vector.Generic.slice`.
  -- (Negative overflow is not an issue as an `Date.Vector.Generic.slice`
  -- throws an exception is thrown for negative arguments.)
  then error $ "slice: invalid slice ("
    ++ show i ++ ","
    ++ show n ++ ","
    ++ show (Data.Vector.Generic.length v) ++ ")"
  else Data.Vector.Generic.slice i n v
