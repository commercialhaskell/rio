-- | @List@ partial functions. Import as:
--
-- > import qualified RIO.List.Partial as L'
module RIO.List.Partial
  (
  -- * Basic functions
    Data.List.head
  , Data.List.last
  , Data.List.tail
  , Data.List.init

  -- * Reducing lists (folds)
  , Data.List.foldl1
  , Data.List.foldl1'
  , Data.List.foldr1

  -- ** Special folds
  , Data.List.maximum
  , Data.List.minimum
  , Data.List.maximumBy
  , Data.List.minimumBy

  -- * Building lists

  -- ** Scans
  --
  -- These functions are not partial, they are being exported here for legacy
  -- reasons, they may be removed from this module on a future major release
  , Data.List.scanl1
  , Data.List.scanr1

  -- * Indexing lists
  , (Data.List.!!)
  ) where

import qualified Data.List
