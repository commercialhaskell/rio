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

  -- * Building lists

  -- ** Scans
  , Data.List.scanl1
  , Data.List.scanr1

  -- * Indexing lists
  , (Data.List.!!)
  ) where

import qualified Data.List

