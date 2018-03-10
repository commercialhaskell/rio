-- | @Set@ with hashed members. Import as:
--
-- > import qualified RIO.HashSet as HS
module RIO.HashSet
  (
    Data.HashSet.HashSet

  -- * Construction
  , Data.HashSet.empty
  , Data.HashSet.singleton

  -- * Combine
  , Data.HashSet.union
  , Data.HashSet.unions

  -- * Basic interface
  , Data.HashSet.null
  , Data.HashSet.size
  , Data.HashSet.member
  , Data.HashSet.insert
  , Data.HashSet.delete

  -- * Transformations
  , Data.HashSet.map

    -- * Difference and intersection
  , Data.HashSet.difference
  , Data.HashSet.intersection

  -- * Folds
  , Data.HashSet.foldl'
  , Data.HashSet.foldr

  -- * Filter
  , Data.HashSet.filter

  -- * Conversions

  -- ** Lists
  , Data.HashSet.toList
  , Data.HashSet.fromList

  -- * HashMaps
  , Data.HashSet.toMap
  , Data.HashSet.fromMap
  ) where

import qualified Data.HashSet
