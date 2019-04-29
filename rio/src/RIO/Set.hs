{-# LANGUAGE CPP #-}
-- | @Set@. Import as:
--
-- > import qualified RIO.Set as Set
--
-- This module does not export any partial or unchecked functions.  For those,
-- see "RIO.Set.Partial" and "RIO.Set.Unchecked"
module RIO.Set
  (
  -- * Set type
    Data.Set.Set

  -- * Operators
  , (Data.Set.\\)

  -- * Query
  , Data.Set.null
  , Data.Set.size
  , Data.Set.member
  , Data.Set.notMember
  , Data.Set.lookupLT
  , Data.Set.lookupGT
  , Data.Set.lookupLE
  , Data.Set.lookupGE
  , Data.Set.isSubsetOf
  , Data.Set.isProperSubsetOf

  -- * Construction
  , Data.Set.empty
  , Data.Set.singleton
  , Data.Set.insert
  , Data.Set.delete

  -- * Combine
  , Data.Set.union
  , Data.Set.unions
  , Data.Set.difference
  , Data.Set.intersection

  -- * Filter
  , Data.Set.filter
#if MIN_VERSION_containers(0,5,8)
  , Data.Set.takeWhileAntitone
  , Data.Set.dropWhileAntitone
  , Data.Set.spanAntitone
#endif
  , Data.Set.partition
  , Data.Set.split
  , Data.Set.splitMember
  , Data.Set.splitRoot

  -- * Indexed
  , Data.Set.lookupIndex
#if MIN_VERSION_containers(0,5,8)
  , Data.Set.take
  , Data.Set.drop
  , Data.Set.splitAt
#endif

  -- * Map
  , Data.Set.map

  -- * Folds
  , Data.Set.foldr
  , Data.Set.foldl
  -- ** Strict folds
  , Data.Set.foldr'
  , Data.Set.foldl'

  -- * Min\/Max
#if MIN_VERSION_containers(0,5,9)
  , Data.Set.lookupMin
  , Data.Set.lookupMax
#endif
  , Data.Set.deleteMin
  , Data.Set.deleteMax
  , Data.Set.maxView
  , Data.Set.minView

  -- * Conversion

  -- ** List
  , Data.Set.elems
  , Data.Set.toList
  , Data.Set.fromList

  -- ** Ordered list
  , Data.Set.toAscList
  , Data.Set.toDescList

  -- * Debugging
  , Data.Set.showTree
  , Data.Set.showTreeWith
  , Data.Set.valid
  ) where

import qualified Data.Set
