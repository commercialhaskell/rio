{-# LANGUAGE CPP #-}
-- | Strict @Map@. Import as:
--
-- > import qualified RIO.Map as Map
module RIO.Map
  (
  -- * Map type
    Data.Map.Strict.Map

  -- * Operators
#if MIN_VERSION_containers(0,5,9)
  , (Data.Map.Strict.!?)
#endif
  , (Data.Map.Strict.\\)

  -- * Query
  , Data.Map.Strict.null
  , Data.Map.Strict.size
  , Data.Map.Strict.member
  , Data.Map.Strict.notMember
  , Data.Map.Strict.lookup
  , Data.Map.Strict.findWithDefault
  , Data.Map.Strict.lookupLT
  , Data.Map.Strict.lookupGT
  , Data.Map.Strict.lookupLE
  , Data.Map.Strict.lookupGE

  -- * Construction
  , Data.Map.Strict.empty
  , Data.Map.Strict.singleton

  -- ** Insertion
  , Data.Map.Strict.insert
  , Data.Map.Strict.insertWith
  , Data.Map.Strict.insertWithKey
  , Data.Map.Strict.insertLookupWithKey

  -- ** Delete\/Update
  , Data.Map.Strict.delete
  , Data.Map.Strict.adjust
  , Data.Map.Strict.adjustWithKey
  , Data.Map.Strict.update
  , Data.Map.Strict.updateWithKey
  , Data.Map.Strict.updateLookupWithKey
  , Data.Map.Strict.alter
#if MIN_VERSION_containers(0,5,8)
  , Data.Map.Strict.alterF
#endif

  -- * Combine

  -- ** Union
  , Data.Map.Strict.union
  , Data.Map.Strict.unionWith
  , Data.Map.Strict.unionWithKey
  , Data.Map.Strict.unions
  , Data.Map.Strict.unionsWith

  -- ** Difference
  , Data.Map.Strict.difference
  , Data.Map.Strict.differenceWith
  , Data.Map.Strict.differenceWithKey

  -- ** Intersection
  , Data.Map.Strict.intersection
  , Data.Map.Strict.intersectionWith
  , Data.Map.Strict.intersectionWithKey

  -- ** General combining functions
  -- | See "Data.Map.Merge.Strict"

  -- ** Deprecated general combining function

  , Data.Map.Strict.mergeWithKey

  -- * Traversal
  -- ** Map
  , Data.Map.Strict.map
  , Data.Map.Strict.mapWithKey
  , Data.Map.Strict.traverseWithKey
#if MIN_VERSION_containers(0,5,8)
  , Data.Map.Strict.traverseMaybeWithKey
#endif
  , Data.Map.Strict.mapAccum
  , Data.Map.Strict.mapAccumWithKey
  , Data.Map.Strict.mapAccumRWithKey
  , Data.Map.Strict.mapKeys
  , Data.Map.Strict.mapKeysWith

  -- * Folds
  , Data.Map.Strict.foldr
  , Data.Map.Strict.foldl
  , Data.Map.Strict.foldrWithKey
  , Data.Map.Strict.foldlWithKey
  , Data.Map.Strict.foldMapWithKey

  -- ** Strict folds
  , Data.Map.Strict.foldr'
  , Data.Map.Strict.foldl'
  , Data.Map.Strict.foldrWithKey'
  , Data.Map.Strict.foldlWithKey'

  -- * Conversion
  , Data.Map.Strict.elems
  , Data.Map.Strict.keys
  , Data.Map.Strict.assocs
  , Data.Map.Strict.keysSet
  , Data.Map.Strict.fromSet

  -- ** Lists
  , Data.Map.Strict.toList
  , Data.Map.Strict.fromList
  , Data.Map.Strict.fromListWith
  , Data.Map.Strict.fromListWithKey

  -- * Filter
  , Data.Map.Strict.filter
  , Data.Map.Strict.filterWithKey
#if MIN_VERSION_containers(0,5,8)
  , Data.Map.Strict.restrictKeys
  , Data.Map.Strict.withoutKeys
#endif
  , Data.Map.Strict.partition
  , Data.Map.Strict.partitionWithKey

#if MIN_VERSION_containers(0,5,8)
  , Data.Map.Strict.takeWhileAntitone
  , Data.Map.Strict.dropWhileAntitone
  , Data.Map.Strict.spanAntitone
#endif

  , Data.Map.Strict.mapMaybe
  , Data.Map.Strict.mapMaybeWithKey
  , Data.Map.Strict.mapEither
  , Data.Map.Strict.mapEitherWithKey

  , Data.Map.Strict.split
  , Data.Map.Strict.splitLookup
  , Data.Map.Strict.splitRoot

  -- * Submap
  , Data.Map.Strict.isSubmapOf
  , Data.Map.Strict.isSubmapOfBy
  , Data.Map.Strict.isProperSubmapOf
  , Data.Map.Strict.isProperSubmapOfBy

  -- * Indexed
  , Data.Map.Strict.lookupIndex
  , Data.Map.Strict.elemAt
  , Data.Map.Strict.deleteAt
#if MIN_VERSION_containers(0,5,8)
  , Data.Map.Strict.take
  , Data.Map.Strict.drop
  , Data.Map.Strict.splitAt
#endif

  -- * Min\/Max
#if MIN_VERSION_containers(0,5,9)
  , Data.Map.Strict.lookupMin
  , Data.Map.Strict.lookupMax
#endif
  , Data.Map.Strict.deleteMin
  , Data.Map.Strict.deleteMax
  , Data.Map.Strict.updateMin
  , Data.Map.Strict.updateMax
  , Data.Map.Strict.updateMinWithKey
  , Data.Map.Strict.updateMaxWithKey
  , Data.Map.Strict.minView
  , Data.Map.Strict.maxView
  , Data.Map.Strict.minViewWithKey
  , Data.Map.Strict.maxViewWithKey

  -- * Debugging
  , Data.Map.Strict.showTree
  , Data.Map.Strict.showTreeWith
  , Data.Map.Strict.valid
  ) where

import qualified Data.Map.Strict
