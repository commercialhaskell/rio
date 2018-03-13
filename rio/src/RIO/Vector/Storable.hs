{-# LANGUAGE CPP #-}
-- | Storable @Vector@. Import as:
--
-- > import qualified RIO.Vector.Storable as VS
module RIO.Vector.Storable
  (
  -- * Storable vectors
    Data.Vector.Storable.Vector
  , Data.Vector.Storable.MVector(..)
  , Data.Vector.Storable.Storable

  -- * Accessors
  -- ** Length information
  , Data.Vector.Storable.length
  , Data.Vector.Storable.null

  -- ** Indexing
  , (Data.Vector.Storable.!?)

  -- ** Extracting subvectors
  , Data.Vector.Storable.slice
  , Data.Vector.Storable.take
  , Data.Vector.Storable.drop
  , Data.Vector.Storable.splitAt

  -- * Construction
  -- ** Initialisation
  , Data.Vector.Storable.empty
  , Data.Vector.Storable.singleton
  , Data.Vector.Storable.replicate
  , Data.Vector.Storable.generate
  , Data.Vector.Storable.iterateN

  -- ** Monadic initialisation
  , Data.Vector.Storable.replicateM
  , Data.Vector.Storable.generateM
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Storable.iterateNM
#endif
  , Data.Vector.Storable.create
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Storable.createT
#endif

  -- ** Unfolding
  , Data.Vector.Storable.unfoldr
  , Data.Vector.Storable.unfoldrN
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Storable.unfoldrM
  , Data.Vector.Storable.unfoldrNM
#endif
  , Data.Vector.Storable.constructN
  , Data.Vector.Storable.constructrN

  -- ** Enumeration
  , Data.Vector.Storable.enumFromN
  , Data.Vector.Storable.enumFromStepN
  , Data.Vector.Storable.enumFromTo
  , Data.Vector.Storable.enumFromThenTo

  -- ** Concatenation
  , Data.Vector.Storable.cons
  , Data.Vector.Storable.snoc
  , (Data.Vector.Storable.++)
  , Data.Vector.Storable.concat

  -- ** Restricting memory usage
  , Data.Vector.Storable.force

  -- * Modifying vectors
  -- ** Permutations
  , Data.Vector.Storable.reverse

  -- ** Safe destructive update
  , Data.Vector.Storable.modify

  -- * Elementwise operations
  -- ** Mapping
  , Data.Vector.Storable.map
  , Data.Vector.Storable.imap
  , Data.Vector.Storable.concatMap

  -- ** Monadic mapping
  , Data.Vector.Storable.mapM
  , Data.Vector.Storable.mapM_
  , Data.Vector.Storable.forM
  , Data.Vector.Storable.forM_

  -- ** Zipping
  , Data.Vector.Storable.zipWith
  , Data.Vector.Storable.zipWith3
  , Data.Vector.Storable.zipWith4
  , Data.Vector.Storable.zipWith5
  , Data.Vector.Storable.zipWith6
  , Data.Vector.Storable.izipWith
  , Data.Vector.Storable.izipWith3
  , Data.Vector.Storable.izipWith4
  , Data.Vector.Storable.izipWith5
  , Data.Vector.Storable.izipWith6

  -- ** Monadic zipping
  , Data.Vector.Storable.zipWithM
  , Data.Vector.Storable.zipWithM_

  -- * Working with predicates
  -- ** Filtering
  , Data.Vector.Storable.filter
  , Data.Vector.Storable.ifilter
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Storable.uniq
  , Data.Vector.Storable.mapMaybe
  , Data.Vector.Storable.imapMaybe
#endif
  , Data.Vector.Storable.filterM
  , Data.Vector.Storable.takeWhile
  , Data.Vector.Storable.dropWhile

  -- ** Partitioning
  , Data.Vector.Storable.partition
  , Data.Vector.Storable.unstablePartition
  , Data.Vector.Storable.span
  , Data.Vector.Storable.break

  -- ** Searching
  , Data.Vector.Storable.elem
  , Data.Vector.Storable.notElem
  , Data.Vector.Storable.find
  , Data.Vector.Storable.findIndex
  , Data.Vector.Storable.findIndices
  , Data.Vector.Storable.elemIndex
  , Data.Vector.Storable.elemIndices

  -- * Folding
  , Data.Vector.Storable.foldl
  , Data.Vector.Storable.foldl'
  , Data.Vector.Storable.foldr
  , Data.Vector.Storable.foldr'
  , Data.Vector.Storable.ifoldl
  , Data.Vector.Storable.ifoldl'
  , Data.Vector.Storable.ifoldr
  , Data.Vector.Storable.ifoldr'

  -- ** Specialised folds
  , Data.Vector.Storable.all
  , Data.Vector.Storable.any
  , Data.Vector.Storable.and
  , Data.Vector.Storable.or
  , Data.Vector.Storable.sum
  , Data.Vector.Storable.product

  -- ** Monadic folds
  , Data.Vector.Storable.foldM
  , Data.Vector.Storable.foldM'
  , Data.Vector.Storable.foldM_
  , Data.Vector.Storable.foldM'_

  -- * Prefix sums (scans)
  , Data.Vector.Storable.prescanl
  , Data.Vector.Storable.prescanl'
  , Data.Vector.Storable.postscanl
  , Data.Vector.Storable.postscanl'
  , Data.Vector.Storable.scanl
  , Data.Vector.Storable.scanl'
  , Data.Vector.Storable.prescanr
  , Data.Vector.Storable.prescanr'
  , Data.Vector.Storable.postscanr
  , Data.Vector.Storable.postscanr'
  , Data.Vector.Storable.scanr
  , Data.Vector.Storable.scanr'

  -- * Conversions
  -- ** Lists
  , Data.Vector.Storable.toList
  , Data.Vector.Storable.fromList
  , Data.Vector.Storable.fromListN

  -- ** Different vector types
  , Data.Vector.Storable.convert

  -- ** Mutable vectors
  , Data.Vector.Storable.freeze
  , Data.Vector.Storable.thaw
  , Data.Vector.Storable.copy
  ) where

import qualified Data.Vector.Storable
