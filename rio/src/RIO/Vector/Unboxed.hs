{-# LANGUAGE CPP #-}

-- | Unboxed @Vector@. Import as:
--
-- > import qualified RIO.Vector.Unboxed as VU
--
-- This module does not export any partial or unsafe functions.  For those, see
-- "RIO.Vector.Unboxed.Partial" and "RIO.Vector.Unboxed.Unsafe"
module RIO.Vector.Unboxed
  (
  -- * Unboxed vectors
    Data.Vector.Unboxed.Vector
  , Data.Vector.Unboxed.MVector(..)
  , Data.Vector.Unboxed.Unbox

  -- * Accessors
  -- ** Length information
  , Data.Vector.Unboxed.length
  , Data.Vector.Unboxed.null

  -- ** Indexing
  , (Data.Vector.Unboxed.!?)

  -- ** Extracting subvectors
  , Data.Vector.Unboxed.take
  , Data.Vector.Unboxed.drop
  , Data.Vector.Unboxed.splitAt

  -- * Construction
  -- ** Initialisation
  , Data.Vector.Unboxed.empty
  , Data.Vector.Unboxed.singleton
  , Data.Vector.Unboxed.replicate
  , Data.Vector.Unboxed.generate
  , Data.Vector.Unboxed.iterateN

  -- ** Monadic initialisation
  , Data.Vector.Unboxed.replicateM
  , Data.Vector.Unboxed.generateM
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Unboxed.iterateNM
#endif
  , Data.Vector.Unboxed.create
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Unboxed.createT
#endif

  -- ** Unfolding
  , Data.Vector.Unboxed.unfoldr
  , Data.Vector.Unboxed.unfoldrN
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Unboxed.unfoldrM
  , Data.Vector.Unboxed.unfoldrNM
#endif
  , Data.Vector.Unboxed.constructN
  , Data.Vector.Unboxed.constructrN

  -- ** Enumeration
  , Data.Vector.Unboxed.enumFromN
  , Data.Vector.Unboxed.enumFromStepN
  , Data.Vector.Unboxed.enumFromTo
  , Data.Vector.Unboxed.enumFromThenTo

  -- ** Concatenation
  , Data.Vector.Unboxed.cons
  , Data.Vector.Unboxed.snoc
  , (Data.Vector.Unboxed.++)
  , Data.Vector.Unboxed.concat

  -- ** Restricting memory usage
  , Data.Vector.Unboxed.force

  -- * Modifying vectors
  -- ** Permutations
  , Data.Vector.Unboxed.reverse

  -- ** Safe destructive update
  , Data.Vector.Unboxed.modify

  -- * Elementwise operations
  -- ** Indexing
  , Data.Vector.Unboxed.indexed

  -- ** Mapping
  , Data.Vector.Unboxed.map
  , Data.Vector.Unboxed.imap
  , Data.Vector.Unboxed.concatMap

  -- ** Monadic mapping
  , Data.Vector.Unboxed.mapM
  , Data.Vector.Unboxed.imapM
  , Data.Vector.Unboxed.mapM_
  , Data.Vector.Unboxed.imapM_
  , Data.Vector.Unboxed.forM
  , Data.Vector.Unboxed.forM_

  -- ** Zipping
  , Data.Vector.Unboxed.zipWith
  , Data.Vector.Unboxed.zipWith3
  , Data.Vector.Unboxed.zipWith4
  , Data.Vector.Unboxed.zipWith5
  , Data.Vector.Unboxed.zipWith6
  , Data.Vector.Unboxed.izipWith
  , Data.Vector.Unboxed.izipWith3
  , Data.Vector.Unboxed.izipWith4
  , Data.Vector.Unboxed.izipWith5
  , Data.Vector.Unboxed.izipWith6
  , Data.Vector.Unboxed.zip
  , Data.Vector.Unboxed.zip3
  , Data.Vector.Unboxed.zip4
  , Data.Vector.Unboxed.zip5
  , Data.Vector.Unboxed.zip6

  -- ** Monadic zipping
  , Data.Vector.Unboxed.zipWithM
  , Data.Vector.Unboxed.izipWithM
  , Data.Vector.Unboxed.zipWithM_
  , Data.Vector.Unboxed.izipWithM_

  -- ** Unzipping
  , Data.Vector.Unboxed.unzip
  , Data.Vector.Unboxed.unzip3
  , Data.Vector.Unboxed.unzip4
  , Data.Vector.Unboxed.unzip5
  , Data.Vector.Unboxed.unzip6

  -- * Working with predicates
  -- ** Filtering
  , Data.Vector.Unboxed.filter
  , Data.Vector.Unboxed.ifilter
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Unboxed.uniq
  , Data.Vector.Unboxed.mapMaybe
  , Data.Vector.Unboxed.imapMaybe
#endif
  , Data.Vector.Unboxed.filterM
  , Data.Vector.Unboxed.takeWhile
  , Data.Vector.Unboxed.dropWhile

  -- ** Partitioning
  , Data.Vector.Unboxed.partition
  , Data.Vector.Unboxed.unstablePartition
  , Data.Vector.Unboxed.span
  , Data.Vector.Unboxed.break

  -- ** Searching
  , Data.Vector.Unboxed.elem
  , Data.Vector.Unboxed.notElem
  , Data.Vector.Unboxed.find
  , Data.Vector.Unboxed.findIndex
  , Data.Vector.Unboxed.findIndices
  , Data.Vector.Unboxed.elemIndex
  , Data.Vector.Unboxed.elemIndices

  -- * Folding
  , Data.Vector.Unboxed.foldl
  , Data.Vector.Unboxed.foldl'
  , Data.Vector.Unboxed.foldr
  , Data.Vector.Unboxed.foldr'
  , Data.Vector.Unboxed.ifoldl
  , Data.Vector.Unboxed.ifoldl'
  , Data.Vector.Unboxed.ifoldr
  , Data.Vector.Unboxed.ifoldr'

  -- ** Specialised folds
  , Data.Vector.Unboxed.all
  , Data.Vector.Unboxed.any
  , Data.Vector.Unboxed.and
  , Data.Vector.Unboxed.or
  , Data.Vector.Unboxed.sum
  , Data.Vector.Unboxed.product

  -- ** Monadic folds
  , Data.Vector.Unboxed.foldM
  , Data.Vector.Unboxed.ifoldM
  , Data.Vector.Unboxed.foldM'
  , Data.Vector.Unboxed.ifoldM'
  , Data.Vector.Unboxed.foldM_
  , Data.Vector.Unboxed.ifoldM_
  , Data.Vector.Unboxed.foldM'_
  , Data.Vector.Unboxed.ifoldM'_

  -- * Prefix sums (scans)
  , Data.Vector.Unboxed.prescanl
  , Data.Vector.Unboxed.prescanl'
  , Data.Vector.Unboxed.postscanl
  , Data.Vector.Unboxed.postscanl'
  , Data.Vector.Unboxed.scanl
  , Data.Vector.Unboxed.scanl'
  , Data.Vector.Unboxed.prescanr
  , Data.Vector.Unboxed.prescanr'
  , Data.Vector.Unboxed.postscanr
  , Data.Vector.Unboxed.postscanr'
  , Data.Vector.Unboxed.scanr
  , Data.Vector.Unboxed.scanr'

  -- * Conversions
  -- ** Lists
  , Data.Vector.Unboxed.toList
  , Data.Vector.Unboxed.fromList
  , Data.Vector.Unboxed.fromListN

  -- ** Different vector types
  , Data.Vector.Unboxed.convert

  -- ** Mutable vectors
  , Data.Vector.Unboxed.freeze
  , Data.Vector.Unboxed.thaw
  , Data.Vector.Unboxed.copy
  ) where

import qualified Data.Vector.Unboxed
