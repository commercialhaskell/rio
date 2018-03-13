{-# LANGUAGE CPP #-}
-- | Boxed @Vector@. Import as:
--
-- > import qualified RIO.Vector.Boxed as VB
module RIO.Vector.Boxed
  (
  -- * Boxed vectors
    Data.Vector.Vector
  , Data.Vector.MVector

  -- * Accessors
  -- ** Length information
  , Data.Vector.length
  , Data.Vector.null

  -- ** Indexing
  , (Data.Vector.!?)

  -- ** Extracting subvectors
  , Data.Vector.slice
  , Data.Vector.take
  , Data.Vector.drop
  , Data.Vector.splitAt

  -- * Construction
  -- ** Initialisation
  , Data.Vector.empty
  , Data.Vector.singleton
  , Data.Vector.replicate
  , Data.Vector.generate
  , Data.Vector.iterateN

  -- ** Monadic initialisation
  , Data.Vector.replicateM
  , Data.Vector.generateM
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.iterateNM
#endif
  , Data.Vector.create
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.createT
#endif

  -- ** Unfolding
  , Data.Vector.unfoldr
  , Data.Vector.unfoldrN
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.unfoldrM
  , Data.Vector.unfoldrNM
#endif
  , Data.Vector.constructN
  , Data.Vector.constructrN

  -- ** Enumeration
  , Data.Vector.enumFromN
  , Data.Vector.enumFromStepN
  , Data.Vector.enumFromTo
  , Data.Vector.enumFromThenTo

  -- ** Concatenation
  , Data.Vector.cons
  , Data.Vector.snoc
  , (Data.Vector.++)
  , Data.Vector.concat

  -- ** Restricting memory usage
  , Data.Vector.force

  -- * Modifying vectors
  -- ** Permutations
  , Data.Vector.reverse

  -- ** Safe destructive update
  , Data.Vector.modify

  -- * Elementwise operations
  -- ** Indexing
  , Data.Vector.indexed

  -- ** Mapping
  , Data.Vector.map
  , Data.Vector.imap
  , Data.Vector.concatMap

  -- ** Monadic mapping
  , Data.Vector.mapM
  , Data.Vector.imapM
  , Data.Vector.mapM_
  , Data.Vector.imapM_
  , Data.Vector.forM
  , Data.Vector.forM_

  -- ** Zipping
  , Data.Vector.zipWith
  , Data.Vector.zipWith3
  , Data.Vector.zipWith4
  , Data.Vector.zipWith5
  , Data.Vector.zipWith6
  , Data.Vector.izipWith
  , Data.Vector.izipWith3
  , Data.Vector.izipWith4
  , Data.Vector.izipWith5
  , Data.Vector.izipWith6
  , Data.Vector.zip
  , Data.Vector.zip3
  , Data.Vector.zip4
  , Data.Vector.zip5
  , Data.Vector.zip6

  -- ** Monadic zipping
  , Data.Vector.zipWithM
  , Data.Vector.izipWithM
  , Data.Vector.zipWithM_
  , Data.Vector.izipWithM_

  -- ** Unzipping
  , Data.Vector.unzip
  , Data.Vector.unzip3
  , Data.Vector.unzip4
  , Data.Vector.unzip5
  , Data.Vector.unzip6

  -- * Working with predicates
  -- ** Filtering
  , Data.Vector.filter
  , Data.Vector.ifilter
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.uniq
  , Data.Vector.mapMaybe
  , Data.Vector.imapMaybe
#endif
  , Data.Vector.filterM
  , Data.Vector.takeWhile
  , Data.Vector.dropWhile

  -- ** Partitioning
  , Data.Vector.partition
  , Data.Vector.unstablePartition
  , Data.Vector.span
  , Data.Vector.break

  -- ** Searching
  , Data.Vector.elem
  , Data.Vector.notElem
  , Data.Vector.find
  , Data.Vector.findIndex
  , Data.Vector.findIndices
  , Data.Vector.elemIndex
  , Data.Vector.elemIndices

  -- * Folding
  , Data.Vector.foldl
  , Data.Vector.foldl'
  , Data.Vector.foldr
  , Data.Vector.foldr'
  , Data.Vector.ifoldl
  , Data.Vector.ifoldl'
  , Data.Vector.ifoldr
  , Data.Vector.ifoldr'

  -- ** Specialised folds
  , Data.Vector.all
  , Data.Vector.any
  , Data.Vector.and
  , Data.Vector.or
  , Data.Vector.sum
  , Data.Vector.product

  -- ** Monadic folds
  , Data.Vector.foldM
  , Data.Vector.ifoldM
  , Data.Vector.foldM'
  , Data.Vector.ifoldM'
  , Data.Vector.foldM_
  , Data.Vector.ifoldM_
  , Data.Vector.foldM'_
  , Data.Vector.ifoldM'_

  -- ** Monadic sequencing
  , Data.Vector.sequence
  , Data.Vector.sequence_

  -- * Prefix sums (scans)
  , Data.Vector.prescanl
  , Data.Vector.prescanl'
  , Data.Vector.postscanl
  , Data.Vector.postscanl'
  , Data.Vector.scanl
  , Data.Vector.scanl'
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.iscanl
  , Data.Vector.iscanl'
#endif
  , Data.Vector.prescanr
  , Data.Vector.prescanr'
  , Data.Vector.postscanr
  , Data.Vector.postscanr'
  , Data.Vector.scanr
  , Data.Vector.scanr'
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.iscanr
  , Data.Vector.iscanr'
#endif

  -- * Conversions
  -- ** Lists
  , Data.Vector.toList
  , Data.Vector.fromList
  , Data.Vector.fromListN

  -- ** Different vector types
  , Data.Vector.convert

  -- ** Mutable vectors
  , Data.Vector.freeze
  , Data.Vector.thaw
  , Data.Vector.copy
  ) where

import qualified Data.Vector
