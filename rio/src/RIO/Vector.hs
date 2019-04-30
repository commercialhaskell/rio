{-# LANGUAGE CPP #-}

-- | Generic @Vector@ interface. Import as:
--
-- > import qualified RIO.Vector as V
--
-- This module does not export any partial or unsafe functions.  For those, see
-- "RIO.Vector.Partial" and "RIO.Vector.Unsafe"
module RIO.Vector
  (
  -- * Immutable vectors
    Data.Vector.Generic.Vector

  -- * Accessors
  -- ** Length information
  , Data.Vector.Generic.length
  , Data.Vector.Generic.null

  -- ** Indexing
  , (Data.Vector.Generic.!?)

  -- ** Extracting subvectors
  , Data.Vector.Generic.slice
  , Data.Vector.Generic.take
  , Data.Vector.Generic.drop
  , Data.Vector.Generic.splitAt

  -- * Construction
  -- ** Initialisation
  , Data.Vector.Generic.empty
  , Data.Vector.Generic.singleton
  , Data.Vector.Generic.replicate
  , Data.Vector.Generic.generate
  , Data.Vector.Generic.iterateN

  -- ** Monadic initialisation
  , Data.Vector.Generic.replicateM
  , Data.Vector.Generic.generateM
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.iterateNM
#endif
  , Data.Vector.Generic.create
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.createT
#endif

  -- ** Unfolding
  , Data.Vector.Generic.unfoldr
  , Data.Vector.Generic.unfoldrN
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.unfoldrM
  , Data.Vector.Generic.unfoldrNM
#endif
  , Data.Vector.Generic.constructN
  , Data.Vector.Generic.constructrN

  -- ** Enumeration
  , Data.Vector.Generic.enumFromN
  , Data.Vector.Generic.enumFromStepN
  , Data.Vector.Generic.enumFromTo
  , Data.Vector.Generic.enumFromThenTo

  -- ** Concatenation
  , Data.Vector.Generic.cons
  , Data.Vector.Generic.snoc
  , (Data.Vector.Generic.++)
  , Data.Vector.Generic.concat
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.concatNE
#endif

  -- ** Restricting memory usage
  , Data.Vector.Generic.force

  -- * Modifying vectors
  -- ** Permutations
  , Data.Vector.Generic.reverse

  -- ** Safe destructive update
  , Data.Vector.Generic.modify

  -- * Elementwise operations
  -- ** Indexing
  , Data.Vector.Generic.indexed

  -- ** Mapping
  , Data.Vector.Generic.map
  , Data.Vector.Generic.imap
  , Data.Vector.Generic.concatMap

  -- ** Monadic mapping
  , Data.Vector.Generic.mapM
  , Data.Vector.Generic.imapM
  , Data.Vector.Generic.mapM_
  , Data.Vector.Generic.imapM_
  , Data.Vector.Generic.forM
  , Data.Vector.Generic.forM_

  -- ** Zipping
  , Data.Vector.Generic.zipWith
  , Data.Vector.Generic.zipWith3
  , Data.Vector.Generic.zipWith4
  , Data.Vector.Generic.zipWith5
  , Data.Vector.Generic.zipWith6
  , Data.Vector.Generic.izipWith
  , Data.Vector.Generic.izipWith3
  , Data.Vector.Generic.izipWith4
  , Data.Vector.Generic.izipWith5
  , Data.Vector.Generic.izipWith6
  , Data.Vector.Generic.zip
  , Data.Vector.Generic.zip3
  , Data.Vector.Generic.zip4
  , Data.Vector.Generic.zip5
  , Data.Vector.Generic.zip6

  -- ** Monadic zipping
  , Data.Vector.Generic.zipWithM
  , Data.Vector.Generic.izipWithM
  , Data.Vector.Generic.zipWithM_
  , Data.Vector.Generic.izipWithM_

  -- ** Unzipping
  , Data.Vector.Generic.unzip
  , Data.Vector.Generic.unzip3
  , Data.Vector.Generic.unzip4
  , Data.Vector.Generic.unzip5
  , Data.Vector.Generic.unzip6

  -- * Working with predicates
  -- ** Filtering
  , Data.Vector.Generic.filter
  , Data.Vector.Generic.ifilter
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.uniq
  , Data.Vector.Generic.mapMaybe
  , Data.Vector.Generic.imapMaybe
#endif
  , Data.Vector.Generic.filterM
  , Data.Vector.Generic.takeWhile
  , Data.Vector.Generic.dropWhile

  -- ** Partitioning
  , Data.Vector.Generic.partition
  , Data.Vector.Generic.unstablePartition
  , Data.Vector.Generic.span
  , Data.Vector.Generic.break

  -- ** Searching
  , Data.Vector.Generic.elem
  , Data.Vector.Generic.notElem
  , Data.Vector.Generic.find
  , Data.Vector.Generic.findIndex
  , Data.Vector.Generic.findIndices
  , Data.Vector.Generic.elemIndex
  , Data.Vector.Generic.elemIndices

  -- * Folding
  , Data.Vector.Generic.foldl
  , Data.Vector.Generic.foldl'
  , Data.Vector.Generic.foldr
  , Data.Vector.Generic.foldr'
  , Data.Vector.Generic.ifoldl
  , Data.Vector.Generic.ifoldl'
  , Data.Vector.Generic.ifoldr
  , Data.Vector.Generic.ifoldr'

  -- ** Specialised folds
  , Data.Vector.Generic.all
  , Data.Vector.Generic.any
  , Data.Vector.Generic.and
  , Data.Vector.Generic.or
  , Data.Vector.Generic.sum
  , Data.Vector.Generic.product

  -- ** Monadic folds
  , Data.Vector.Generic.foldM
  , Data.Vector.Generic.ifoldM
  , Data.Vector.Generic.foldM'
  , Data.Vector.Generic.ifoldM'
  , Data.Vector.Generic.foldM_
  , Data.Vector.Generic.ifoldM_
  , Data.Vector.Generic.foldM'_
  , Data.Vector.Generic.ifoldM'_

  -- ** Monadic sequencing
  , Data.Vector.Generic.sequence
  , Data.Vector.Generic.sequence_

  -- * Prefix sums (scans)
  , Data.Vector.Generic.prescanl
  , Data.Vector.Generic.prescanl'
  , Data.Vector.Generic.postscanl
  , Data.Vector.Generic.postscanl'
  , Data.Vector.Generic.scanl
  , Data.Vector.Generic.scanl'
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.iscanl
  , Data.Vector.Generic.iscanl'
#endif
  , Data.Vector.Generic.prescanr
  , Data.Vector.Generic.prescanr'
  , Data.Vector.Generic.postscanr
  , Data.Vector.Generic.postscanr'
  , Data.Vector.Generic.scanr
  , Data.Vector.Generic.scanr'
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.iscanr
  , Data.Vector.Generic.iscanr'
#endif

  -- * Conversions
  -- ** Lists
  , Data.Vector.Generic.toList
  , Data.Vector.Generic.fromList
  , Data.Vector.Generic.fromListN

  -- ** Different vector types
  , Data.Vector.Generic.convert

  -- ** Mutable vectors
  , Data.Vector.Generic.freeze
  , Data.Vector.Generic.thaw
  , Data.Vector.Generic.copy

  -- * Fusion support
  -- ** Conversion to/from Bundles
  , Data.Vector.Generic.stream
  , Data.Vector.Generic.unstream
  , Data.Vector.Generic.streamR
  , Data.Vector.Generic.unstreamR

  -- ** Recycling support
  , Data.Vector.Generic.new
  , Data.Vector.Generic.clone

  -- * Utilities
  -- ** Comparisons
  , Data.Vector.Generic.eq
  , Data.Vector.Generic.cmp
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.eqBy
  , Data.Vector.Generic.cmpBy
#endif

  -- ** Show and Read
  , Data.Vector.Generic.showsPrec
  , Data.Vector.Generic.readPrec
#if MIN_VERSION_vector(0,12,0)
  , Data.Vector.Generic.liftShowsPrec
  , Data.Vector.Generic.liftReadsPrec
#endif

  -- ** @Data@ and @Typeable@
  , Data.Vector.Generic.gfoldl
  , Data.Vector.Generic.dataCast
  , Data.Vector.Generic.mkType
  ) where

import qualified Data.Vector.Generic
