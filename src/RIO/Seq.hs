{-# LANGUAGE CPP #-}
-- | @Seq@. Import as:
--
-- > import qualified RIO.Seq as Seq
module RIO.Seq
  (
    Data.Sequence.Seq(..)

    -- * Construction
  , Data.Sequence.empty
  , Data.Sequence.singleton
  , (Data.Sequence.<|)
  , (Data.Sequence.|>)
  , (Data.Sequence.><)
  , Data.Sequence.fromList
  , Data.Sequence.fromFunction
  , Data.Sequence.fromArray

    -- ** Repetition
  , Data.Sequence.replicate
  , Data.Sequence.replicateA
  , Data.Sequence.replicateM
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.cycleTaking
#endif

    -- ** Iterative construction
  , Data.Sequence.iterateN
  , Data.Sequence.unfoldr
  , Data.Sequence.unfoldl

    -- * Deconstruction
    -- | Additional functions for deconstructing sequences are available via the
    -- 'Foldable' instance of 'Seq'.

    -- ** Queries
  , Data.Sequence.null
  , Data.Sequence.length
    -- ** Views
  , Data.Sequence.ViewL(..)
  , Data.Sequence.viewl
  , Data.Sequence.ViewR(..)
  , Data.Sequence.viewr

    -- * Scans
  , Data.Sequence.scanl
  , Data.Sequence.scanl1
  , Data.Sequence.scanr
  , Data.Sequence.scanr1

    -- * Sublists
  , Data.Sequence.tails
  , Data.Sequence.inits
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.chunksOf
#endif

    -- ** Sequential searches
  , Data.Sequence.takeWhileL
  , Data.Sequence.takeWhileR
  , Data.Sequence.dropWhileL
  , Data.Sequence.dropWhileR
  , Data.Sequence.spanl
  , Data.Sequence.spanr
  , Data.Sequence.breakl
  , Data.Sequence.breakr
  , Data.Sequence.partition
  , Data.Sequence.filter

    -- * Sorting
  , Data.Sequence.sort
  , Data.Sequence.sortBy
  , Data.Sequence.unstableSort
  , Data.Sequence.unstableSortBy

    -- * Indexing
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.lookup
  , (Data.Sequence.!?)
#endif
  , Data.Sequence.index
  , Data.Sequence.adjust

#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.adjust'
#endif
  , Data.Sequence.update
  , Data.Sequence.take
  , Data.Sequence.drop
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.insertAt
  , Data.Sequence.deleteAt
#endif
  , Data.Sequence.splitAt

    -- ** Indexing with predicates
    -- | These functions perform sequential searches from the left or right ends
    -- of the sequence elements.
  , Data.Sequence.elemIndexL
  , Data.Sequence.elemIndicesL
  , Data.Sequence.elemIndexR
  , Data.Sequence.elemIndicesR
  , Data.Sequence.findIndexL
  , Data.Sequence.findIndicesL
  , Data.Sequence.findIndexR
  , Data.Sequence.findIndicesR

    -- * Folds
    -- | General folds are available via the 'Foldable' instance of 'Seq'.
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.foldMapWithIndex
#endif
  , Data.Sequence.foldlWithIndex
  , Data.Sequence.foldrWithIndex

    -- * Transformations
  , Data.Sequence.mapWithIndex
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.traverseWithIndex
#endif
  , Data.Sequence.reverse
#if MIN_VERSION_containers(0, 5, 8)
  , Data.Sequence.intersperse
#endif

    -- ** Zips
  , Data.Sequence.zip
  , Data.Sequence.zipWith
  , Data.Sequence.zip3
  , Data.Sequence.zipWith3
  , Data.Sequence.zip4
  , Data.Sequence.zipWith4
  ) where

import qualified Data.Sequence
