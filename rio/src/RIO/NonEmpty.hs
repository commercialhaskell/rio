-- | @NonEmpty@ list. Import as:
--
-- > import qualified RIO.NonEmpty as NE
--
-- This module does not export any partial functions.  For those, see
-- "RIO.NonEmpty.Partial"
module RIO.NonEmpty
  (
  -- * The type of non-empty streams
    Data.List.NonEmpty.NonEmpty(..)

  -- * Non-empty stream transformations
  , Data.List.NonEmpty.map
  , Data.List.NonEmpty.intersperse
  , Data.List.NonEmpty.scanl
  , Data.List.NonEmpty.scanr
  , Data.List.NonEmpty.scanl1
  , Data.List.NonEmpty.scanr1
  , Data.List.NonEmpty.transpose
  , Data.List.NonEmpty.sortBy
  , Data.List.NonEmpty.sortWith

  -- * Basic functions
  , Data.List.NonEmpty.length
  , Data.List.NonEmpty.head
  , Data.List.NonEmpty.tail
  , Data.List.NonEmpty.last
  , Data.List.NonEmpty.init
  , (Data.List.NonEmpty.<|)
  , Data.List.NonEmpty.cons
  , Data.List.NonEmpty.uncons
  , Data.List.NonEmpty.unfoldr
  , Data.List.NonEmpty.sort
  , Data.List.NonEmpty.reverse
  , Data.List.NonEmpty.inits
  , Data.List.NonEmpty.tails

  -- * Building streams
  , Data.List.NonEmpty.iterate
  , Data.List.NonEmpty.repeat
  , Data.List.NonEmpty.cycle
  , Data.List.NonEmpty.insert
  , Data.List.NonEmpty.some1

  -- * Extracting sublists
  , Data.List.NonEmpty.take
  , Data.List.NonEmpty.drop
  , Data.List.NonEmpty.splitAt
  , Data.List.NonEmpty.takeWhile
  , Data.List.NonEmpty.dropWhile
  , Data.List.NonEmpty.span
  , Data.List.NonEmpty.break
  , Data.List.NonEmpty.filter
  , Data.List.NonEmpty.partition
  , Data.List.NonEmpty.group
  , Data.List.NonEmpty.groupBy
  , Data.List.NonEmpty.groupWith
  , Data.List.NonEmpty.groupAllWith
  , Data.List.NonEmpty.group1
  , Data.List.NonEmpty.groupBy1
  , Data.List.NonEmpty.groupWith1
  , Data.List.NonEmpty.groupAllWith1

  -- * Sublist predicates
  , Data.List.NonEmpty.isPrefixOf

  -- * Set-like operations
  , Data.List.NonEmpty.nub
  , Data.List.NonEmpty.nubBy

  -- * Zipping and unzipping streams
  , Data.List.NonEmpty.zip
  , Data.List.NonEmpty.zipWith
  , Data.List.NonEmpty.unzip

  -- * Converting to and from a list
  , Data.List.NonEmpty.nonEmpty
  , Data.List.NonEmpty.toList
  , Data.List.NonEmpty.xor

  ) where

import qualified Data.List.NonEmpty
