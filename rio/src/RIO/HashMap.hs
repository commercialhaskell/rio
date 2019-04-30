-- | Strict @Map@ with hashed keys. Import as:
--
-- > import qualified RIO.HashMap as HM
--
-- This module does not export any partial functions.  For those, see
-- "RIO.HashMap.Partial"
module RIO.HashMap
  (
    Data.HashMap.Strict.HashMap

    -- * Construction
  , Data.HashMap.Strict.empty
  , Data.HashMap.Strict.singleton

    -- * Basic interface
  , Data.HashMap.Strict.null
  , Data.HashMap.Strict.size
  , Data.HashMap.Strict.member
  , Data.HashMap.Strict.lookup
  , Data.HashMap.Strict.lookupDefault
  , Data.HashMap.Strict.insert
  , Data.HashMap.Strict.insertWith
  , Data.HashMap.Strict.delete
  , Data.HashMap.Strict.adjust
  , Data.HashMap.Strict.update
  , Data.HashMap.Strict.alter

    -- * Combine
    -- ** Union
  , Data.HashMap.Strict.union
  , Data.HashMap.Strict.unionWith
  , Data.HashMap.Strict.unionWithKey
  , Data.HashMap.Strict.unions

    -- * Transformations
  , Data.HashMap.Strict.map
  , Data.HashMap.Strict.mapWithKey
  , Data.HashMap.Strict.traverseWithKey

    -- * Difference and intersection
  , Data.HashMap.Strict.difference
  , Data.HashMap.Strict.differenceWith
  , Data.HashMap.Strict.intersection
  , Data.HashMap.Strict.intersectionWith
  , Data.HashMap.Strict.intersectionWithKey

    -- * Folds
  , Data.HashMap.Strict.foldl'
  , Data.HashMap.Strict.foldlWithKey'
  , Data.HashMap.Strict.foldr
  , Data.HashMap.Strict.foldrWithKey

    -- * Filter
  , Data.HashMap.Strict.filter
  , Data.HashMap.Strict.filterWithKey
  , Data.HashMap.Strict.mapMaybe
  , Data.HashMap.Strict.mapMaybeWithKey

    -- * Conversions
  , Data.HashMap.Strict.keys
  , Data.HashMap.Strict.elems

    -- ** Lists
  , Data.HashMap.Strict.toList
  , Data.HashMap.Strict.fromList
  , Data.HashMap.Strict.fromListWith
  ) where

import Data.HashMap.Strict
