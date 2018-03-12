{-# LANGUAGE CPP #-}
-- | This module contains functions from "Data.Map.strict" that have unchecked
--   preconditions on their input.  If these preconditions are not satisfied,
--   the data structure may end up in an invalid state and other operations
--   may misbehave.

module RIO.Map.Unchecked
  (
  -- * Traversal
  -- ** Map
    Data.Map.Strict.mapKeysMonotonic

  -- * Conversion
  -- ** Ordered lists
  , Data.Map.Strict.toAscList
  , Data.Map.Strict.fromAscList
  , Data.Map.Strict.fromAscListWith
  , Data.Map.Strict.fromAscListWithKey
  , Data.Map.Strict.fromDistinctAscList
#if MIN_VERSION_containers(0,5,8)
  , Data.Map.Strict.toDescList
  , Data.Map.Strict.fromDescList
  , Data.Map.Strict.fromDescListWith
  , Data.Map.Strict.fromDescListWithKey
  , Data.Map.Strict.fromDistinctDescList
#endif

  ) where

import qualified Data.Map.Strict
