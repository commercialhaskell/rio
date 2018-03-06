{-# LANGUAGE CPP #-}

-- | This module contains functions from "Data.Set" that have unchecked
--   preconditions on their input.  If these preconditions are not satisfied,
--   the data structure may end up in an invalid state and other operations
--   may misbehave.
module RIO.Set.Unchecked
  (
  -- * Map
    Data.Set.mapMonotonic

  -- * Ordered list
  , Data.Set.fromAscList
#if MIN_VERSION_containers(0,5,8)
  , Data.Set.fromDescList
#endif
  , Data.Set.fromDistinctAscList
#if MIN_VERSION_containers(0,5,8)
  , Data.Set.fromDistinctDescList
#endif
  ) where

import qualified Data.Set
