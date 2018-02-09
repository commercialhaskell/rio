-- | @List@. Import as:
--
-- > import qualified RIO.List as L
module RIO.List
  ( module Data.List
  , stripSuffix
  , dropPrefix
  , dropSuffix
  ) where

import Data.List
import Data.Maybe (fromMaybe)

-- | Remove the suffix from the given list, if present
--
-- @since 0.0.0
stripSuffix :: Eq a
            => [a] -- ^ suffix
            -> [a]
            -> Maybe [a]
stripSuffix suffix list =
  fmap reverse (stripPrefix (reverse suffix) (reverse list))

-- | Drop prefix if present, otherwise return original list.
--
-- @since 0.0.0.0
dropPrefix :: Eq a
           => [a] -- ^ prefix
           -> [a]
           -> [a]
dropPrefix prefix t = fromMaybe t (stripPrefix prefix t)

-- | Drop prefix if present, otherwise return original list.
--
-- @since 0.0.0.0
dropSuffix :: Eq a
           => [a] -- ^ suffix
           -> [a]
           -> [a]
dropSuffix suffix t = fromMaybe t (stripSuffix suffix t)
