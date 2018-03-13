-- | This module exports all the partial functions from 'Data.ByteString'

module RIO.ByteString.Partial
  (
  -- * Basic interface
    Data.ByteString.head
  , Data.ByteString.last
  , Data.ByteString.tail
  , Data.ByteString.init

  -- * Reducing 'ByteString's (folds)
  , Data.ByteString.foldl1
  , Data.ByteString.foldl1'
  , Data.ByteString.foldr1
  , Data.ByteString.foldr1'

  -- * Special folds
  , Data.ByteString.maximum
  , Data.ByteString.minimum
  ) where

import qualified Data.ByteString
