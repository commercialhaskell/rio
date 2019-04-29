-- | Lazy @ByteString@ partial functions. Import as:
--
-- > import qualified RIO.ByteString.Lazy.Partial as BL'
module RIO.ByteString.Lazy.Partial
  (
  -- * Basic interface
    Data.ByteString.Lazy.head
  , Data.ByteString.Lazy.last
  , Data.ByteString.Lazy.tail
  , Data.ByteString.Lazy.init

  -- * Reducing 'ByteString's (folds)
  , Data.ByteString.Lazy.foldl1
  , Data.ByteString.Lazy.foldl1'
  , Data.ByteString.Lazy.foldr1

  -- ** Special folds
  , Data.ByteString.Lazy.maximum
  , Data.ByteString.Lazy.minimum
  ) where

import qualified Data.ByteString.Lazy
