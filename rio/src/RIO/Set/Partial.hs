-- | @Set@ partial functions. Import as:
--
-- > import qualified RIO.Set.Partial as Set'
module RIO.Set.Partial
  (
  -- * Indexed
    Data.Set.findIndex
  , Data.Set.elemAt
  , Data.Set.deleteAt

  -- * Min\/Max
  , Data.Set.findMin
  , Data.Set.findMax
  , Data.Set.deleteFindMin
  , Data.Set.deleteFindMax
  ) where

import qualified Data.Set
