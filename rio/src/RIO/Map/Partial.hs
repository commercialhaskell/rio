-- | Strict @Map@ partial functions. Import as:
--
-- > import qualified RIO.Map.Partial as Map'
module RIO.Map.Partial
  (
  -- * Operators
    (Data.Map.Strict.!)
  -- * Indexed
  , Data.Map.Strict.elemAt
  , Data.Map.Strict.deleteAt
  , Data.Map.Strict.findIndex
  , Data.Map.Strict.updateAt

  -- * Min\/Max
  , Data.Map.Strict.findMin
  , Data.Map.Strict.findMax
  , Data.Map.Strict.deleteFindMin
  , Data.Map.Strict.deleteFindMax
  ) where

import qualified Data.Map.Strict
