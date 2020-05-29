-- | Extra utilities from @microlens@.
module RIO.Lens
  ( -- * Fold
    SimpleFold
  , toListOf
  , has
    -- * Lens
  , _1, _2, _3, _4, _5
  , at
  , lens
    -- * Iso
  , non
    -- * Traversal
  , singular
  , failing
  , filtered
  , both
  , traversed
  , each
  , ix
  , _head
  , _tail
  , _init
  , _last
    -- * Prism
  , _Left
  , _Right
  , _Just
  , _Nothing
  ) where

import Lens.Micro
