-- | This module exports all the partial functions from "Data.Text.Lazy"

module RIO.Text.Lazy.Partial
    (
    -- * Creation and elimination
      Data.Text.Lazy.head
    , Data.Text.Lazy.last
    , Data.Text.Lazy.tail
    , Data.Text.Lazy.init

    -- * Transformations
    , Data.Text.Lazy.replace

    -- * Folds
    , Data.Text.Lazy.foldl1
    , Data.Text.Lazy.foldl1'
    , Data.Text.Lazy.foldr1

    -- ** Special folds
    , Data.Text.Lazy.maximum
    , Data.Text.Lazy.minimum

    -- * Substrings

    -- ** Breaking strings
    , Data.Text.Lazy.breakOn
    , Data.Text.Lazy.breakOnEnd

    -- ** Breaking into many substrings
    , Data.Text.Lazy.splitOn

    -- * Searching
    , Data.Text.Lazy.breakOnAll
    ) where

import qualified Data.Text.Lazy
