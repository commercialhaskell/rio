-- | This module exports all the partial functions from "Data.Text"

module RIO.Text.Partial
    (
    -- * Basic interface
      Data.Text.head
    , Data.Text.last
    , Data.Text.tail
    , Data.Text.init

    -- * Transformations
    , Data.Text.replace

    -- * Folds
    , Data.Text.foldl1
    , Data.Text.foldl1'
    , Data.Text.foldr1

    -- ** Special folds
    , Data.Text.maximum
    , Data.Text.minimum

    -- * Substrings

    -- ** Breaking strings
    , Data.Text.breakOn
    , Data.Text.breakOnEnd

    -- ** Breaking into many substrings
    , Data.Text.splitOn

    -- * Searching
    , Data.Text.breakOnAll

    -- * Indexing
    , Data.Text.count
    ) where

import qualified Data.Text
