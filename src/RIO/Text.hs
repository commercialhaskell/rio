module RIO.Text
  ( module Data.Text
  , Data.Text.Encoding.encodeUtf8
  , Data.Text.Encoding.decodeUtf8With
  , Data.Text.Encoding.decodeUtf8'
  , Data.Text.Encoding.Error.lenientDecode
  ) where

import           Data.Text -- FIXME hide partials
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
