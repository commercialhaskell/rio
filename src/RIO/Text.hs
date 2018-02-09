-- | Strict @Text@. Import as:
--
-- > import qualified RIO.Text as T
module RIO.Text
  ( module Data.Text
  , Data.Text.Encoding.encodeUtf8
  , Data.Text.Encoding.decodeUtf8With
  , Data.Text.Encoding.decodeUtf8'
  , Data.Text.Encoding.Error.lenientDecode
  , dropPrefix
  , dropSuffix
  ) where

import           Data.Text -- FIXME hide partials
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import           Data.Maybe (fromMaybe)

-- | Drop prefix if present, otherwise return original 'Text'.
--
-- @since 0.0.0.0
dropPrefix :: Text -- ^ prefix
           -> Text
           -> Text
dropPrefix prefix t = fromMaybe t (stripPrefix prefix t)

-- | Drop prefix if present, otherwise return original 'Text'.
--
-- @since 0.0.0.0
dropSuffix :: Text -- ^ suffix
           -> Text
           -> Text
dropSuffix suffix t = fromMaybe t (stripSuffix suffix t)
