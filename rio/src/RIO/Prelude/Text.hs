{-# LANGUAGE OverloadedStrings #-}
module RIO.Prelude.Text
  ( decodeUtf8Lenient
  , tshow
  ) where

import qualified Data.Text                as T
import Data.Text.Encoding (decodeUtf8With)
import RIO.Prelude.Reexports
import Data.Text.Encoding.Error (lenientDecode)

tshow :: Show a => a -> Text
tshow = T.pack . show

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode
