module RIO.Prelude.Text
  ( decodeUtf8Lenient
  , tshow
  ) where

import Data.ByteString (ByteString)
import qualified Data.Text                as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

decodeUtf8Lenient :: ByteString -> T.Text
decodeUtf8Lenient = decodeUtf8With lenientDecode
