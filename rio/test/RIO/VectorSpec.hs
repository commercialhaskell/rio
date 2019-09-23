{-# LANGUAGE NoImplicitPrelude #-}
module RIO.VectorSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Test.QuickCheck as QC
import RIO
import qualified RIO.Vector as V
import qualified RIO.Vector.Partial as V'

spec :: Spec
spec =
  describe "sliceMaybe" $ do
    prop "is consistent with `slice` (pathological cases)" $
      \(QC.Large i) (QC.Large n) v
        -> sliceTest i n (V.fromList v)
    -- The next property is a subset of the previous one but with
    -- significantly greater likelyhood of having "realistic"
    -- arguments to `slice`.
    prop "is consistent with `slice` (more realistic cases)" $
      \(QC.NonNegative i) (QC.NonNegative n) (QC.NonEmpty v)
        -> sliceTest i n (V.fromList v)
  where
    sliceTest :: Int -> Int -> Vector Char -> QC.Property
    sliceTest i n v = QC.withMaxSuccess 1000 $ case V.sliceMaybe i n v of
      Just v' -> V'.slice i n v `shouldBe` v'
      Nothing -> evaluate (V'.slice i n v) `shouldThrow` anyException
