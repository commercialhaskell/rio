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
      Nothing -> do
        -- Special case handling for cases when `i+n` overflows. This is
        -- required due to <https://gitlab.haskell.org/ghc/ghc/issues/17233>.
        -- Once that GHC issue is closed the `when` clause can be removed.
        -- (Negative overflow is not an issue as an exception is thrown for
        -- negative arguments.)
        when (i > 0 && n > 0 && n + i < 0) QC.discard  -- `i+n` overflows
        evaluate (V'.slice i n v) `shouldThrow` anyException
