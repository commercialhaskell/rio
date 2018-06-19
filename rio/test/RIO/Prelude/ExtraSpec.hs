module RIO.Prelude.ExtraSpec (spec) where

import RIO
import Test.Hspec

spec :: Spec
spec = do
  describe "foldMapM" $ do
    it "sanity" $ do
      let helper :: Applicative f => Int -> f [Int]
          helper = pure . pure
      res <- foldMapM helper [1..10]
      res `shouldBe` [1..10]
