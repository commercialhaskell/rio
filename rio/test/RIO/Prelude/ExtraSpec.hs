{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module RIO.Prelude.ExtraSpec (spec) where

import RIO
import RIO.Process
import Test.Hspec

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified System.FilePath as FP

spec :: Spec
spec = do
  describe "foldMapM" $ do
    it "sanity" $ do
      let helper :: Applicative f => Int -> f [Int]
          helper = pure . pure
      res <- foldMapM helper [1..10]
      res `shouldBe` [1..10]
  describe "augmentPathMap" $ do
    -- https://github.com/commercialhaskell/rio/issues/234
    it "Doesn't duplicate PATH keys on windows" $ do
      let pathKey :: T.Text
#if WINDOWS
          pathKey = "Path"
#else
          pathKey = "PATH"
#endif
          origEnv :: EnvVars
          origEnv = Map.fromList [ ("foo", "3")
                                 , ("bar", "baz")
                                 , (pathKey, makePath ["/local/bin", "/usr/bin"])
                                 ]
      let res = second (fmap getPaths . Map.lookup "PATH") $ augmentPathMap ["/bin"] origEnv
      res `shouldBe` Right (Just ["/bin", "/local/bin", "/usr/bin"])
  where
    makePath :: [T.Text] -> T.Text
    makePath = T.intercalate (T.singleton FP.searchPathSeparator)

    getPaths :: T.Text -> [T.Text]
    getPaths = fmap T.pack . FP.splitSearchPath . T.unpack
