{-# LANGUAGE NoImplicitPrelude #-}
module RIO.OrphansSpec (spec) where

import Test.Hspec
import RIO
import RIO.Orphans
import Control.Monad.Trans.Resource

spec :: Spec
spec = do
  it "success" $ do
    ref <- newIORef False
    withResourceMap $ \rm -> runRIO rm $ do
      void $ register $ writeIORef ref True
    readIORef ref `shouldReturn` True
  it "exceptions" $ do
    ref <- newIORef False
    void $ tryAny $ withResourceMap $ \rm -> runRIO rm $ do
      void $ register $ writeIORef ref True
      throwString "ignored"
    readIORef ref `shouldReturn` True
  it "no release" $ do
    ref <- newIORef False
    void $ tryAny $ withResourceMap $ \rm -> runRIO rm $ do
      () <- throwString "ignored"
      writeIORef ref True
    readIORef ref `shouldReturn` False
