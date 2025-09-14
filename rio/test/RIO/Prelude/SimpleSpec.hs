{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module RIO.Prelude.SimpleSpec (spec) where

import RIO
import RIO.Process
import Test.Hspec

spec :: Spec
spec = do
  it "logging works" $ asIO $ runSimpleApp $ logDebug "logging allowed"
  it "process calling works" $ asIO $ runSimpleApp $ proc "echo" ["hello"] runProcess_
