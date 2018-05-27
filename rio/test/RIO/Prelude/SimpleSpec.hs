{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module RIO.Prelude.SimpleSpec (spec) where

import RIO
import RIO.Process
import Test.Hspec

spec :: Spec
spec = do
  it "logging works" $ asIO $ runRIOSimple $ logDebug "logging allowed"
  it "process calling works" $ asIO $ runRIOSimple $ proc "echo" ["hello"] runProcess_
