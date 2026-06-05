module Drasil.TestingKit.TestMain (testMain) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, TestTree)

testMain :: TestTree -> IO ()
testMain testTree = do
  setLocaleEncoding utf8
  defaultMain testTree
