module Drasil.TestingKit.TestMain (testMain) where

import GHC.IO.Encoding (setLocaleEncoding, utf8)
import Test.Tasty (defaultMain, TestTree)

-- | A wrapper around tasty's `defaultMain` which sets the encoding to UTF-8.
testMain :: TestTree -> IO ()
testMain testTree = do
  setLocaleEncoding utf8
  defaultMain testTree
