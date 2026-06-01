{-# LANGUAGE QuasiQuotes #-}

module Spec.Drasil.Build.Artifacts.FilePath (filePathTests) where

import Drasil.Build.Artifacts (ps, toPath)
import System.OsPath (OsPath, encodeUtf)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

encodeUtf' :: FilePath -> OsPath
encodeUtf' = either (error . show) id . encodeUtf

filePathTests :: TestTree
filePathTests =
  testGroup
    "Drasil.Build.Artifacts.FilePath"
    [ psTests
    ]

psTests :: TestTree
psTests =
  testGroup
    "ps"
    [ testGroup
        "Unit Tests"
        [ testCase "ps quasiquoter - simple" $
            toPath [ps|test|] @?= encodeUtf' "test",
          testCase "ps quasiquoter - interpolation" $
            toPath [ps|{x}|] @?= encodeUtf' "x",
          testCase "ps quasiquoter - escaped curly brace" $
            toPath [ps|\{x}|] @?= encodeUtf' "{x}",
          testCase "ps quasiquoter - mixed" $
            toPath [ps|pre_{x}_post|] @?= encodeUtf' "pre_x_post",
          testCase "ps quasiquoter - multiple interpolations" $
            toPath [ps|a{x}b_c{y}d{z}|] @?= encodeUtf' "axb_cydz"
        ]
    ]
  where
    x = "x"
    y = "y"
    z = "z"
