{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Spec.Drasil.Data.Formats.CSV (csvTests) where

import Drasil.Data.Formats.CSV (CSV, CSVRenderOptions,
  DoubleQuotationPolicy(..), columnCount, csvRenderOpts, header, mkCSV,
  renderCSV, rowCount, rows)
import Drasil.FileHandling (file, ps)
import Drasil.TestingKit.GoldenTesting (goldenTest, goldenTestingGroup)
import Data.Text (Text)
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

csvTests :: TestTree
csvTests =
  testGroup
    "Drasil.Data.Formats.CSV"
    [ mkCSVTests,
      destructCSVTests,
      renderCSVTests
    ]

expectCSV :: Either String CSV -> CSV
expectCSV (Right csv) = csv
expectCSV (Left err) = error $ "Expected valid CSV, got error: " ++ err

simpleCSVHeader :: [Text]
simpleCSVHeader =
  [ "a", "b", "c" ]

simpleCSVData :: [[Text]]
simpleCSVData =
  [ [ "1", "2", "3" ],
    [ "4", "5", "6" ]
  ]

simpleCSV :: CSV
simpleCSV = expectCSV $ mkCSV Nothing (Just simpleCSVHeader) simpleCSVData

simpleCSVnoH :: CSV
simpleCSVnoH = expectCSV $ mkCSV Nothing Nothing simpleCSVData

complexCSVHeader :: [Text]
complexCSVHeader =
  [ "a b", "a\nb" ]

complexCSVData :: [[Text]]
complexCSVData =
  [ [ "1", "2" ],
    [ "1\r", "2\n" ],
    [ "1,3", "2,4" ],
    [ "\"1\"", "" ],
    [ "α", "β" ]
  ]

complexCSV :: CSV
complexCSV = expectCSV $ mkCSV (Just 2) (Just complexCSVHeader) complexCSVData

mkCSVTests :: TestTree
mkCSVTests =
  testGroup
    "mkCSV"
    [ testGroup
        "Unit Tests"
        [ testCase "explicit mismatched header size" $
            mkCSV (Just 2) (Just ["a"]) [["a"]]
              @?= Left "Header has 1 columns, but expected 2 (based on expected columns input)",
          testCase "explicit mismatched row size" $
            mkCSV (Just 2) (Just ["a", "a"]) [["a"]]
              @?= Left "Row 1 has 1 columns, but expected 2 (based on expected columns input)",
          testCase "implicit mismatched row size" $
            mkCSV Nothing (Just ["a", "a"]) [["a"]]
              @?= Left "Row 1 has 1 columns, but expected 2 (based on header length)",
          testCase "no header mismatched row size" $
            mkCSV Nothing Nothing [["a"], ["a", "a"]]
              @?= Left "Row 2 has 2 columns, but expected 1 (based on first row length)"
        ]
    ]

destructCSVTests :: TestTree
destructCSVTests =
  testGroup
    "CSV"
    [ testGroup
        "Unit Tests"
        [ testCase "rows simpleCSV" $
            rows simpleCSV @?= simpleCSVData,
          testCase "header simpleCSV" $
            header simpleCSV @?= Just simpleCSVHeader,
          testCase "header simpleCSVnoH" $
            header simpleCSVnoH @?= Nothing,
          testCase "rowCount simpleCSV" $
            rowCount simpleCSV @?= 2,
          testCase "columnCount simpleCSV" $
            columnCount simpleCSV @?= 3
        ]
    ]

minimal :: CSVRenderOptions
minimal = csvRenderOpts Minimal

everywhere :: CSVRenderOptions
everywhere = csvRenderOpts Everywhere

renderCSVTests :: TestTree
renderCSVTests =
  testGroup
    "renderCSV"
    [ goldenTestingGroup
        [osp|test/build/csv|]
        [osp|test/golden/csv|]
        "Golden Tests"
        [
          goldenTest "simpleCSV minimal" $
            file [ps|simple_m.csv|] $ renderCSV simpleCSV minimal,
          goldenTest "simpleCSVnoH minimal" $
            file [ps|simple_m_nh.csv|] $ renderCSV simpleCSVnoH minimal,
          goldenTest "simpleCSV everywhere" $
            file [ps|simple_e.csv|] $ renderCSV simpleCSV everywhere,
          goldenTest "simpleCSVnoH everywhere" $
            file [ps|simple_e_nh.csv|] $ renderCSV simpleCSVnoH everywhere,
          goldenTest "complexCSV minimal" $
            file [ps|complex_m.csv|] $ renderCSV complexCSV minimal,
          goldenTest "complexCSV everywhere" $
            file [ps|complex_e.csv|] $ renderCSV complexCSV everywhere
        ]
    ]
