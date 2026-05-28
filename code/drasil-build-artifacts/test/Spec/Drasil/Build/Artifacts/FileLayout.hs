{-# LANGUAGE QuasiQuotes #-}

module Spec.Drasil.Build.Artifacts.FileLayout (fileLayoutTests) where

import Data.ByteString.Lazy.Char8 qualified as LB
import Data.Text qualified as T
import Prettyprinter (Pretty (..))
import System.OsPath (osp)
import Text.PrettyPrint qualified as PLegacy
import Test.Tasty (TestTree, testGroup)

import Drasil.Build.Artifacts (FileLayout, directory, file, goldenTest,
  goldenTestingGroup, ps)

fileLayoutTests :: TestTree
fileLayoutTests =
  testGroup
    "Drasil.Build.Artifacts.FileLayout"
    [ writeFilesTests,
      renderToFileTests
    ]

writeFilesTests :: TestTree
writeFilesTests =
  testGroup
    "writeFiles"
    [ goldenTestingGroup
        [osp|test/build|]
        [osp|test/golden|]
        "Golden Tests"
        [ goldenTest "Nested files" nestedFiles,
          goldenTest "Single file" helloWorldFile
        ]
    ]

renderToFileTests :: TestTree
renderToFileTests =
  testGroup
    "renderToFile"
    [ goldenTestingGroup
        [osp|test/build/renderToFile|]
        [osp|test/golden/renderToFile|]
        "Golden Tests"
        [ goldenTest "PLegacy.Doc ends with newline" plegacyDocFile,
          goldenTest "PNew.Doc ends with newline" pnewDocFile,
          goldenTest "String ends with newline" stringFile,
          goldenTest "T.Text ends with newline" textFile,
          goldenTest "LB.ByteString ends with newline" byteStringFile
        ]
    ]

plegacyDocFile :: FileLayout
plegacyDocFile = file [ps|plegacy-doc.txt|] (PLegacy.text "plegacy-doc")

pnewDocFile :: FileLayout
pnewDocFile = file [ps|pnew-doc.txt|] (pretty "pnew-doc")

stringFile :: FileLayout
stringFile = file [ps|string.txt|] ("string" :: String)

textFile :: FileLayout
textFile = file [ps|text.txt|] (T.pack "text")

byteStringFile :: FileLayout
byteStringFile = file [ps|bytestring.txt|] (LB.pack "bytestring")

nestedFiles :: FileLayout
nestedFiles =
  directory
    [ps|nested-files|]
    [ file [ps|a.txt|] (pretty "a"),
      directory
        [ps|a|]
        [ file [ps|b.txt|] (pretty "b"),
          directory
            [ps|b|]
            [ file [ps|c.txt|] (pretty "c"),
              file [ps|d.txt|] (pretty "d")
            ],
          directory
            [ps|c|]
            [ file [ps|e.txt|] (pretty "e")
            ]
        ]
    ]

helloWorldFile :: FileLayout
helloWorldFile = file [ps|hello-world.txt|] (pretty "Hello, World!")
