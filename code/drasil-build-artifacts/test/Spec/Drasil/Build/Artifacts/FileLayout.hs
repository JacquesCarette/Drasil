{-# LANGUAGE QuasiQuotes #-}

module Spec.Drasil.Build.Artifacts.FileLayout (fileLayoutTests) where

import Data.ByteString.Char8 qualified as B (pack)
import Data.ByteString.Lazy.Char8 qualified as LB (pack)
import Data.Text qualified as T (pack)
import Prettyprinter qualified as PNew (Pretty (..))
import System.OsPath (osp)
import Text.PrettyPrint qualified as PLegacy (text)
import Test.Tasty (TestTree, testGroup)

import Drasil.Build.Artifacts (FileLayout, directory, file, exactFile, goldenTest,
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
          goldenTest "B.ByteString ends with newline" strictByteStringFile,
          goldenTest "LB.ByteString ends with newline" lazyByteStringFile,
          goldenTest "PLegacy.Doc exact bytes" plegacyDocFileExact,
          goldenTest "PNew.Doc exact bytes" pnewDocFileExact,
          goldenTest "String exact bytes" stringFileExact,
          goldenTest "T.Text exact bytes" textFileExact,
          goldenTest "B.ByteString exact bytes" strictByteStringFileExact,
          goldenTest "LB.ByteString exact bytes" lazyByteStringFileExact
        ]
    ]

plegacyDocFile :: FileLayout
plegacyDocFile = file [ps|plegacy-doc.txt|] (PLegacy.text "plegacy-doc")

pnewDocFile :: FileLayout
pnewDocFile = file [ps|pnew-doc.txt|] (PNew.pretty "pnew-doc")

stringFile :: FileLayout
stringFile = file [ps|string.txt|] ("string" :: String)

textFile :: FileLayout
textFile = file [ps|text.txt|] (T.pack "text")

strictByteStringFile :: FileLayout
strictByteStringFile = file [ps|strict-bytestring.txt|] (B.pack "strict-bytestring")

lazyByteStringFile :: FileLayout
lazyByteStringFile = file [ps|lazy-bytestring.txt|] (LB.pack "lazy-bytestring")

plegacyDocFileExact :: FileLayout
plegacyDocFileExact = exactFile [ps|exact-plegacy-doc.txt|] (PLegacy.text "plegacy-doc")

pnewDocFileExact :: FileLayout
pnewDocFileExact = exactFile [ps|exact-pnew-doc.txt|] (PNew.pretty "pnew-doc")

stringFileExact :: FileLayout
stringFileExact = exactFile [ps|exact-string.txt|] ("string" :: String)

textFileExact :: FileLayout
textFileExact = exactFile [ps|exact-text.txt|] (T.pack "text")

strictByteStringFileExact :: FileLayout
strictByteStringFileExact = exactFile [ps|exact-strict-bytestring.txt|] (B.pack "strict-bytestring")

lazyByteStringFileExact :: FileLayout
lazyByteStringFileExact = exactFile [ps|exact-lazy-bytestring.txt|] (LB.pack "lazy-bytestring")

nestedFiles :: FileLayout
nestedFiles =
  directory
    [ps|nested-files|]
    [ file [ps|a.txt|] (PNew.pretty "a"),
      directory
        [ps|a|]
        [ file [ps|b.txt|] (PNew.pretty "b"),
          directory
            [ps|b|]
            [ file [ps|c.txt|] (PNew.pretty "c"),
              file [ps|d.txt|] (PNew.pretty "d")
            ],
          directory
            [ps|c|]
            [ file [ps|e.txt|] (PNew.pretty "e")
            ]
        ]
    ]

helloWorldFile :: FileLayout
helloWorldFile = file [ps|hello-world.txt|] (PNew.pretty "Hello, World!")
