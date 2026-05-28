{-# LANGUAGE QuasiQuotes #-}

module Spec.Drasil.Build.Artifacts.FileLayout (fileLayoutTests) where

import Drasil.Build.Artifacts (FileLayout, directory, file, goldenTest, goldenTestingGroup, ps)
import Prettyprinter (Pretty (..))
import System.OsPath (osp)
import Test.Tasty (TestTree, testGroup)

fileLayoutTests :: TestTree
fileLayoutTests =
  testGroup
    "Drasil.Build.Artifacts.FileLayout"
    [ writeFilesTests
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
