-- | A golden testing suite based on @tasty-golden@, specialized to 'FileLayout'
-- and prioritizing checking @diff@s of whole directories:
--
-- <https://hackage-content.haskell.org/package/tasty-golden-2.3.6>
--
-- Run your test suite with @--accept@ to accept files as the new golden tests.
module Drasil.TestingKit.GoldenTesting
  ( goldenTestingGroup,
    GoldenTestCase,
    goldenTest,
  )
where

import Data.Proxy (Proxy (..))
import Drasil.FileHandling (FileLayout, OverwritePolicy(..), name, localPath, toPath, writeFiles)
import System.Directory.OsPath (createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (ExitSuccess))
import System.OsPath (OsPath, decodeUtf, (</>))
import System.Process (readProcessWithExitCode)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.Options (IsOption (..), OptionDescription (..), flagCLParser, lookupOption, safeReadBool)
import Test.Tasty.Providers (IsTest (..), TestName, singleTest, testFailed, testPassed)

-- | Internal: @tasty@ CLI option for accepting fresh artifacts as the new
-- golden.
newtype AcceptTests = AcceptTests Bool

instance IsOption AcceptTests where
  defaultValue = AcceptTests False
  parseValue = fmap AcceptTests . safeReadBool
  optionName = pure "accept"
  optionHelp = pure "Accept actual results as new golden outputs."
  optionCLParser = flagCLParser Nothing (AcceptTests True)

-- | A golden test case.
newtype GoldenTestCase = GTC
  { -- | A 'TestTree' waiting on the target folder and the golden artifacts folder.
    unGTC :: OsPath -> OsPath -> TestTree
  }

-- | Create a golden testing group relative to a main build folder and golden
-- artifacts folder.
goldenTestingGroup ::
  -- | The relative directory where newly generated files should be written.
  OsPath ->
  -- | The relative directory where the expected/golden files are stored.
  OsPath ->
  -- | The name of the test group (e.g., @"Golden Tests"@).
  TestName ->
  -- | A list of golden test cases to run.
  [GoldenTestCase] ->
  TestTree
goldenTestingGroup buildPath goldenPath groupName mkTests =
  withResource
    setup
    (const $ pure ())
    ( const $
        testGroup groupName (map (\gt -> unGTC gt buildPath goldenPath) mkTests)
    )
  where
    setup = do
      createDirectoryIfMissing True $ localPath </> buildPath
      createDirectoryIfMissing True $ localPath </> goldenPath

-- | Internal: Tasty golden test representation.
--
-- 1. Adds an @--accept@ CLI option.
-- 2. Uses system @diff@ command to compare freshly built artifacts with golden
--    ones.
data GoldenTest = GoldenTest OsPath OsPath OsPath (OsPath -> IO ())

instance IsTest GoldenTest where
  testOptions = pure [Option (Proxy :: Proxy AcceptTests)]
  run opts (GoldenTest build golden target action) _ = do
    let AcceptTests accept = lookupOption opts
        buildPath = localPath </> build
        goldenPath = localPath </> golden

        buildTarget = buildPath </> target
        goldenTarget = goldenPath </> target

    buildTargetStr <- decodeUtf buildTarget
    goldenTargetStr <- decodeUtf goldenTarget

    if accept
      then do
        removePathForcibly goldenTarget
        action goldenPath
        pure $
          testPassed $
            "Accepted fresh artifacts as golden for: " ++ goldenTargetStr ++ "."
      else do
        removePathForcibly buildTarget
        action buildPath
        (exitCode, stdout, stderr) <-
          readProcessWithExitCode
            "diff"
            [ "-ru",
              "--color=always",
              "--strip-trailing-cr",
              goldenTargetStr,
              buildTargetStr
            ]
            ""
        case exitCode of
          ExitSuccess -> pure $ testPassed ""
          _ -> pure $ testFailed $
            "Outputs differ:\n" ++ stdout ++ stderr ++
            "\nIf this is expected, you can accept the changes using `stack test --test-arguments=\"--accept\"`"

-- | Create a golden test case for a given 'FileLayout'. Within the context of a
-- 'goldenTestingGroup', will be dumped to the build folder and compared with
-- the golden artifacts folder.
goldenTest :: TestName -> FileLayout -> GoldenTestCase
goldenTest tName layout = GTC $
  \buildRoot goldenRoot ->
    singleTest
      tName
      ( GoldenTest
          buildRoot
          goldenRoot
          (toPath (name layout))
          (\p -> writeFiles NeverOverwrite p layout)
      )
