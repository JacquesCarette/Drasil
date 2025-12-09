-- | This module defines structures and functions for handling README files,
-- which are used as auxiliary documentation files for the system.
module Language.Drasil.Code.Imperative.ReadMe.Import (
  ReadMeInfo(..),
) where

import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Choices (ImplementationType)

-- | Language name.
type LangAbbrev = String
-- | Programming language version.
type LangVers = String
-- | Case name.
type CaseName = String
-- | Purpose of example
type ExamplePurpose = String
-- | Description of example
type ExampleDescr = String
-- | Motivation of example
type ExampleMotivation = String
-- | Scope of example
type ExampleScope = String
-- | File contributors
type Contributor = String
-- | Input File
type InFile = String
-- | Output File
type OutFile = String

-- | Holds all information needed to create a README file.
data ReadMeInfo = ReadMeInfo {
  langName :: LangAbbrev,
  langVersion :: LangVers,
  invalidOS :: Maybe String,
  implementType :: ImplementationType,
  extLibNV :: [(Name, Version)],
  extLibFP :: [FilePath],
  contributors :: [Contributor],
  configFP :: [FilePath],
  caseName :: CaseName,
  examplePurpose :: ExamplePurpose,
  exampleDescr :: ExampleDescr,
  exampleMotivation :: ExampleMotivation,
  exampleScope :: ExampleScope,
  folderNum :: Int,
  inputOutput :: (InFile, OutFile)
}
