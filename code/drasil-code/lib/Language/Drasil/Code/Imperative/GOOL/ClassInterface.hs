{-# LANGUAGE TypeFamilies #-}

-- | Defines a package extension for GOOL, with functions for pairing a GOOL
-- program with auxiliary, non-source-code files.
module Language.Drasil.Code.Imperative.GOOL.ClassInterface (
  ReadMeInfo(..),
  -- Typeclasses
  PackageSym(..), AuxiliarySym(..)
) where

import Language.Drasil (Expr)
import Database.Drasil (ChunkDB)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Mod (Name, Version)
import Language.Drasil.Choices (Comments, ImplementationType, Verbosity)

import GOOL.Drasil (ProgData, GOOLState)

import Text.PrettyPrint.HughesPJ (Doc)

-- | Members of this class must have all the information necessary for
-- the 'AuxiliarySym' in addition to information necessary to create a package.
class (AuxiliarySym r) => PackageSym r where
  type Package r
  package :: ProgData -> [r (Auxiliary r)] -> r (Package r)

-- | Members of this class must have a doxygen configuration, ReadMe file,
-- sample input, omptimize doxygen document, information necessary for a makefile,
-- auxiliary helper documents, and auxiliary from data documents.
class AuxiliarySym r where
  type Auxiliary r
  type AuxHelper r
  doxConfig :: String -> GOOLState -> Verbosity -> r (Auxiliary r)
  readMe ::  ReadMeInfo -> r (Auxiliary r)
  sampleInput :: ChunkDB -> DataDesc -> [Expr] -> r (Auxiliary r)

  optimizeDox :: r (AuxHelper r)

  makefile :: [FilePath] -> ImplementationType -> [Comments] -> GOOLState ->
    ProgData -> r (Auxiliary r)

  auxHelperDoc :: r (AuxHelper r) -> Doc
  auxFromData :: FilePath -> Doc -> r (Auxiliary r)

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
-- | File contributors
type Contributor = String
-- | Input File
type InFile = String -- TODO: There may not always be an Input/Output File
-- | Output File
type OutFile = String
-- | Holds all information needed to create a README file.
data ReadMeInfo = ReadMeInfo {
  langName :: LangAbbrev,
  langVersion :: LangVers,
  invalidOS :: Maybe String,
  implementType :: ImplementationType,
  extLibNV :: [(Name,Version)],
  extLibFP :: [FilePath],
  contributors :: [Contributor],
  configFP :: [FilePath],
  caseName :: CaseName,
  examplePurpose :: ExamplePurpose,
  exampleDescr :: ExampleDescr,
  folderNum :: Int,
  inputOutput :: (InFile, OutFile)
}
