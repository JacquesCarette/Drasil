-- | This module defines structures and functions for handling README files,
-- which are used as auxiliary documentation files for the system.
module Language.Drasil.Code.Imperative.ReadMe.Import (
  ReadMeInfo(..), makeReadMe
) where

import Prelude hiding ((<>))

import Data.List.NonEmpty (nonEmpty, toList)
import Text.PrettyPrint.HughesPJ (Doc, empty)

import Language.Drasil.Printers (makeMd, introInfo, verInfo, unsupOS,
    extLibSec, instDoc, endNote, whatInfo)

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Mod (Name, Version)

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

-- | Generates a README file.
makeReadMe :: ReadMeInfo -> Doc
makeReadMe ReadMeInfo {
        langName = progLang,
        langVersion = progLangVers,
        invalidOS = unsupportedOSs,
        implementType = imptype,
        extLibNV = extLibns,
        extLibFP = extLibfp,
        contributors = auths,
        configFP = configFPs,
        caseName = name,
        examplePurpose = purp,
        exampleDescr = descr,
        exampleMotivation = motiv,
        exampleScope = sc,
        folderNum = number,
        inputOutput = inoutf} =
    makeMd [introInfo name auths (fieldEmptySTR motiv)
      (fieldEmptySTR purp),
    whatInfo (fieldEmptySTR descr) (fieldEmptySTR sc),
    makeInstr imptype configFPs name inoutf,
    verInfo progLang progLangVers,
    unsupOS unsupportedOSs,
    extLibSec extLibns extLibfp,
    endNote number auths] -- add date information to end note for license

-- | Helper for encoding the type of program (either library or controller-based) in a README file.
makeInstr :: ImplementationType -> [FilePath] -> String -> (String, String) -> Doc
makeInstr Library _ _ _ = empty
makeInstr Program cfp n inOutf = instDoc cfp n inOutf

-- |Helper that checks if the field is empty; allowing optional content
-- rendering in a README file
fieldEmptySTR :: String -> Maybe String
fieldEmptySTR = fmap toList . nonEmpty
