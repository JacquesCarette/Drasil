-- | Defines Drasil generator functions.
module Drasil.Generator.Composed (
  -- * Generators
  exportSmithEtAlSrsWCode, exportSmithEtAlSrsWCodeZoo,
) where

import Control.Lens ((^.))
import Data.Char (toLower)
import Language.Drasil.Code (Choices)
import Drasil.SRSDocument (SRSDecl)
import Drasil.System (SmithEtAlSRS, programName)
import System.Directory (getCurrentDirectory, setCurrentDirectory)

import Drasil.Build.Artifacts.Legacy (createDirIfMissing)
import Drasil.Generator.Code (exportCode, exportCodeZoo)
import Drasil.Generator.SRS (exportSmithEtAlSrs)

-- | Generate an SRS softifact with a specific solution softifact.
exportSmithEtAlSrsWCode :: SmithEtAlSRS -> SRSDecl -> String -> Choices -> IO ()
exportSmithEtAlSrsWCode syst srsDecl srsFileName chcs = do
  let exampleName = map toLower (syst ^. programName)
  exportSmithEtAlSrs syst srsDecl srsFileName
  workingDir <- getCurrentDirectory
  createDirIfMissing False exampleName
  setCurrentDirectory exampleName
  exportCode syst chcs
  setCurrentDirectory workingDir

-- | Generate an SRS softifact with a zoo of solution softifacts.
exportSmithEtAlSrsWCodeZoo :: SmithEtAlSRS -> SRSDecl -> String -> [Choices] -> IO ()
exportSmithEtAlSrsWCodeZoo syst srsDecl srsFileName chcs = do
  let exampleName = map toLower (syst ^. programName)
  exportSmithEtAlSrs syst srsDecl srsFileName
  workingDir <- getCurrentDirectory
  createDirIfMissing False exampleName
  setCurrentDirectory exampleName
  exportCodeZoo syst chcs
  setCurrentDirectory workingDir

