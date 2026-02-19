{-# LANGUAGE TemplateHaskell #-}

-- | Defines a package extension for GOOL, with functions for pairing a GOOL
-- program with auxiliary, non-source-code files.
module Language.Drasil.SoftwareDossier.ClassInterface (
  -- DataTypes
  SoftwareDossierState, makeSds, headers, sources, mainMod,
  -- Typeclasses
  AuxiliarySym(..),
  -- Functions
  sampleInput, auxFromData
) where

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.GOOL (ProgData)
import Language.Drasil.Printers (PrintingInformation)

import Language.Drasil (Expr)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Code.FileData (FileAndContents(..), fileAndContents)
import Language.Drasil.Code.FileNames (sampleInputName)
import Language.Drasil.Choices (Comments, ImplementationType, Verbosity)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..))

import Control.Lens (makeLenses)

data SoftwareDossierState = Sds {
  _headers :: [FilePath], -- Used by Drasil for doxygen config gen
  _sources :: [FilePath], -- Used by Drasil for doxygen config and Makefile gen
  _mainMod :: Maybe FilePath -- Used by Drasil generator to access main
                             -- mod file path (needed in Makefile generation)
}
makeLenses ''SoftwareDossierState

makeSds :: [FilePath] -> [FilePath] -> Maybe FilePath -> SoftwareDossierState
makeSds headerFiles sourceFiles mainModule = Sds {
    _headers = headerFiles,
    _sources = sourceFiles,
    _mainMod = mainModule
  }

-- | Members of this class must have a doxygen configuration, ReadMe file,
-- omptimize doxygen document, information necessary for a makefile, and
-- auxiliary helper documents
class AuxiliarySym r where
  doxConfig :: String -> SoftwareDossierState -> Verbosity -> r FileAndContents
  readMe ::  ReadMeInfo -> r FileAndContents

  optimizeDox :: r Doc

  makefile :: [FilePath] -> ImplementationType -> [Comments] -> SoftwareDossierState ->
    ProgData -> r FileAndContents

  auxHelperDoc :: r Doc -> Doc

sampleInput :: (Applicative r) => PrintingInformation -> DataDesc -> [Expr] ->
  r FileAndContents
sampleInput db d sd = auxFromData sampleInputName (makeInputFile db d sd)

auxFromData :: Applicative r => FilePath -> Doc -> r FileAndContents
auxFromData fp d = pure $ fileAndContents fp d
