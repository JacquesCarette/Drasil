{-# LANGUAGE TypeFamilies #-}

-- | Defines a package extension for GOOL, with functions for pairing a GOOL
-- program with auxiliary, non-source-code files.
module Language.Drasil.Code.Imperative.GOOL.ClassInterface (
  -- Typeclasses
  PackageSym(..), AuxiliarySym(..), sampleInput, auxFromData
) where

import Text.PrettyPrint.HughesPJ (Doc)

import Drasil.GOOL (ProgData, GOOLState)
import Language.Drasil.Printers (PrintingInformation)

import Language.Drasil (Expr)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Code.FileData (FileAndContents(..), PackageData,
  fileAndContents)
import Language.Drasil.Code.FileNames (sampleInputName)
import Language.Drasil.Choices (Comments, ImplementationType, Verbosity)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..))

-- | Members of this class must have all the information necessary for
-- the 'AuxiliarySym' in addition to information necessary to create a package.
class (AuxiliarySym r) => PackageSym r where
  package :: ProgData -> [r FileAndContents] -> r (PackageData ProgData)

-- | Members of this class must have a doxygen configuration, ReadMe file,
-- sample input, omptimize doxygen document, information necessary for a makefile,
-- auxiliary helper documents, and auxiliary from data documents.
class AuxiliarySym r where
  doxConfig :: String -> GOOLState -> Verbosity -> r FileAndContents
  readMe ::  ReadMeInfo -> r FileAndContents

  optimizeDox :: r Doc

  makefile :: [FilePath] -> ImplementationType -> [Comments] -> GOOLState ->
    ProgData -> r FileAndContents

  auxHelperDoc :: r Doc -> Doc

sampleInput :: (Applicative r) => PrintingInformation -> DataDesc -> [Expr] ->
  r FileAndContents
sampleInput db d sd = auxFromData sampleInputName (makeInputFile db d sd)

auxFromData :: Applicative r => FilePath -> Doc -> r FileAndContents
auxFromData fp d = pure $ fileAndContents fp d
