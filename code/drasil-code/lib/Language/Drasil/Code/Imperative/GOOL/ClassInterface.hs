{-# LANGUAGE TypeFamilies #-}

-- | Defines a package extension for GOOL, with functions for pairing a GOOL
-- program with auxiliary, non-source-code files.
module Language.Drasil.Code.Imperative.GOOL.ClassInterface (
  -- Typeclasses
  PackageSym(..), AuxiliarySym(..)
) where

import Language.Drasil (Expr)
import Database.Drasil (ChunkDB)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Choices (Comments, ImplementationType, Verbosity)
import Language.Drasil.Code.Imperative.ReadMe.Import (ReadMeInfo(..))

import Drasil.GOOL (ProgData, GOOLState)

import Text.PrettyPrint.HughesPJ (Doc)
import Language.Drasil.Printers (PrintingInformation)

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
  sampleInput :: PrintingInformation ->ChunkDB -> DataDesc -> [Expr] -> r (Auxiliary r)

  optimizeDox :: r (AuxHelper r)

  makefile :: [FilePath] -> ImplementationType -> [Comments] -> GOOLState ->
    ProgData -> r (Auxiliary r)

  auxHelperDoc :: r (AuxHelper r) -> Doc
  auxFromData :: FilePath -> Doc -> r (Auxiliary r)
