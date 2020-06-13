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

import GOOL.Drasil (ProgData, GOOLState)

import Text.PrettyPrint.HughesPJ (Doc)

class (AuxiliarySym r) => PackageSym r where
  type Package r 
  package :: ProgData -> [r (Auxiliary r)] -> r (Package r)

class AuxiliarySym r where
  type Auxiliary r
  type AuxHelper r
  doxConfig :: String -> GOOLState -> Verbosity -> r (Auxiliary r)
  sampleInput :: ChunkDB -> DataDesc -> [Expr] -> r (Auxiliary r)

  optimizeDox :: r (AuxHelper r)

  makefile :: [FilePath] -> ImplementationType -> [Comments] -> GOOLState -> 
    ProgData -> r (Auxiliary r)

  auxHelperDoc :: r (AuxHelper r) -> Doc
  auxFromData :: FilePath -> Doc -> r (Auxiliary r)