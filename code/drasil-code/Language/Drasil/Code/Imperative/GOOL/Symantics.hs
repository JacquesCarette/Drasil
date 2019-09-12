{-# LANGUAGE TypeFamilies #-}

module Language.Drasil.Code.Imperative.GOOL.Symantics (
  -- Typeclasses
  PackageSym(..), AuxiliarySym(..)
) where

import Language.Drasil (Expr)
import Database.Drasil (ChunkDB)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.CodeSpec (Comments)

import GOOL.Drasil (ProgramSym(..), RenderSym(..), KeywordSym(..))

class (ProgramSym repr, AuxiliarySym repr) => PackageSym repr where
  type Package repr 
  package :: repr (Program repr) -> [repr (Auxiliary repr)] -> 
    repr (Package repr)

class (KeywordSym repr, RenderSym repr) => AuxiliarySym repr where
  type Auxiliary repr
  doxConfig :: String -> repr (Program repr) -> repr (Auxiliary repr)
  sampleInput :: ChunkDB -> DataDesc -> [Expr] -> repr (Auxiliary repr)

  optimizeDox :: repr (Keyword repr)

  makefile :: [Comments] -> repr (Program repr) -> repr (Auxiliary repr)