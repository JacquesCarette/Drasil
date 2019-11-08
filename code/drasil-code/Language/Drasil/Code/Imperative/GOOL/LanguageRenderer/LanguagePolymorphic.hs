module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic (
  -- * Common Syntax
  doxConfig, sampleInput, makefile
) where

import Language.Drasil (Expr)

import Database.Drasil (ChunkDB)

import GOOL.Drasil (ProgData, GOOLState)

import Language.Drasil.CodeSpec (Comments, Verbosity)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Symantics (
  AuxiliarySym(Auxiliary, AuxHelper, auxHelperDoc, auxFromData))

doxConfig :: (AuxiliarySym repr) => repr (AuxHelper repr) -> String -> 
  ProgData -> Verbosity -> repr (Auxiliary repr)
doxConfig opt pName p v = auxFromData doxConfigName (makeDoxConfig pName p 
  (auxHelperDoc opt) v)

sampleInput :: (AuxiliarySym repr) => ChunkDB -> DataDesc -> [Expr] -> 
  repr (Auxiliary repr)
sampleInput db d sd = auxFromData sampleInputName (makeInputFile db d sd)

makefile :: (AuxiliarySym repr) => Maybe BuildConfig -> Runnable -> [Comments] 
  -> GOOLState -> ProgData -> repr (Auxiliary repr)
makefile bc r cms s p = auxFromData makefileName (makeBuild cms bc r s p)
