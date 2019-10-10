module Language.Drasil.Code.Imperative.GOOL.Generic (
  -- * Common Syntax
  doxConfig, sampleInput, makefile
) where

import Language.Drasil (Expr)

import Database.Drasil (ChunkDB)

import GOOL.Drasil (ProgData)

import Language.Drasil.CodeSpec (Comments)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Symantics (
  AuxiliarySym(Auxiliary, AuxHelper, auxHelperDoc, auxFromData))

doxConfig :: (AuxiliarySym repr) => repr (AuxHelper repr) -> String -> ProgData 
  -> repr (Auxiliary repr)
doxConfig opt pName p = auxFromData doxConfigName (makeDoxConfig pName p 
  (auxHelperDoc opt))

sampleInput :: (AuxiliarySym repr) => ChunkDB -> DataDesc -> [Expr] -> 
  repr (Auxiliary repr)
sampleInput db d sd = auxFromData sampleInputName (makeInputFile db d sd)

makefile :: (AuxiliarySym repr) => Maybe BuildConfig -> Runnable -> [Comments] 
  -> ProgData -> repr (Auxiliary repr)
makefile bc r cms p = auxFromData makefileName (makeBuild cms bc r p)
