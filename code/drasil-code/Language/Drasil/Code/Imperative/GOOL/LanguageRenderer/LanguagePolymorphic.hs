module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic (
  -- * Common Syntax
  doxConfig, sampleInput, makefile, noRunIfLib
) where

import Language.Drasil (Expr)

import Database.Drasil (ChunkDB)

import GOOL.Drasil (ProgData, GOOLState)

import Language.Drasil.Choices (Comments, ImplementationType(..), Verbosity)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (
  AuxiliarySym(Auxiliary, AuxHelper, auxHelperDoc, auxFromData))

-- Defines a Doxygen configuration file
doxConfig :: (AuxiliarySym r) => r (AuxHelper r) -> String -> 
  GOOLState -> Verbosity -> r (Auxiliary r)
doxConfig opt pName s v = auxFromData doxConfigName (makeDoxConfig pName s 
  (auxHelperDoc opt) v)

-- Defines a sample input file
sampleInput :: (AuxiliarySym r) => ChunkDB -> DataDesc -> [Expr] -> 
  r (Auxiliary r)
sampleInput db d sd = auxFromData sampleInputName (makeInputFile db d sd)

-- Defines a Makefile
makefile :: (AuxiliarySym r) => Maybe BuildConfig -> Maybe Runnable -> 
  [Comments] -> GOOLState -> ProgData -> r (Auxiliary r)
makefile bc r cms s p = auxFromData makefileName (makeBuild cms bc r s p)

-- Changes a Runnable to Nothing if the user chose Library.
noRunIfLib :: ImplementationType -> Maybe Runnable -> Maybe Runnable
noRunIfLib Library _ = Nothing
noRunIfLib Program r = r
