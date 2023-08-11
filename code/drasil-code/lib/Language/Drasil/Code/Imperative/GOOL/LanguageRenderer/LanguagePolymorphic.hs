module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic (
  -- * Common Syntax
  doxConfig, readMe,sampleInput, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled
) where

import Language.Drasil (Expr)

import Database.Drasil (ChunkDB)

import GOOL.Drasil (ProgData, GOOLState)

import Language.Drasil.Choices (Comments, ImplementationType(..), Verbosity)
import Language.Drasil.Code.DataDesc (DataDesc)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable,
  DocConfig, doxygenDocConfig)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)
import Language.Drasil.Code.Imperative.WriteReadMe (makeReadMe)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName,
  makefileName, sampleInputName, readMeName)

import Language.Drasil.Code.Imperative.GOOL.ClassInterface ( ReadMeInfo(..),
  AuxiliarySym(Auxiliary, AuxHelper, auxHelperDoc, auxFromData))

-- | Defines a Doxygen configuration file.
doxConfig :: (AuxiliarySym r) => r (AuxHelper r) -> String ->
  GOOLState -> Verbosity -> r (Auxiliary r)
doxConfig opt pName s v = auxFromData doxConfigName (makeDoxConfig pName s
  (auxHelperDoc opt) v)

-- | Defines a markdown file.
readMe :: (AuxiliarySym r) => ReadMeInfo -> r (Auxiliary r)
readMe rmi= auxFromData readMeName (makeReadMe rmi)

-- | Defines a sample input file.
sampleInput :: (AuxiliarySym r) => ChunkDB -> DataDesc -> [Expr] ->
  r (Auxiliary r)
sampleInput db d sd = auxFromData sampleInputName (makeInputFile db d sd)

-- | Defines a Makefile.
makefile :: (AuxiliarySym r) => Maybe BuildConfig -> Maybe Runnable ->
  Maybe DocConfig -> GOOLState -> ProgData -> r (Auxiliary r)
makefile bc r d s p = auxFromData makefileName (makeBuild d bc r s p)

-- | Changes a 'Runnable' to 'Nothing' if the user chose 'Library' for the 'ImplementationType'.
noRunIfLib :: ImplementationType -> Maybe Runnable -> Maybe Runnable
noRunIfLib Library _ = Nothing
noRunIfLib Program r = r

-- | A DocConfig for Doxygen documentation.
doxDocConfig :: DocConfig
doxDocConfig = doxygenDocConfig doxConfigName

-- | Returns Nothing if no comments are enabled.
docIfEnabled :: [Comments] -> DocConfig -> Maybe DocConfig
docIfEnabled [] _ = Nothing
docIfEnabled _ d = Just d
