module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic (
  -- * Common Syntax
  doxConfig, readMe, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled
) where

import Text.PrettyPrint.HughesPJ (Doc)

import Language.Drasil.Choices (Comments, ImplementationType(..), Verbosity)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable,
  DocConfig, doxygenDocConfig)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.FileNames (doxConfigName, makefileName, readMeName)
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (
    AuxiliarySym(auxHelperDoc), auxFromData
  )
import Language.Drasil.Code.FileData (FileAndContents)
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..), makeReadMe)

-- | Defines a Doxygen configuration file.
doxConfig :: (AuxiliarySym r, Applicative r) => r Doc -> String ->
  stateRepr -> Verbosity -> r FileAndContents
doxConfig opt pName s v = auxFromData doxConfigName (makeDoxConfig pName s
  (auxHelperDoc opt) v)

-- | Defines a markdown file.
readMe :: (Applicative r) => ReadMeInfo -> r FileAndContents
readMe rmi= auxFromData readMeName (makeReadMe rmi)

-- | Defines a Makefile.
makefile :: (Applicative r) => Maybe BuildConfig -> Maybe Runnable ->
  Maybe DocConfig -> stateRepr -> progRepr -> r FileAndContents
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
