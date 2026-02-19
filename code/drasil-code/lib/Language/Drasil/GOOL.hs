{-# LANGUAGE PatternSynonyms #-}

-- | Re-export code-related smart constructors for external code writing and generation.
module Language.Drasil.GOOL (
  AuxiliarySym(..), package,
  FileAndContents(..), hasPathAndDocToFileAndContents,
  PackageData(..), pattern PackageData,
  unPP, unJP, unCSP, unCPPP, unSP, unJLP
) where

import Language.Drasil.SoftwareDossier.ClassInterface (AuxiliarySym(..))
import Language.Drasil.Code.FileData (FileAndContents(..),
  hasPathAndDocToFileAndContents)
import Language.Drasil.Code.PackageData (PackageData(..), pattern PackageData,
  package)

import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (unPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (unJP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (unCSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (unCPPP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.SwiftRenderer (unSP)
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JuliaRenderer (unJLP)
