{-# LANGUAGE TypeFamilies #-}
-- | The logic to render Python auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (
  PythonProject(..)
) where

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))

import Drasil.GOOL (ProgData, onCodeList, pyName, pyVersion)

import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), AuxiliarySym(..))
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..))

import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (doxConfig, readMe, sampleInput, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled)
import Language.Drasil.Code.FileData (PackageData(..), packageData)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, interpMM)
import Language.Drasil.Code.Imperative.Doxygen.Import (yes)

-- | Holds a Python project.
newtype PythonProject a = PP {unPP :: a}

instance Functor PythonProject where
  fmap f (PP x) = PP (f x)

instance Applicative PythonProject where
  pure = PP
  (PP f) <*> (PP x) = PP (f x)

instance Monad PythonProject where
  PP x >>= f = f x

instance PackageSym PythonProject where
  type Package PythonProject = PackageData ProgData
  package p = onCodeList (packageData p)

instance AuxiliarySym PythonProject where
  doxConfig = G.doxConfig optimizeDox
  readMe rmi =
    G.readMe rmi {
        langName = pyName,
        langVersion = pyVersion}
  sampleInput = G.sampleInput

  optimizeDox = pure yes

  makefile _ it cms = G.makefile Nothing (G.noRunIfLib it pyRunnable)
    (G.docIfEnabled cms G.doxDocConfig)

  auxHelperDoc = unPP

-- | Default runnable information for Python files.
pyRunnable :: Maybe Runnable
pyRunnable = interpMM "python3"
