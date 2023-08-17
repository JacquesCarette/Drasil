{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (
  PythonProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),PackageSym(..),
  AuxiliarySym(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (doxConfig, readMe, sampleInput, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, interpMM)
import Language.Drasil.Code.Imperative.Doxygen.Import (yes)

import GOOL.Drasil (onCodeList, pyName, pyVersion)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc)

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
  type Package PythonProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym PythonProject where
  type Auxiliary PythonProject = AuxData
  type AuxHelper PythonProject = Doc
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
  auxFromData fp d = pure $ ad fp d

-- | Default runnable information for Python files.
pyRunnable :: Maybe Runnable
pyRunnable = interpMM "python3"
