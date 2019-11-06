{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (
  PythonProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (doxConfig, sampleInput, makefile)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, interpMM)
import Language.Drasil.Code.Imperative.Doxygen.Import (yes)

import GOOL.Drasil (liftList, initialState)

import Control.Monad.State (evalState)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc)

newtype PythonProject a = PP {unPP :: a}

instance Functor PythonProject where
  fmap f (PP x) = PP (f x)

instance Applicative PythonProject where
  pure = PP
  (PP f) <*> (PP x) = PP (f x)

instance Monad PythonProject where
  return = PP
  PP x >>= f = f x

instance PackageSym PythonProject where
  type Package PythonProject = PackData
  package p = liftList (packD $ evalState p initialState)

instance AuxiliarySym PythonProject where
  type Auxiliary PythonProject = AuxData
  type AuxHelper PythonProject = Doc
  doxConfig = G.doxConfig optimizeDox
  sampleInput = G.sampleInput

  optimizeDox = return yes

  makefile = G.makefile Nothing pyRunnable

  auxHelperDoc = unPP
  auxFromData fp d = return $ ad fp d

pyRunnable :: Runnable
pyRunnable = interpMM "python"
