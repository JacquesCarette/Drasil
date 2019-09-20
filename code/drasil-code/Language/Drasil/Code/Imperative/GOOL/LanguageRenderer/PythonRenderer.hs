{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (
  PythonProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, interpMM)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import GOOL.Drasil (liftList)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text)

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
  package p = liftList (packD p)

instance AuxiliarySym PythonProject where
  type Auxiliary PythonProject = AuxData
  type AuxHelper PythonProject = Doc
  doxConfig pName p = fmap (ad doxConfigName . makeDoxConfig pName p)
    optimizeDox
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "YES"

  makefile cms p = return $ ad makefileName (makeBuild cms Nothing pyRunnable p)

pyRunnable :: Runnable
pyRunnable = interpMM "python"
