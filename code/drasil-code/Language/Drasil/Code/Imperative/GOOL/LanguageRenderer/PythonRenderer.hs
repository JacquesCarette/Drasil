{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Python auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.PythonRenderer (
  PythonCode(..)
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

import GOOL.Drasil (lift1List, PythonCode(..))

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Control.Applicative (liftA2)
import Text.PrettyPrint.HughesPJ (text)

instance PackageSym PythonCode where
  type Package PythonCode = PackData
  package = lift1List packD

instance AuxiliarySym PythonCode where
  type Auxiliary PythonCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "YES"

  makefile cms = fmap (ad makefileName . makeBuild cms Nothing pyRunnable)

pyRunnable :: Runnable
pyRunnable = interpMM "python"
