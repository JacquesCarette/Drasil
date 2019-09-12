{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer ( 
  JavaCode(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (
  doxConfigName, makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  NameOpts(NameOpts), asFragment, buildSingle, includeExt, inCodePackage, 
  interp, mainModule, mainModuleFile, packSep, withExt)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import GOOL.Drasil (lift1List, JavaCode(..))

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Control.Applicative (liftA2)
import Text.PrettyPrint.HughesPJ (text)

jNameOpts :: NameOpts
jNameOpts = NameOpts {
  packSep = ".",
  includeExt = False
}

instance PackageSym JavaCode where
  type Package JavaCode = PackData
  package = lift1List packD

instance AuxiliarySym JavaCode where
  type Auxiliary JavaCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "YES"

  makefile cms = fmap (ad makefileName . makeBuild cms jBuildConfig jRunnable)

jBuildConfig :: Maybe BuildConfig
jBuildConfig = buildSingle (\i _ -> asFragment "javac" : i) $
  inCodePackage mainModuleFile

jRunnable :: Runnable
jRunnable = interp (flip withExt ".class" $ inCodePackage mainModule) 
  jNameOpts "java"