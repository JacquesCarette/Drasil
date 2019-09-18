{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (
  JavaProject(..)
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

import GOOL.Drasil (liftList)

import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc, text)

jNameOpts :: NameOpts
jNameOpts = NameOpts {
  packSep = ".",
  includeExt = False
}

newtype JavaProject a = JP {unJP :: a}

instance Functor JavaProject where
  fmap f (JP x) = JP (f x)

instance Applicative JavaProject where
  pure = JP
  (JP f) <*> (JP x) = JP (f x)

instance Monad JavaProject where
  return = JP
  JP x >>= f = f x

instance PackageSym JavaProject where
  type Package JavaProject = PackData
  package p = liftList (packD p)

instance AuxiliarySym JavaProject where
  type Auxiliary JavaProject = AuxData
  type AuxHelper JavaProject = Doc
  doxConfig pName p = fmap (ad doxConfigName . makeDoxConfig pName p)
    optimizeDox
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "YES"

  makefile cms p = return $ ad makefileName (makeBuild cms jBuildConfig 
    jRunnable p)

jBuildConfig :: Maybe BuildConfig
jBuildConfig = buildSingle (\i _ -> asFragment "javac" : i) $
  inCodePackage mainModuleFile

jRunnable :: Runnable
jRunnable = interp (flip withExt ".class" $ inCodePackage mainModule) 
  jNameOpts "java"