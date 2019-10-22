{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (
  CppProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..),
  AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (doxConfig, sampleInput, makefile)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, 
  PackData(..), packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, cppCompiler, nativeBinary)

import GOOL.Drasil (liftList)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import Text.PrettyPrint.HughesPJ (Doc, text)

newtype CppProject a = CPPP {unCPPP :: a}

instance Functor CppProject where
  fmap f (CPPP x) = CPPP (f x)

instance Applicative CppProject where
  pure = CPPP
  (CPPP f) <*> (CPPP x) = CPPP (f x)

instance Monad CppProject where
  return = CPPP
  CPPP x >>= f = f x

instance PackageSym CppProject where
  type Package CppProject = PackData
  package p = liftList (packD p)

instance AuxiliarySym CppProject where
  type Auxiliary CppProject = AuxData
  type AuxHelper CppProject = Doc
  doxConfig = G.doxConfig optimizeDox
  sampleInput = G.sampleInput

  optimizeDox = return $ text "NO"
  
  makefile = G.makefile cppBuildConfig cppRunnable
  
  auxHelperDoc = unCPPP
  auxFromData fp d = return $ ad fp d

-- helpers

cppBuildConfig :: Maybe BuildConfig
cppBuildConfig = buildAll $ \i o -> cppCompiler : i ++ map asFragment
  ["--std=c++11", "-o"] ++ [o]

cppRunnable :: Runnable
cppRunnable = nativeBinary