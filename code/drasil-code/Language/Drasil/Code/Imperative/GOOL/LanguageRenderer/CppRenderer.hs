{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (
  CppProject(..)
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..),
  AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (doxConfig, sampleInput, makefile, noRunIfLib)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, 
  PackData(..), packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, cppCompiler, nativeBinary, executable, sharedLibrary)
import Language.Drasil.Code.Imperative.Doxygen.Import (no)

import GOOL.Drasil (onCodeList)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import Text.PrettyPrint.HughesPJ (Doc)

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
  package p = onCodeList (packD p)

instance AuxiliarySym CppProject where
  type Auxiliary CppProject = AuxData
  type AuxHelper CppProject = Doc
  doxConfig = G.doxConfig optimizeDox
  sampleInput = G.sampleInput

  optimizeDox = return no
  
  makefile fs it = G.makefile (cppBuildConfig fs it) (G.noRunIfLib it cppRunnable)
  
  auxHelperDoc = unCPPP
  auxFromData fp d = return $ ad fp d

-- helpers

cppBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
cppBuildConfig fs it = buildAll (\i o -> [cppCompiler : i ++ map asFragment
  ("--std=c++11" : target it ++ ["-o"]) ++ [o] ++ concatMap (\f -> map 
  asFragment ["-I", f]) fs]) (outName it)
  where target Library = ["-shared", "-fPIC"]
        target Program = []
        outName Library = sharedLibrary
        outName Program = executable

cppRunnable :: Maybe Runnable
cppRunnable = nativeBinary