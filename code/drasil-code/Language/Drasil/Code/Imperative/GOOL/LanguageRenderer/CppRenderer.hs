{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (
  CppSrcCode(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..),
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, emptyAux, 
  PackData(..), packD, emptyPack)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, cppCompiler, nativeBinary)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import GOOL.Drasil (Pair(..), lift1List, CppSrcCode(..), CppHdrCode)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import Control.Applicative (liftA2)
import Text.PrettyPrint.HughesPJ (text, empty)

instance (Pair p) => PackageSym (p CppSrcCode CppHdrCode) where
  type Package (p CppSrcCode CppHdrCode) = PackData
  package p aux = pair (package (pfst p) (map pfst aux)) (return emptyPack)

instance (Pair p) => AuxiliarySym (p CppSrcCode CppHdrCode) where
  type Auxiliary (p CppSrcCode CppHdrCode) = AuxData
  doxConfig pName p = pair (doxConfig pName $ pfst p) (return emptyAux)
  sampleInput db d sd = pair (sampleInput db d sd) (return emptyAux)

  optimizeDox = pair optimizeDox (return empty)

  makefile cms p = pair (makefile cms $ pfst p) (return emptyAux)

-----------------
-- Source File --
-----------------

instance PackageSym CppSrcCode where
  type Package CppSrcCode = PackData
  package = lift1List packD

instance AuxiliarySym CppSrcCode where
  type Auxiliary CppSrcCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "NO"
  
  makefile cms = fmap (ad makefileName . makeBuild cms cppBuildConfig 
    cppRunnable)

-- helpers

cppBuildConfig :: Maybe BuildConfig
cppBuildConfig = buildAll $ \i o -> cppCompiler : i ++ map asFragment
  ["--std=c++11", "-o"] ++ [o]

cppRunnable :: Runnable
cppRunnable = nativeBinary