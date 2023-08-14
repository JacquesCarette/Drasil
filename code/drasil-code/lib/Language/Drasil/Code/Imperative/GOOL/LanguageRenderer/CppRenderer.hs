{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C++ auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (
  CppProject(..)
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),
  PackageSym(..), AuxiliarySym(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (doxConfig, readMe, sampleInput, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad,
  PackData(..), packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable,
  asFragment, buildAll, cppCompiler, nativeBinary, executable, sharedLibrary)
import Language.Drasil.Code.Imperative.Doxygen.Import (no)

import GOOL.Drasil (onCodeList, cppName, cppVersion)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)
import Text.PrettyPrint.HughesPJ (Doc)

-- | Holds a C++ project.
newtype CppProject a = CPPP {unCPPP :: a}

instance Functor CppProject where
  fmap f (CPPP x) = CPPP (f x)

instance Applicative CppProject where
  pure = CPPP
  (CPPP f) <*> (CPPP x) = CPPP (f x)

instance Monad CppProject where
  CPPP x >>= f = f x

instance PackageSym CppProject where
  type Package CppProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym CppProject where
  type Auxiliary CppProject = AuxData
  type AuxHelper CppProject = Doc
  doxConfig = G.doxConfig optimizeDox
  readMe rmi =
    G.readMe rmi {
        langName = cppName,
        langVersion = cppVersion}
  sampleInput = G.sampleInput

  optimizeDox = pure no

  makefile fs it cms = G.makefile (cppBuildConfig fs it) (G.noRunIfLib it cppRunnable) (G.docIfEnabled cms G.doxDocConfig)

  auxHelperDoc = unCPPP
  auxFromData fp d = pure $ ad fp d

-- helpers
-- | Create a build configuration for C++ files. Takes in 'FilePath's and the type of implementation.
cppBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
cppBuildConfig fs it = buildAll (\i o -> [cppCompiler : i ++ map asFragment
  ("--std=c++11" : target it ++ ["-o"]) ++ [o] ++ concatMap (\f -> map
  asFragment ["-I", f]) fs]) (outName it)
  where target Library = ["-shared", "-fPIC"]
        target Program = []
        outName Library = sharedLibrary
        outName Program = executable

-- | Default runnable information for C++ files.
cppRunnable :: Maybe Runnable
cppRunnable = nativeBinary
