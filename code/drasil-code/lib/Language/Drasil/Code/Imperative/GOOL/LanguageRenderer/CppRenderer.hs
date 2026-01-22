{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PostfixOperators #-}
-- | The logic to render C++ auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CppRenderer (
  CppProject(..)
) where

import Prelude hiding (break,print,(<>),sin,cos,tan,floor,const,log,exp)

import Drasil.GOOL (ProgData, onCodeList, cppName, cppVersion)

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), AuxiliarySym(..))
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (doxConfig, readMe, sampleInput, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled)
import Language.Drasil.Code.FileData (PackageData(..), packageData)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable,
  asFragment, buildAll, cppCompiler, nativeBinary, executable, sharedLibrary)
import Language.Drasil.Code.Imperative.Doxygen.Import (no)

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
  type Package CppProject = PackageData ProgData
  package p = onCodeList (packageData p)

instance AuxiliarySym CppProject where
  doxConfig = G.doxConfig optimizeDox
  readMe rmi =
    G.readMe rmi {
        langName = cppName,
        langVersion = cppVersion}
  sampleInput = G.sampleInput

  optimizeDox = pure no

  makefile fs it cms = G.makefile (cppBuildConfig fs it) (G.noRunIfLib it cppRunnable) (G.docIfEnabled cms G.doxDocConfig)

  auxHelperDoc = unCPPP

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
