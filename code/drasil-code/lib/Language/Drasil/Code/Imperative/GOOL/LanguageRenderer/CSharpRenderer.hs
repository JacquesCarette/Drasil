{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (
  CSharpProject(..)
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),
  PackageSym(..), AuxiliarySym(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (doxConfig, readMe, sampleInput, makefile, noRunIfLib, doxDocConfig,
  docIfEnabled)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable,
  asFragment, buildAll, nativeBinary, osClassDefault, executable, sharedLibrary)
import Language.Drasil.Code.Imperative.Doxygen.Import (no)

import GOOL.Drasil (onCodeList, csName, csVersion)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Prelude as P ((<>))
import Text.PrettyPrint.HughesPJ (Doc)

-- | Holds a C# project.
newtype CSharpProject a = CSP {unCSP :: a}

instance Functor CSharpProject where
  fmap f (CSP x) = CSP (f x)

instance Applicative CSharpProject where
  pure = CSP
  (CSP f) <*> (CSP x) = CSP (f x)

instance Monad CSharpProject where
  CSP x >>= f = f x

instance PackageSym CSharpProject where
  type Package CSharpProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym CSharpProject where
  type Auxiliary CSharpProject = AuxData
  type AuxHelper CSharpProject = Doc
  doxConfig = G.doxConfig optimizeDox
  readMe rmi =
    G.readMe rmi {
        langName = csName,
        langVersion = csVersion,
        invalidOS = Just "All OS's except Windows"}
  sampleInput = G.sampleInput

  optimizeDox = pure no

  makefile fs it cms = G.makefile (csBuildConfig fs it)
    (G.noRunIfLib it csRunnable) (G.docIfEnabled cms G.doxDocConfig)

  auxHelperDoc = unCSP
  auxFromData fp d = pure $ ad fp d

-- | Create a build configuration for C# files. Takes in 'FilePath's and the type of implementation.
csBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
csBuildConfig fs it = buildAll (\i o -> [osClassDefault "CSC" "csc" "mcs"
  : target it ++ [asFragment "-out:" P.<> o] ++ map (asFragment . ("-r:" ++)) fs
  ++ i]) (outName it)
  where target Library = [asFragment "-t:library"]
        target Program = []
        outName Library = sharedLibrary
        outName Program = executable

-- | Default runnable information for C# files.
csRunnable :: Maybe Runnable
csRunnable = nativeBinary
