{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (
  CSharpProject(..)
) where

import Language.Drasil.CodeSpec (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (doxConfig, sampleInput, makefile, noRunIfLib)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, osClassDefault, executable, sharedLibrary)
import Language.Drasil.Code.Imperative.Doxygen.Import (no)

import GOOL.Drasil (onCodeList)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Prelude as P ((<>))
import Text.PrettyPrint.HughesPJ (Doc)

newtype CSharpProject a = CSP {unCSP :: a}

instance Functor CSharpProject where
  fmap f (CSP x) = CSP (f x)

instance Applicative CSharpProject where
  pure = CSP
  (CSP f) <*> (CSP x) = CSP (f x)

instance Monad CSharpProject where
  return = CSP
  CSP x >>= f = f x

instance PackageSym CSharpProject where
  type Package CSharpProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym CSharpProject where
  type Auxiliary CSharpProject = AuxData
  type AuxHelper CSharpProject = Doc
  doxConfig = G.doxConfig optimizeDox
  sampleInput = G.sampleInput

  optimizeDox = return no

  makefile it = G.makefile (csBuildConfig it) (G.noRunIfLib it csRunnable)

  auxHelperDoc = unCSP
  auxFromData fp d = return $ ad fp d

csBuildConfig :: ImplementationType -> Maybe BuildConfig
csBuildConfig it = buildAll (\i o -> [osClassDefault "CSC" "csc" "mcs" 
  : target it ++ [asFragment "-out:" P.<> o] ++ i]) (outName it)
  where target Library = [asFragment "-t:library"]
        target Program = []
        outName Library = sharedLibrary
        outName Program = executable

csRunnable :: Maybe Runnable
csRunnable = nativeBinary