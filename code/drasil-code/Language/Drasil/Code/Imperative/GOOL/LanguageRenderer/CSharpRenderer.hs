{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (
  CSharpProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (doxConfig, sampleInput, makefile)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, osClassDefault)
import Language.Drasil.Code.Imperative.Doxygen.Import (no)

import GOOL.Drasil (liftList, initialState)

import Control.Monad.State (evalState)
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
  package p = liftList (packD $ evalState p initialState)

instance AuxiliarySym CSharpProject where
  type Auxiliary CSharpProject = AuxData
  type AuxHelper CSharpProject = Doc
  doxConfig = G.doxConfig optimizeDox
  sampleInput = G.sampleInput

  optimizeDox = return no

  makefile = G.makefile csBuildConfig csRunnable

  auxHelperDoc = unCSP
  auxFromData fp d = return $ ad fp d

csBuildConfig :: Maybe BuildConfig
csBuildConfig = buildAll $ \i o -> [osClassDefault "CSC" "csc" "mcs", 
  asFragment "-out:" P.<> o] ++ i

csRunnable :: Runnable
csRunnable = nativeBinary