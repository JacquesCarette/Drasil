{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render C# auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.CSharpRenderer (
  CSharpCode(..)
) where

import Language.Drasil.Code.Imperative.GOOL.Symantics (PackageSym(..), 
  AuxiliarySym(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName, 
  makefileName, sampleInputName)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), 
  ad, PackData(..), packD)
import Language.Drasil.Code.Imperative.Doxygen.Import (makeDoxConfig)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, osClassDefault)
import Language.Drasil.Code.Imperative.Build.Import (makeBuild)
import Language.Drasil.Code.Imperative.WriteInput (makeInputFile)

import GOOL.Drasil (lift1List, CSharpCode(..))

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import qualified Prelude as P ((<>))
import Control.Applicative (liftA2)
import Text.PrettyPrint.HughesPJ (text)

instance PackageSym CSharpCode where
  type Package CSharpCode = PackData
  package = lift1List packD

instance AuxiliarySym CSharpCode where
  type Auxiliary CSharpCode = AuxData
  doxConfig pName p = fmap (ad doxConfigName) (liftA2 (makeDoxConfig pName)
    optimizeDox p)
  sampleInput db d sd = return $ ad sampleInputName (makeInputFile db d sd)

  optimizeDox = return $ text "NO"

  makefile cms = fmap (ad makefileName . makeBuild cms csBuildConfig csRunnable)

csBuildConfig :: Maybe BuildConfig
csBuildConfig = buildAll $ \i o -> [osClassDefault "CSC" "csc" "mcs", 
  asFragment "-out:" P.<> o] ++ i

csRunnable :: Runnable
csRunnable = nativeBinary