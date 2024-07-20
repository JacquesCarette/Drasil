{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Julia auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JuliaRenderer (
  JuliaProject(..)
) where

import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),
  PackageSym(..), AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (sampleInput, readMe, makefile, noRunIfLib, docIfEnabled)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, DocConfig(..), interpMM)

import Drasil.GProc (onCodeList, jlName, jlVersion)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Text.PrettyPrint.HughesPJ (Doc, empty)

-- | Holds a Julia project
newtype JuliaProject a = JLP {unJLP :: a}

instance Functor JuliaProject where
  fmap f (JLP x) = JLP (f x)

instance Applicative JuliaProject where
  pure = JLP
  (JLP f) <*> (JLP x) = JLP (f x)

instance Monad JuliaProject where
  JLP x >>= f = f x

instance PackageSym JuliaProject where
  type Package JuliaProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym JuliaProject where
  type Auxiliary JuliaProject = AuxData
  type AuxHelper JuliaProject = Doc
  doxConfig _ _ _ = auxFromData "" empty -- Doxygen does not support Julia
  readMe rmi = G.readMe rmi {
        langName = jlName,
        langVersion = jlVersion}
  sampleInput = G.sampleInput

  optimizeDox = error doxError

  makefile _ it cms = G.makefile Nothing (G.noRunIfLib it jlRunnable)
                            (G.docIfEnabled cms (DocConfig [] []))

  auxHelperDoc = unJLP
  auxFromData fp d = pure $ ad fp d

-- | Default runnable information for Julia files
jlRunnable :: Maybe Runnable
jlRunnable = interpMM "julia"
  
-- | Julia is not compatible with Doxygen, so raise an error if trying to compile Doxygen documentation.
doxError :: String
doxError = jlName ++ " is not compatible with Doxygen."
  