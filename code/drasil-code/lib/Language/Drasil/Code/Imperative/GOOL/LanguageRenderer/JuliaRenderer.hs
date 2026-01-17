{-# LANGUAGE TypeFamilies #-}
-- | The logic to render Julia auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JuliaRenderer (
  JuliaProject(..)
) where

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Text.PrettyPrint.HughesPJ (Doc, empty)

import Drasil.GProc (ProgData, onCodeList, jlName, jlVersion)

import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), AuxiliarySym(..))
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (sampleInput, readMe, makefile, noRunIfLib, docIfEnabled)
import Language.Drasil.Code.FileData (FileAndContents(..),
  fileAndContents, PackageData(..), packageData)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, DocConfig(..), interpMM)

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
  type Package JuliaProject = PackageData ProgData
  package p = onCodeList (packageData p)

instance AuxiliarySym JuliaProject where
  type Auxiliary JuliaProject = FileAndContents
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
  auxFromData fp d = pure $ fileAndContents fp d

-- | Default runnable information for Julia files
jlRunnable :: Maybe Runnable
jlRunnable = interpMM "julia"

-- | Julia is not compatible with Doxygen, so raise an error if trying to compile Doxygen documentation.
doxError :: String
doxError = jlName ++ " is not compatible with Doxygen."
