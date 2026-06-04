-- | The logic to render MATLAB auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.MatlabRenderer (
  MatlabProject(..)
) where

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)

import Drasil.GProc (mlName, mlVersion)

import Language.Drasil.SoftwareDossier.SoftwareDossierSym (SoftwareDossierSym(..))
import Language.Drasil.Code.Imperative.README (ReadMeInfo(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (readMe, makefile, noRunIfLib, docIfEnabled)
import Language.Drasil.Code.Imperative.Build.AST (Runnable, DocConfig(..), interpMM)

-- | Holds a MATLAB project
newtype MatlabProject a = MLP {unMLP :: a}

instance Functor MatlabProject where
  fmap f (MLP x) = MLP (f x)

instance Applicative MatlabProject where
  pure = MLP
  (MLP f) <*> (MLP x) = MLP (f x)

instance Monad MatlabProject where
  MLP x >>= f = f x

instance SoftwareDossierSym MatlabProject where
  doxConfig _ _ _ = Nothing
  readMe rmi = G.readMe rmi {
        langName = mlName,
        langVersion = mlVersion}

  optimizeDox = error doxError

  makefile _ it cms = G.makefile Nothing (G.noRunIfLib it mlRunnable)
                            (G.docIfEnabled cms (DocConfig [] []))

  unReprDoc = unMLP

-- | Default runnable information for MATLAB files
mlRunnable :: Maybe Runnable
mlRunnable = interpMM "octave"

doxError :: String
doxError = mlName ++ " is not compatible with Doxygen."
