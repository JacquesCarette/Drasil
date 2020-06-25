{-# LANGUAGE TypeFamilies #-}

-- | The logic to render Swift auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.SwiftRenderer (
  SwiftProject(..)
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),
  PackageSym(..), AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (sampleInput, readMe, makefile, noRunIfLib)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable, 
  asFragment, buildAll, nativeBinary, executable, sharedLibrary)

import GOOL.Drasil (onCodeList, swiftName, swiftVersion)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Text.PrettyPrint.HughesPJ (Doc)

newtype SwiftProject a = SP {unSP :: a}

instance Functor SwiftProject where
  fmap f (SP x) = SP (f x)

instance Applicative SwiftProject where
  pure = SP
  (SP f) <*> (SP x) = SP (f x)

instance Monad SwiftProject where
  return = SP
  SP x >>= f = f x

instance PackageSym SwiftProject where
  type Package SwiftProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym SwiftProject where
  type Auxiliary SwiftProject = AuxData
  type AuxHelper SwiftProject = Doc
  doxConfig = error doxError
  readMe ReadMeInfo {
        implementType = impl,
        extLibNV = exlnv,
        extLibFP = exlfp,
        contributors = auths, 
        configFP = cfp,
        caseName = n} =
    G.readMe ReadMeInfo {
        langName = swiftName,
        langVersion = swiftVersion,
        invalidOS = Nothing,
        implementType = impl,
        extLibNV = exlnv,
        extLibFP = exlfp,
        contributors = auths, 
        configFP = cfp,
        caseName = n}
  sampleInput = G.sampleInput

  optimizeDox = error doxError

  makefile fs it = G.makefile (swiftBuildConfig fs it) (G.noRunIfLib it swiftRunnable)

  auxHelperDoc = unSP
  auxFromData fp d = return $ ad fp d

swiftBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
swiftBuildConfig fs it = buildAll (\i o -> [asFragment "swiftc" : i ++
  [asFragment "-o", o] ++ concatMap (\f -> map asFragment ["-I", f]) fs ++
  asLib it]) (outName it)
  where asLib Library = [asFragment "-parse-as-library"]
        asLib Program = []
        outName Library = sharedLibrary
        outName Program = executable

swiftRunnable :: Maybe Runnable
swiftRunnable = nativeBinary

doxError :: String
doxError = swiftName ++ " is not compatible with Doxygen."