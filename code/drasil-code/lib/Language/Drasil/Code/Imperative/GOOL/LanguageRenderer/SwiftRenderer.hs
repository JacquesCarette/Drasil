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
  G (sampleInput, readMe, makefile, noRunIfLib, docIfEnabled)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, Runnable,
  DocConfig(..), asFragment, buildAll, nativeBinary, executable, sharedLibrary)

import GOOL.Drasil (onCodeList, swiftName, swiftVersion)

import Prelude hiding (break,print,(<>),sin,cos,tan,floor)
import Text.PrettyPrint.HughesPJ (Doc, empty)

-- | Holds a Swift project.
newtype SwiftProject a = SP {unSP :: a}

instance Functor SwiftProject where
  fmap f (SP x) = SP (f x)

instance Applicative SwiftProject where
  pure = SP
  (SP f) <*> (SP x) = SP (f x)

instance Monad SwiftProject where
  SP x >>= f = f x

instance PackageSym SwiftProject where
  type Package SwiftProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym SwiftProject where
  type Auxiliary SwiftProject = AuxData
  type AuxHelper SwiftProject = Doc
  doxConfig _ _ _ = auxFromData "" empty
  readMe rmi = G.readMe rmi {
        langName = swiftName,
        langVersion = swiftVersion}
  sampleInput = G.sampleInput

  optimizeDox = error doxError

  makefile fs it cms = G.makefile (swiftBuildConfig fs it) (G.noRunIfLib it swiftRunnable) (G.docIfEnabled cms (DocConfig [] []))

  auxHelperDoc = unSP
  auxFromData fp d = pure $ ad fp d

-- | Create a build configuration for Swift files. Takes in 'FilePath's and the type of implementation.
swiftBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
swiftBuildConfig fs it = buildAll (\i o -> [asFragment "swiftc" : i ++
  [asFragment "-o", o] ++ concatMap (\f -> map asFragment ["-I", f]) fs ++
  asLib it]) (outName it)
  where asLib Library = [asFragment "-emit-library"]
        asLib Program = []
        outName Library = sharedLibrary
        outName Program = executable

-- | Default runnable information for Swift files.
swiftRunnable :: Maybe Runnable
swiftRunnable = nativeBinary

-- | Swift is not compatible with Doxygen, so raise an error if trying to compile Doxygen documentation.
doxError :: String
doxError = swiftName ++ " is not compatible with Doxygen."
