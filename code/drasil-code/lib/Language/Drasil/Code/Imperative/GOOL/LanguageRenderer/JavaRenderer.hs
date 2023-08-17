{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (
  JavaProject(..)
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (ReadMeInfo(..),
  PackageSym(..), AuxiliarySym(..))
import qualified
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as
  G (doxConfig, readMe, sampleInput, makefile, noRunIfLib, doxDocConfig, docIfEnabled)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, BuildName(..),
  Ext(..), Runnable, NameOpts(NameOpts), asFragment, buildSingle,
  buildAllAdditionalName, includeExt, inCodePackage, interp, mainModule,
  mainModuleFile, packSep, withExt)
import Language.Drasil.Code.Imperative.Doxygen.Import (yes)

import GOOL.Drasil (onCodeList, jName, jVersion)

import Data.List (intercalate)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc)

-- | Name options for Java files.
jNameOpts :: NameOpts
jNameOpts = NameOpts {
  packSep = ".",
  includeExt = False
}

-- | Holds a Java project.
newtype JavaProject a = JP {unJP :: a}

instance Functor JavaProject where
  fmap f (JP x) = JP (f x)

instance Applicative JavaProject where
  pure = JP
  (JP f) <*> (JP x) = JP (f x)

instance Monad JavaProject where
  JP x >>= f = f x

instance PackageSym JavaProject where
  type Package JavaProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym JavaProject where
  type Auxiliary JavaProject = AuxData
  type AuxHelper JavaProject = Doc
  doxConfig = G.doxConfig optimizeDox
  readMe rmi =
    G.readMe rmi {
        langName = jName,
        langVersion = jVersion}
  sampleInput = G.sampleInput

  optimizeDox = pure yes

  makefile fs it cms = G.makefile (jBuildConfig fs it)
    (G.noRunIfLib it (jRunnable fs)) (G.docIfEnabled cms G.doxDocConfig)

  auxHelperDoc = unJP
  auxFromData fp d = pure $ ad fp d

-- | Create a build configuration for Java files. Takes in 'FilePath's and the type of implementation.
jBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
jBuildConfig fs Program = buildSingle (\i _ -> [asFragment "javac" : map
  asFragment (classPath fs) ++ i]) (withExt (inCodePackage mainModule)
  ".class") $ inCodePackage mainModuleFile
jBuildConfig fs Library = buildAllAdditionalName (\i o a ->
  [asFragment "javac" : map asFragment (classPath fs) ++ i,
    map asFragment ["jar", "-cvf"] ++ [o, a]])
  (BWithExt BPackName $ OtherExt $ asFragment ".jar") BPackName

-- | Default runnable information for Java files.
jRunnable :: [FilePath] -> Maybe Runnable
jRunnable fs = interp (flip withExt ".class" $ inCodePackage mainModule)
  jNameOpts "java" (classPath fs)

-- | Helper for formating file paths for use in 'jBuildConfig'.
classPath :: [FilePath] -> [String]
classPath fs = if null fs then [] else
  ["-cp", "\"" ++ intercalate ":" (fs ++ ["."]) ++ "\""]
