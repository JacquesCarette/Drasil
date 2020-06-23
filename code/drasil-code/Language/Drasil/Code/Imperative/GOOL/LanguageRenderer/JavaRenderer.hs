{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PostfixOperators #-}

-- | The logic to render Java auxiliary files is contained in this module
module Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.JavaRenderer (
  JavaProject(..)
) where

import Language.Drasil.Choices (ImplementationType(..))
import Language.Drasil.Code.Imperative.GOOL.ClassInterface (PackageSym(..), 
  AuxiliarySym(..))
import qualified 
  Language.Drasil.Code.Imperative.GOOL.LanguageRenderer.LanguagePolymorphic as 
  G (doxConfig, sampleInput, makefile, noRunIfLib)
import Language.Drasil.Code.Imperative.GOOL.Data (AuxData(..), ad, PackData(..),
  packD)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig, BuildName(..), 
  Ext(..), Runnable, NameOpts(NameOpts), asFragment, buildSingle, 
  buildAllAdditionalName, includeExt, inCodePackage, interp, mainModule, 
  mainModuleFile, packSep, withExt)
import Language.Drasil.Code.Imperative.Doxygen.Import (yes)

import GOOL.Drasil (onCodeList)

import Data.List (intercalate)
import Prelude hiding (break,print,sin,cos,tan,floor,(<>))
import Text.PrettyPrint.HughesPJ (Doc)

jNameOpts :: NameOpts
jNameOpts = NameOpts {
  packSep = ".",
  includeExt = False
}

newtype JavaProject a = JP {unJP :: a}

instance Functor JavaProject where
  fmap f (JP x) = JP (f x)

instance Applicative JavaProject where
  pure = JP
  (JP f) <*> (JP x) = JP (f x)

instance Monad JavaProject where
  return = JP
  JP x >>= f = f x

instance PackageSym JavaProject where
  type Package JavaProject = PackData
  package p = onCodeList (packD p)

instance AuxiliarySym JavaProject where
  type Auxiliary JavaProject = AuxData
  type AuxHelper JavaProject = Doc
  doxConfig = G.doxConfig optimizeDox
  sampleInput = G.sampleInput

  optimizeDox = return yes

  makefile fs it = G.makefile (jBuildConfig fs it) 
    (G.noRunIfLib it (jRunnable fs))

  auxHelperDoc = unJP
  auxFromData fp d = return $ ad fp d

jBuildConfig :: [FilePath] -> ImplementationType -> Maybe BuildConfig
jBuildConfig fs Program = buildSingle (\i _ -> [asFragment "javac" : map 
  asFragment (classPath fs) ++ i]) (withExt (inCodePackage mainModule) 
  ".class") $ inCodePackage mainModuleFile
jBuildConfig fs Library = buildAllAdditionalName (\i o a -> 
  [asFragment "javac" : map asFragment (classPath fs) ++ i,
    map asFragment ["jar", "-cvf"] ++ [o, a]]) 
  (BWithExt BPackName $ OtherExt $ asFragment ".jar") BPackName

jRunnable :: [FilePath] -> Maybe Runnable
jRunnable fs = interp (flip withExt ".class" $ inCodePackage mainModule) 
  jNameOpts "java" (classPath fs)

classPath :: [FilePath] -> [String]
classPath fs = if null fs then [] else 
  ["-cp", "\"" ++ intercalate ":" (fs ++ ["."]) ++ "\""]
