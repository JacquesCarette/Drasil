module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.CodeSpec (Comments)
import Language.Drasil.Code.Imperative.GOOL.Data (FileData(..), isSource, isHeader, 
  ModData(..), ProgData(..))
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName)

import Build.Drasil ((+:+), genMake, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, RuleTransformer(makeRule))

import Control.Applicative (liftA2)
import Data.Maybe (maybe, maybeToList)
import System.FilePath.Posix (takeExtension, takeBaseName)
import Text.PrettyPrint.HughesPJ (Doc)

data CodeHarness = Ch (Maybe BuildConfig) Runnable ProgData [Comments]

instance RuleTransformer CodeHarness where
  makeRule (Ch b r m cms) = [
    mkRule buildTarget (map (const $ renderBuildName m nameOpts nm) 
      $ maybeToList b) []
    ] ++
    maybe [] (\(BuildConfig comp bt) -> [
    mkFile (renderBuildName m nameOpts nm) (map (makeS . filePath) (progMods m))
      [
      mkCheckedCommand $ foldr (+:+) mempty $ comp (getCompilerInput bt m) $
        renderBuildName m nameOpts nm
      ]
    ]) b ++ [
    mkRule (makeS "run") [buildTarget] [
      mkCheckedCommand $ buildRunTarget (renderBuildName m no nm) ty +:+ mkFreeVar "RUNARGS"
      ]
    ] ++ [
    mkRule (makeS "doc") (getCommentedFiles (progMods m))
      [mkCheckedCommand $ makeS $ "doxygen " ++ doxConfigName] | not (null cms)
    ] where
      (Runnable nm no ty) = r
      buildTarget = makeS "build"

renderBuildName :: ProgData -> NameOpts -> BuildName -> MakeString
renderBuildName p _ BMain = makeS $ takeBaseName $ getMainModule p
renderBuildName p _ BPackName = makeS (progName p)
renderBuildName p o (BPack a) = renderBuildName p o BPackName <> 
  makeS (packSep o) <> renderBuildName p o a
renderBuildName p o (BWithExt a e) = renderBuildName p o a <> 
  if includeExt o then renderExt e (getMainModule p) else makeS ""

renderExt :: Ext -> FilePath -> MakeString
renderExt CodeExt f = makeS $ takeExtension f
renderExt (OtherExt e) _ = e

getMainModule :: ProgData -> FilePath
getMainModule p = mainName $ filter (isMainMod . fileMod) (progMods p)
  where mainName [FileD _ fp _] = fp
        mainName _ = error "Expected a single main module."

getCompilerInput :: BuildDependencies -> ProgData -> [MakeString]
getCompilerInput BcSource p = map (makeS . filePath) $ filter isSource $ 
  progMods p
getCompilerInput (BcSingle n) p = [renderBuildName p nameOpts n]

getCommentedFiles :: [FileData] -> [MakeString]
getCommentedFiles fs = map makeS (doxConfigName : map filePath (filter (liftA2 
  (||) (isMainMod . fileMod) isHeader) fs))

buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = i +:+ fn

makeBuild :: [Comments] -> Maybe BuildConfig -> Runnable -> ProgData -> Doc
makeBuild cms b r m = genMake [Ch b r m cms]
