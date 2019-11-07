module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.CodeSpec (Comments)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName)

import GOOL.Drasil (FileData(..), isHeader, ModData(..), ProgData(..), 
  GOOLState(..), sources)

import Build.Drasil ((+:+), genMake, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, RuleTransformer(makeRule))

import Control.Applicative (liftA2)
import Control.Lens ((^.))
import Data.Maybe (maybe, maybeToList)
import System.FilePath.Posix (takeExtension, takeBaseName)
import Text.PrettyPrint.HughesPJ (Doc)

data CodeHarness = Ch {
  buildConfig :: Maybe BuildConfig,
  runnable :: Runnable, 
  goolState :: GOOLState,
  progData :: ProgData,
  cmts :: [Comments]}

instance RuleTransformer CodeHarness where
  makeRule (Ch b r s m cms) = [
    mkRule buildTarget (map (const $ renderBuildName m nameOpts nm) 
      $ maybeToList b) []
    ] ++
    maybe [] (\(BuildConfig comp bt) -> [
    mkFile (renderBuildName m nameOpts nm) (map (makeS . filePath) (progMods m))
      [
      mkCheckedCommand $ foldr (+:+) mempty $ comp (getCompilerInput bt m s) $
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

getCompilerInput :: BuildDependencies -> ProgData -> GOOLState -> [MakeString]
getCompilerInput BcSource _ s = map makeS $ s ^. sources
getCompilerInput (BcSingle n) p _ = [renderBuildName p nameOpts n]

getCommentedFiles :: [FileData] -> [MakeString]
getCommentedFiles fs = map makeS (doxConfigName : map filePath (filter (liftA2 
  (||) (isMainMod . fileMod) isHeader) fs))

buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = i +:+ fn

makeBuild :: [Comments] -> Maybe BuildConfig -> Runnable -> GOOLState -> 
  ProgData -> Doc
makeBuild cms b r s p = genMake [Ch {
  buildConfig = b,
  runnable = r,
  goolState = s,
  progData = p,
  cmts = cms}]
