module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.CodeSpec (Comments)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))
import Language.Drasil.Code.Imperative.GOOL.LanguageRenderer (doxConfigName)

import GOOL.Drasil (FileData(..), ProgData(..), GOOLState(..), headers, sources,
  mainMod)

import Build.Drasil ((+:+), genMake, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, RuleTransformer(makeRule))

import Control.Lens ((^.))
import Data.Maybe (maybe, maybeToList)
import Data.List (nub)
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
    mkRule buildTarget (map (const $ renderBuildName s m nameOpts nm) 
      $ maybeToList b) []
    ] ++
    maybe [] (\(BuildConfig comp bt) -> [
    mkFile (renderBuildName s m nameOpts nm) (map (makeS . filePath) (progMods m))
      [
      mkCheckedCommand $ foldr (+:+) mempty $ comp (getCompilerInput bt s m) $
        renderBuildName s m nameOpts nm
      ]
    ]) b ++ [
    mkRule (makeS "run") [buildTarget] [
      mkCheckedCommand $ buildRunTarget (renderBuildName s m no nm) ty +:+ mkFreeVar "RUNARGS"
      ]
    ] ++ [
    mkRule (makeS "doc") (getCommentedFiles s)
      [mkCheckedCommand $ makeS $ "doxygen " ++ doxConfigName] | not (null cms)
    ] where
      (Runnable nm no ty) = r
      buildTarget = makeS "build"

renderBuildName :: GOOLState -> ProgData -> NameOpts -> BuildName -> MakeString
renderBuildName s _ _ BMain = makeS $ maybe (error "Main module missing") 
  takeBaseName (s ^. mainMod)
renderBuildName _ p _ BPackName = makeS (progName p)
renderBuildName s p o (BPack a) = renderBuildName s p o BPackName <> 
  makeS (packSep o) <> renderBuildName s p o a
renderBuildName s p o (BWithExt a e) = renderBuildName s p o a <> 
  if includeExt o then maybe (error "Main module missing") (renderExt e) 
  (s ^. mainMod) else makeS ""

renderExt :: Ext -> FilePath -> MakeString
renderExt CodeExt f = makeS $ takeExtension f
renderExt (OtherExt e) _ = e

getCompilerInput :: BuildDependencies -> GOOLState -> ProgData -> [MakeString]
getCompilerInput BcSource s _ = map makeS $ s ^. sources
getCompilerInput (BcSingle n) s p = [renderBuildName s p nameOpts n]

getCommentedFiles :: GOOLState -> [MakeString]
getCommentedFiles s = map makeS (doxConfigName : nub (s ^. headers ++ 
  maybeToList (s ^. mainMod)))

buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = foldr (+:+) mempty $ i ++ [fn]

makeBuild :: [Comments] -> Maybe BuildConfig -> Runnable -> GOOLState -> 
  ProgData -> Doc
makeBuild cms b r s p = genMake [Ch {
  buildConfig = b,
  runnable = r,
  goolState = s,
  progData = p,
  cmts = cms}]
