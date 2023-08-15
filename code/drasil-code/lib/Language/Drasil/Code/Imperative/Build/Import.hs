module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Imperative.Build.AST (asFragment, DocConfig(..),
  BuildConfig(BuildConfig), BuildDependencies(..), Ext(..), includeExt,
  NameOpts, nameOpts, packSep, Runnable(Runnable), BuildName(..), RunType(..))

import GOOL.Drasil (FileData(..), ProgData(..), GOOLState(..), headers, sources,
  mainMod)

import Build.Drasil (Annotation, (+:+), genMake, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, RuleTransformer(makeRule))

import Control.Lens ((^.))
import Data.Maybe (maybeToList)
import Data.List (nub)
import System.FilePath.Posix (takeExtension, takeBaseName)
import Text.PrettyPrint.HughesPJ (Doc)
import Utils.Drasil (capitalize)
import Metadata.Drasil.DrasilMetaCall (drasilMeta, DrasilMeta(..), watermark)

-- | Holds all the needed information to run a program.
data CodeHarness = Ch {
  buildConfig :: Maybe BuildConfig,
  runnable :: Maybe Runnable,
  goolState :: GOOLState,
  progData :: ProgData,
  docConfig :: Maybe DocConfig}

-- | Transforms information in 'CodeHarness' into a list of Makefile rules.
instance RuleTransformer CodeHarness where
  makeRule (Ch b r s m d) = maybe [mkRule (openingComments m) buildTarget [] []]
    (\(BuildConfig comp onm anm bt) ->
    let outnm = maybe (asFragment "") (renderBuildName s m nameOpts) onm
        addnm = maybe (asFragment "") (renderBuildName s m nameOpts) anm
    in [
    mkRule (openingComments m) buildTarget [outnm] [],
    mkFile [] outnm (map (makeS . filePath) (progMods m)) $
      map (mkCheckedCommand . foldr (+:+) mempty) $
        comp (getCompilerInput bt s m) outnm addnm
    ]) b ++ maybe [] (\(Runnable nm no ty) -> [
    mkRule [] (makeS "run") [buildTarget] [
      mkCheckedCommand $ buildRunTarget (renderBuildName s m no nm) ty +:+
      mkFreeVar "RUNARGS"
      ]
    ]) r ++ maybe [] (\(DocConfig dps cmds) -> [
      mkRule [] (makeS "doc") (dps ++ getCommentedFiles s) cmds
    ]) d where
      buildTarget = makeS "build"

openingComments :: ProgData -> Annotation
openingComments m = [watermark ++ version drasilMeta,"Project Name: " ++ progName m, progPurpAdd m]

-- | Helper that renders project purpose into a string if there is one.
progPurpAdd :: ProgData -> String
progPurpAdd m = if progPurp m /= [] then "Project Purpose: " ++
                  capitalize (progPurp m)
                else []

-- | Helper that renders information into a MakeString. Dependent on the 'BuildName' criteria.
renderBuildName :: GOOLState -> ProgData -> NameOpts -> BuildName -> MakeString
renderBuildName s _ _ BMain = makeS $ maybe (error "Main module missing")
  takeBaseName (s ^. mainMod)
renderBuildName _ p _ BPackName = makeS (progName p)
renderBuildName s p o (BPack a) = renderBuildName s p o BPackName <>
  makeS (packSep o) <> renderBuildName s p o a
renderBuildName s p o (BWithExt a e) = renderBuildName s p o a <>
  if includeExt o then renderExt e (takeSrc $ s ^. sources) else makeS ""
  where takeSrc (src:_) = src
        takeSrc [] = error "Generated code has no source files"

-- | Helper that renders an extension onto a 'FilePath'.
renderExt :: Ext -> FilePath -> MakeString
renderExt CodeExt f = makeS $ takeExtension f
renderExt (OtherExt e) _ = e

-- | Helper that records the compiler input information.
getCompilerInput :: BuildDependencies -> GOOLState -> ProgData -> [MakeString]
getCompilerInput BcSource s _ = map makeS $ s ^. sources
getCompilerInput (BcSingle n) s p = [renderBuildName s p nameOpts n]

-- | Helper that retrieves commented files.
getCommentedFiles :: GOOLState -> [MakeString]
getCommentedFiles s = map makeS (nub (s ^. headers ++
  maybeToList (s ^. mainMod)))

-- | Helper that builds and runs a target.
buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = foldr (+:+) mempty $ i ++ [fn]

-- | Creates a Makefile.
makeBuild :: Maybe DocConfig -> Maybe BuildConfig -> Maybe Runnable ->
  GOOLState -> ProgData -> Doc
makeBuild d b r s p = genMake [Ch {
  buildConfig = b,
  runnable = r,
  goolState = s,
  progData = p,
  docConfig = d}]
