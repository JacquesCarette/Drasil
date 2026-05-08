module Language.Drasil.Code.Imperative.Build.Import (
  buildMakefile
) where

import Control.Lens ((^.))
import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (maybeToList)
import System.FilePath.Posix (takeExtension, takeBaseName)
import Text.PrettyPrint.HughesPJ (Doc)
import Utils.Drasil (capitalize)

import Language.Drasil.Code.Imperative.Build.AST (asFragment, DocConfig(..),
  BuildConfig(BuildConfig), BuildDependencies(..), Ext(..), includeExt,
  NameOpts, nameOpts, packSep, Runnable(Runnable), BuildName(..), RunType(..))
import Language.Drasil.SoftwareDossier.SoftwareDossierSym (SoftwareDossierState,
  headers, sources, mainMod)

import Build.Drasil (Annotation, (+:+), printMakefile, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, mkMakefile)
import Drasil.GOOL (FileData(..), ProgData(..))
import Drasil.Metadata (watermark)

-- | Creates a Makefile.
buildMakefile :: Maybe DocConfig -> Maybe BuildConfig -> Maybe Runnable ->
  SoftwareDossierState -> ProgData -> Doc
buildMakefile d b r s m = printMakefile $ mkMakefile $ maybe [mkRule (openingComments m) buildTarget [] []]
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
openingComments m = [watermark,"Project Name: " ++ progName m, progPurpAdd m]

-- | Helper that renders project purpose into a string if there is one.
progPurpAdd :: ProgData -> String
progPurpAdd m = if progPurp m /= [] then "Project Purpose: " ++
                  capitalize (progPurp m)
                else []

-- | Helper that renders information into a MakeString. Dependent on the 'BuildName' criteria.
renderBuildName :: SoftwareDossierState -> ProgData -> NameOpts -> BuildName -> MakeString
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
getCompilerInput :: BuildDependencies -> SoftwareDossierState -> ProgData -> [MakeString]
getCompilerInput BcSource s _ = map makeS $ s ^. sources
getCompilerInput (BcSingle n) s p = [renderBuildName s p nameOpts n]

-- | Helper that retrieves commented files.
getCommentedFiles :: SoftwareDossierState -> [MakeString]
getCommentedFiles s = map makeS (nubOrd (s ^. headers ++
  maybeToList (s ^. mainMod)))

-- | Helper that builds and runs a target.
buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = foldr (+:+) mempty $ i ++ [fn]
