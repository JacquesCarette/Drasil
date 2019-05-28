module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New (Label)
import Language.Drasil.Code.Imperative.Helpers (tripThird)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))

import Build.Drasil ((+:+), genMake, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, RuleTransformer(makeRule))

import Text.PrettyPrint.HughesPJ (Doc)
import Data.List.Utils (endswith)
import Data.Maybe (maybe, maybeToList)

data CodeHarness = Ch (Maybe BuildConfig) Runnable [String] ([(Doc, Label, Bool)], Label) Code

instance RuleTransformer CodeHarness where
  makeRule (Ch b r e m co@(Code code)) = [
    mkRule buildTarget (map (const $ renderBuildName e m nameOpts nm) $ maybeToList b) []
    ] ++
    maybe [] (\(BuildConfig comp bt) -> [
    mkFile (renderBuildName e m nameOpts nm) (map (makeS . fst) code) [
      mkCheckedCommand $ foldr (+:+) mempty $ comp (getCompilerInput bt e m co) $
        renderBuildName e m nameOpts nm
      ]
    ]) b ++ [
    mkRule (makeS "run") [buildTarget] [
      mkCheckedCommand $ buildRunTarget (renderBuildName e m no nm) ty +:+ mkFreeVar "RUNARGS"
      ]
    ] where
      (Runnable nm no ty) = r
      buildTarget = makeS "build"

renderBuildName :: [String] -> ([(Doc, Label, Bool)], Label) -> NameOpts -> BuildName -> MakeString
renderBuildName _ (m, _) _ BMain = makeS $ getMainModule m
renderBuildName _ (_, l) _ BPackName = makeS l
renderBuildName ext p o (BPack a) = renderBuildName ext p o BPackName <> makeS(packSep o) <> renderBuildName ext p o a
renderBuildName ext p o (BWithExt a e) = renderBuildName ext p o a <> if includeExt o then renderExt ext e else makeS ""

renderExt :: [String] -> Ext -> MakeString
renderExt e CodeExt = makeS $ head e
renderExt _ (OtherExt e) = e

getMainModule :: [(Doc, Label, Bool)] -> Label
getMainModule c = mainName $ filter tripThird c
  where mainName [(_, a, _)] = a
        mainName _ = error "Expected a single main module."

getCompilerInput :: BuildDependencies -> [String] -> ([(Doc, Label, Bool)], Label) -> Code -> [MakeString]
getCompilerInput BcSource e _ a = map makeS $ filter (endswith $ head e) $ map fst $ unCode a
getCompilerInput (BcSingle n) e p _ = [renderBuildName e p nameOpts n]


buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = i +:+ fn

makeBuild :: ([(Doc, Label, Bool)], Label) -> Maybe BuildConfig -> Runnable -> [String] -> Code -> Code
makeBuild m b r e code@(Code c) = Code $ ("Makefile", genMake [Ch b r e m code]) : c
