module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.New (Label)
import Language.Drasil.Code.Imperative.Helpers (tripThird)
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))

import Build.Drasil (RuleTransformer(makeRule), genMake, mkFile, mkRule, mkCheckedCommand)

import Text.PrettyPrint.HughesPJ (Doc)
import Data.Maybe (maybe, maybeToList)

data CodeHarness = Ch (Maybe BuildConfig) Runnable [String] ([(Doc, Label, Bool)], Label) Code

instance RuleTransformer CodeHarness where
  makeRule (Ch b r e m co@(Code code)) = [
    mkRule "build" (map (const $ renderBuildName e m nameOpts nm) $ maybeToList $
      b) []
    ] ++
    (maybe [] (\(BuildConfig comp bt) -> [
    mkFile (renderBuildName e m nameOpts nm) (map fst code) [
      mkCheckedCommand $ unwords $ comp (getCompilerInput bt e m co) $
        renderBuildName e m nameOpts nm
      ]
    ]) $ b) ++ [
    mkRule "run" ["build"] [
      mkCheckedCommand $ (buildRunTarget (renderBuildName e m no nm) ty) ++ " $(RUNARGS)"
      ]
    ] where (Runnable nm no ty) = r

renderBuildName :: [String] -> ([(Doc, Label, Bool)], Label) -> NameOpts -> BuildName -> String
renderBuildName _ (m, _) _ BMain = getMainModule m
renderBuildName _ (_, l) _ BPackName = l
renderBuildName exts p o (BPack a) = renderBuildName exts p o BPackName ++ packSep o ++ renderBuildName exts p o a
renderBuildName exts p o (BWithExt a e) = renderBuildName exts p o a ++ if includeExt o then renderExt exts e else ""

renderExt :: [String] -> Ext -> String
renderExt e CodeExt = head e
renderExt _ (OtherExt e) = e

getMainModule :: [(Doc, Label, Bool)] -> Label
getMainModule c = mainName $ filter (tripThird) c
  where mainName [(_, a, _)] = a
        mainName _ = error $ "Expected a single main module."

getCompilerInput :: BuildDependencies -> [String] -> ([(Doc, Label, Bool)], Label) -> Code -> [String]
getCompilerInput BcAll _ _ a = map fst $ unCode a
getCompilerInput (BcSingle n) e p _ = [renderBuildName e p nameOpts n]

buildRunTarget :: String -> RunType -> String
buildRunTarget fn Standalone = "./" ++ fn
buildRunTarget fn (Interpreter i) = unwords [i, fn]

makeBuild :: ([(Doc, Label, Bool)], Label) -> Maybe BuildConfig -> Runnable -> [String] -> Code -> Code
makeBuild m b r e code@(Code c) = Code $ ("Makefile", genMake [Ch b r e m code]) : c
