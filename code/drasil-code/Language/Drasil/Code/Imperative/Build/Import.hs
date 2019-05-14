module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST (Label, Module(Mod), notMainModule, Package(Pack))
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (Config, buildConfig, ext, runnable)

import Build.Drasil (RuleTransformer(makeRule), genMake, mkFile, mkRule, mkCheckedCommand)

import Data.Maybe (maybe, maybeToList)

data CodeHarness = Ch Config Package Code

instance RuleTransformer CodeHarness where
  makeRule (Ch c m co@(Code code)) = [
    mkRule "build" (map (const $ renderBuildName c m nameOpts nm) $ maybeToList $
      buildConfig c) []
    ] ++
    (maybe [] (\(BuildConfig comp bt) -> [
    mkFile (renderBuildName c m nameOpts nm) (map fst code) [
      mkCheckedCommand $ unwords $ comp (getCompilerInput bt c m co) $
        renderBuildName c m nameOpts nm
      ]
    ]) $ buildConfig c) ++ [
    mkRule "run" ["build"] [
      mkCheckedCommand $ (buildRunTarget (renderBuildName c m no nm) ty) ++ " $(RUNARGS)"
      ]
    ] where (Runnable nm no ty) = runnable c

renderBuildName :: Config -> Package -> NameOpts -> BuildName -> String
renderBuildName _ (Pack _ m) _ BMain = getMainModule m
renderBuildName _ (Pack l _) _ BPackName = l
renderBuildName c p o (BPack a) = renderBuildName c p o BPackName ++ packSep o ++ renderBuildName c p o a
renderBuildName c p o (BWithExt a e) = renderBuildName c p o a ++ if includeExt o then renderExt c e else ""

renderExt :: Config -> Ext -> String
renderExt c CodeExt = ext c
renderExt _ (OtherExt e) = e

getMainModule :: [Module] -> Label
getMainModule c = mainName $ filter (not . notMainModule) c
  where mainName [(Mod a _ _ _ _)] = a
        mainName _ = error $ "Expected a single main module."

getCompilerInput :: BuildDependencies -> Config -> Package -> Code -> [String]
getCompilerInput BcAll _ _ a = map fst $ unCode a
getCompilerInput (BcSingle n) c p _ = [renderBuildName c p nameOpts n]

buildRunTarget :: String -> RunType -> String
buildRunTarget fn Standalone = "./" ++ fn
buildRunTarget fn (Interpreter i) = unwords [i, fn]

makeBuild :: Package -> Config -> Code -> Code
makeBuild m p code@(Code c) = Code $ ("Makefile", genMake [Ch p m code]) : c
