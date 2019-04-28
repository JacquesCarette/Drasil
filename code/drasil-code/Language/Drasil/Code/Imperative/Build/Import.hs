module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST (Label, Module(Mod), notMainModule, Package(Pack))
import Language.Drasil.Code.Imperative.Build.AST (Ext(..), includeExt, NameOpts, packSep, Runnable(Runnable), BuildName(..), RunType(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (Config, ext, runnable)

import Build.Drasil (RuleTransformer(makeRule), genMake, mkRule, mkCheckedCommand)

data CodeHarness = Ch Config Package

instance RuleTransformer CodeHarness where
  makeRule (Ch c m) = [
    mkRule "run" [] [
      mkCheckedCommand $ (buildRunTarget (renderBuildName c m no nm) ty) ++ " $(RUNARGS)"
      ]
    ] where (Runnable nm no ty) = runnable c

renderBuildName :: Config -> Package -> NameOpts -> BuildName -> String
renderBuildName _ (Pack _ m) _ BMain = getMainModule m
renderBuildName _ (Pack l _) _ BPackName = l
renderBuildName c p o (BPack a) = (renderBuildName c p o BPackName) ++ (packSep o) ++ renderBuildName c p o a
renderBuildName c p o (BWithExt a e) = renderBuildName c p o a ++ if includeExt o then renderExt c e else ""

renderExt :: Config -> Ext -> String
renderExt c CodeExt = ext c
renderExt _ (OtherExt e) = e

getMainModule :: [Module] -> Label
getMainModule c = mainName $ filter (not . notMainModule) c
  where mainName [(Mod a _ _ _ _)] = a
        mainName _ = error $ "Expected a single main module."

buildRunTarget :: String -> RunType -> String
buildRunTarget fn Standalone = "./" ++ fn
buildRunTarget fn (Interpreter i) = unwords [i, fn]

makeBuild :: Package -> Config -> Code -> Code
makeBuild m p (Code c) = Code $ ("Makefile", genMake [Ch p m]) : c
