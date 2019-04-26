module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST (Label, Module(Mod), notMainModule, Package(Pack))
import Language.Drasil.Code.Imperative.Build.AST (Ext(..), includeExt, NameOpts, packSep, Runnable(Runnable), RunName(..), RunType(..))

import Build.Drasil (RuleTransformer(makeRule), genMake, mkRule, mkCheckedCommand)

data CodeHarness = Ch Runnable [String] Package

instance RuleTransformer CodeHarness where
  makeRule (Ch r e m) = [
    mkRule "run" [] [
      mkCheckedCommand $ (buildRunTarget (renderRunName e m no nm) ty) ++ " $(RUNARGS)"
      ]
    ] where (Runnable nm no ty) = r

renderRunName :: [String] -> Package -> NameOpts -> RunName -> String
renderRunName exts p o (RConcat a b) = (renderRunName c p o a) ++ (renderRunName c p o b)
renderRunName _ _ _ (RLit s) = s
renderRunName _ (Pack _ m) _ RMain = getMainModule m
renderRunName _ (Pack l _) _ RPackName = l
renderRunName exts p o (RPack a) = (renderRunName c p o RPackName) ++ (packSep o) ++ renderRunName c p o a
renderRunName exts p o (RWithExt a e) = renderRunName c p o a ++ if includeExt o then renderExt exts e else ""

renderExt :: [String] -> Ext -> String
renderExt e CodeExt = head e
renderExt _ (OtherExt e) = e

getMainModule :: [Module] -> Label
getMainModule c = mainName $ filter (not . notMainModule) c
  where mainName [(Mod a _ _ _ _)] = a
        mainName _ = error $ "Expected a single main module."

buildRunTarget :: String -> RunType -> String
buildRunTarget fn Standalone = "./" ++ fn
buildRunTarget fn (Interpreter i) = unwords [i, fn]

makeBuild :: Package -> Runnable -> [String] -> Code -> Code
makeBuild m r e (Code c) = Code $ ("Makefile", genMake [Ch r e m]) : c
