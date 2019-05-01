module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.AST (Module(Mod), notMainModule)
import Language.Drasil.Code.Imperative.New (Label)
import Language.Drasil.Code.Imperative.Helpers (tripThird)
import Language.Drasil.Code.Imperative.Build.AST (Ext(..), includeExt, NameOpts, packSep, Runnable(Runnable), RunName(..), RunType(..))

import Build.Drasil (RuleTransformer(makeRule), genMake, mkRule, mkCheckedCommand)

import Text.PrettyPrint.HughesPJ (Doc)

data CodeHarness = Ch Runnable [String] ([(Doc, Label, Bool)], Label)

instance RuleTransformer CodeHarness where
  makeRule (Ch r e m) = [
    mkRule "run" [] [
      mkCheckedCommand $ (buildRunTarget (renderRunName e m no nm) ty) ++ " $(RUNARGS)"
      ]
    ] where (Runnable nm no ty) = r

renderRunName :: [String] -> ([(Doc, Label, Bool)], Label) -> NameOpts -> RunName -> String
renderRunName exts p o (RConcat a b) = (renderRunName exts p o a) ++ (renderRunName exts p o b)
renderRunName _ _ _ (RLit s) = s
renderRunName _ (m, _) _ RMain = getMainModule m
renderRunName _ (_, l) _ RPackName = l
renderRunName exts p o (RPack a) = (renderRunName exts p o RPackName) ++ (packSep o) ++ renderRunName exts p o a
renderRunName exts p o (RWithExt a e) = renderRunName exts p o a ++ if includeExt o then renderExt exts e else ""

renderExt :: [String] -> Ext -> String
renderExt e CodeExt = head e
renderExt _ (OtherExt e) = e

getMainModule :: [(Doc, Label, Bool)] -> Label
getMainModule c = mainName $ filter (tripThird) c
  where mainName [(_, a, _)] = a
        mainName _ = error $ "Expected a single main module."

buildRunTarget :: String -> RunType -> String
buildRunTarget fn Standalone = "./" ++ fn
buildRunTarget fn (Interpreter i) = unwords [i, fn]

makeBuild :: ([(Doc, Label, Bool)], Label) -> Runnable -> [String] -> Code -> Code
makeBuild m r e (Code c) = Code $ ("Makefile", genMake [Ch r e m]) : c
