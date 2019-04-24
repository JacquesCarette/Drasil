module Language.Drasil.Code.Imperative.Build.Import where

import Language.Drasil.Code.Imperative.AST (Label, Module(Mod), notMainModule, Package(Pack))
import Language.Drasil.Code.Imperative.Build.AST (Ext(..), includeExt, NameOpts, packSep, RunName(..), RunType(..))
import Language.Drasil.Code.Imperative.LanguageRenderer (Config, ext)

renderRunName :: Config -> Package -> NameOpts -> RunName -> String
renderRunName c p o (RConcat a b) = (renderRunName c p o a) ++ (renderRunName c p o b)
renderRunName _ _ _ (RLit s) = s
renderRunName _ (Pack _ m) _ RMain = getMainModule m
renderRunName _ (Pack l _) _ RPackName = l
renderRunName c p o (RPack a) = (renderRunName c p o RPackName) ++ (packSep o) ++ renderRunName c p o a
renderRunName c p o (RWithExt a e) = renderRunName c p o a ++ if includeExt o then renderExt c e else ""

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
