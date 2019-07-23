module Language.Drasil.Code.Imperative.Build.Import (
  makeBuild
) where

import Language.Drasil.Code.Code (Code(..))
import Language.Drasil.Code.Imperative.Symantics (Label)
import Language.Drasil.Code.Imperative.Data (FileData(..), ModData(..))
import Language.Drasil.Code.Imperative.Build.AST (BuildConfig(BuildConfig),
  BuildDependencies(..), Ext(..), includeExt, NameOpts, nameOpts, packSep,
  Runnable(Runnable), BuildName(..), RunType(..))

import Build.Drasil ((+:+), genMake, makeS, MakeString, mkFile, mkRule,
  mkCheckedCommand, mkFreeVar, RuleTransformer(makeRule))

import Data.List.Utils (endswith)
import Data.Maybe (maybe, maybeToList)

data CodeHarness = Ch (Maybe BuildConfig) Runnable [String] ([FileData], Label) Code

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

renderBuildName :: [String] -> ([FileData], Label) -> NameOpts -> BuildName -> MakeString
renderBuildName _ (m, _) _ BMain = makeS $ getMainModule m
renderBuildName _ (_, l) _ BPackName = makeS l
renderBuildName ext p o (BPack a) = renderBuildName ext p o BPackName <> makeS(packSep o) <> renderBuildName ext p o a
renderBuildName ext p o (BWithExt a e) = renderBuildName ext p o a <> if includeExt o then renderExt ext e else makeS ""

renderExt :: [String] -> Ext -> MakeString
renderExt e CodeExt = makeS $ last e
renderExt _ (OtherExt e) = e

getMainModule :: [FileData] -> Label
getMainModule c = mainName $ filter (isMainMod . fileMod) c
  where mainName [FileD _ m] = name m
        mainName _ = error "Expected a single main module."

getCompilerInput :: BuildDependencies -> [String] -> ([FileData], Label) -> Code -> [MakeString]
getCompilerInput BcSource e _ a = map makeS $ filter (endswith $ last e) $ map fst $ unCode a
getCompilerInput (BcSingle n) e p _ = [renderBuildName e p nameOpts n]

buildRunTarget :: MakeString -> RunType -> MakeString
buildRunTarget fn Standalone = makeS "./" <> fn
buildRunTarget fn (Interpreter i) = i +:+ fn

makeBuild :: ([FileData], Label) -> Maybe BuildConfig -> Runnable -> [String] -> Code -> Code
makeBuild m b r e code = Code [("Makefile", genMake [Ch b r e m code])]
