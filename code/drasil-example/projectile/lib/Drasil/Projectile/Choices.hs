{-# LANGUAGE TupleSections #-}
module Drasil.Projectile.Choices where

import Language.Drasil (Space(..))
import Language.Drasil.Code (Choices(..), Comments(..), Mod,
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..),
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..),
  ConstantRepr(..), CodeConcept(..), matchConcepts, SpaceMatch,
  matchSpaces, AuxFile(..), Visibility(..), defaultChoices, makeArchit,
  makeData, makeMaps, spaceToCodeType, makeConstraints,
  makeDocConfig, makeLogConfig, makeOptFeats)
import Drasil.GOOL (CodeType(..))
import Data.Drasil.Quantities.Math (piConst)

choiceCombos :: [(Choices, [Mod])]
choiceCombos = map (,[]) [
  baseChoices {
    lang = [Python, Cpp, CSharp, Java, Swift, Julia]
  },
  baseChoices {
    architecture = makeArchit Modular Program,
    dataInfo = makeData Bundled (Store Unbundled) Var
  },
  baseChoices {
    lang = [Python, Cpp, CSharp, Java, Swift, Julia],
    architecture = makeArchit Modular Library,
    dataInfo = makeData Unbundled (Store Unbundled) Var,
    maps = makeMaps (matchConcepts [(piConst, [Pi])]) matchToFloats
  },
  baseChoices {
    dataInfo = makeData Bundled (Store Bundled) Const,
    optFeats = makeOptFeats
      (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
      (makeLogConfig [LogVar, LogFunc] "log.txt")
      [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME],
    folderVal = 5
  },
  baseChoices {
    dataInfo = makeData Bundled WithInputs Var,
    maps = makeMaps (matchConcepts [(piConst, [Pi])]) matchToFloats,
    optFeats = makeOptFeats
      (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
      (makeLogConfig [LogVar, LogFunc] "log.txt")
      [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME],
    folderVal = 5
  }]

matchToFloats :: SpaceMatch
matchToFloats = matchSpaces (map (,[Float, Double]) [Real, Rational])

baseChoices :: Choices
baseChoices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  architecture = makeArchit Unmodular Program,
  dataInfo = makeData Unbundled WithInputs Var,
  maps = makeMaps (matchConcepts [(piConst, [Pi])]) spaceToCodeType,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../../../datafiles/projectile/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Warning Warning,
  folderVal = 5
}
