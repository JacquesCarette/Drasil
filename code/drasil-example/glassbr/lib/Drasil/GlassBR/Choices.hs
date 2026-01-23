module Drasil.GlassBR.Choices where

import Language.Drasil.Code (Choices(..), defaultChoices, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..),
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..),
  ConstantRepr(..), AuxFile(..), Visibility(..), makeArchit,
  makeData, makeConstraints, makeDocConfig, makeLogConfig, makeOptFeats)

import Drasil.GlassBR.DataDefs (configFp)
import Drasil.GlassBR.ModuleDefs (allMods)

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  architecture = makeArchit Modular Program,
  dataInfo = makeData Bundled Inline Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [LogVar, LogFunc] "log.txt")
    [SampleInput "../../datafiles/glassbr/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Exception Exception,
  defaultConfigFiles = configFp,
  extraMods = allMods
}
