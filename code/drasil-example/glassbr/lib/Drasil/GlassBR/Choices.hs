module Drasil.GlassBR.Choices where

import Language.Drasil (mkQuantDef, SimpleQDef, ExprC(..), LiteralC(..))
import Language.Drasil.Code (Choices(..), defaultChoices, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..),
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..),
  ConstantRepr(..), AuxFile(..), Visibility(..), makeArchit,
  makeData, makeConstraints, makeDocConfig, makeLogConfig, makeOptFeats)
import Utils.Drasil (RelativeFile, relativeFile)

import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.Unitals (aspectRatio, standOffDist, stressDistFac,
  demand, eqTNTWeight, dimlessLoad, sdfTol, tolLoad, interpY, interpZ)

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
  extraMods = allMods,
  handWiredDefs = [strDisFacQD, calofDemandQD, tolPreQD]
}

strDisFacQD :: SimpleQDef
strDisFacQD = mkQuantDef stressDistFac
  $ apply interpZ [str "SDF.txt", sy aspectRatio, sy dimlessLoad]

calofDemandQD :: SimpleQDef
calofDemandQD = mkQuantDef demand
  $ apply interpY [str "TSD.txt", sy standOffDist, sy eqTNTWeight]

tolPreQD :: SimpleQDef
tolPreQD = mkQuantDef tolLoad
  $ apply interpY [str "SDF.txt", sy aspectRatio, sy sdfTol]

configFp :: [RelativeFile]
configFp = map relativeFile ["SDF.txt", "TSD.txt"]
