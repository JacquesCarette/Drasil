module Drasil.GlassBR.Choices where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), AuxFile(..), Visibility(..), makeArchit,
  makeData, makeConstraints, makeDocConfig, makeLogConfig, makeOptFeats,
  defaultChoices)

import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.Body (fullSI)

code :: CodeSpec
code = codeSpec fullSI choices allMods

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  architecture = makeArchit Modular Program,
  dataInfo = makeData Bundled Inline Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Quiet Hide)
    (makeLogConfig [LogVar, LogFunc] "log.txt")
    [SampleInput "../../datafiles/glassbr/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Exception Exception
}
  