module Drasil.PDController.Choices where

import Language.Drasil.Code (AuxFile(..), Choices(..), CodeSpec, Comments(..), 
  ConstantRepr(..), ConstantStructure(..), ConstraintBehaviour(..), 
  ImplementationType(..), Lang(..), Modularity(..), Structure(..), 
  Verbosity(..), Visibility(..), codeSpec, defaultChoices, makeArchit, makeData, 
  makeConstraints, makeODE, makeDocConfig, makeLogConfig, makeOptFeats, ExtLib(..))

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODEPckg, osloPckg,
  apacheODEPckg, odeintPckg)
import Drasil.PDController.Body (pidODEInfo, fullSI)

codeSpecs :: CodeSpec
codeSpecs = codeSpec fullSI codeChoices []

codeChoices :: Choices
codeChoices = defaultChoices{
  lang = [Python, CSharp, Java, Cpp],
  architecture = makeArchit Modular Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Verbose Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../../datafiles/pdcontroller/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Exception Exception,
  extLibs = [Math (makeODE [pidODEInfo] [scipyODEPckg, osloPckg, apacheODEPckg, odeintPckg])]
}
