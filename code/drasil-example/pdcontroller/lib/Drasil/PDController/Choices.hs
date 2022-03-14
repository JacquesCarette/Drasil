module Drasil.PDController.Choices where

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODELSodaPkg)
import Drasil.PDController.Body (pidODEInfo, fullSI)
import Language.Drasil.Code (AuxFile(..), Choices(..), CodeSpec, Comments(..), 
  ConstantRepr(..), ConstantStructure(..), ConstraintBehaviour(..), 
  ImplementationType(..), InputModule(..), Lang(..), Modularity(..), Structure(..), 
  Verbosity(..), Visibility(..), codeSpec, defaultChoices, makeArchit, makeData, 
  makeConstraints, makeODE, makeDocConfig, makeLogConfig, makeOptFeats, ExtLib(..))

codeSpecs :: CodeSpec
codeSpecs = codeSpec fullSI codeChoices []

codeChoices :: Choices
codeChoices = defaultChoices{
  lang = [Python],
  architecture = makeArchit (Modular Combined) Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  optFeats = makeOptFeats
    (makeDocConfig [CommentFunc, CommentClass, CommentMod] Verbose Hide)
    (makeLogConfig [] "log.txt")
    [SampleInput "../../datafiles/pdcontroller/sampleInput.txt", ReadME],
  srsConstraints = makeConstraints Exception Exception,
  extLib = Math (makeODE [pidODEInfo] [scipyODELSodaPkg])
}
