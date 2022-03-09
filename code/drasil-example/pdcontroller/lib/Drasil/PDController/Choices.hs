module Drasil.PDController.Choices where

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODELSodaPkg)
import Drasil.PDController.Body (pidODEInfo, fullSI)
import Language.Drasil.Code
       (AuxFile(..), Choices(..), CodeSpec, Comments(..), ConstantRepr(..),
        ConstantStructure(..), ConstraintBehaviour(..), ImplementationType(..),
        InputModule(..), Lang(..), Modularity(..), Structure(..), Verbosity(..),
        Visibility(..), codeSpec, defaultChoices, makeArchit)

codeSpecs :: CodeSpec
codeSpecs = codeSpec fullSI codeChoices []

codeChoices :: Choices
codeChoices = defaultChoices{
  lang = [Python],
  architecture = makeArchit (Modular Combined) Program,
  logFile = "log.txt",
  logging = [],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Verbose,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Exception,
  inputStructure = Unbundled,
  constStructure = Store Bundled,
  constRepr = Const,
  auxFiles = [SampleInput "../../datafiles/pdcontroller/sampleInput.txt", ReadME],
  odeLib = [scipyODELSodaPkg],
  odes = [pidODEInfo]
}
