module Drasil.NoPCM.Choices where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..), 
  InputModule(..), AuxFile(..), Visibility(..), defaultChoices, makeArchit,
  makeData, makeConstraints, makeODE)

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODEPckg, osloPckg, 
  apacheODEPckg, odeintPckg)

import Drasil.NoPCM.Body (noPCMODEInfo, fullSI)

code :: CodeSpec
code = codeSpec fullSI choices []
-- Sub interpolation mod into list when possible

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java],
  architecture = makeArchit (Modular Combined) Program,
  dataInfo = makeData Unbundled (Store Bundled) Const,
  logFile = "log.txt",
  logging = [],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  srsConstraints = makeConstraints Warning Warning,
  auxFiles = [SampleInput "../../datafiles/nopcm/sampleInput.txt", ReadME],
  ode = makeODE [noPCMODEInfo] [scipyODEPckg, osloPckg, apacheODEPckg, odeintPckg] 
}