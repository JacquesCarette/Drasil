module Drasil.GlassBR.Choices where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), AuxFile(..), Visibility(..), makeArchit,
  makeData, makeConstraints,
  defaultChoices)

import Drasil.GlassBR.ModuleDefs (allMods)
import Drasil.GlassBR.Body (fullSI)

code :: CodeSpec
code = codeSpec fullSI choices allMods

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  architecture = makeArchit (Modular Separated) Program,
  logFile = "log.txt",
  logging = [LogVar, LogFunc],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  srsConstraints = makeConstraints Exception Exception,
  dataInfo = makeData Bundled Inline Const,
  auxFiles = [SampleInput "../../datafiles/glassbr/sampleInput.txt", ReadME] 
}
  