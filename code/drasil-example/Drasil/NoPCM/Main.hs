module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Modularity(..), Structure(..), ConstantStructure(..), ConstantRepr(..), 
  InputModule(..), AuxFile(..), Visibility(..), defaultChoices)
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Data.Drasil.ExternalLibraries.ODELibraries (scipyODEPckg, osloPckg, 
  apacheODEPckg, odeintPckg)

import Drasil.NoPCM.Body (si, srs, printSetting, noPCMODEInfo)

code :: CodeSpec
code = codeSpec si choices []
-- Sub interpolation mod into list when possible

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Modular Combined,
  impType = Program,
  logFile = "log.txt",
  logging = [],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Unbundled,
  constStructure = Store Bundled,
  constRepr = Const,
  auxFiles = [SampleInput "../../datafiles/NoPCM/sampleInput.txt", ReadME],
  odeLib = [scipyODEPckg, osloPckg, apacheODEPckg, odeintPckg],
  odes = [noPCMODEInfo]
}       
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS "NoPCM_SRS") srs printSetting
  gen (DocSpec Website "NoPCM_SRS") srs printSetting
  genCode choices code
