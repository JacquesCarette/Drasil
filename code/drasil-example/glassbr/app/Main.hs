module Main (main) where

import GHC.IO.Encoding

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), AuxFile(..), Visibility(..),
  defaultChoices)
import Language.Drasil.Generate (gen, genCode, genDot, DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.GlassBR.Body (srs, printSetting, fullSI)
import Drasil.GlassBR.ModuleDefs (allMods)

code :: CodeSpec
code = codeSpec fullSI choices allMods

choices :: Choices
choices = defaultChoices {
  lang = [Python, Cpp, CSharp, Java, Swift],
  modularity = Modular Separated,
  impType = Program,
  logFile = "log.txt",
  logging = [LogVar, LogFunc],
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Exception,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  auxFiles = [SampleInput "../../datafiles/GlassBR/sampleInput.txt", ReadME] 
}
  
main :: IO()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS "GlassBR_SRS")     srs printSetting
  gen (DocSpec Website "GlassBR_SRS") srs printSetting
  genCode choices code
  genDot fullSI
