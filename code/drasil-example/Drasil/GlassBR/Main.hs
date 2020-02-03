module Main (main) where

import Language.Drasil (QDefinition)
import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), matchConcepts, AuxFile(..), Visibility(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.GlassBR.Body (si, srs, printSetting)
import Drasil.GlassBR.ModuleDefs (allMods)

code :: CodeSpec
code = codeSpec si choices allMods

choices :: Choices
choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Modular,
  impType = Program,
  logFile = "log.txt",
  logging = LogAll,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Exception,
  onPhysConstraint = Exception,
  inputStructure = Bundled,
  constStructure = Inline,
  constRepr = Const,
  inputModule = Separated,
  conceptMatch = matchConcepts ([] :: [QDefinition]) [],
  auxFiles = [SampleInput] 
}
  
main :: IO()
main = do
  gen (DocSpec SRS "GlassBR_SRS")     srs printSetting
  gen (DocSpec Website "GlassBR_SRS") srs printSetting
  genCode choices code
