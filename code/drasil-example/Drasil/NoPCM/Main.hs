module Main (main) where

import Language.Drasil (QDefinition)
import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
  Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
  Logging(..), Modularity(..), Structure(..), ConstantStructure(..), 
  ConstantRepr(..), InputModule(..), matchConcepts, AuxFile(..), Visibility(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.NoPCM.Body (si, srs, printSetting)

code :: CodeSpec
code = codeSpec si choices []
-- Sub interpolation mod into list when possible

choices :: Choices
choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  modularity = Modular,
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [CommentFunc, CommentClass, CommentMod],
  doxVerbosity = Quiet,
  dates = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Unbundled,
  constStructure = Store Bundled,
  constRepr = Const,
  inputModule = Combined,
  conceptMatch = matchConcepts ([] :: [QDefinition]) [],
  auxFiles = [SampleInput]
}       
       
main :: IO ()            
main = do
  gen (DocSpec SRS "NoPCM_SRS") srs printSetting
  gen (DocSpec Website "NoPCM_SRS") srs printSetting
  genCode choices code
