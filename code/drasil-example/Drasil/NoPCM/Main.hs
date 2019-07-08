module Main (main) where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  ConstraintBehaviour(..), ImplementationType(..), Lang(..), Logging(..), 
  Structure(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.NoPCM.Body (si, srs, printSetting)

code :: CodeSpec
code = codeSpec si choices []
-- Sub interpolation mod into list when possible

choices :: Choices
choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = [],
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Unbundled
}       
       
main :: IO ()            
main = do
  gen (DocSpec SRS "NoPCM_SRS") srs printSetting
  gen (DocSpec Website "NoPCM_SRS") srs printSetting
  genCode choices code
