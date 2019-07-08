module Main (main) where

import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
  ConstraintBehaviour(..), ImplementationType(..), Lang(..), Logging(..), 
  Structure(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.GlassBR.Body (systInfo, srs, printSetting)
import Drasil.GlassBR.ModuleDefs (allMods)

code :: CodeSpec
code = codeSpec systInfo choices allMods

choices :: Choices
choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogAll,
  comments = [CommentFunc, CommentClass, CommentMod],
  onSfwrConstraint = Exception,
  onPhysConstraint = Exception,
  inputStructure = Bundled
}
  
main :: IO()
main = do
  gen (DocSpec SRS "GlassBR_SRS")     srs printSetting
  gen (DocSpec Website "GlassBR_SRS") srs printSetting
  genCode choices code
