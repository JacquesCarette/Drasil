module Main (main) where

import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
  ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.GlassBR.Body (glassBRCode, srs, printSetting)

glassChoices :: Choices
glassChoices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = CommentNone,
  onSfwrConstraint = Exception,
  onPhysConstraint = Exception,
  inputStructure = AsClass
}
  
main :: IO()
main = do
  gen (DocSpec SRS "GlassBR_SRS")     srs printSetting
  gen (DocSpec Website "GlassBR_SRS") srs printSetting
  genCode glassChoices glassBRCode
