module Main (main) where

import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
  ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocSpec(DocSpec), DocType(SRS, Website))

import Drasil.GlassBR.Body (glassBRCode, glassBRSrs, printSetting)

glassChoices :: Choices
glassChoices = Choices {
  lang = [Python, Java, CSharp, Cpp],
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
  gen (DocSpec SRS "GlassBR_SRS")     glassBRSrs printSetting
  gen (DocSpec Website "GlassBR_SRS") glassBRSrs printSetting
  genCode glassChoices glassBRCode
