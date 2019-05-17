module Main (main) where

import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
  ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocType(SRS, Website, MIS), DocSpec(DocSpec))

import qualified Drasil.GlassBR.SRS as SRS (glassBRCode, glassBRSrs, printSetting)
import qualified Drasil.GlassBR.MIS as MIS (glassBRMis, printSetting)

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
  gen (DocSpec SRS     "GlassBR_SRS") SRS.glassBRSrs SRS.printSetting
  gen (DocSpec Website "GlassBR_SRS") SRS.glassBRSrs SRS.printSetting
  gen (DocSpec MIS     "GlassBR_MIS") MIS.glassBRMis MIS.printSetting
  gen (DocSpec Website "GlassBR_MIS") MIS.glassBRMis MIS.printSetting
  genCode glassChoices SRS.glassBRCode
