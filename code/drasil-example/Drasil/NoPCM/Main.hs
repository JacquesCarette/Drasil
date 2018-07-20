module Main (main) where

import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
  ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen, genCode)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.NoPCM.Body (nopcm_srs, nopcm_code, nopcm_SymbMap)

nopcm_Choices :: Choices
nopcm_Choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = CommentNone,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Loose
}       
       
main :: IO ()            
main = do
  gen (DocSpec SRS "NoPCM_SRS") nopcm_srs nopcm_SymbMap
  gen (DocSpec Website "NoPCM_SRS") nopcm_srs nopcm_SymbMap
  genCode nopcm_Choices nopcm_code
