module Main where

import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
  ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.GamePhysics.Body (chipmunkSRS', everything)

chipChoices :: Choices
chipChoices = Choices {
  lang             = [Python, Cpp, CSharp, Java],
  impType          = Library,
  logFile          = "log.txt",
  logging          = LogNone,
  comments         = CommentNone,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure   = Loose
}       
       
main :: IO ()
main = do
  gen (DocSpec SRS "Chipmunk_SRS") chipmunkSRS'  everything
  gen (DocSpec Website "Chipmunk_SRS") chipmunkSRS' everything
