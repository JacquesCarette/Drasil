module Main where

import Language.Drasil (Choices(..), Comments(..), ConstraintBehaviour(..), 
  DocSpec(DocSpec), DocType(SRS, Website), ImplementationType(..), Lang(..), 
  Logging(..), Structure(..), gen)

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
