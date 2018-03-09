module Main where

import Language.Drasil (DocType(SRS, Website), DocSpec(DocSpec), Recipe(..), gen
  , Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , Lang(..))

import Drasil.GamePhysics.Body (chipmunkSRS', everything)

docs :: [Recipe]
docs = [Recipe (DocSpec SRS "Chipmunk_SRS") chipmunkSRS',
        Recipe (DocSpec Website "Chipmunk_SRS") chipmunkSRS'
       ]

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
  gen docs everything
