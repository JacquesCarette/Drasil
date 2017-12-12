module Main where

import Language.Drasil (DocType(SRS, Website), Recipe(..), gen
  , Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , Lang(..))

import Drasil.GamePhysics.Body (chipmunkSRS', everything)

docs :: [Recipe]
docs = [Recipe (SRS "Chipmunk_SRS") chipmunkSRS',
        Recipe (Website "Chipmunk_SRS") chipmunkSRS'
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
