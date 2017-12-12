module Main (main) where

import Language.Drasil

import Drasil.HGHC.HGHC (srsBody, allSymbols)

docs :: [Recipe]
docs = [
  Recipe (Website "SRS") srsBody,
  Recipe (SRS "SRS") srsBody --,
  ]

thisChoices :: Choices
thisChoices = Choices {
  lang             = [Python, Cpp, CSharp, Java],
  impType          = Program,
  logFile          = "log.txt",
  logging          = LogNone,
  comments         = CommentNone, 
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure   = AsClass
}  
  
main :: IO ()            
main = do
  gen docs allSymbols
  --genCode thisChoices thisCode
