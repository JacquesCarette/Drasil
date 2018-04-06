module Main (main) where

import Language.Drasil

import Drasil.GlassBR.Body (glassBR_srs, glassBR_code, gbSymbMap)

docs :: [Recipe]
docs = 
  [Recipe (DocSpec SRS "GlassBR_SRS")     glassBR_srs, 
   Recipe (DocSpec Website "GlassBR_SRS") glassBR_srs
  ]

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
  gen docs gbSymbMap
  genCode glassChoices glassBR_code
