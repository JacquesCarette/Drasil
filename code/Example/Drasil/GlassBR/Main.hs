module Main (main) where

import Language.Drasil

import Drasil.GlassBR.Body (glassBR_srs, glassBR_code, gbSymbMap)

docs :: [Recipe]
docs = 
  [Recipe (SRS "GlassBR_SRS")     glassBR_srs, 
   Recipe (Website "GlassBR_SRS") glassBR_srs
  ]

glassChoices :: Choices
glassChoices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,         -- LogNone, LogFunc
  comments = CommentNone,    -- CommentNone, CommentFunc
  onSfwrConstraint = Exception,  -- Warning, Exception
  onPhysConstraint = Exception,  -- Warning, Exception
  inputStructure = AsClass    -- Loose, AsClass
}
  
main :: IO()
main = do
  gen docs gbSymbMap
  genCode glassChoices glassBR_code
