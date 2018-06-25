module Main (main) where

import Language.Drasil

import Drasil.GlassBR.Body (gbSymbMap, glassBR_code, glassBR_srs)

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
  gen (DocSpec SRS "GlassBR_SRS")     glassBR_srs gbSymbMap
  gen (DocSpec Website "GlassBR_SRS") glassBR_srs gbSymbMap
  genCode glassChoices glassBR_code
