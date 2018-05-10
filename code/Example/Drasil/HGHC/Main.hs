module Main (main) where

import Language.Drasil

import Drasil.HGHC.HGHC (srsBody, allSymbols)

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
  gen (DocSpec Website "SRS") srsBody allSymbols
  gen (DocSpec SRS "SRS")     srsBody allSymbols
  --genCode thisChoices thisCode
