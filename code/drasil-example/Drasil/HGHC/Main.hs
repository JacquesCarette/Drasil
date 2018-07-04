module Main (main) where

import Language.Drasil
import Language.Drasil.Code hiding (for)

import Drasil.HGHC.HGHC (allSymbols, srsBody)

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
  gen (DocSpec Website "Tiny_SRS") srsBody allSymbols
  gen (DocSpec SRS "Tiny_SRS")     srsBody allSymbols
  --genCode thisChoices thisCode
