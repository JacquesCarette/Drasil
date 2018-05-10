module Main (main) where

import Language.Drasil (DocType(SRS,Website), DocSpec(DocSpec), gen
  , Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , Lang(..))

import Drasil.SSP.Body (ssp_srs, sspSymMap)

sspChoices :: Choices
sspChoices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,         -- LogNone, LogFunc
  comments = CommentNone,    -- CommentNone, CommentFunc
  onSfwrConstraint = Warning,  -- Warning, Exception
  onPhysConstraint = Warning,  -- Warning, Exception
  inputStructure = Loose    -- Loose, AsClass
}  
       
main :: IO ()            
main = do
  gen (DocSpec Website "SSP_SRS") ssp_srs sspSymMap
  gen (DocSpec SRS "SSP_SRS")     ssp_srs sspSymMap
  --genCode ssp_code
