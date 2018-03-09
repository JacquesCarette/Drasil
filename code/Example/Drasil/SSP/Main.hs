module Main (main) where

import Language.Drasil (DocType(SRS,Website), DocSpec(DocSpec),Recipe(Recipe), gen
  , Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , Lang(..))

import Drasil.SSP.Body (ssp_srs, sspSymMap)

docs :: [Recipe]
docs = [Recipe (DocSpec Website "SSP_SRS") ssp_srs,
        Recipe (DocSpec SRS "SSP_SRS") ssp_srs
       ]

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
  gen docs sspSymMap
  --genCode ssp_code
