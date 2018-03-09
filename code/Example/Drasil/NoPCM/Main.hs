module Main (main) where

import Language.Drasil (DocType(SRS,Website),DocSpec(DocSpec),Recipe(..),gen
  , genCode, Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , Lang(..))

import Drasil.NoPCM.Body (nopcm_srs, nopcm_code, nopcm_SymbMap)

docs :: [Recipe]
docs = [Recipe (DocSpec SRS "NoPCM_SRS") nopcm_srs,
        Recipe (DocSpec Website "NoPCM_SRS") nopcm_srs
       ]

nopcm_Choices :: Choices
nopcm_Choices = Choices {
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
  gen docs nopcm_SymbMap
  genCode nopcm_Choices nopcm_code
