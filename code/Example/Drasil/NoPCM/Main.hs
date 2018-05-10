module Main (main) where

import Language.Drasil (DocType(SRS,Website),DocSpec(DocSpec),gen
  , genCode, Choices(..), ImplementationType(..)
  , Logging(..), ConstraintBehaviour(..), Structure(..), Comments(..)
  , Lang(..))

import Drasil.NoPCM.Body (nopcm_srs, nopcm_code, nopcm_SymbMap)

nopcm_Choices :: Choices
nopcm_Choices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,
  comments = CommentNone,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure = Loose
}       
       
main :: IO ()            
main = do
  gen (DocSpec SRS "NoPCM_SRS") nopcm_srs nopcm_SymbMap
  gen (DocSpec Website "NoPCM_SRS") nopcm_srs nopcm_SymbMap
  genCode [] nopcm_Choices nopcm_code
