module Main (main) where

import GHC.IO.Encoding

-- import Language.Drasil (QDefinition)
-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
--   Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
--   Modularity(..), Structure(..), ConstantStructure(..), 
--   ConstantRepr(..), InputModule(..), matchConcepts, AuxFile(..), 
--   Visibility(..), defaultChoices)
import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.HGHC.Body (srs, printSetting) --thisSI

-- thisCode :: CodeSpec
-- thisCode = codeSpec thisSI choices []

{- When we want to actually generate code from this again, uncomment
thisChoices :: Choices
thisChoices = defaultChoices {
  lang             = [Python, Cpp, CSharp, Java],
  modularity       = Modular Combined,
  impType          = Program,
  logFile          = "log.txt",
  logging          = [],
  comments         = [], 
  doxVerbosity     = Quiet,
  dates            = Hide,
  onSfwrConstraint = Warning,
  onPhysConstraint = Warning,
  inputStructure   = Bundled,
  constStructure   = Inline,
  constRepr        = Const,
  conceptMatch     = matchConcepts ([] :: [QDefinition]) [],
  auxFiles         = [SampleInput "../../datafiles/HGHC/sampleInput.txt"] 
} -}
  
main :: IO ()            
main = do
  setLocaleEncoding utf8
  gen (DocSpec Website "HGHC_SRS") srs printSetting
  gen (DocSpec SRS "HGHC_SRS")     srs printSetting
  -- When ready to generate code, uncomment this file
  --genCode thisChoices thisCode
