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

import Drasil.SSP.Body (srs, printSetting) -- si

-- code :: CodeSpec
-- code = codeSpec si choices []

-- choices :: Choices
-- choices = defaultChoices {
--   lang = [Python, Cpp, CSharp, Java],
--   modularity = Modular Combined,
--   impType = Program,
--   logFile = "log.txt",
--   logging = [],         -- LogVar, LogFunc
--   comments = [],    -- CommentFunc, CommentClass, CommentMod
--   doxVerbosity = Quiet, -- Verbose, Quiet
--   dates = Hide,      -- Show, Hide
--   onSfwrConstraint = Warning,  -- Warning, Exception
--   onPhysConstraint = Warning,  -- Warning, Exception
--   inputStructure = Unbundled,    -- Unbundled, Bundled
--   constStructure = Inline,   -- Inline, WithInputs, Store Structure
--   constRepr = Const,    -- Var, Const
--   conceptMatch = matchConcepts ([] :: [QDefinition]) [],
--   auxFiles = [SampleInput "../../datafiles/SSP/sampleInput.txt"]
-- }
       
main :: IO ()            
main = do
  setLocaleEncoding utf8
  gen (DocSpec Website "SSP_SRS") srs printSetting
  gen (DocSpec SRS "SSP_SRS")     srs printSetting
  -- for when we can generate code again, uncomment this file
  --genCode choices code
