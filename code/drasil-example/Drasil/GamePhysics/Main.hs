module Drasil.GamePhysics.Main where

import GHC.IO.Encoding

-- import Language.Drasil (QDefinition)
-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
--   Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
--   Modularity(..), Structure(..), ConstantStructure(..), 
--   ConstantRepr(..), InputModule(..), matchConcepts, AuxFile(..), 
--   Visibility(..), defaultChoices)
import Language.Drasil.Generate (gen, genDot, DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.GamePhysics.Body (srs, printSetting, fullSI) -- sysInfo

-- code :: CodeSpec
-- code = codeSpec sysInfo choices []

-- choices :: Choices
-- choices = defaultChoices {
--   lang             = [Python, Cpp, CSharp, Java],
--   modularity       = Modular Combined,
--   impType          = Library,
--   logFile          = "log.txt",
--   logging          = [],
--   comments         = CommentNone,
--   doxVerbosity     = Quiet,
--   dates            = Hide,
--   onSfwrConstraint = Warning,
--   onPhysConstraint = Warning,
--   inputStructure   = Unbundled,
--   constStructure   = Inline,
--   constRepr        = Const,
--   conceptMatch     = matchConcepts ([] :: [QDefinition]) [],
--   auxFiles         = [SampleInput "../../datafiles/GamePhysics/sampleInput.txt"]
-- }       
       
main :: IO ()
main = do
  setLocaleEncoding utf8
  gen (DocSpec SRS "GamePhysics_SRS") srs  printSetting
  gen (DocSpec Website "GamePhysics_SRS") srs printSetting
  genDot fullSI
  -- When ready to generate code from GamePhysics, uncomment this file
  -- genCode choices code
