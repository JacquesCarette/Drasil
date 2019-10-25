module Main where

-- import Language.Drasil (QDefinition)
-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
--   Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
--   Logging(..), Structure(..), ConstantStructure(..), ConstantRepr(..), 
--   InputModule(..), matchConcepts, AuxFile(..), Visibility(..))
import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.GamePhysics.Body (srs, printSetting) -- sysInfo

-- code :: CodeSpec
-- code = codeSpec sysInfo choices []

-- choices :: Choices
-- choices = Choices {
--   lang             = [Python, Cpp, CSharp, Java],
--   impType          = Library,
--   logFile          = "log.txt",
--   logging          = LogNone,
--   comments         = CommentNone,
--   doxVerbosity     = Quiet,
--   dates            = Hide,
--   onSfwrConstraint = Warning,
--   onPhysConstraint = Warning,
--   inputStructure   = Unbundled,
--   constStructure   = Inline,
--   constRepr        = Const,
--   inputModule      = Combined,
--   conceptMatch     = matchConcepts ([] :: [QDefinition]) [],
--   auxFiles         = [SampleInput]
-- }       
       
main :: IO ()
main = do
  gen (DocSpec SRS "GamePhysics_SRS") srs  printSetting
  gen (DocSpec Website "GamePhysics_SRS") srs printSetting
  -- When ready to generate code from GamePhysics, uncomment this file
  -- genCode choices code
