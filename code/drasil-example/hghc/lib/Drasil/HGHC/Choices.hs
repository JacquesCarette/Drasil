module Drasil.HGHC.Choices where

-- import Language.Drasil (QDefinition)
-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
--   Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..), 
--   Modularity(..), Structure(..), ConstantStructure(..), 
--   ConstantRepr(..), matchConcepts, AuxFile(..), 
--   Visibility(..), defaultChoices)
-- import Drasil.HGHC.Body (fullSI)

-- thisCode :: CodeSpec
-- thisCode = codeSpec thisSI choices []

{- When we want to actually generate code from this again, uncomment
thisChoices :: Choices
thisChoices = defaultChoices {
  lang             = [Python, Cpp, CSharp, Java],
  modularity       = Modular,
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
  auxFiles         = [SampleInput "../../datafiles/hghc/sampleInput.txt"] 
} -}