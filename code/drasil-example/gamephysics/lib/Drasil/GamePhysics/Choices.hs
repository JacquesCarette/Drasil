module Drasil.GamePhysics.Choices where

-- import Language.Drasil (QDefinition)
-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..),
--   Verbosity(..), ConstraintBehaviour(..), ImplementationType(..), Lang(..),
--   Modularity(..), Structure(..), ConstantStructure(..),
--   ConstantRepr(..), matchConcepts, AuxFile(..),
--   Visibility(..), defaultChoices)
-- import Drasil.GamePhysics.Body (fullSI)

-- code :: CodeSpec
-- code = codeSpec fullSI choices []

-- choices :: Choices
-- choices = defaultChoices {
--   lang             = [Python, Cpp, CSharp, Java],
--   modularity       = Modular,
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
--   auxFiles         = [SampleInput "../../datafiles/gamephysics/sampleInput.txt"]
-- }
