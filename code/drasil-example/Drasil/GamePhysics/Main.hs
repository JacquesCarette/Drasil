module Main where

-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
--   ConstraintBehaviour(..), ImplementationType(..), Lang(..), Logging(..), 
--   Structure(..), InputModule(..), Visibility(..))
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
--   dates            = Hide,
--   onSfwrConstraint = Warning,
--   onPhysConstraint = Warning,
--   inputStructure   = Unbundled,
--   inputModule      = Combined
-- }       
       
main :: IO ()
main = do
  gen (DocSpec SRS "GamePhysics_SRS") srs  printSetting
  gen (DocSpec Website "GamePhysics_SRS") srs printSetting
  -- When ready to generate code from GamePhysics, uncomment this file
  -- genCode choices code
