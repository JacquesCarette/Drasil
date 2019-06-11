module Main where

-- import Language.Drasil.Code (Choices(..), CodeSpec, codeSpec, Comments(..), 
--   ConstraintBehaviour(..), ImplementationType(..), Lang(..), Logging(..), 
--   Structure(..))
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
--   onSfwrConstraint = Warning,
--   onPhysConstraint = Warning,
--   inputStructure   = Loose
-- }       
       
main :: IO ()
main = do
  gen (DocSpec SRS "Chipmunk_SRS") srs  printSetting
  gen (DocSpec Website "Chipmunk_SRS") srs printSetting
  -- When ready to generate code from GamePhys, uncomment this file
  -- genCode choices code
