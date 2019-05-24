module Drasil.SWHS.Generate (generate) where

-- import Language.Drasil.Code (Choices(..), Comments(..), ConstraintBehaviour(..), 
--   ImplementationType(..), Lang(..), Logging(..), Structure(..))
import Language.Drasil.Generate (gen)
import Language.Drasil.Printers (DocType(SRS, Website), DocSpec(DocSpec))

import Drasil.SWHS.Body (srs', printSetting)

{- When ready to generate code from SWHS, re-enable
swhsChoices :: Choices
swhsChoices = Choices {
  lang = [Python, Cpp, CSharp, Java],
  impType = Program,
  logFile = "log.txt",
  logging = LogNone,         -- LogNone, LogFunc
  comments = CommentNone,    -- CommentNone, CommentFunc
  onSfwrConstraint = Warning,  -- Warning, Exception
  onPhysConstraint = Warning,  -- Warning, Exception
  inputStructure = Loose    -- Loose, AsClass
} -}

generate :: IO ()
generate = do
  gen (DocSpec SRS "SWHS_SRS")     srs' printSetting
  gen (DocSpec Website "SWHS_SRS") srs' printSetting
       
