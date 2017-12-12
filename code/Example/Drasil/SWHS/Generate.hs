module Drasil.SWHS.Generate (generate) where

import Language.Drasil

import Drasil.SWHS.Body (swhs_srs', swhsSymMap)

docs :: [Recipe]
docs = [Recipe (SRS "SWHS_SRS") swhs_srs',
        Recipe (Website "SWHS_SRS") swhs_srs'
       ]
       
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
}

generate :: IO ()
generate = gen docs swhsSymMap