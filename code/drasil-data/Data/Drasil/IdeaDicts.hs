module Data.Drasil.IdeaDicts where

import Language.Drasil


compScience :: IdeaDict
-------------------------------------------------------------------------------
--  IdeaDict     |   |      id       |       term                    |  abbreviation
-------------------------------------------------------------------------------
compScience  = mkIdea  "compScience"    (cn' "Computer Science")      (Just "CS")
softEng      = mkIdea  "softEng"        (cn' "Software Engineering")  (Just "SE")
mathematics  = mkIdea  "mathematics"    (cn' "Mathematics")           Nothing
progLanguage = mkIdea  "progLanguage"   (cn' "Programming Language")  Nothing
idglass      = mkIdea  "glass"          (cn' "GlassBR")               (Just "GlassBR")
physics      = mkIdea  "physics"        (cn' "physics")               Nothing
civilEng     = mkIdea  "civilEng"       (cn' "civil engineering")     Nothing
materialEng  = mkIdea  "materialEng"    (cn' "material engineering")  Nothing
        



