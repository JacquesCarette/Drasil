-- | Defines domains of knowledge for use in Drasil.
module Drasil.Metadata.Domains where

import Language.Drasil (IdeaDict, mkIdea, cn')

-- | Various domains that are used in Drasil. May have an abbreviation.
compScience, softEng, mathematics, progLanguage, physics, civilEng,
  materialEng, documentc, knowledgemng :: IdeaDict
-------------------------------------------------------------------------------
--  IdeaDict     |   |      id       |       term                    |  abbreviation
-------------------------------------------------------------------------------
-- | For ideas, concepts, or terms related to Computer Science.
compScience  = mkIdea  "compScience"    (cn' "Computer Science")      (Just "CS")
-- | For ideas, concepts, or terms related to Software Engineering.
softEng      = mkIdea  "softEng"        (cn' "Software Engineering")  (Just "SE")
-- | For ideas, concepts, or terms related to Mathematics.
mathematics  = mkIdea  "mathematics"    (cn' "Mathematics")           Nothing
-- | For ideas, concepts, or terms related to Programming Languages.
progLanguage = mkIdea  "progLanguage"   (cn' "Programming Language")  Nothing
-- | For ideas, concepts, or terms related to Physics.
physics      = mkIdea  "physics"        (cn' "Physics")               Nothing
-- | For ideas, concepts, or terms related to Civil Engineering.
civilEng     = mkIdea  "civilEng"       (cn' "Civil Engineering")     Nothing
-- | For ideas, concepts, or terms related to Material Engineering.
materialEng  = mkIdea  "materialEng"    (cn' "Material Engineering")  Nothing
-- | For ideas, concepts, or terms related to Documents.
documentc    = mkIdea  "documentc"      (cn' "Document")              (Just "Doc")
-- | For ideas, concepts, or terms related to Knowledge Management.
knowledgemng = mkIdea  "knowledgemng"   (cn' "Knowledge Management")  Nothing
