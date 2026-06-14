-- | Defines domains of knowledge for use in Drasil.
module Drasil.Metadata.Domains where

import Drasil.Database (mkUid)
import Language.Drasil (IdeaDict, idea, idea', cn')

-- | Various domains that are used in Drasil. May have an abbreviation.
compScience, softEng, mathematics, progLanguage, physics, civilEng,
  materialEng, documentc, knowledgemng :: IdeaDict
-------------------------------------------------------------------------------
--  IdeaDict     |   |      id       |       term                    |  abbreviation
-------------------------------------------------------------------------------
-- | For ideas, concepts, or terms related to Computer Science.
compScience  = idea  (mkUid "compScience")    (cn' "Computer Science")      "CS"
-- | For ideas, concepts, or terms related to Software Engineering.
softEng      = idea  (mkUid "softEng")        (cn' "Software Engineering")  "SE"
-- | For ideas, concepts, or terms related to Mathematics.
mathematics  = idea' (mkUid "mathematics")    (cn' "Mathematics")
-- | For ideas, concepts, or terms related to Programming Languages.
progLanguage = idea' (mkUid "progLanguage")   (cn' "Programming Language")
-- | For ideas, concepts, or terms related to Physics.
physics      = idea' (mkUid "physics")        (cn' "Physics")
-- | For ideas, concepts, or terms related to Civil Engineering.
civilEng     = idea' (mkUid "civilEng")       (cn' "Civil Engineering")
-- | For ideas, concepts, or terms related to Material Engineering.
materialEng  = idea' (mkUid "materialEng")    (cn' "Material Engineering")
-- | For ideas, concepts, or terms related to Documents.
documentc    = idea  (mkUid "documentc")      (cn' "Document")              "Doc"
-- | For ideas, concepts, or terms related to Knowledge Management.
knowledgemng = idea' (mkUid "knowledgemng")   (cn' "Knowledge Management")
