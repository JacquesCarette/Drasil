module Drasil.NBSections.Introduction (introductionSection, purposeOfDoc) where

import Language.Drasil
import qualified Drasil.DocLang.Notebook as NB (intro, prpsOfDoc)

-- | Constructor for the Notebook introduction section
-- problemIntroduction - Sentence introducing the specific example problem
-- programDefinition  - Sentence definition of the specific example
-- **** programDefinition : maybe just topic 
introductionSection :: [Contents] -> [Section] -> Section
introductionSection problemIntroduction = NB.intro problemIntroduction

-- | Constructor for purpose of document subsection
-- purposeOfProgramParagraph - a sentence explaining the purpose of the document
purposeOfDoc :: [Sentence] -> Section
purposeOfDoc [purposeOfProgram] = NB.prpsOfDoc [mkParagraph purposeOfProgram] []
purposeOfDoc _ = NB.prpsOfDoc [] []