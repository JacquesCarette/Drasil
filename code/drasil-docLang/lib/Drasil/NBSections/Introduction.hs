-- | Defines the lesson plan notebook introduction section constructors.
module Drasil.NBSections.Introduction (
  -- * Constructors
  introductionSection, purposeOfDoc) where

import Language.Drasil
import qualified Drasil.DocLang.Notebook as NB (intro, prpsOfDoc)

-- programDefinition : maybe just topic 
-- | Constructor for the Notebook introduction section
-- problemIntroduction - Sentence introducing the specific example problem
-- programDefinition  - Sentence definition of the specific example
introductionSection :: [Contents] -> Section
introductionSection = NB.intro 

-- | Constructor for purpose of document subsection
-- purposeOfProgramParagraph - a sentence explaining the purpose of the document
purposeOfDoc :: [Sentence] -> Section
purposeOfDoc [purposeOfProgram] = NB.prpsOfDoc [mkParagraph purposeOfProgram]
purposeOfDoc _ = NB.prpsOfDoc []