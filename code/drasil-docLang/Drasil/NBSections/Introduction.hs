module Drasil.NBSections.Introduction (introductionSection, purposeOfDoc) where

import Language.Drasil
import qualified Drasil.DocLang.Notebook as NB (intro, prpsOfDoc)
import Utils.Drasil
import Utils.Drasil.Sentence

-- | Constructor for the Notebook introduction section
-- problemIntroduction - Sentence introducing the specific example problem
-- programDefinition  - Sentence definition of the specific example
-- **** programDefinition : maybe just topic 
introductionSection :: Sentence -> Sentence -> [Section] -> Section
introductionSection problemIntroduction programDefinition = NB.intro 
  [mkParagraph problemIntroduction, overviewParagraph programDefinition]


-- | Constructor for the overview paragraph for the introduction
-- programDefinition - defintion of the specific example being generated
-- **** list of subsections for intro sec
overviewParagraph :: Sentence -> Contents
overviewParagraph programDefinition = foldlSP [S "The presentation below is based on 
  Section 12.6 (Motion of a Projectile) from the classic Hibbler text 
  \"Engineering Mechanics Dynamnics, 10th edition\""]

-- | Constructor for purpose of document subsection
-- purposeOfProgramParagraph - a sentence explaining the purpose of the document
purposeOfDoc :: [Sentence] -> Section
purposeOfDoc [purposeOfProgram] = NB.prpsOfDoc [mkParagraph purposeOfProgram] []