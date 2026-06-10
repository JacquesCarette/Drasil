-- | Document language for lesson plan notebooks.
module Drasil.LessonPlan.Renderer (mkNb) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Language.Drasil (Sentence(S), CI, foldlList, SepType(Comma),
  FoldType(List), fullName, Idea, titleize, titleize')
import Language.Drasil.Document (Section, Document(Notebook), Contents(UlC),
  ulcc, RawContent(Bib), section, makeSecRef)
import Drasil.System (LessonPlan, HasSystemMeta(..))
import Drasil.Metadata.Documentation (notebook)
import qualified Drasil.Metadata.Documentation as Doc (caseProb, introduction,
  learnObj, review, summary, example, appendix, reference)

import Drasil.LessonPlan.Document (LsnDesc, LsnChapter(..))
import Drasil.LessonPlan.ExtractBib (extractBib)

-- | Creates a notebook from a lesson description and system information.
mkNb :: LessonPlan -> LsnDesc -> (CI -> CI -> Sentence) -> Document
mkNb plan dd comb = Notebook nm as $ mkSections (plan ^. systemdb) dd
  where
    nm = notebook `comb` (plan ^. sysName)
    as = foldlList Comma List $ map (S . fullName) $ plan ^. authors

-- | Helper for creating the notebook sections.
mkSections :: ChunkDB -> LsnDesc -> [Section]
mkSections db dd = map doit dd
  where
    bib = [UlC $ ulcc (Bib $ extractBib db dd)]
    doit :: LsnChapter -> Section
    doit (Intro i)        = mkLsnSec  "Intro"      Doc.introduction i   []
    doit (LearnObj lo)    = mkLsnSec' "LearnObj"   Doc.learnObj     lo  []
    doit (Review r ss)    = mkLsnSec  "Review"     Doc.review       r   ss
    doit (CaseProb cp ss) = mkLsnSec  "CaseProb"   Doc.caseProb     cp  ss
    doit (Example e)      = mkLsnSec  "Example"    Doc.example      e   []
    doit (Smmry s)        = mkLsnSec  "Summary"    Doc.summary      s   []
    doit BibSec           = mkLsnSec' "References" Doc.reference    bib []
    doit (Apndx a)        = mkLsnSec  "Appendix"   Doc.appendix     a   []

-- | Internal: Create a section of the lesson plan. Title is singular.
mkLsnSec :: Idea c => String -> c -> [Contents] -> [Section] -> Section
mkLsnSec r c cs ss = section t cs ss (makeSecRef r t)
  where t = titleize c

-- | Internal: Create a section of the lesson plan. Title is made plural.
mkLsnSec' :: Idea c => String -> c -> [Contents] -> [Section] -> Section
mkLsnSec' r c cs ss = section t cs ss (makeSecRef r t)
  where t = titleize' c
