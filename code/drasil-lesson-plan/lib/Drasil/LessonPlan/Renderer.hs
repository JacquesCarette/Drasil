-- | Document language for lesson plan notebooks.
module Drasil.LessonPlan.Renderer (mkNb) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Language.Drasil (Sentence(S), CI, BibRef,
  foldlList, SepType(Comma), FoldType(List), fullName)
import Language.Drasil.Document (Section, Document(Notebook), Contents(UlC), ulcc, RawContent(Bib))
import Drasil.System (LessonPlan, HasSystemMeta(..))
import Drasil.Metadata.Documentation (notebook)

import Drasil.LessonPlan.Document (LsnDesc, LsnChapter(..),
  intro, learnObj, caseProb, example, appendix, review, reference, summary)
import Drasil.LessonPlan.ExtractBib (extractBib)

-- | Creates a notebook from a lesson description and system information.
mkNb :: LessonPlan -> LsnDesc -> (CI -> CI -> Sentence) -> Document
mkNb plan dd comb = Notebook nm as $ mkSections (plan ^. systemdb) dd
  where
    nm = notebook `comb` (plan ^. sysName)
    as = foldlList Comma List $ map (S . fullName) $ plan ^. authors

-- | Helper for creating the notebook sections.
mkSections :: ChunkDB -> LsnDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: LsnChapter -> Section
    doit (Intro i)        = intro i []
    doit (LearnObj lo)    = learnObj lo []
    doit (Review r ss)    = review r ss
    doit (CaseProb cp ss) = caseProb cp ss
    doit (Example e)      = example e []
    doit (Smmry s)        = summary s []
    doit BibSec           = mkBib (extractBib si dd)
    doit (Apndx a)        = appendix a []

-- | Helper for making the 'Bibliography' section.
mkBib :: BibRef -> Section
mkBib bib = reference [UlC $ ulcc (Bib bib)] []
