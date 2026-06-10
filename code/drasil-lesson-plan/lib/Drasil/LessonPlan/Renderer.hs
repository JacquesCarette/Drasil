-- | Document language for lesson plan notebooks.
module Drasil.LessonPlan.Renderer (mkNb) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Language.Drasil (Sentence(S), CI, BibRef,
  foldlList, SepType(Comma), FoldType(List), fullName)
import Language.Drasil.Document (Section, Document(Notebook), Contents(UlC), ulcc, RawContent(Bib))
import Drasil.System (LessonPlan, HasSystemMeta(..))
import Drasil.Metadata.Documentation (notebook)

import Drasil.LessonPlan.Document (LsnDesc, LsnChapter(..), Intro(..),
  LearnObj(..), Review(..), CaseProb(..), Example(..), Smmry(..), Apndx(..),
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
    doit (Intro i)     = mkIntro i
    doit (LearnObj lo) = mkLearnObj lo
    doit (Review r)    = mkReview r
    doit (CaseProb cp) = mkCaseProb cp
    doit (Example e)   = mkExample e
    doit (Smmry s)     = mkSmmry s
    doit BibSec        = mkBib (extractBib si dd)
    doit (Apndx a)     = mkAppndx a

-- | Helper for making the 'Introduction' section.
mkIntro :: Intro -> Section
mkIntro (IntrodProg i) = intro i []

-- | Helper for making the 'Learning Objectives' section.
mkLearnObj :: LearnObj -> Section
mkLearnObj (LrnObjProg cs) = learnObj cs []

-- | Helper for making the 'Review' section.
mkReview :: Review -> Section
mkReview (ReviewProg r ss) = review r ss

-- | Helper for making the 'Case Problem' section.
mkCaseProb :: CaseProb -> Section
mkCaseProb (CaseProbProg cp ss) = caseProb cp ss

-- | Helper for making the 'Example' section.
mkExample:: Example -> Section
mkExample (ExampleProg cs) = example cs []

-- | Helper for making the 'Summary' section.
mkSmmry :: Smmry -> Section
mkSmmry (SmmryProg cs) = summary cs []

-- | Helper for making the 'Bibliography' section.
mkBib :: BibRef -> Section
mkBib bib = reference [UlC $ ulcc (Bib bib)] []

-- | Helper for making the 'Appendix' section.
mkAppndx :: Apndx -> Section
mkAppndx (ApndxProg cs) = appendix cs []
