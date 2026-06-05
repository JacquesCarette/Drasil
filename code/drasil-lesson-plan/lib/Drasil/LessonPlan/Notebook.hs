-- | Document language for lesson plan notebooks.
module Drasil.LessonPlan.Notebook
  (mkNb, LsnDesc, LsnChapter(..), LearnObj(..), Review(..), CaseProb(..), Example(..)
  ) where

import Control.Lens ((^.))

import Drasil.Database (ChunkDB)
import Language.Drasil (Sentence(S), CI, BibRef,
  foldlList, SepType(Comma), FoldType(List), fullName)
import Language.Drasil.Docs (Section, Document(Notebook), Contents(UlC), ulcc, RawContent(Bib))
import Drasil.System (LessonPlan, HasSystemMeta(..))
import Drasil.Metadata.Documentation (notebook)

import Drasil.LessonPlan.Core (LsnDesc, LsnChapter(..),
  Intro(..), LearnObj(..), Review(..), CaseProb(..), Example(..), Smmry(..), Apndx(..))
import qualified Drasil.LessonPlan.Sections as Lsn (intro, learnObj, caseProb, example,
  appendix, review, reference, summary)
import Drasil.LessonPlan.ExtractLsnDesc (extractLsnPlanBib)

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
    doit BibSec        = mkBib (extractLsnPlanBib si dd)
    doit (Apndx a)     = mkAppndx a

-- | Helper for making the 'Introduction' section.
mkIntro :: Intro -> Section
mkIntro (IntrodProg i) = Lsn.intro i []

-- | Helper for making the 'Learning Objectives' section.
mkLearnObj :: LearnObj -> Section
mkLearnObj (LrnObjProg cs) = Lsn.learnObj cs []

-- | Helper for making the 'Review' section.
mkReview :: Review -> Section
mkReview (ReviewProg r ss) = Lsn.review r ss

-- | Helper for making the 'Case Problem' section.
mkCaseProb :: CaseProb -> Section
mkCaseProb (CaseProbProg cp ss) = Lsn.caseProb cp ss

-- | Helper for making the 'Example' section.
mkExample:: Example -> Section
mkExample (ExampleProg cs) = Lsn.example cs []

-- | Helper for making the 'Summary' section.
mkSmmry :: Smmry -> Section
mkSmmry (SmmryProg cs) = Lsn.summary cs []

-- | Helper for making the 'Bibliography' section.
mkBib :: BibRef -> Section
mkBib bib = Lsn.reference [UlC $ ulcc (Bib bib)] []

-- | Helper for making the 'Appendix' section.
mkAppndx :: Apndx -> Section
mkAppndx (ApndxProg cs) = Lsn.appendix cs []
