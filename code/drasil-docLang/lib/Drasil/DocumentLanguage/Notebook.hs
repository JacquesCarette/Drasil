-- | Document language for lesson plan notebooks.
module Drasil.DocumentLanguage.Notebook
  (mkNb, LsnDesc, LsnChapter(..), LearnObj(..), Review(..), CaseProb(..), Example(..)
  ) where

import Control.Lens ((^.))

import Drasil.DocumentLanguage.Notebook.Core (LsnDesc, LsnChapter(..),
  Intro(..), LearnObj(..), Review(..), CaseProb(..), Example(..), Smmry(..), Apndx(..))

import Language.Drasil (IdeaDict, Sentence(S), Section, CI, Document(Notebook), BibRef,
  foldlList, SepType(Comma), FoldType(List), name, Contents(UlC), ulcc, RawContent(Bib))

import Drasil.System (System(SI), _authors, whatsTheBigIdea, sysName)
import Drasil.GetChunks (citeDBLsn)

import qualified Drasil.DocLang.Notebook as Lsn (intro, learnObj, caseProb, example,
  appendix, review, reference, summary)

-- | Creates a notebook from a lesson description and system information.
mkNb :: LsnDesc -> (IdeaDict -> CI -> Sentence) -> System -> Document
mkNb dd comb si@SI { _authors = authors } =
  Notebook (whatsTheBigIdea si `comb` (si ^. sysName)) (foldlList Comma List $ map (S . name) authors) $
  mkSections si dd

-- | Helper for creating the notebook sections.
mkSections :: System -> LsnDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: LsnChapter -> Section
    doit (Intro i)     = mkIntro i
    doit (LearnObj lo) = mkLearnObj lo
    doit (Review r)    = mkReview r
    doit (CaseProb cp) = mkCaseProb cp
    doit (Example e)   = mkExample e
    doit (Smmry s)     = mkSmmry s
    doit BibSec        = mkBib (citeDBLsn si dd)
    doit (Apndx a)     = mkAppndx a

-- | Helper for making the 'Introduction' section.
mkIntro :: Intro -> Section
mkIntro (IntrodProg i) = Lsn.intro i []

-- | Helper for making the 'Learning Objectives' section.
mkLearnObj :: LearnObj -> Section
mkLearnObj (LrnObjProg cs) = Lsn.learnObj cs []

-- | Helper for making the 'Review' section.
mkReview :: Review -> Section
mkReview (ReviewProg r) = Lsn.review r []

-- | Helper for making the 'Case Problem' section.
mkCaseProb :: CaseProb -> Section
mkCaseProb (CaseProbProg cp) = Lsn.caseProb cp []

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
