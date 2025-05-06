-- | Document language for lesson plan notebooks.
module Drasil.DocumentLanguage.Notebook.DocumentLanguage(mkNb) where

import Drasil.DocumentLanguage.Notebook.LsnDecl (LsnDecl, mkLsnDesc)
import Drasil.DocumentLanguage.Notebook.Core (LsnDesc, LsnChapter(..), 
  Intro(..), LearnObj(..), Review(..), CaseProb(..), Example(..), Smmry(..), Apndx(..))

import Language.Drasil hiding (kind)

import SysInfo.Drasil (System(SI), _authors, _kind, _sys, citeDB)

import qualified Drasil.DocLang.Notebook as Lsn (intro, learnObj, caseProb, example, 
  appendix, review, reference, summary)

-- | Creates a notebook from a lesson description and system information.
mkNb :: LsnDecl -> (IdeaDict -> IdeaDict -> Sentence) -> System -> Document
mkNb dd comb si@SI {_sys = sys, _kind = kind, _authors = authors} =
  Notebook (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) $
  mkSections si l where
    l = mkLsnDesc si dd

-- | Helper for creating the notebook sections.
mkSections :: System -> LsnDesc -> [Section]
mkSections si = map doit  
  where
    doit :: LsnChapter -> Section
    doit (Intro i)     = mkIntro i
    doit (LearnObj lo) = mkLearnObj lo
    doit (Review r)    = mkReview r
    doit (CaseProb cp) = mkCaseProb cp
    doit (Example e)   = mkExample e
    doit (Smmry s)     = mkSmmry s
    doit BibSec        = mkBib (citeDB si)
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

    
