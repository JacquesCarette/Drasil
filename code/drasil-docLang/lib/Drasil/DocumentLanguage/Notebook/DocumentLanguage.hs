-- | Document language for lesson plan notebooks.
module Drasil.DocumentLanguage.Notebook.DocumentLanguage(mkNb) where

import Drasil.DocumentLanguage.Notebook.NBDecl (NBDecl, mkNBDesc)
import Drasil.DocumentLanguage.Notebook.Core (ApndxSec(..), NBDesc, DocSection(..), 
  IntrodSec(..), IntrodSub(..), BodySec(..), BodySub(..), SmmrySec(..))

import Language.Drasil hiding (kind)

import SysInfo.Drasil (SystemInformation(SI), _authors, _kind, _sys, citeDB)

import qualified Drasil.DocLang.Notebook as NB (appendix, body, reference, summary)
import qualified Drasil.NBSections.Introduction as Intro (introductionSection, purposeOfDoc)
import qualified Drasil.NBSections.Body as Body (reviewSec, mainIdeaSec, mthdAndanls, exampleSec)

-- | Creates a notebook from a document description and system information.
mkNb :: NBDecl -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkNb dd comb si@SI {_sys = sys, _kind = kind, _authors = authors} =
  Notebook (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) $
  mkSections si l where
    l = mkNBDesc si dd

-- | Helper for creating the notebook sections.
-- Add NBDesc for references.
mkSections :: SystemInformation -> NBDesc -> [Section]
mkSections si = map doit  
  where
    doit :: DocSection -> Section
    doit (IntrodSec is)      = mkIntroSec si is
    doit (BodySec bs)        = mkBodySec bs
    doit (SmmrySec ss)       = mkSmmrySec ss
    doit BibSec              = mkBib (citeDB si)
    doit (ApndxSec a)        = mkAppndxSec a

-- Add more intro subsections
-- | Helper for making the 'Introduction' section.
mkIntroSec :: SystemInformation -> IntrodSec -> Section
mkIntroSec si (IntrodProg probIntro l) =
  Intro.introductionSection probIntro $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntrodSub -> Section
    mkSubIntro _ (InPurpose intro) = Intro.purposeOfDoc intro

-- | Helper for making the 'Body' section.
mkBodySec :: BodySec -> Section
mkBodySec (BodyProg l) = NB.body [] $ map mkSubs l
  where
    mkSubs :: BodySub -> Section
    mkSubs (Review cs )                 = Body.reviewSec cs 
    mkSubs (MainIdea cntnts subsec)     = Body.mainIdeaSec cntnts subsec
    mkSubs (MethsAndAnls cntnts subsec) = Body.mthdAndanls cntnts subsec
    mkSubs (Example cntnts subsec)      = Body.exampleSec cntnts subsec

-- | Helper for making the 'Summary' section.
mkSmmrySec :: SmmrySec -> Section
mkSmmrySec (SmmryProg cs) = NB.summary cs []

-- | Helper for making the 'Bibliography' section.
mkBib :: BibRef -> Section
mkBib bib = NB.reference [UlC $ ulcc (Bib bib)] []

-- | Helper for making the 'Appendix' section.
mkAppndxSec :: ApndxSec -> Section
mkAppndxSec (ApndxProg cs) = NB.appendix cs []

    