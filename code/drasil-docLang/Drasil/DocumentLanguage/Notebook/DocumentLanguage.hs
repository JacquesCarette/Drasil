module Drasil.DocumentLanguage.Notebook.DocumentLanguage where

import Drasil.DocumentLanguage.Notebook.NBDecl (NBDecl, mkNBDesc)
import Drasil.DocumentLanguage.Notebook.Core (AppndxSec(..), NBDesc, DocSection(..), 
  IntroSec(..), IntroSub(..), BodySec(..), BodySub(..), SmmrySec(..))

import Language.Drasil

import Utils.Drasil

import Database.Drasil(SystemInformation(SI), _authors, _kind, _sys, citeDB)

import qualified Drasil.DocLang.Notebook as NB (appendix, body, reference, summary)
import qualified Drasil.NBSections.Introduction as Intro (introductionSection, purposeOfDoc)
import qualified Drasil.NBSections.Body as Body (reviewSec, mainIdeaSec, mthdAndanls)

-- | Creates a document from a document description and system information
mkDoc :: NBDecl -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc dd comb si@SI {_sys = sys, _kind = kind, _authors = authors} =
  Document (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) $
  mkSections si l where
    l = mkNBDesc si dd

-- | Helper for creating the notebook sections
mkSections :: SystemInformation -> NBDesc -> [Section]
mkSections si dd = map doit dd
  where
    doit :: DocSection -> Section
    doit (IntroSec is)       = mkIntroSec si is
    doit (BodySec bs)        = mkBodySec bs
    doit Bibliography        = mkBib (citeDB si)
    doit (SmmrySec ss)       = mkSmmrySec ss
    doit (AppndxSec a)       = mkAppndxSec a


-- | Helper for making the 'Introduction' section
-- **** Add intro subsections
mkIntroSec :: SystemInformation -> IntroSec -> Section
mkIntroSec si (IntroProg probIntro l) =
  Intro.introductionSection probIntro $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> Section
    mkSubIntro _ (IPurpose intro) = Intro.purposeOfDoc intro

-- | Helper for making the 'Body' section
mkBodySec :: BodySec -> Section
mkBodySec (BodyProg l) = NB.body [] $ map mkSubs l
  where
    mkSubs :: BodySub -> Section
    mkSubs (Review cs )                 = Body.reviewSec cs 
    mkSubs (MainIdea cntnts subsec)     = Body.mainIdeaSec cntnts subsec
    mkSubs (MethsAndAnls cntnts subsec) = Body.mthdAndanls cntnts subsec

-- | Helper for making the 'Summary' section
mkSmmrySec :: SmmrySec -> Section
mkSmmrySec (SmmryProg cs) = NB.summary cs []

-- | Helper for making the 'Bibliography' section
mkBib :: BibRef -> Section
mkBib bib = NB.reference [UlC $ ulcc (Bib bib)] []

-- | Helper for making the 'Appendix' section
mkAppndxSec :: AppndxSec -> Section
mkAppndxSec (AppndxProg cs) = NB.appendix cs []

    