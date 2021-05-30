module Drasil.DocumentLanguage.Notebook.DocumentLanguage where

import Drasil.DocDecl (NBDecl, mkNBDesc)

import Notebook.Core (AppndxSec(..), NBDesc, DocSection(..), 
  IntroSec(..), IntroSub(..), BodySec(..), BodySub(..), SmmrySec(..))

import Language.Drasil

import qualified Drasil.DocLang.Notebook as NB (appendix, body, reference, summary)
import qualified Drasil.NBSections.Introduction as Intro (introductionSection)
import qualified Drasil.NBSections.Body as Body (bodyIntro, reviewSec, motionSec, mthdAndanls)

-- | Creates a document from a document description and system information
mkDoc :: NBDecl -> (IdeaDict -> IdeaDict -> Sentence) -> SystemInformation -> Document
mkDoc dd comb si@SI {_sys = sys, _kind = kind, _authors = authors} =
  Document (nw kind `comb` nw sys) (foldlList Comma List $ map (S . name) authors) $
  mkSections (fillTraceMaps l (fillReqs l si)) l where
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
mkIntroSec si (IntroProg probIntro progDefn l) =
  Intro.introductionSection probIntro progDefn $ map (mkSubIntro si) l
  where
    mkSubIntro :: SystemInformation -> IntroSub -> Section
    mkSubIntro _ (IPurpose intro) = Intro.purposeOfDoc intro

-- | Helper for making the 'Body' section
mkBodySec :: BodySec -> Section
mkBodySec (BodyProg l) = NB.body [Body.bodyIntro] $ map mkSubs l
  where
    mkSubs :: BodySub -> Section
    mkSubs (Review cs )                 = Body.reviewSec cs 
    mkSubs (Motion cntnts subsec)       = Body.motionSec cntnts subsec
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

    