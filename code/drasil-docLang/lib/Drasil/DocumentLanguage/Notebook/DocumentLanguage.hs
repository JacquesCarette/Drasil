-- | Document language for lesson plan notebooks.
module Drasil.DocumentLanguage.Notebook.DocumentLanguage(mkNb) where

import Drasil.DocumentLanguage.Notebook.NBDecl (NBDecl, mkNBDesc)
import Drasil.DocumentLanguage.Notebook.Core (ApndxSec(..), NBDesc, DocSection(..), 
  IntrodSec(..), InPurposeSub(..), BodySec(..), ReviewSub(..), MainIdeaSub(..),
  MethsAnlsSub(..), ExampleSub(..), SmmrySec(..))

import Language.Drasil

import Utils.Drasil

import Database.Drasil(SystemInformation(SI), _authors, _kind, _sys, citeDB)

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
    doit (IntrodSec is)      = mkIntroSec is
    doit (InPurposeSub ips)  = mkInPurpSub ips
    doit (BodySec bs)        = mkBodySec bs
    doit (ReviewSub rs)      = mkReviewSub rs
    doit (MainIdeaSub mis)   = mkMainIdeaSub mis
    doit (MethsAnlsSub mas)  = mkMethAnlSub mas   
    doit (ExampleSub es)     = mkExpSub es  
    doit (SmmrySec ss)       = mkSmmrySec ss
    doit BibSec              = mkBib (citeDB si)
    doit (ApndxSec a)        = mkAppndxSec a

-- Add more intro subsections
-- | Helper for making the 'Introduction' section.
mkIntroSec :: IntrodSec -> Section
mkIntroSec (IntrodProg probIntro) = Intro.introductionSection probIntro

mkInPurpSub :: InPurposeSub -> Section
mkInPurpSub (InPurposeProg intro) = Intro.purposeOfDoc intro

-- | Helper for making the 'Body' section.
--mkBodySec :: BodySec -> Section
--mkBodySec (BodyProg l) = NB.body [] $ map mkSubs l

-- | Helper for making the 'Body' section
mkBodySec :: BodySec -> Section
mkBodySec (BodyProg bodyIntro) = NB.body 0 bodyIntro

mkReviewSub :: ReviewSub -> Section
mkReviewSub (ReviewProg cntnts) = Body.reviewSec cntnts   

mkMainIdeaSub :: MainIdeaSub -> Section
mkMainIdeaSub (MainIdeaProg cntnts) = Body.mainIdeaSec cntnts

mkMethAnlSub :: MethsAnlsSub -> Section
mkMethAnlSub (MethsAnlsProg cntnts) = Body.mthdAndanls cntnts

mkExpSub :: ExampleSub -> Section
mkExpSub (ExampleProg cntnts) = Body.exampleSec cntnts


-- | Helper for making the 'Summary' section.
mkSmmrySec :: SmmrySec -> Section
mkSmmrySec (SmmryProg cs) = NB.summary 0 cs

-- | Helper for making the 'Bibliography' section.
mkBib :: BibRef -> Section
mkBib bib = NB.reference 0 [UlC $ ulcc (Bib bib)]

-- | Helper for making the 'Appendix' section.
mkAppndxSec :: ApndxSec -> Section
mkAppndxSec (ApndxProg cs) = NB.appendix 0 cs 

    