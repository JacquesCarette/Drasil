module Drasil.DocumentLanguage.Notebook.DocumentLanguage where

import Drasil.DocDecl (NBDecl, mkNBDesc)

import Notebook.Core (AppndxSec(..), NBDesc, DocSection(..), 
  IntroSec(..), IntroSub(..), BodySec(..), BodySub(..), SmmrySec(..))

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
