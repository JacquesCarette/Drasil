module Drasil.DocumentLanguage.Notebook.NBDecl where

import qualified Drasil.DocumentLanguage.Notebook.Core as NB (ApndxSec(..), NBDesc, DocSection(..), 
  IntrodSec(..), BodySec(..), SmmrySec(..))

import Database.Drasil (SystemInformation)

type NBDecl  = [NbSection]

data NbSection = IntrodSec NB.IntrodSec
                | BodySec NB.BodySec
                | SmmrySec NB.SmmrySec
                | BibSec
                | ApndxSec NB.ApndxSec

mkNBDesc :: SystemInformation -> NBDecl -> NB.NBDesc
mkNBDesc _ = map sec where
  sec :: NbSection -> NB.DocSection
  sec (IntrodSec i) = NB.IntrodSec i
  sec (BodySec bs)  = NB.BodySec bs  
  sec (SmmrySec ss) = NB.SmmrySec ss
  sec BibSec        = NB.BibSec
  sec (ApndxSec a)  = NB.ApndxSec a