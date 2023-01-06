-- | Lesson plan notebook section declaration types and functions.
module Drasil.DocumentLanguage.Notebook.NBDecl where

import qualified Drasil.DocumentLanguage.Notebook.Core as NB (ApndxSec(..), NBDesc, DocSection(..), 
  IntrodSec(..), LearnObj(..), BodySec(..), SmmrySec(..))

import SysInfo.Drasil (SystemInformation)

-- * Types

-- | A Lesson Plan notebook declaration is made up of all necessary sections ('NbSection's).
type NBDecl  = [NbSection]

-- | Contains all the different sections needed for a notebook lesson plan ('NBDecl').
data NbSection = IntrodSec NB.IntrodSec
                | LearnObj NB.LearnObj
                | BodySec NB.BodySec
                | SmmrySec NB.SmmrySec
                | BibSec
                | ApndxSec NB.ApndxSec

-- * Functions

-- | Creates the notebook description (translates 'NBDecl' into a more usable form for generating documents).
mkNBDesc :: SystemInformation -> NBDecl -> NB.NBDesc
mkNBDesc _ = map sec where
  sec :: NbSection -> NB.DocSection
  sec (IntrodSec i) = NB.IntrodSec i
  sec (LearnObj lo) = NB.LearnObj lo
  sec (BodySec bs)  = NB.BodySec bs  
  sec (SmmrySec ss) = NB.SmmrySec ss
  sec BibSec        = NB.BibSec
  sec (ApndxSec a)  = NB.ApndxSec a