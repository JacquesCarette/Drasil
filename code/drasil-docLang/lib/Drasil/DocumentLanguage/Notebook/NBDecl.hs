-- | Lesson plan notebook section declaration types and functions.
module Drasil.DocumentLanguage.Notebook.NBDecl where

import qualified Drasil.DocumentLanguage.Notebook.Core as NB (ApndxSec(..), NBDesc, DocSection(..), 
  IntrodSec(..), InPurposeSub(..), BodySec(..), SmmrySec(..),
  ReviewSub(..), MainIdeaSub(..), MethsAnlsSub(..), ExampleSub(..))

import Database.Drasil (SystemInformation)

-- * Types

-- | A Lesson Plan notebook declaration is made up of all necessary sections ('NbSection's).
type NBDecl  = [NbSection]

-- | Contains all the different sections needed for a notebook lesson plan ('NBDecl').
data NbSection = IntrodSec NB.IntrodSec
                | InPurposeSub NB.InPurposeSub
                | BodySec NB.BodySec
                | ReviewSub NB.ReviewSub
                | MainIdeaSub NB.MainIdeaSub
                | MethsAnlsSub NB.MethsAnlsSub
                | ExampleSub NB.ExampleSub
                | SmmrySec NB.SmmrySec
                | BibSec
                | ApndxSec NB.ApndxSec

-- * Functions

-- | Creates the notebook description (translates 'NBDecl' into a more usable form for generating documents).
mkNBDesc :: SystemInformation -> NBDecl -> NB.NBDesc
mkNBDesc _ = map sec where
  sec :: NbSection -> NB.DocSection
  sec (IntrodSec i)     = NB.IntrodSec i
  sec (InPurposeSub ip) = NB.InPurposeSub ip
  sec (BodySec bs)      = NB.BodySec bs  
  sec (ReviewSub r)     = NB.ReviewSub r
  sec (MainIdeaSub mi)  = NB.MainIdeaSub mi
  sec (MethsAnlsSub ma) = NB.MethsAnlsSub ma 
  sec (ExampleSub e)    = NB.ExampleSub e  
  sec (SmmrySec ss)     = NB.SmmrySec ss
  sec BibSec            = NB.BibSec
  sec (ApndxSec a)      = NB.ApndxSec a