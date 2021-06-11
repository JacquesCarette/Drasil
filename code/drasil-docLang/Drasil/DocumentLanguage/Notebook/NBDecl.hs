module Drasil.DocumentLanguage.Notebook.NBDecl where

import qualified Drasil.DocumentLanguage.Notebook.Core as NB (AppndxSec(..), NBDesc, DocSection(..), 
  IntroSec(..), IntroSub(..), BodySec(..), BodySub(..), SmmrySec(..))

import Database.Drasil (SystemInformation(SI), _inputs, _sysinfodb)

type NBDecl  = [DocSection]

data DocSection = IntroSec NB.IntroSec
                | BodySec NB.BodySec
                | SmmrySec NB.SmmrySec
                | Bibliography
                | AppndxSec NB.AppndxSec

mkNBDesc :: SystemInformation -> NBDecl -> NB.NBDesc
mkNBDesc SI{_inputs = is, _sysinfodb = db} = map sec where
  sec :: DocSection -> NB.DocSection
  sec (IntroSec i) = NB.IntroSec i
  sec (BodySec bs) = NB.BodySec bs
  sec Bibliography = NB.Bibliography
  sec (SmmrySec ss) = NB.SmmrySec ss
  sec (AppndxSec a) = NB.AppndxSec a