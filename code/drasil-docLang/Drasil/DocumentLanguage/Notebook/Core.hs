{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage.Notebook.Core where

import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate))
import Language.Drasil

type NBDesc = [DocSection]

data DocSection = IntrodSec IntrodSec
                | BodySec BodySec
                | SmmrySec SmmrySec
                | BibSec
                | ApndxSec ApndxSec

-- TODO: Work on detail structure of notebooks

{--}

-- | Introduction section. Contents are top level followed by a list of subsections.
data IntrodSec = IntrodProg [Contents] [IntrodSub]

-- | Introduction subsections
data IntrodSub where
  InPurpose :: [Sentence] -> IntrodSub -- TODO: maybe change to [Contents]  

{--}

newtype BodySec = BodyProg [BodySub]

data BodySub where
  Review       :: [Contents] -> BodySub
  MainIdea     :: [Contents] -> [Section] -> BodySub
  MethsAndAnls :: [Contents] -> [Section] -> BodySub
  Example      :: [Contents] -> [Section] -> BodySub
  
{--}

newtype SmmrySec = SmmryProg [Contents]

{--}

newtype ApndxSec = ApndxProg [Contents]

{--}

data DLPlate f = DLPlate {
  docSec :: DocSection -> f DocSection,
  introdSec :: IntrodSec -> f IntrodSec,
  introdSub :: IntrodSub -> f IntrodSub,
  bodySec :: BodySec -> f BodySec,
  bodySub :: BodySub -> f BodySub,
  smmrySec ::SmmrySec -> f SmmrySec,
  apendSec :: ApndxSec -> f ApndxSec
}

instance Multiplate DLPlate where
  multiplate p = DLPlate ds intro intro' body body' smry aps where
    ds (IntrodSec x) = IntrodSec <$> introdSec p x
    ds (BodySec x) = BodySec <$> bodySec p x
    ds (SmmrySec x) = SmmrySec <$> smmrySec p x
    ds (ApndxSec x) = ApndxSec <$> apendSec p x
    ds BibSec = pure BibSec

    intro (IntrodProg c progs) = IntrodProg c <$>
      traverse (introdSub p) progs
    intro' (InPurpose s) = pure $ InPurpose s
    body (BodyProg progs) = BodyProg <$> traverse (bodySub p) progs
    body' (Review c) = pure $ Review c
    body' (MainIdea c s) = pure $ MainIdea c s
    body' (MethsAndAnls c s) = pure $ MethsAndAnls c s
    body' (Example c s) = pure $ Example c s
    smry (SmmryProg con) = pure $ SmmryProg con 
    aps (ApndxProg con) = pure $ ApndxProg con
  mkPlate b = DLPlate (b docSec) (b introdSec) (b introdSub) (b bodySec)
    (b bodySub) (b smmrySec) (b apendSec)