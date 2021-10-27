{-# LANGUAGE GADTs #-}
module Drasil.DocumentLanguage.Notebook.Core where

import Data.Generics.Multiplate (Multiplate(multiplate, mkPlate))
import Language.Drasil

type NBDesc = [DocSection]

data DocSection = IntrodSec IntrodSec
                | InPurposeSub InPurposeSub
                | BodySec BodySec
                | ReviewSub ReviewSub
                | MainIdeaSub MainIdeaSub
                | MethsAnlsSub MethsAnlsSub
                | ExampleSub ExampleSub
                | SmmrySec SmmrySec
                | BibSec
                | ApndxSec ApndxSec

-- **TODO: Work on detail structure of notebooks

{--}

-- | Introduction section. Contents are top level followed by a list of subsections.
newtype IntrodSec = IntrodProg [Contents] 

-- | Introduction subsections
newtype InPurposeSub = InPurposeProg [Sentence] -- **maybe change to [Contents]  

{--}

newtype BodySec = BodyProg [Contents]

newtype ReviewSub    =  ReviewProg [Contents] 
newtype MainIdeaSub  =  MainIdeaProg [Contents] 
newtype MethsAnlsSub =  MethsAnlsProg [Contents] 
newtype ExampleSub   =  ExampleProg [Contents] 
  
{--}

newtype SmmrySec = SmmryProg [Contents]

{--}

newtype ApndxSec = ApndxProg [Contents]

{--}

data DLPlate f = DLPlate {
  docSec :: DocSection -> f DocSection,
  introdSec :: IntrodSec -> f IntrodSec,
  inpurposeSub :: InPurposeSub -> f InPurposeSub,
  bodySec :: BodySec -> f BodySec,
  reviewSub :: ReviewSub -> f ReviewSub,
  mainIdeaSub :: MainIdeaSub -> f MainIdeaSub,
  methsAnlsSub :: MethsAnlsSub -> f MethsAnlsSub,
  exampleSub :: ExampleSub -> f ExampleSub,
  smmrySec ::SmmrySec -> f SmmrySec,
  apendSec :: ApndxSec -> f ApndxSec
}

instance Multiplate DLPlate where
  multiplate p = DLPlate ds intro inpurp body review mainid methanl exmp smry aps where
    ds (IntrodSec x) = IntrodSec <$> introdSec p x
    ds (InPurposeSub x) = InPurposeSub <$> inpurposeSub p x
    ds (BodySec x) = BodySec <$> bodySec p x
    ds (ReviewSub x) = ReviewSub <$> reviewSub p x
    ds (MainIdeaSub x) = MainIdeaSub <$> mainIdeaSub p x
    ds (MethsAnlsSub x) = MethsAnlsSub <$> methsAnlsSub p x
    ds (ExampleSub x) = ExampleSub <$> exampleSub p x
    ds (SmmrySec x) = SmmrySec <$> smmrySec p x
    ds (ApndxSec x) = ApndxSec <$> apendSec p x
    ds BibSec = pure BibSec

    intro (IntrodProg c) = pure $ IntrodProg c 
    inpurp (InPurposeProg s) = pure $ InPurposeProg s
    body (BodyProg c) = pure $ BodyProg c 
    review (ReviewProg c) = pure $ ReviewProg c
    mainid (MainIdeaProg c) = pure $ MainIdeaProg c
    methanl (MethsAnlsProg c) = pure $ MethsAnlsProg c
    exmp (ExampleProg c) = pure $ ExampleProg c
    smry (SmmryProg c) = pure $ SmmryProg c 
    aps (ApndxProg c) = pure $ ApndxProg c
  mkPlate b = DLPlate (b docSec) (b introdSec) (b inpurposeSub) (b bodySec)
    (b reviewSub) (b mainIdeaSub) (b methsAnlsSub) (b exampleSub) (b smmrySec) (b apendSec)