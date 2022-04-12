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
                | SupportSS1 SupportSS1
                | SupportSS2 SupportSS2
                | SupportSS3 SupportSS3
                | MethsAnlsSub MethsAnlsSub
                | ExampleSub ExampleSub
                | SmmrySec SmmrySec
                | BibSec
                | ApndxSec ApndxSec

-- *TODO: Work on detail structure of notebooks

{--}

-- | Introduction section. Contents are top level followed by a list of subsections.
newtype IntrodSec = IntrodProg [Contents] 

-- | Introduction subsections
-- *TODO: maybe change to [Contents]
newtype InPurposeSub = InPurposeProg [Sentence]   

{--}

newtype BodySec = BodyProg [Contents]

newtype ReviewSub    =  ReviewProg [Contents] 
newtype MainIdeaSub  =  MainIdeaProg [Contents] 

newtype SupportSS1  =  SupportSS1Prog [Contents]
newtype SupportSS2  =  SupportSS2Prog [Contents]
newtype SupportSS3  =  SupportSS3Prog [Contents]

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
  supportSS1 :: SupportSS1 -> f SupportSS1,
  supportSS2 :: SupportSS2 -> f SupportSS2,
  supportSS3 :: SupportSS3 -> f SupportSS3,
  methsAnlsSub :: MethsAnlsSub -> f MethsAnlsSub,
  exampleSub :: ExampleSub -> f ExampleSub,
  smmrySec ::SmmrySec -> f SmmrySec,
  apendSec :: ApndxSec -> f ApndxSec
}

instance Multiplate DLPlate where
  multiplate p = DLPlate ds intro inpurp body review mainid suportss1 suportss2 suportss3 
    methanl exmp smry aps where
    ds (IntrodSec x) = IntrodSec <$> introdSec p x
    ds (InPurposeSub x) = InPurposeSub <$> inpurposeSub p x
    ds (BodySec x) = BodySec <$> bodySec p x
    ds (ReviewSub x) = ReviewSub <$> reviewSub p x
    ds (MainIdeaSub x) = MainIdeaSub <$> mainIdeaSub p x
    ds (SupportSS1 x) = SupportSS1 <$> supportSS1 p x
    ds (SupportSS2 x) = SupportSS2 <$> supportSS2 p x
    ds (SupportSS3 x) = SupportSS3 <$> supportSS3 p x
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
    suportss1 (SupportSS1Prog c) = pure $ SupportSS1Prog c 
    suportss2 (SupportSS2Prog c) = pure $ SupportSS2Prog c
    suportss3 (SupportSS3Prog c) = pure $ SupportSS3Prog c
    methanl (MethsAnlsProg c) = pure $ MethsAnlsProg c
    exmp (ExampleProg c) = pure $ ExampleProg c
    smry (SmmryProg c) = pure $ SmmryProg c 
    aps (ApndxProg c) = pure $ ApndxProg c
  mkPlate b = DLPlate (b docSec) (b introdSec) (b inpurposeSub) (b bodySec)
    (b reviewSub) (b mainIdeaSub) (b supportSS1) (b supportSS2) (b supportSS3) 
    (b methsAnlsSub) (b exampleSub) (b smmrySec) (b apendSec)