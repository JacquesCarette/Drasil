module Drasil.GlassBR.IMods (symb, iMods, pbIsSafe, lrIsSafe, instModIntro) where

import Prelude hiding (exp)
import Control.Lens ((^.))
import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDeriv)
import Drasil.GlassBR.DataDefs (probOfBreak, calofCapacity, calofDemand)
import Drasil.GlassBR.Concepts (lResistance)
import Drasil.GlassBR.Goals (willBreakGS)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (charWeight, demand, 
  demandq, plateLen, plateWidth, 
  standOffDist, isSafePb, isSafeLR, lRe, pbTol, probBr)
import Data.Drasil.Concepts.Documentation (goal)
import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

iMods :: [InstanceModel]
iMods = [pbIsSafe, lrIsSafe]

symb :: [DefinedQuantityDict]
symb = map dqdWr [plateLen, plateWidth, charWeight, standOffDist] ++ 
  [dqdQd (qw calofDemand) demandq]


{--}

pbIsSafe :: InstanceModel
pbIsSafe = imNoDeriv pbIsSafeRC [qw probBr, qw pbTol]
  [sy probBr $> 0, sy pbTol $> 0] (qw isSafePb) []
  [makeCite astm2009] "isSafePb"
  [pbIsSafeDesc]


pbIsSafeRC :: RelationConcept
pbIsSafeRC = makeRC "safetyReqPb" (nounPhraseSP "Safety Req-Pb")
  pbIsSafeDesc ((sy isSafePb) $= (sy probBr) $< (sy pbTol))


pbIsSafeDesc :: Sentence
pbIsSafeDesc = iModDesc (isSafePb) s ending
    where 
      s = (ch isSafePb) `sAnd` (ch isSafePb) +:+ sParen (S "from" +:+
        (makeRef2S lrIsSafe))
      ending = ((ch probBr) `isThe` (phrase probBr)) `sC` S "as calculated in" +:+.
        (makeRef2S probOfBreak) +:+ (ch pbTol) `isThe` (phrase pbTol) +:+ S "entered by the user"

lrIsSafe :: InstanceModel
lrIsSafe = imNoDeriv lrIsSafeRC [qw isSafeLR, qw lRe, qw demand]
  [sy lRe $> 0, sy demand $> 0] (qw isSafeLR) []
  [makeCite astm2009] "isSafeLR"
  [lrIsSafeDesc] 

lrIsSafeRC :: RelationConcept
lrIsSafeRC = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
    lrIsSafeDesc ( (sy isSafeLR) $= (sy lRe) $> (sy demand))

lrIsSafeDesc :: Sentence
lrIsSafeDesc = iModDesc (isSafeLR) s ending
      where 
        s = ((ch isSafePb) +:+ sParen (S "from" +:+ (makeRef2S pbIsSafe)) `sAnd` (ch isSafeLR))
        ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
          sParen (S "also called capacity") `sC` S "as defined in" +:+. 
          (makeRef2S calofCapacity) +:+ (ch demand) +:+ sParen (S "also referred as the" +:+ 
          (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
          makeRef2S calofDemand
  
iModDesc :: QuantityDict -> Sentence -> Sentence -> Sentence
iModDesc main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
    S "considered safe", s +:+. S "are either both True or both False", ending]
  
-- Intro --

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S willBreakGS, 
  S "is met by", makeRef2S pbIsSafe `sC` makeRef2S lrIsSafe]