module Drasil.GlassBR.IMods (symb, iMods, pbIsSafe, lrIsSafe, instModIntro) where

import Prelude hiding (exp)
import Language.Drasil
import Theory.Drasil (InstanceModel, imNoDeriv, qwC, ModelKinds (OthModel))
import Utils.Drasil
import qualified Utils.Drasil.Sentence as S

import Drasil.GlassBR.DataDefs (probOfBreak, calofCapacity, calofDemand,
  pbTolUsr, qRef)
import Drasil.GlassBR.Goals (willBreakGS)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (charWeight, demand, demandq, isSafeLR, isSafePb,
  lRe, pbTol, plateLen, plateWidth, probBr, standOffDist)

import Data.Drasil.Concepts.Documentation (goal)

iMods :: [InstanceModel]
iMods = [pbIsSafe, lrIsSafe]

symb :: [DefinedQuantityDict]
symb = map dqdWr [plateLen, plateWidth, charWeight, standOffDist] ++ 
  [dqdQd (qw calofDemand) demandq]

{--}

pbIsSafe :: InstanceModel
pbIsSafe = imNoDeriv (OthModel pbIsSafeRC)
  [qwC probBr $ UpFrom (Exc, dbl 0), qwC pbTol $ UpFrom (Exc, dbl 0)]
  (qw isSafePb) []
  [makeCite astm2009] "isSafePb"
  [pbIsSafeDesc, probBRRef, pbTolUsr]


pbIsSafeRC :: RelationConcept
pbIsSafeRC = makeRC "safetyReqPb" (nounPhraseSP "Safety Req-Pb")
  EmptyS (sy isSafePb $= sy probBr $< sy pbTol)

{--}

lrIsSafe :: InstanceModel
lrIsSafe = imNoDeriv (OthModel lrIsSafeRC) 
  [qwC lRe $ UpFrom (Exc, dbl 0), qwC demand $ UpFrom (Exc, dbl 0)]
  (qw isSafeLR) []
  [makeCite astm2009] "isSafeLR"
  [lrIsSafeDesc, capRef, qRef] 

lrIsSafeRC :: RelationConcept
lrIsSafeRC = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
  EmptyS (sy isSafeLR $= sy lRe $> sy demand)
  
iModDesc :: QuantityDict -> Sentence -> Sentence
iModDesc main s = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
    S "considered safe", s `S.sAre` S "either both True or both False"]
  
-- Intro --

instModIntro :: Sentence
instModIntro = foldlSent [S "The", phrase goal, makeRef2S willBreakGS, 
  S "is met by", makeRef2S pbIsSafe `sC` makeRef2S lrIsSafe]

-- Notes --

capRef :: Sentence
capRef = definedIn' calofCapacity (S "and is also called capacity")

lrIsSafeDesc :: Sentence
lrIsSafeDesc = iModDesc isSafeLR
  (ch isSafePb +:+ fromSource pbIsSafe `S.sAnd` ch isSafeLR)

pbIsSafeDesc :: Sentence
pbIsSafeDesc = iModDesc isSafePb
  (ch isSafePb `S.sAnd` ch isSafePb +:+ fromSource lrIsSafe)

probBRRef :: Sentence
probBRRef = definedIn probOfBreak
