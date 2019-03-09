module Drasil.GlassBR.TMods (gbrTMods, pbIsSafe, lrIsSafe) where

import Language.Drasil
import Language.Drasil.Code (relToQD) -- FIXME, this should not be needed

import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

import Drasil.GlassBR.Concepts (lResistance)
import Drasil.GlassBR.DataDefs (probOfBreak, calofCapacity, calofDemand)
import Drasil.GlassBR.IMods (glassBRsymb)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (demand, demandq, is_safePb, is_safeLR, lRe, pb_tol, prob_br)

import qualified Data.Map as Map

{--}

gbrTMods :: [TheoryModel]
gbrTMods = [pbIsSafe, lrIsSafe]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the lrIsSafe_RC,
-- so basically we have to combine the old function with the new function
-- glass_concept :: [ConceptInstance]
-- glass_concept = []


lrIsSafe :: TheoryModel
lrIsSafe = tm (cw lrIsSafe_RC)
   [qw is_safeLR, qw lRe, qw demand] ([] :: [ConceptChunk])
   [relToQD locSymbMap lrIsSafe_RC] [(sy is_safeLR) $= (sy lRe) $> (sy demand)] [] [makeCite astm2009] 
   "isSafeLR" [lrIsSafeDesc]
   where locSymbMap = cdb ([] :: [QuantityDict]) ([] :: [IdeaDict]) glassBRsymb
                          ([] :: [UnitDefn]) Map.empty Map.empty [] [] [] [] []
                           [] []

lrIsSafe_RC :: RelationConcept
lrIsSafe_RC = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
  lrIsSafeDesc ( (sy is_safeLR) $= (sy lRe) $> (sy demand))

lrIsSafeDesc :: Sentence
lrIsSafeDesc = tModDesc (is_safeLR) s ending
  where 
    s = ((ch is_safePb) +:+ sParen (S "from" +:+ (makeRef2S pbIsSafe)) `sAnd` (ch is_safeLR))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (makeRef2S calofCapacity) +:+ (ch demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      makeRef2S calofDemand

pbIsSafe :: TheoryModel
pbIsSafe = tm (cw pbIsSafe_RC) 
  [qw is_safePb, qw prob_br, qw pb_tol] ([] :: [ConceptChunk])
  [] [(sy is_safePb) $= (sy prob_br) $< (sy pb_tol)] [] [makeCite astm2009]
  "isSafePb" [pbIsSafeDesc]

pbIsSafe_RC :: RelationConcept
pbIsSafe_RC = makeRC "safetyReqPb" (nounPhraseSP "Safety Req-Pb")
  pbIsSafeDesc ((sy is_safePb) $= (sy prob_br) $< (sy pb_tol))

pbIsSafeDesc :: Sentence
pbIsSafeDesc = tModDesc (is_safePb) s ending
  where 
    s = (ch is_safePb) `sAnd` (ch is_safeLR) +:+ sParen (S "from" +:+
      (makeRef2S lrIsSafe))
    ending = ((ch prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (makeRef2S probOfBreak) +:+ (ch pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"

tModDesc :: QuantityDict -> Sentence -> Sentence -> Sentence
tModDesc main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]
