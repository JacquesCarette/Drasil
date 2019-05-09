module Drasil.GlassBR.TMods (gbrTMods, pbIsSafe, lrIsSafe) where

import Language.Drasil
import Language.Drasil.Code (relToQD) -- FIXME, this should not be needed

import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

import Drasil.GlassBR.Concepts (lResistance)
import Drasil.GlassBR.DataDefs (probOfBreak, calofCapacity, calofDemand)
import Drasil.GlassBR.IMods (glassBRsymb)
import Drasil.GlassBR.References (astm2009)
--import Drasil.GlassBR.Unitals (demand, demandq, is_safePb, is_safeProb, is_safeLR, is_safeLoad, lRe, pb_tol, pb_tolfail, prob_br, prob_fail)
import Drasil.GlassBR.Unitals (tm_demand, demandq, is_safeProb, is_safeLoad, tm_lRe, pb_tolfail, prob_fail)
import Drasil.GlassBR.Symbols (this_symbols)

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
   [qw is_safeLoad, qw tm_lRe, qw tm_demand] ([] :: [ConceptChunk])
   [relToQD locSymbMap lrIsSafe_RC] [(sy is_safeLoad) $= (sy tm_lRe) $> (sy tm_demand)] [] [makeCite astm2009] 
   "isSafeLoad" [lrIsSafeDesc]
   where locSymbMap = cdb (this_symbols) ([] :: [IdeaDict]) glassBRsymb
                          ([] :: [UnitDefn]) Map.empty Map.empty [] [] [] [] []
                           [] []

lrIsSafe_RC :: RelationConcept
lrIsSafe_RC = makeRC "safetyLoad" (nounPhraseSP "Safety Load")
  lrIsSafeDesc ( (sy is_safeLoad) $= (sy tm_lRe) $> (sy tm_demand))

lrIsSafeDesc :: Sentence
lrIsSafeDesc = tModDesc (is_safeLoad) s ending
  where 
    s = ((ch is_safeProb) +:+ sParen (S "from" +:+ (makeRef2S pbIsSafe)) `sAnd` (ch is_safeLoad))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (makeRef2S calofCapacity) +:+ (ch tm_demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      makeRef2S calofDemand

pbIsSafe :: TheoryModel
pbIsSafe = tm (cw pbIsSafe_RC) 
  [qw is_safeProb, qw prob_fail, qw pb_tolfail] ([] :: [ConceptChunk])
  [relToQD locSymbMap pbIsSafe_RC] [(sy is_safeProb) $= (sy prob_fail) $< (sy pb_tolfail)] [] [makeCite astm2009]
  "isSafeProb" [pbIsSafeDesc]
  where locSymbMap = cdb (this_symbols) ([] :: [IdeaDict]) glassBRsymb
                          ([] :: [UnitDefn]) Map.empty Map.empty [] [] [] [] []
                          [] []

pbIsSafe_RC :: RelationConcept
pbIsSafe_RC = makeRC "safetyProbability" (nounPhraseSP "Safety Probability")
  pbIsSafeDesc ((sy is_safeProb) $= (sy prob_fail) $< (sy pb_tolfail))

pbIsSafeDesc :: Sentence
pbIsSafeDesc = tModDesc (is_safeProb) s ending
  where 
    s = (ch is_safeProb) `sAnd` (ch is_safeLoad) +:+ sParen (S "from" +:+
      (makeRef2S lrIsSafe))
    ending = ((ch prob_fail) `isThe` (phrase prob_fail)) `sC` S "as calculated in" +:+.
      (makeRef2S probOfBreak) +:+ (ch pb_tolfail) `isThe` (phrase pb_tolfail) +:+ S "entered by the user"

tModDesc :: QuantityDict -> Sentence -> Sentence -> Sentence
tModDesc main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]
