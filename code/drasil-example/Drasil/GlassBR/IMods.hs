module Drasil.GlassBR.IMods (glassBRsymb, gbrIMods, calofDemandi) where

import Prelude hiding (exp)
import Drasil.GlassBR.Labels (calOfDemandL)

----------------------------------------------
import qualified Data.Map as Map
import Language.Drasil
import Language.Drasil.Code (relToQD) -- FIXME, this should not be needed
import Language.Drasil.Development (UnitDefn) -- FIXME

import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

import Drasil.GlassBR.Concepts (lResistance)
import Drasil.GlassBR.DataDefs (probOfBreak, calofCapacity, calofDemand)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (demand, demandq, is_safePb, is_safeLR, lRe, pb_tol, prob_br,char_weight, demand, 
demandq, eqTNTWeight, plate_len, plate_width, 
standOffDist, wtntWithEqn)



gbrIMods :: [InstanceModel]
gbrIMods = [calofDemandi]

glassBRsymb :: [DefinedQuantityDict]
glassBRsymb = map dqdWr [plate_len, plate_width, char_weight, standOffDist] ++ 
  [dqdQd (qw calofDemand) demandq]

 glass_concept :: ConceptInstanceMap
glass_concept = Map.fromList $ map (\x -> (x ^. uid, x)) ([] :: [ConceptInstance])



  ----Vajiheh------
lrIsSafe :: InstanceModel
lrIsSafe = im' (cw lrIsSafe_RC)
   [qw is_safeLR, qw lRe, qw demand] ([])
   [relToQD locSymbMap lrIsSafe_RC] [(sy is_safeLR) $= (sy lRe) $> (sy demand)] [] [makeCite astm2009] 
   "isSafeLR" [lrIsSafeDesc]
  

lrIsSafe_RC :: RelationConcept
lrIsSafe_RC = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
  lrIsSafeDesc ( (sy is_safeLR) $= (sy lRe) $> (sy demand)) 

lrIsSafeDesc :: Sentence
lrIsSafeDesc = foldlSent [(ch is_safePb) +:+ sParen (S "from" +:+ (makeRef2S pbIsSafe)) `sAnd` (ch is_safeLR))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (makeRef2S calofCapacity) +:+ (ch demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      makeRef2S calofDemand]
------------------------------------------------
pbIsSafe :: InstanceModel
pbIsSafe = im' (cw pbIsSafe_RC) 
  [qw is_safePb, qw prob_br, qw pb_tol] ([])
  [] [(sy is_safePb) $= (sy prob_br) $< (sy pb_tol)] [] [makeCite astm2009]
  "isSafePb" [pbIsSafeDesc]

pbIsSafe_RC :: RelationConcept
pbIsSafe_RC = makeRC "safetyReqPb" (nounPhraseSP "Safety Req-Pb")
  pbIsSafeDesc ((sy is_safePb) $= (sy prob_br) $< (sy pb_tol)) -- l2

pbIsSafeDesc :: Sentence
pbIsSafeDesc = foldlSent[(ch is_safePb) `sAnd` (ch is_safeLR) +:+ sParen (S "from" +:+
      (makeRef2S lrIsSafe))
    ending = ((ch prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (makeRef2S probOfBreak) +:+ (ch pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"]

