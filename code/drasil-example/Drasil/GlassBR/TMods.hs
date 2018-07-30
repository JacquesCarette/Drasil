module Drasil.GlassBR.TMods (gbrTMods, tModels, pbSafetyReq, lrSafetyReq, pbIsSafe, lrIsSafe) where

import Drasil.GlassBR.Unitals (demand, demandq, is_safe1, is_safe2, lRe,
  pb_tol, prob_br)
import Drasil.GlassBR.IMods (calofCapacity, calofDemand, probOfBreak)
import Drasil.GlassBR.Concepts (lResistance)

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

{--}

--FIXME: should be removed with closure of #891
tModels :: [RelationConcept]
tModels = [pbSafetyReq, lrSafetyReq]

gbrTMods :: [TheoryModel]
gbrTMods = [pbIsSafe, lrIsSafe]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the pbSafetyReq,
-- so basically we have to combine the old function with the new function

pbIsSafe :: TheoryModel
pbIsSafe = tm' (cw pbSafetyReq) 
  (tc' "isSafe" [qw is_safe1, qw prob_br, qw pb_tol] ([] :: [ConceptChunk])
  [] [TCon Invariant $ (sy is_safe1) $= (sy prob_br) $< (sy pb_tol)] [])
  (mkLabelRA'' "isSafe") [pbSafeDescr]

pbSafetyReq :: RelationConcept
pbSafetyReq = makeRC "safetyReqPb" (nounPhraseSP "Safety Req-Pb")
  pbSafeDescr ((sy is_safe1) $= (sy prob_br) $< (sy pb_tol))
  Nothing--label

pbSafeDescr :: Sentence
pbSafeDescr = tDescr (is_safe1) s ending
  where 
    s = (ch is_safe1) `sAnd` (ch is_safe2) +:+ sParen (S "from" +:+
      (ref lrSafetyReq))
    ending = ((ch prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (makeRef probOfBreak) +:+ (ch pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"

lrIsSafe :: TheoryModel
lrIsSafe = tm' (cw lrSafetyReq)
   (tc' "isSafe2" [qw is_safe2, qw lRe, qw demand] ([] :: [ConceptChunk])
   [] [TCon Invariant $ (sy is_safe2) $= (sy lRe) $> (sy demand)] []) 
   (mkLabelRA'' "isSafe2") [lrSafeDescr]

lrSafetyReq :: RelationConcept
lrSafetyReq = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
  lrSafeDescr ( (sy is_safe2) $= (sy lRe) $> (sy demand))
  Nothing--label

lrSafeDescr :: Sentence
lrSafeDescr = tDescr (is_safe2) s ending
  where 
    s = ((ch is_safe1) +:+ sParen (S "from" +:+ (ref pbSafetyReq)) `sAnd` (ch is_safe2))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (makeRef calofCapacity) +:+ (ch demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      makeRef calofDemand

tDescr :: VarChunk -> Sentence -> Sentence -> Sentence
tDescr main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]

ref :: RelationConcept -> Sentence
ref = makeRef . reldefn
