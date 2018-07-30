module Drasil.GlassBR.TMods (tModels, pbSafetyReq, lrSafetyReq, pbIsSafe, lrIsSafe) where

import Drasil.GlassBR.Unitals (demand, demandq, is_safePb, is_safeLR, lRe,
  pb_tol, prob_br)
import Drasil.GlassBR.IMods (calOfCap, calOfDe, probOfBr)
import Drasil.GlassBR.Concepts (lResistance)

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

{--}

tModels :: [RelationConcept]
tModels = [pbSafetyReq, lrSafetyReq]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the lrSafetyReq,
-- so basically we have to combine the old function with the new function

lrIsSafe :: TheoryModel
lrIsSafe = tm' (cw lrSafetyReq)
   (tc' "issafeLR" [qw is_safeLR, qw lRe, qw demand] ([] :: [ConceptChunk])
   [] [TCon Invariant $ (sy is_safeLR) $= (sy lRe) $> (sy demand)] []) "issafeLR"
   [lrSafeDescr]

lrSafetyReq :: RelationConcept
lrSafetyReq = makeRC "safetyReqLR" (nounPhraseSP "Safety Req-LR")
  lrSafeDescr ( (sy is_safeLR) $= (sy lRe) $> (sy demand))

lrSafeDescr :: Sentence
lrSafeDescr = tDescr (is_safeLR) s ending
  where 
    s = ((ch is_safePb) +:+ sParen (S "from" +:+ (ref pbSafetyReq)) `sAnd` (ch is_safeLR))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (ref calOfCap) +:+ (ch demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      ref calOfDe

pbIsSafe :: TheoryModel
pbIsSafe = tm' (cw pbSafetyReq) 
  (tc' "isSafe" [qw is_safePb, qw prob_br, qw pb_tol] ([] :: [ConceptChunk])
  [] [TCon Invariant $ (sy is_safePb) $= (sy prob_br) $< (sy pb_tol)] [])
  "isSafe" --shortname
  [pbSafeDescr]

pbSafetyReq :: RelationConcept
pbSafetyReq = makeRC "safetyReqPb" (nounPhraseSP "Safety Req-Pb")
  pbSafeDescr ((sy is_safePb) $= (sy prob_br) $< (sy pb_tol))

pbSafeDescr :: Sentence
pbSafeDescr = tDescr (is_safePb) s ending
  where 
    s = (ch is_safePb) `sAnd` (ch is_safeLR) +:+ sParen (S "from" +:+ 
      (ref lrSafetyReq))
    ending = ((ch prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (ref probOfBr) +:+ (ch pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"

tDescr :: VarChunk -> Sentence -> Sentence -> Sentence
tDescr main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]

ref :: RelationConcept -> Sentence
ref = makeRef . reldefn
