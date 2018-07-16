module Drasil.GlassBR.TMods (tModels, gbrTMods,
  t1SafetyReq, t2SafetyReq,t1IsSafe,t2IsSafe) where

import Drasil.GlassBR.Unitals (demand, demandq, is_safe1, is_safe2, lRe,
  pb_tol, prob_br)
import Drasil.GlassBR.IMods (calofCapacity, calofDemand, probOfBreak)
import Drasil.GlassBR.Concepts (lResistance)

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)

{--}

tModels :: [RelationConcept]
tModels = [t1SafetyReq, t2SafetyReq]

gbrTMods :: [TheoryModel]
gbrTMods = [t1IsSafe, t2IsSafe]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the t1SafetyReq,
-- so basiclly we have to combine the old function with the new function

t1IsSafe :: TheoryModel
t1IsSafe = tm' (cw t1SafetyReq) 
  (tc' "isSafe" [qw is_safe1, qw prob_br, qw pb_tol] ([] :: [ConceptChunk])
  [] [TCon Invariant $ (sy is_safe1) $= (sy prob_br) $< (sy pb_tol)] [])
  (mkLabelRA'' "isSafeLabel") [t1descr]

t1SafetyReq :: RelationConcept
t1SafetyReq = makeRC "t1SafetyReq" (nounPhraseSP "Safety Requirement-1")
  t1descr ((sy is_safe1) $= (sy prob_br) $< (sy pb_tol)) Nothing--label

t1descr :: Sentence
t1descr = tDescr (is_safe1) s ending
  where 
    s = (ch is_safe1) `sAnd` (ch is_safe2) +:+ sParen (S "from" +:+ 
      (makeRef t2IsSafe))
    ending = ((ch prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (makeRef probOfBreak) +:+ (ch pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"


t2IsSafe :: TheoryModel
t2IsSafe = tm' (cw t2SafetyReq)
   (tc' "isSafe2" [qw is_safe2, qw lRe, qw demand] ([] :: [ConceptChunk])
   [] [TCon Invariant $ (sy is_safe2) $= (sy lRe) $> (sy demand)] []) 
   (mkLabelRA'' "isSafe2Label") [t2descr]

t2SafetyReq :: RelationConcept
t2SafetyReq = makeRC "t2SafetyReq" (nounPhraseSP "Safety Requirement-2")
  t2descr ( (sy is_safe2) $= (sy lRe) $> (sy demand)) Nothing--label

t2descr :: Sentence
t2descr = tDescr (is_safe2) s ending
  where 
    s = ((ch is_safe1) +:+ sParen (S "from" +:+ (makeRef t1IsSafe)))
     `sAnd` (ch is_safe2)
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (makeRef calofCapacity) +:+ (ch demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      makeRef calofDemand

tDescr :: VarChunk -> Sentence -> Sentence -> Sentence
tDescr main s ending = foldlSent [S "If", ch main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]