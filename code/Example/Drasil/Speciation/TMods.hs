module Drasil.Speciation.TMods where

{-
import Drasil.GlassBR.Unitals (is_safe1, is_safe2, demand, 
  demandq, lRe, pb_tol, prob_br)
import Drasil.GlassBR.IMods (calOfCap, calOfDe, probOfBr)
import Drasil.GlassBR.Concepts (lResistance)
-}

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)
import Data.Drasil.Utils (getS)

{--}
{-
tModels :: [RelationConcept]
tModels = [t1SafetyReq, t2SafetyReq]

t1SafetyReq :: RelationConcept
t1SafetyReq = makeRC "t1SafetyReq" (nounPhraseSP "Safety Requirement-1")
  t1descr $ (C is_safe1) := (C prob_br) :< (C pb_tol)

t1descr :: Sentence
t1descr = tDescr (is_safe1) s ending
  where 
    s = (getS is_safe1) `sAnd` (getS is_safe2) +:+ sParen (S "from" +:+ 
      (ref t2SafetyReq))
    ending = ((getS prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (ref probOfBr) +:+ (getS pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"

t2SafetyReq :: RelationConcept
t2SafetyReq = makeRC "t2SafetyReq" (nounPhraseSP "Safety Requirement-2")
  t2descr $ (C is_safe2) := (C lRe) :> (C demand)

t2descr :: Sentence
t2descr = tDescr (is_safe2) s ending
  where 
    s = ((getS is_safe1) +:+ sParen (S "from" +:+ (makeRef ((Definition . Theory) 
        t1SafetyReq))) `sAnd` (getS is_safe2))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (ref calOfCap) +:+ (getS demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      ref calOfDe

tDescr :: VarChunk -> Sentence -> Sentence -> Sentence
tDescr main s ending = foldlSent [S "If", getS main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]

ref :: RelationConcept -> Sentence
ref = makeRef . (Definition . Theory)
-}