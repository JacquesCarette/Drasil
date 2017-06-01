module Drasil.GlassBR.TMods where

import Drasil.GlassBR.Unitals
import Drasil.GlassBR.IMods
import Drasil.GlassBR.Concepts

import Language.Drasil
import Data.Drasil.Utils (foldlSent, isThe, isThe')
import Prelude hiding (id)
import Control.Lens ((^.))

tModels :: [RelationConcept]
tModels = [t1SafetyReq, t2SafetyReq]

t1SafetyReq :: RelationConcept
t1SafetyReq = makeRC "t1SafetyReq" (nounPhraseSP "Safety Requirement-1")
  t1descr safety_require1_rel

safety_require1_rel :: Relation
safety_require1_rel = (C is_safe1) := (C prob_br) :< (C pb_tol)

--relation within relation
t1descr :: Sentence
t1descr = 
  foldlSent [S "If", (P $ is_safe1 ^. symbol), S "= True, the glass is" +:+. 
  S "considered safe", (P $ is_safe1 ^.symbol), S "and", (P $ is_safe2 ^. symbol),
  S "(from", (makeRef ((Definition (symbolMap glassBRSymbols) . Theory) t2SafetyReq))
  :+: S ") are either" +:+. S "both True or both False",
  ((P $ prob_br ^. symbol) `isThe` (phrase $ prob_br ^. term)) 
  `sC` S "as calculated in" +:+.
  (makeRef ((Definition (symbolMap glassBRSymbols) . Theory) probOfBr)),
  (P $ pb_tol ^. symbol) `isThe` (phrase $ pb_tol ^. term),
  S "entered by the user"]

t2SafetyReq :: RelationConcept
t2SafetyReq = makeRC "t2SafetyReq" (nounPhraseSP "Safety Requirement-2")
  t2descr safety_require2_rel

safety_require2_rel :: Relation
safety_require2_rel = (C is_safe2) := (C lRe) :> (C demand)

--relation within relation
t2descr :: Sentence
t2descr = 
  foldlSent [S "If", (P $ is_safe2 ^. symbol), S "= True, the glass is" +:+.
  S "considered safe", (P $ is_safe1 ^. symbol), S "(from", 
  (makeRef ((Definition (symbolMap glassBRSymbols) . Theory) t1SafetyReq)),
  S "and", (P $ is_safe2 ^. symbol) +:+. S "are either both True or both False",
  (short lResistance) `isThe` (phrase $ lResistance ^. term), 
  S "(also called capacity, as defined in" +:+. 
  (makeRef ((Definition (symbolMap glassBRSymbols) . Theory) calOfCap)), 
  (P $ demand ^. symbol), S "(also referred as the", (titleize $ demandq ^. term) :+:
  S ") is the", (demandq ^. defn) `sC` S "as defined in", (makeRef ((Definition (symbolMap glassBRSymbols) . Theory) calOfDe))]
