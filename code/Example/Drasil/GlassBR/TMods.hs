module Drasil.GlassBR.TMods (tModels, t1SafetyReq, t2SafetyReq,t1IsSafe) where

import Drasil.GlassBR.Unitals (is_safe1, is_safe2, demand, 
  demandq, lRe, pb_tol, prob_br)
import Drasil.GlassBR.IMods (calOfCap, calOfDe, probOfBr)
import Drasil.GlassBR.Concepts (lResistance)

import Language.Drasil
import Control.Lens ((^.))

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd)
import Data.Drasil.Utils (getES)

{--}

tModels :: [RelationConcept]
tModels = [t1SafetyReq, t2SafetyReq]

-- FIXME: This is a hack to see if TheoryModel printing will work. This chunk
-- needs to be updated properly.
-- this is the new function but it still uses the t1SafetyReq,
-- so basiclly we have to combine the old function with the new function


{-t1IsSafe :: TheoryModel
t1IsSafe = tm//all the changes should include this, means theoritical model constructor// (cw//means the constraints wrapper constructor// t1SafetyReq//this is the name of the old chunk, we have to always includes this//) 
  (tc'//tc' we are using now instead of the td// "isSafe" [qw is_safe1, qw prob_br, qw pb_tol// include all the quantifiers here] ([] :: [CWrapper])
  []//maybe some quatifiers [TCon Invariant $ (C is_safe1) $= (C prob_br) $< (C pb_tol)] []//maybe the new function, but just in case we have to use it)-}

-- in the body file , we have to modify to include more the chunks meaning the import
--grep rn "keyword" folder_name

t1IsSafe :: TheoryModel
t1IsSafe = tm (cw t1SafetyReq) 
  (tc' "isSafe" [qw is_safe1, qw prob_br, qw pb_tol] ([] :: [FundUnit])
  [] [TCon Invariant $ (sy is_safe1) $= (sy prob_br) $< (sy pb_tol)] [])

t1SafetyReq :: RelationConcept
t1SafetyReq = makeRC "t1SafetyReq" (nounPhraseSP "Safety Requirement-1")
  t1descr $ (sy is_safe1) $= (sy prob_br) $< (sy pb_tol)

t1descr :: Sentence
t1descr = tDescr (is_safe1) s ending
  where 
    s = (getES is_safe1) `sAnd` (getES is_safe2) +:+ sParen (S "from" +:+ 
      (ref t2SafetyReq))
    ending = ((getES prob_br) `isThe` (phrase prob_br)) `sC` S "as calculated in" +:+.
      (ref probOfBr) +:+ (getES pb_tol) `isThe` (phrase pb_tol) +:+ S "entered by the user"

t2SafetyReq :: RelationConcept
t2SafetyReq = makeRC "t2SafetyReq" (nounPhraseSP "Safety Requirement-2")
  t2descr $ (sy is_safe2) $= (sy lRe) $> (sy demand)

t2descr :: Sentence
t2descr = tDescr (is_safe2) s ending
  where 
    s = ((getES is_safe1) +:+ sParen (S "from" +:+ (makeRef ((Definition . Theory) 
        t1SafetyReq))) `sAnd` (getES is_safe2))
    ending = (short lResistance) `isThe` (phrase lResistance) +:+ 
      sParen (S "also called capacity") `sC` S "as defined in" +:+. 
      (ref calOfCap) +:+ (getES demand) +:+ sParen (S "also referred as the" +:+ 
      (titleize demandq)) `isThe` (demandq ^. defn) `sC` S "as defined in" +:+ 
      ref calOfDe

tDescr :: VarChunk -> Sentence -> Sentence -> Sentence
tDescr main s ending = foldlSent [S "If", getES main `sC` S "the glass is" +:+.
  S "considered safe", s +:+. S "are either both True or both False", ending]

ref :: RelationConcept -> Sentence
ref = makeRef . (Definition . Theory)
