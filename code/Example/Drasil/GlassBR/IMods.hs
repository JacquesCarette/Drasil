module Drasil.GlassBR.IMods where

import Language.Drasil
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Prelude hiding (id)
import Control.Lens ((^.))
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.DataDefs
import Drasil.GlassBR.Concepts
import Data.Drasil.Concepts.Documentation

iModels :: [RelationConcept]
iModels = [probOfBr, calOfCap, calOfDe]

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" (nounPhraseSP "Probability of Glass Breakage") --make into an NPNC type? (along with calOfCap, calOfDe)
  pbdescr pb_rel 

pb_rel :: Relation
pb_rel = (C prob_br) := 1 - (V "e") :^ (Neg (V "B"))

pbdescr :: Sentence
pbdescr =
  foldlSent [(P $ prob_br ^. symbol) `isThe` (S "calculated" +:+. (phrase prob_br)),
  (P $ risk_fun ^. symbol) `isThe` (phrase risk)]

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr cap_rel

cap_rel :: Relation
cap_rel = (C lRe) := ((C nonFL):*(C glaTyFac):*(C loadSF)) 

capdescr :: Sentence
capdescr =
  foldlSent [(short lResistance) `isThe` (phrase lResistance) `sC`
  S "which" +:+. S "is also called capacity" +:+. ((P $ nonFL ^. symbol) `isThe`
  (phrase nonFL)) +:+. ((short glassTypeFac) `isThe` (phrase glassTypeFac))
  +:+. ((short lShareFac) `isThe` (phrase lShareFac)), S "Follows"
  +:+ (short assumption) :+: S "2 and", (short assumption) :+: S "1 (" :+:
  Quote (S "In development of this procedure, it was assumed that" +:+
  S "all four edges of the glass are simply supported and free to slip in the" +:+
  S "plane of the glass. This boundary condition has been shown to be typical" +:+
  S "of many glass installations)") +:+ S "from [4 (pg. 53)]"]

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" (nounPhraseSP "Calculation of Demand(q)") 
  dedescr de_rel

de_rel :: Relation
de_rel = (C demand) := FCall (C demand) [C eqTNTWeight, C standOffDist] 

dedescr :: Sentence
dedescr = 
  foldlSent [(P $ demand ^. symbol), S "or", (phrase demandq) `sC`
  S "is the", (demandq ^. defn), S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), S "(" :+: (P $ standOffDist ^. symbol) :+: S ") and", 
  (P $ eqTNTWeight ^. symbol) +:+. S "as parameters", 
  (P $ eqTNTWeight ^. symbol), S "is defined as", (P $ eqTNTWeight ^. symbol),
  S "=", (P $ char_weight ^. symbol) +:+. S "* TNT" +:+. ((P $ char_weight ^. symbol) `isThe`
  (phrase char_weight)) +:+. ((P $ tNT ^. symbol) `isThe`
  (phrase tNT)), (P $ standOffDist ^.symbol) `isThe`
  (phrase standOffDist), S "where", (P $ standOffDist ^. symbol), S "= "]
  --equation in sentence
