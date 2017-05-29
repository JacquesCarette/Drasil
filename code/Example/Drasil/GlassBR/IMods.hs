module Drasil.GlassBR.IMods where

import Language.Drasil
import Data.Drasil.Utils (foldlSent)
import Prelude hiding (id)
import Control.Lens ((^.))
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.DataDefs
import Drasil.GlassBR.Concepts
import Data.Drasil.Concepts.Documentation
--import Drasil.GlassBR.Units

iModels :: [RelationConcept]
iModels = [probOfBr, calOfCap, calOfDe]

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" (nounPhraseSP "Probability of Glass Breakage") --make into an NPNC type? (along with calOfCap, calOfDe)
  pbdescr pb_rel 

pb_rel :: Relation
pb_rel = (C prob_br) := 1 - (V "e") :^ (Neg (V "B"))

pbdescr :: Sentence
pbdescr =
  foldlSent [(P $ prob_br ^. symbol), S "is the calculated" +:+. (phrase $ prob_br ^. term),
  (P $ risk_fun ^. symbol), S "is the", (phrase $ risk ^. term)]

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr cap_rel

cap_rel :: Relation
cap_rel = (C lRe) := ((C nonFL):*(C glaTyFac):*(C loadSF)) 

capdescr :: Sentence
capdescr =
  foldlSent [(short lResistance), S "is the", (phrase $ lResistance ^. term) `sC`
  S "which" +:+. S "is also called capacity", (P $ nonFL ^. symbol),
  S "is the" +:+. (phrase $ nonFL ^. term), (short glassTypeFac),
  S "is the" +:+. (phrase $ glassTypeFac ^. term), (short lShareFac),
  S "is the" +:+. (phrase $ lShareFac ^. term), S "Follows" +:+ (short assumption) :+: S "2 and",
  (short assumption) :+: S "1 (" :+:
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
  foldlSent [(P $ demand ^. symbol), S "or", (phrase $ demandq ^. term) `sC`
  S "is the", (demandq ^. defn), S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase $ standOffDist ^. term), S "(" :+: (P $ standOffDist ^. symbol) :+: S ") and", 
  (P $ eqTNTWeight ^. symbol) +:+. S "as parameters", 
  (P $ eqTNTWeight ^. symbol), S "is defined as", (P $ eqTNTWeight ^. symbol),
  S "=", (P $ char_weight ^. symbol) +:+. S "* TNT", (P $ char_weight ^. symbol),
  S "is the" +:+. (phrase $ char_weight ^. term), (P $ tNT ^. symbol), S "is the" +:+.
  (phrase $ tNT ^. term), (P $ standOffDist ^.symbol), S "is the",
  (phrase $ standOffDist ^. term), S "where", (P $ standOffDist ^. symbol), S "= "]
  --equation in sentence
