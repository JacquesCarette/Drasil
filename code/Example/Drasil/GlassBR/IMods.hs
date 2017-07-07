module Drasil.GlassBR.IMods where

import Language.Drasil
import Data.Drasil.SentenceStructures (foldlSent, isThe)
import Prelude hiding (id, exp)
import Control.Lens ((^.))
import Drasil.GlassBR.Unitals
import Drasil.GlassBR.DataDefs
import Drasil.GlassBR.Concepts
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Utils (getS)

iModels :: [RelationConcept]
iModels = [probOfBr, calOfCap, calOfDe]
--make probOfBr into an NPNC type? (along with calOfCap, calOfDe)

{--}

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" (nounPhraseSP "Probability of Glass Breakage")
  pbdescr pb_rel 

pb_rel :: Relation
pb_rel = (C prob_br) := 1 - (exp (Neg (C risk)))

pbdescr :: Sentence
pbdescr =
  foldlSent [(getS prob_br) `isThe` (S "calculated" +:+. (phrase prob_br)),
  (getS risk_fun) `isThe` (phrase risk)]

{--}

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr cap_rel

cap_rel :: Relation
cap_rel = (C lRe) := ((C nonFL)*(C glaTyFac)*(C loadSF)) 

capdescr :: Sentence
capdescr =
  foldlSent [(short lResistance) `isThe` (phrase lResistance) `sC`
  S "which" +:+. S "is also called capacity" +:+. ((getS nonFL) `isThe`
  (phrase nonFL)) +:+. ((short glassTypeFac) `isThe` (phrase glassTypeFac))
  +:+. ((short lShareFac) `isThe` (phrase lShareFac)), S "Follows",
  (acroA 2), S "and", (acroA 1), sParen (Quote 
  (S "In development of this procedure, it was assumed that" +:+
  S "all four edges of the glass are simply supported and free to slip" +:+
  S "in the plane of the glass. This boundary condition has been shown" +:+
  S "to be typical of many glass installations")) +:+ S "from [4 (pg. 53)]"]

{--}

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" (nounPhraseSP "Calculation of Demand(q)") 
  dedescr de_rel

de_rel :: Relation
de_rel = (C demand) := FCall (C demand) [C eqTNTWeight, C standOffDist] 

dedescr :: Sentence
dedescr = 
  foldlSent [(getS demand), S "or", (phrase demandq) `sC`
  S "is the", (demandq ^. defn), S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (getS standOffDist), S "and", 
  (getS eqTNTWeight) +:+. S "as parameters", 
  (getS eqTNTWeight), S "is defined as" +:+.
  E (equat wtntWithEqn) +:+. ((getS char_weight) `isThe`
  (phrase char_weight)) +:+. ((getS tNT) `isThe`
  (phrase tNT)), (getS standOffDist) `isThe`
  (phrase standOffDist), S "where", E (equat sdWithEqn), S "where",
  sParen (sdVectorSent), S "are coordinates"]

{--}