module Drasil.GlassBR.IMods (calOfCap, calOfDe, iModels, probOfBr, probOfBreak) where

import Language.Drasil

import Drasil.GlassBR.Assumptions (gbRefDB, newA1, newA2)
import Drasil.GlassBR.Concepts (glassTypeFac, lResistance, lShareFac)
import Drasil.GlassBR.DataDefs (glaTyFac, nonFL, risk)
import Drasil.GlassBR.Unitals (char_weight, demand, demandq, eqTNTWeight, lRe, 
  loadSF, prob_br, risk_fun, sdVectorSent, sdWithEqn, standOffDist, tNT,
  wtntWithEqn)

import Drasil.DocumentLanguage.RefHelpers (refA)

import Data.Drasil.Concepts.Documentation (coordinate)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd, sOr)

import Prelude hiding (exp)
import Control.Lens ((^.))

iModels :: [RelationConcept]
iModels = [probOfBr, calOfCap, calOfDe]

{--}

probOfBreak :: InstanceModel
probOfBreak = im probOfBr [qw risk] 
  [TCon AssumedCon $ sy risk $> 0] (qw prob_br) [TCon AssumedCon $ sy prob_br $> 0]
  "probOfBrIM" --shortname

{--}

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" (nounPhraseSP "Probability of Glass Breakage")
  pbdescr ( (sy prob_br) $= 1 - (exp (negate (sy risk)))) 

pbdescr :: Sentence
pbdescr =
  foldlSent [(ch prob_br) `isThe` (S "calculated" +:+. (phrase prob_br)),
  (ch risk_fun) `isThe` (phrase risk)]

{--}

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr ( (sy lRe) $= ((sy nonFL) * (sy glaTyFac) * (sy loadSF)))

capdescr :: Sentence
capdescr =
  foldlSent [(ch lRe) `isThe` (phrase lResistance) `sC`
  S "which" +:+. S "is also called capacity" +:+. ((ch nonFL) `isThe`
  (phrase nonFL)) +:+. ((ch glaTyFac) `isThe` (phrase glassTypeFac))
  +:+. ((ch loadSF) `isThe` (phrase lShareFac)), S "Follows",
  (refA gbRefDB newA2) `sAnd` (refA gbRefDB newA1), sParen (Quote 
  (S "In the development of this procedure, it was assumed that" +:+
  S "all four edges of the glass are simply supported and free to slip" +:+
  S "in the plane of the glass. This boundary condition has been shown" +:+
  S "to be typical of many glass installations")) +:+ S "from [4 (pg. 53)]"
  {-astm_LR2009-}]

{--}

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" (nounPhraseSP "Calculation of Demand(q)") 
  dedescr ( (sy demand) $= apply2 demand eqTNTWeight standOffDist)
  --dedescr $ (C demand) $= FCall (asExpr interpY) [V "TSD.txt", sy standOffDist, sy eqTNTWeight] 
  
dedescr :: Sentence
dedescr = 
  foldlSent [(ch demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (ch standOffDist) `sAnd`
  (ch eqTNTWeight), S "as" +:+. plural parameter, 
  (ch eqTNTWeight), S "is defined as" +:+.
  E (wtntWithEqn^.equat) +:+. ((ch char_weight) `isThe`
  (phrase char_weight)) +:+. ((ch tNT) `isThe`
  (phrase tNT)), (ch standOffDist) `isThe`
  (phrase standOffDist), S "where", E (sdWithEqn^.equat), S "where",
  sParen (sdVectorSent), S "are", plural coordinate]
