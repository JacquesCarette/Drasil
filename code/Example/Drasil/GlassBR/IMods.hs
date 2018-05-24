module Drasil.GlassBR.IMods (iModels, probOfBr, calOfCap, calOfDe, probOfBreak) where

import Language.Drasil

import Drasil.GlassBR.Assumptions 
import Drasil.DocumentLanguage.RefHelpers

import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd, sOr)
import Data.Drasil.Utils (getES)
import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.Concepts.Documentation (coordinate, assumption)

import Prelude hiding (exp)
import Control.Lens ((^.))

import Drasil.GlassBR.Unitals (tNT, sdWithEqn, demand, standOffDist, 
  char_weight, eqTNTWeight, demandq, sdVectorSent, wtntWithEqn, loadSF,
  lRe, risk_fun, prob_br)
import Drasil.GlassBR.DataDefs (nonFL, risk, glaTyFac)
import Drasil.GlassBR.Concepts (lResistance, glassTypeFac, lShareFac)

iModels :: [RelationConcept]
iModels = [probOfBr, calOfCap, calOfDe]

{--}

probOfBreak :: InstanceModel
probOfBreak = im probOfBr [qw risk] 
  [TCon AssumedCon $ sy risk $> 0] (qw prob_br) [TCon AssumedCon $ sy prob_br $> 0] [] []

{--}

probOfBr :: RelationConcept
probOfBr = makeRC "probOfBr" (nounPhraseSP "Probability of Glass Breakage")
  pbdescr ( (sy prob_br) $= 1 - (exp (negate (sy risk)))) []

pbdescr :: Sentence
pbdescr =
  foldlSent [(getES prob_br) `isThe` (S "calculated" +:+. (phrase prob_br)),
  (getES risk_fun) `isThe` (phrase risk)]

{--}

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr ( (sy lRe) $= ((sy nonFL) * (sy glaTyFac) * (sy loadSF))) []

capdescr :: Sentence
capdescr =
  foldlSent [(getES lRe) `isThe` (phrase lResistance) `sC`
  S "which" +:+. S "is also called capacity" +:+. ((getES nonFL) `isThe`
  (phrase nonFL)) +:+. ((getES glaTyFac) `isThe` (phrase glassTypeFac))
  +:+. ((getES loadSF) `isThe` (phrase lShareFac)), S "Follows",
  (refA gbRefDB newA2) `sAnd` (refA gbRefDB newA1), sParen (Quote 
  (S "In the development of this procedure, it was assumed that" +:+
  S "all four edges of the glass are simply supported and free to slip" +:+
  S "in the plane of the glass. This boundary condition has been shown" +:+
  S "to be typical of many glass installations")) +:+ S "from [4 (pg. 53)]"
  {-astm_LR2009-}]

{--}

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" (nounPhraseSP "Calculation of Demand(q)") 
  dedescr ( (sy demand) $= apply2 demand eqTNTWeight standOffDist) []
  --dedescr $ (C demand) $= FCall (asExpr interpY) [V "TSD.txt", sy standOffDist, sy eqTNTWeight] 
  
dedescr :: Sentence
dedescr = 
  foldlSent [(getES demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (getES standOffDist) `sAnd`
  (getES eqTNTWeight), S "as" +:+. plural parameter, 
  (getES eqTNTWeight), S "is defined as" +:+.
  E (wtntWithEqn^.equat) +:+. ((getES char_weight) `isThe`
  (phrase char_weight)) +:+. ((getES tNT) `isThe`
  (phrase tNT)), (getES standOffDist) `isThe`
  (phrase standOffDist), S "where", E (sdWithEqn^.equat), S "where",
  sParen (sdVectorSent), S "are", plural coordinate]
