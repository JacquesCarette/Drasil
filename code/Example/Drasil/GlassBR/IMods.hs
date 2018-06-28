module Drasil.GlassBR.IMods (iModels, probOfBr, calOfCap, calOfDe, probOfBreak, calofCapacity, calofDemand) where

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
import Data.Drasil.Utils (getES)

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
  foldlSent [(getES prob_br) `isThe` (S "calculated" +:+. (phrase prob_br)),
  (getES risk_fun) `isThe` (phrase risk)]

{--}

calofCapacity :: InstanceModel
calofCapacity = im' calOfCap [qw nonFL, qw glaTyFac, qw loadSF] [TCon AssumedCon $ sy nonFL $> 0,
  TCon AssumedCon $ sy glaTyFac $> 0, TCon AssumedCon $ sy loadSF $> 0] (qw lRe) [] [] [capdescr]

{--}

calOfCap :: RelationConcept
calOfCap = makeRC "calOfCap" (nounPhraseSP "Calculation of Capacity(LR)") 
  capdescr ( (sy lRe) $= ((sy nonFL) * (sy glaTyFac) * (sy loadSF)))

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

calofDemand :: InstanceModel
calofDemand = im' calOfDe [qw demand, qw eqTNTWeight, qw standOffDist] [TCon AssumedCon $ sy demand $> 0,
  TCon AssumedCon $ sy eqTNTWeight $> 0, TCon AssumedCon $ sy standOffDist $> 0] (qw demand) [] [] [dedescr]

{--}

calOfDe :: RelationConcept
calOfDe = makeRC "calOfDe" (nounPhraseSP "Calculation of Demand(q)") 
  dedescr ( (sy demand) $= apply2 demand eqTNTWeight standOffDist)
  --dedescr $ (C demand) $= FCall (asExpr interpY) [V "TSD.txt", sy standOffDist, sy eqTNTWeight] 
  
dedescr :: Sentence
dedescr = 
  foldlSent [(getES demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (getES standOffDist) `sAnd`
  (getES eqTNTWeight), S "as" +:+. plural parameter, 
  (getES eqTNTWeight), S "is defined as" +:+.
  E (wtntWithEqn^.equat) +:+. (getES standOffDist) `isThe`
  (phrase standOffDist), S "where", E (sdWithEqn^.equat), S "where",
  sParen (sdVectorSent), S "are", plural coordinate]
