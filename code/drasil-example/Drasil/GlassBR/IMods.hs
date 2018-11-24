module Drasil.GlassBR.IMods (glassBRsymb, gbrIMods,
  calofCapacity, calofDemand) where

import Prelude hiding (exp)
import Control.Lens ((^.))
import Language.Drasil

import Drasil.GlassBR.Assumptions (standardValues, glassLite,
  boundaryConditions, responseType)
import Drasil.GlassBR.DataDefs (glaTyFac, nonFL, risk, standOffDis)
import Drasil.GlassBR.Labels (calOfCapacityL, calOfDemandL)
import Drasil.GlassBR.References (astm2009, beasonEtAl1998)
import Drasil.GlassBR.Unitals (capacity, char_weight, demand, 
  demandq, eqTNTWeight, lRe, loadSF, plate_len, plate_width, 
  probBreak, prob_br, risk_fun, standOffDist, wtntWithEqn)

import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd, sOr)

gbrIMods :: [InstanceModel]
gbrIMods = [calofCapacity, calofDemand]

glassBRsymb :: [DefinedQuantityDict]
glassBRsymb = map dqdWr [plate_len, plate_width, char_weight, standOffDist] ++ 
  [dqdQd (qw calofCapacity) capacity, dqdQd (qw calofDemand) demandq]


{--}

calofCapacity :: InstanceModel
calofCapacity = im' calofCapacity_RC [qw nonFL, qw glaTyFac, qw loadSF] 
  [sy nonFL $> 0, sy glaTyFac $> 0, sy loadSF $> 0] (qw lRe) [] [makeRef astm2009]
  calOfCapacityL [calofCapacityDesc]

calofCapacity_RC :: RelationConcept
calofCapacity_RC = makeRC "calofCapacity_RC" (nounPhraseSP "Calculation of Capacity") 
  calofCapacityDesc ( (sy lRe) $= ((sy nonFL) * (sy glaTyFac) * (sy loadSF))) -- calOfCapacityL

calofCapacityDesc :: Sentence
calofCapacityDesc =
  foldlSent [makeRef2S glassLite, makeRef2S glaTyFac, makeRef2S nonFL]

{--}

calofDemand :: InstanceModel
calofDemand = im' calofDemand_RC [qw demand, qw eqTNTWeight, qw standOffDist]
  [sy demand $> 0, sy eqTNTWeight $> 0, sy standOffDist $> 0] (qw demand) []
  [makeRef astm2009] calOfDemandL
  [calofDemandDesc]

calofDemand_RC :: RelationConcept
calofDemand_RC = makeRC "calofDemand_RC" (nounPhraseSP "Calculation of Demand") 
  calofDemandDesc ( (sy demand) $= apply2 demand eqTNTWeight standOffDist) -- calOfDemandL
  --calofDemandDesc $ (C demand) $= FCall (asExpr interpY) [V "TSD.txt", sy standOffDist, sy eqTNTWeight] 
  
calofDemandDesc :: Sentence
calofDemandDesc = 
  foldlSent [(ch demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (ch standOffDist) `sAnd`
  (ch eqTNTWeight), S "as" +:+. plural parameter, 
  (ch eqTNTWeight), S "is defined as" +:+.
  E (wtntWithEqn^.equat), (ch standOffDist) `isThe`
  (phrase standOffDist), S "as defined in", makeRef2S standOffDis]
