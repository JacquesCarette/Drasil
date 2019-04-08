module Drasil.GlassBR.IMods (glassBRsymb, gbrIMods, calofDemandi) where

import Prelude hiding (exp)
import Control.Lens ((^.))
import Language.Drasil
import Language.Drasil.Code (asExpr')

import Drasil.GlassBR.DataDefs (standOffDis, eqTNTWDD, calofDemand)
import Drasil.GlassBR.References (astm2009)
import Drasil.GlassBR.Unitals (char_weight, demand, 
  demandq, eqTNTWeight, plate_len, plate_width, 
  standOffDist)
import Drasil.GlassBR.ModuleDefs (interpY)

import Data.Drasil.Concepts.Math (parameter)
import Data.Drasil.SentenceStructures (foldlSent, isThe, sAnd, sOr)

gbrIMods :: [InstanceModel]
gbrIMods = [calofDemandi]

glassBRsymb :: [DefinedQuantityDict]
glassBRsymb = map dqdWr [plate_len, plate_width, char_weight, standOffDist] ++ 
  [dqdQd (qw calofDemand) demandq]


{--}

calofDemandi :: InstanceModel
calofDemandi = im' calofDemand_RCi [qw demand, qw eqTNTWeight, qw standOffDist]
  [sy demand $> 0, sy eqTNTWeight $> 0, sy standOffDist $> 0] (qw demand) []
  [astm2009] "calOfDemand"
  [calofDemandDesc]

calofDemand_RCi :: RelationConcept
calofDemand_RCi = makeRC "calofDemand_RC" (nounPhraseSP "Calculation of Demand") 
  --calofDemandDesc ( (sy demand) $= apply2 demand eqTNTWeight standOffDist)
  calofDemandDesc $ (sy demand) $= apply (asExpr' interpY) [Str "TSD.txt", sy standOffDist, sy eqTNTWeight] 
  
calofDemandDesc :: Sentence
calofDemandDesc = 
  foldlSent [(ch demand `sOr` phrase demandq) `sC`
  S "is the", (demandq ^. defn), 
  S "obtained from Figure 2 by interpolation using", --use MakeRef? Issue #216
  (phrase standOffDist), sParen (ch standOffDist) `sAnd`
  (ch eqTNTWeight), S "as" +:+. plural parameter, 
  (ch eqTNTWeight), S "is defined in" +:+.
  makeRef2S eqTNTWDD, (ch standOffDist) `isThe`
  (phrase standOffDist), S "as defined in", makeRef2S standOffDis]
