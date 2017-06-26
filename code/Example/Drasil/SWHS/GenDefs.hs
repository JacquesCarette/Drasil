module Drasil.SWHS.GenDefs where

import Prelude hiding (sin, cos, tan)

import Language.Drasil
import Data.Drasil.SentenceStructures (foldlSent)
import Data.Drasil.Quantities.PhysicalProperties as QPP
import Data.Drasil.Quantities.Thermodynamics as QT
import Data.Drasil.Quantities.Physics as QP
import Drasil.SWHS.Unitals

---------------------------
--  General Definitions  --
---------------------------

swhsGenDefs :: [RelationConcept]
swhsGenDefs = [nwtnCooling, rocTempSimp]

--
nwtnCooling :: RelationConcept
nwtnCooling = makeRC "nwtnCooling" (nounPhraseSP "Newton's law of cooling") 
  nwtnCooling_desc nwtnCooling_rel

nwtnCooling_rel :: Relation
nwtnCooling_rel = ((FCall (C thFluxVect) [C QP.time]) := (C htTransCoeff) :* 
  (FCall (C temp_diff) [C QP.time]))
--FIMXE:^ function of t?

nwtnCooling_desc :: Sentence
nwtnCooling_desc = foldlSent [S "Newton's law of cooling describes convective", 
  S "cooling from a surface. The law is stated as: the rate of heat loss from a", 
  S "body is proportional to the difference in temperatures between the body" +:+.
  S "and its surroundings", fixmeS]

--
rocTempSimp :: RelationConcept
rocTempSimp = makeRC "rocTempSimp" (nounPhraseSP "Simplified rate of change of temperature") 
  rocTempSimp_desc rocTempSimp_rel

rocTempSimp_rel :: Relation
rocTempSimp_rel = ((C QPP.mass):*(C QT.heat_cap_spec):*(Deriv Total (C QT.temp) (C QP.time)) := 
  (C ht_flux_in):*(C in_SA) :- (C ht_flux_out):*(C out_SA) :+ (C vol_ht_gen):*(C QPP.vol))

rocTempSimp_desc :: Sentence
rocTempSimp_desc = fixmeS

--

fixmeS :: Sentence
fixmeS = S "Add Description"