module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Control.Lens ((^.))
import Drasil.DocLang (ModelDB, mdb)
import Theory.Drasil (DataDefinition, dd, mkQuantDef)

import Drasil.SWHS.Assumptions (assumpCWTAT, assumpTPCAV, assumpLCCCW,
  assumpTHCCoT, assumpTHCCoL, assumpLCCWP)
import Drasil.SWHS.References (bueche1986, koothoor2013, lightstone2012)
import Drasil.SWHS.Unitals (melt_frac, latentE_P, htFusion, pcm_mass,
  temp_W, temp_PCM, ht_flux_P, pcm_HTC, coil_HTC, temp_C, ht_flux_C, htCap_S_P,
  htCap_L_P, pcm_HTC, pcm_SA, tau_S_P, tau_L_P)

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latentHeat)

refMDB :: ModelDB
refMDB = mdb [] [] dataDefs []

qDefs :: [QDefinition]
qDefs = [dd1HtFluxCQD, dd2HtFluxPQD, ddBalanceSolidPCMQD,
  ddBalanceLiquidPCMQD, dd3HtFusionQD, dd4MeltFracQD]

dataDefs :: [DataDefinition] 
dataDefs = [dd1HtFluxC, dd2HtFluxP, ddBalanceSolidPCM,
  ddBalanceLiquidPCM, dd3HtFusion, dd4MeltFrac]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxCQD :: QDefinition
dd1HtFluxCQD = mkQuantDef ht_flux_C htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = (sy coil_HTC) * ((sy temp_C) - apply1 temp_W time)

dd1HtFluxC :: DataDefinition
dd1HtFluxC = dd dd1HtFluxCQD [makeCite koothoor2013] [] "ht_flux_C"
  [makeRef2S assumpLCCCW, makeRef2S assumpTHCCoT, makeRef2S assumpTHCCoL]

--Can't include info in description beyond definition of variables?
----

dd2HtFluxPQD :: QDefinition
dd2HtFluxPQD = mkQuantDef ht_flux_P htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = (sy pcm_HTC) * (apply1 temp_W time - apply1 temp_PCM time)

dd2HtFluxP :: DataDefinition
dd2HtFluxP = dd dd2HtFluxPQD [makeCite koothoor2013] [] "ht_flux_P"
  [makeRef2S assumpCWTAT, makeRef2S assumpTPCAV, makeRef2S assumpLCCWP]

----

ddBalanceSolidPCMQD :: QDefinition
ddBalanceSolidPCMQD = mkQuantDef tau_S_P balanceSolidPCMEqn

balanceSolidPCMEqn :: Expr
balanceSolidPCMEqn = ((sy pcm_mass) * (sy htCap_S_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))

ddBalanceSolidPCM :: DataDefinition
ddBalanceSolidPCM = dd ddBalanceSolidPCMQD [makeCite lightstone2012] []
  "balanceSolidPCM" []

----

ddBalanceLiquidPCMQD :: QDefinition
ddBalanceLiquidPCMQD = mkQuantDef tau_L_P balanceLiquidPCMEqn

balanceLiquidPCMEqn :: Expr
balanceLiquidPCMEqn = ((sy pcm_mass) * (sy htCap_L_P)) /
  ((sy pcm_HTC) * (sy pcm_SA))

ddBalanceLiquidPCM :: DataDefinition
ddBalanceLiquidPCM = dd ddBalanceLiquidPCMQD [makeCite lightstone2012] []
  "balanceLiquidPCM" []

----

dd3HtFusionQD :: QDefinition
dd3HtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = (sy latentHeat) / (sy mass)

-- FIXME: need to allow page references in references.
dd3HtFusion :: DataDefinition
dd3HtFusion = dd dd3HtFusionQD [makeCiteInfo bueche1986 $ Page [282]]
  [] "htFusion" []

----

dd4MeltFracQD :: QDefinition
dd4MeltFracQD = fromEqn' (melt_frac ^. uid) -- FIXME Should (^. id) be used
  (melt_frac ^. term) (S "fraction of the PCM that is liquid")
  (eqSymb melt_frac) melt_frac_eqn 

--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

melt_frac_eqn :: Expr
melt_frac_eqn = (sy latentE_P) / ((sy htFusion) * (sy pcm_mass))

dd4MeltFrac :: DataDefinition
dd4MeltFrac = dd dd4MeltFracQD [makeCite koothoor2013] [] "melt_frac"
 [makeRef2S dd3HtFusion]

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
