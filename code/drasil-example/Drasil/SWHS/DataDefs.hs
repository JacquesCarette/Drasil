module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Drasil.DocLang (ModelDB, mdb)
import Theory.Drasil (DataDefinition, dd, mkQuantDef)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (value)

import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latentHeat)

import Drasil.SWHS.Assumptions (assumpLCCCW, assumpTHCCoT)
import Drasil.SWHS.References (bueche1986, koothoor2013, lightstone2012)
import Drasil.SWHS.Unitals (meltFrac, latentEP, htFusion, pcmMass,
  tempW, tempPCM, htFluxP, pcmHTC, coilHTC, tempC, htFluxC, htCapSP,
  htCapLP, pcmHTC, pcmSA, tauSP, tauLP)

refMDB :: ModelDB
refMDB = mdb [] [] dataDefs []

qDefs :: [QDefinition]
qDefs = [ddHtFluxCQD, ddHtFluxPQD, ddBalanceSolidPCMQD,
  ddBalanceLiquidPCMQD, ddHtFusionQD, dd4MeltFracQD]

dataDefs :: [DataDefinition] 
dataDefs = [ddHtFluxC, ddHtFluxP, ddBalanceSolidPCM,
  ddBalanceLiquidPCM, ddHtFusion, dd4MeltFrac]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

ddHtFluxCQD :: QDefinition
ddHtFluxCQD = mkQuantDef htFluxC htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = sy coilHTC * (sy tempC - apply1 tempW time)

ddHtFluxC :: DataDefinition
ddHtFluxC = dd ddHtFluxCQD [makeCite koothoor2013] [] "htFluxC"
  [makeRef2S assumpLCCCW, makeRef2S assumpTHCCoT]

--Can't include info in description beyond definition of variables?
----

ddHtFluxPQD :: QDefinition
ddHtFluxPQD = mkQuantDef htFluxP htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = sy pcmHTC * (apply1 tempW time - apply1 tempPCM time)

ddHtFluxP :: DataDefinition
ddHtFluxP = dd ddHtFluxPQD [makeCite koothoor2013] [] "htFluxP"
  [makeRef2S assumpLCCCW]

----

ddBalanceSolidPCMQD :: QDefinition
ddBalanceSolidPCMQD = mkQuantDef tauSP balanceSolidPCMEqn

balanceSolidPCMEqn :: Expr
balanceSolidPCMEqn = (sy pcmMass * sy htCapSP) /
  (sy pcmHTC * sy pcmSA)

ddBalanceSolidPCM :: DataDefinition
ddBalanceSolidPCM = dd ddBalanceSolidPCMQD [makeCite lightstone2012] []
  "balanceSolidPCM" []

----

ddBalanceLiquidPCMQD :: QDefinition
ddBalanceLiquidPCMQD = mkQuantDef tauLP balanceLiquidPCMEqn

balanceLiquidPCMEqn :: Expr
balanceLiquidPCMEqn = (sy pcmMass * sy htCapLP) /
  (sy pcmHTC * sy pcmSA)

ddBalanceLiquidPCM :: DataDefinition
ddBalanceLiquidPCM = dd ddBalanceLiquidPCMQD [makeCite lightstone2012] []
  "balanceLiquidPCM" []

----

ddHtFusionQD :: QDefinition
ddHtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = sy latentHeat / sy mass

-- FIXME: need to allow page references in references.
ddHtFusion :: DataDefinition
ddHtFusion = dd ddHtFusionQD [makeCiteInfo bueche1986 $ Page [282]]
  [] "htFusion" []

----

dd4MeltFracQD :: QDefinition
dd4MeltFracQD = mkQuantDef meltFrac meltFracEqn

--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

meltFracEqn :: Expr
meltFracEqn = sy latentEP / (sy htFusion * sy pcmMass)

dd4MeltFrac :: DataDefinition
dd4MeltFrac = dd dd4MeltFracQD [makeCite koothoor2013] [] "meltFrac"
 [S "The" +:+ phrase value `sOf` E (sy meltFrac) `sIs` S "constrained to" +:+.
  E (0 $<= sy meltFrac $<= 1), makeRef2S ddHtFusion]

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
