module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Control.Lens ((^.))
import Drasil.DocLang (ModelDB, mdb)
import Theory.Drasil (DataDefinition, dd, ddNoRefs, mkQuantDef)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Thermodynamics (heat)

import Data.Drasil.Quantities.Physics (energy, pressure, time)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latentHeat)

import Drasil.SWHS.Assumptions (assumpLCCCW, assumpTHCCoT)
import Drasil.SWHS.References (bueche1986, koothoor2013, lightstone2012)
import Drasil.SWHS.Unitals (meltFrac, latentEP, htFusion, pcmMass,
  tempW, tempPCM, htFluxP, pcmHTC, coilHTC, tempC, htFluxC, htCapSP,
  htCapLP, pcmHTC, pcmSA, tauSP, tauLP, aspectRatio, diam, tankLength)

refMDB :: ModelDB
refMDB = mdb [] [] dataDefs []

qDefs :: [QDefinition]
qDefs = [dd1HtFluxCQD, dd2HtFluxPQD, ddBalanceSolidPCMQD,
  ddBalanceLiquidPCMQD, dd3HtFusionQD, dd4MeltFracQD, aspRatQD]

dataDefs :: [DataDefinition] 
dataDefs = [dd1HtFluxC, dd2HtFluxP, ddBalanceSolidPCM,
  ddBalanceLiquidPCM, dd3HtFusion, dd4MeltFrac, aspRat]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

dd1HtFluxCQD :: QDefinition
dd1HtFluxCQD = mkQuantDef htFluxC htFluxCEqn

htFluxCEqn :: Expr
htFluxCEqn = sy coilHTC * (sy tempC - apply1 tempW time)

dd1HtFluxC :: DataDefinition
dd1HtFluxC = dd dd1HtFluxCQD [makeCite koothoor2013] [] "htFluxC"
  [makeRef2S assumpLCCCW, makeRef2S assumpTHCCoT]

--Can't include info in description beyond definition of variables?
----

dd2HtFluxPQD :: QDefinition
dd2HtFluxPQD = mkQuantDef htFluxP htFluxPEqn

htFluxPEqn :: Expr
htFluxPEqn = sy pcmHTC * (apply1 tempW time - apply1 tempPCM time)

dd2HtFluxP :: DataDefinition
dd2HtFluxP = dd dd2HtFluxPQD [makeCite koothoor2013] [] "htFluxP"
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

dd3HtFusionQD :: QDefinition
dd3HtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = sy latentHeat / sy mass

-- FIXME: need to allow page references in references.
dd3HtFusion :: DataDefinition
dd3HtFusion = dd dd3HtFusionQD [makeCiteInfo bueche1986 $ Page [282]]
  [] "htFusion" dd3HtFusionNotes

dd3HtFusionNotes :: [Sentence]
dd3HtFusionNotes = [foldlSent [S "The", phrase htFusion,
  sParen (S "also known as the enthalpy of fusion"), S "of a substance is the",
  phrase heat, phrase energy, S "required", sParen (ch latentHeat), S "to change the state of a unit of the",
  phrase mass, sParen (ch mass), S "of the substance from solid to liquid" `sC`
  S "at constant", phrase pressure]]

----

dd4MeltFracQD :: QDefinition
dd4MeltFracQD = fromEqn' (meltFrac ^. uid) -- FIXME Should (^. id) be used
  (meltFrac ^. term) (S "fraction of the PCM that is liquid")
  (eqSymb meltFrac) meltFracEqn 

--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

meltFracEqn :: Expr
meltFracEqn = sy latentEP / (sy htFusion * sy pcmMass)

dd4MeltFrac :: DataDefinition
dd4MeltFrac = dd dd4MeltFracQD [makeCite koothoor2013] [] "meltFrac"
 [S "The" +:+ phrase value `sOf` E (sy meltFrac) `sIs` S "constrained to" +:+.
  E (0 $<= sy meltFrac $<= 1), makeRef2S dd3HtFusion]

----

aspRatQD :: QDefinition
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRatEq :: Expr
aspRatEq = sy diam / sy tankLength

aspRat :: DataDefinition
aspRat = ddNoRefs aspRatQD [{-derivation-}] "aspectRatio" []

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
