module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Drasil.DocLang (ModelDB, mdb)
import Theory.Drasil (DataDefinition, dd, ddNoRefs, mkQuantDef)
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Thermodynamics (heat)

import Data.Drasil.Quantities.Physics (energy, pressure)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latentHeat)

import Drasil.SWHS.References (bueche1986, koothoor2013, lightstone2012)
import Drasil.SWHS.Unitals (aspectRatio, coilHTC, coilSA, diam, eta, htCapLP,
  htCapSP, htCapW, htFusion, latentEP, meltFrac, pcmHTC, pcmMass, pcmSA,
  tankLength, tauLP, tauSP, tauW, wMass)

refMDB :: ModelDB
refMDB = mdb [] [] dataDefs []

qDefs :: [QDefinition]
qDefs = [balanceDecayRateQD, balanceDecayTimeQD, balanceSolidPCMQD,
  balanceLiquidPCMQD, ddHtFusionQD, ddMeltFracQD, aspRatQD]

dataDefs :: [DataDefinition] 
dataDefs = [balanceDecayRate, balanceDecayTime, balanceSolidPCM,
  balanceLiquidPCM, ddHtFusion, ddMeltFrac, aspRat]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

balanceDecayRateQD :: QDefinition
balanceDecayRateQD = mkQuantDef tauW balanceDecayRateEqn

balanceDecayRateEqn :: Expr
balanceDecayRateEqn = sy wMass * sy htCapW / (sy coilHTC * sy coilSA)

balanceDecayRate :: DataDefinition
balanceDecayRate = dd balanceDecayRateQD [makeCite koothoor2013]
  Nothing "balanceDecayRate" []

----

balanceDecayTimeQD :: QDefinition
balanceDecayTimeQD = mkQuantDef eta balanceDecayTimeEqn

balanceDecayTimeEqn :: Expr
balanceDecayTimeEqn = sy pcmHTC * sy pcmSA / (sy coilHTC * sy coilSA)

balanceDecayTime :: DataDefinition
balanceDecayTime = dd balanceDecayTimeQD [makeCite koothoor2013]
  Nothing "balanceDecayTime" []

----

balanceSolidPCMQD :: QDefinition
balanceSolidPCMQD = mkQuantDef tauSP balanceSolidPCMEqn

balanceSolidPCMEqn :: Expr
balanceSolidPCMEqn = (sy pcmMass * sy htCapSP) /
  (sy pcmHTC * sy pcmSA)

balanceSolidPCM :: DataDefinition
balanceSolidPCM = dd balanceSolidPCMQD [makeCite lightstone2012]
  Nothing "balanceSolidPCM" []

----

balanceLiquidPCMQD :: QDefinition
balanceLiquidPCMQD = mkQuantDef tauLP balanceLiquidPCMEqn

balanceLiquidPCMEqn :: Expr
balanceLiquidPCMEqn = (sy pcmMass * sy htCapLP) /
  (sy pcmHTC * sy pcmSA)

balanceLiquidPCM :: DataDefinition
balanceLiquidPCM = dd balanceLiquidPCMQD [makeCite lightstone2012]
  Nothing "balanceLiquidPCM" []

----

ddHtFusionQD :: QDefinition
ddHtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = sy latentHeat / sy mass

ddHtFusion :: DataDefinition
ddHtFusion = dd ddHtFusionQD [makeCiteInfo bueche1986 $ Page [282]]
  Nothing "htFusion" [htFusionNote]

htFusionNote :: Sentence
htFusionNote = foldlSent [S "The", phrase htFusion,
  sParen (S "also known as the enthalpy of fusion"), S "of a substance is the",
  phrase heat, phrase energy, S "required", sParen (ch latentHeat), S "to change the state of a unit of the",
  phrase mass, sParen (ch mass), S "of the substance from solid to liquid" `sC`
  S "at constant", phrase pressure]

----

ddMeltFracQD :: QDefinition
ddMeltFracQD = mkQuantDef meltFrac meltFracEqn

--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

meltFracEqn :: Expr
meltFracEqn = sy latentEP / (sy htFusion * sy pcmMass)

ddMeltFrac :: DataDefinition
ddMeltFrac = dd ddMeltFracQD [makeCite koothoor2013]
  Nothing "meltFrac" [meltFracConst, makeRef2S ddHtFusion]
  where meltFracConst = S "The" +:+ phrase value `sOf` E (sy meltFrac) `sIs`
                        S "constrained to" +:+. E (0 $<= sy meltFrac $<= 1)

----

aspRatQD :: QDefinition
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRatEq :: Expr
aspRatEq = sy diam / sy tankLength

aspRat :: DataDefinition
aspRat = ddNoRefs aspRatQD Nothing "aspectRatio" []

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
