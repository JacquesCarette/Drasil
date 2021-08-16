module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Drasil.DocLang (ModelDB, mdb)
import Theory.Drasil (DataDefinition, dd, ddNoRefs)
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Thermodynamics (heat)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (energy, pressure)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latentHeat)

import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.References (bueche1986, koothoor2013, lightstone2012)
import Drasil.SWHS.Unitals (aspectRatio, coilHTC, coilSA, diam, eta, htCapLP,
  htCapSP, htCapW, htFusion, latentEP, meltFrac, pcmHTC, pcmMass, pcmSA, pcmVol,
  tankLength, tankVol, tauLP, tauSP, tauW, wDensity, wMass, wVol)

refMDB :: ModelDB
refMDB = mdb [] [] dataDefs []

qDefs :: [QDefinition Expr]
qDefs = [waterMassQD, waterVolumeQD, tankVolumeQD, balanceDecayRateQD, 
  balanceDecayTimeQD, balanceSolidPCMQD, balanceLiquidPCMQD, ddHtFusionQD, 
  ddMeltFracQD, aspRatQD]

dataDefs :: [DataDefinition Expr] 
dataDefs = [waterMass, waterVolume, tankVolume, balanceDecayRate, 
  balanceDecayTime, balanceSolidPCM, balanceLiquidPCM, ddHtFusion, ddMeltFrac, 
  aspRat]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

waterMassQD :: QDefinition Expr
waterMassQD = mkQuantDef wMass waterMassEqn

waterMassEqn :: Expr
waterMassEqn = sy wVol `mulRe` sy wDensity

waterMassNotes :: Sentence
waterMassNotes = foldlSent [ch wVol, S "is defined in", refS waterVolume]

waterMass :: DataDefinition Expr
waterMass = ddNoRefs waterMassQD Nothing "waterMass" []

----

waterVolumeQD :: QDefinition Expr
waterVolumeQD = mkQuantDef wVol waterVolumeEqn

waterVolumeEqn :: Expr
waterVolumeEqn = sy tankVol $- sy pcmVol

waterVolumeNotes :: Sentence
waterVolumeNotes = foldlSent [S "Based on" +:+. refS assumpVCN, 
  ch tankVol, S "is defined in", refS tankVolume]

waterVolume :: DataDefinition Expr
waterVolume = ddNoRefs waterVolumeQD Nothing "waterVolume_pcm" 
  [waterVolumeNotes]

----

tankVolumeQD :: QDefinition Expr
tankVolumeQD = mkQuantDef tankVol tankVolumeEqn

tankVolumeEqn :: Expr
tankVolumeEqn = sy pi_ `mulRe` square (half $ sy diam) `mulRe` sy tankLength

tankVolume :: DataDefinition Expr
tankVolume = ddNoRefs tankVolumeQD Nothing "tankVolume" []

----

balanceDecayRateQD :: QDefinition Expr
balanceDecayRateQD = mkQuantDef tauW balanceDecayRateEqn

balanceDecayRateEqn :: Expr
balanceDecayRateEqn = sy wMass `mulRe` sy htCapW $/ (sy coilHTC `mulRe` sy coilSA)

balanceDecayRateNotes :: Sentence
balanceDecayRateNotes = foldlSent [ch wMass, S "is defined in", 
  refS waterMass]

balanceDecayRate :: DataDefinition Expr
balanceDecayRate = dd balanceDecayRateQD [dRef koothoor2013]
  Nothing "balanceDecayRate" []

----

balanceDecayTimeQD :: QDefinition Expr
balanceDecayTimeQD = mkQuantDef eta balanceDecayTimeEqn

balanceDecayTimeEqn :: Expr
balanceDecayTimeEqn = sy pcmHTC `mulRe` sy pcmSA $/ (sy coilHTC `mulRe` sy coilSA)

balanceDecayTime :: DataDefinition Expr
balanceDecayTime = dd balanceDecayTimeQD [dRef koothoor2013]
  Nothing "balanceDecayTime" []

----

balanceSolidPCMQD :: QDefinition Expr
balanceSolidPCMQD = mkQuantDef tauSP balanceSolidPCMEqn

balanceSolidPCMEqn :: Expr
balanceSolidPCMEqn = (sy pcmMass `mulRe` sy htCapSP) $/
  (sy pcmHTC `mulRe` sy pcmSA)

balanceSolidPCM :: DataDefinition Expr
balanceSolidPCM = dd balanceSolidPCMQD [dRef lightstone2012]
  Nothing "balanceSolidPCM" []

----

balanceLiquidPCMQD :: QDefinition Expr
balanceLiquidPCMQD = mkQuantDef tauLP balanceLiquidPCMEqn

balanceLiquidPCMEqn :: Expr
balanceLiquidPCMEqn = (sy pcmMass `mulRe` sy htCapLP) $/
  (sy pcmHTC `mulRe` sy pcmSA)

balanceLiquidPCM :: DataDefinition Expr
balanceLiquidPCM = dd balanceLiquidPCMQD [dRef lightstone2012]
  Nothing "balanceLiquidPCM" []

----

ddHtFusionQD :: QDefinition Expr
ddHtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = sy latentHeat $/ sy mass

ddHtFusion :: DataDefinition Expr
ddHtFusion = dd ddHtFusionQD [dRefInfo bueche1986 $ Page [282]]
  Nothing "htFusion" [htFusionNote]

htFusionNote :: Sentence
htFusionNote = foldlSent [atStartNP (the htFusion),
  sParen (S "also known as the enthalpy of fusion"), S "of a substance is the",
  phrase heat, phrase energy, S "required", sParen (ch latentHeat), S "to change the state of a unit of the",
  phrase mass, sParen (ch mass), S "of the substance from solid to liquid" `sC`
  S "at constant", phrase pressure]

----

ddMeltFracQD :: QDefinition Expr
ddMeltFracQD = mkQuantDef meltFrac meltFracEqn

--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

meltFracEqn :: Expr
meltFracEqn = sy latentEP $/ (sy htFusion `mulRe` sy pcmMass)

ddMeltFrac :: DataDefinition Expr
ddMeltFrac = dd ddMeltFracQD [dRef koothoor2013]
  Nothing "meltFrac" [meltFracConst, refS ddHtFusion]
  where meltFracConst = atStartNP (the value) `S.of_` eS meltFrac `S.is`
                        S "constrained to" +:+. eS (realInterval meltFrac (Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)))

----

aspRatQD :: QDefinition Expr
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRatEq :: Expr
aspRatEq = sy diam $/ sy tankLength

aspRat :: DataDefinition Expr
aspRat = ddNoRefs aspRatQD Nothing "aspectRatio" []

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
