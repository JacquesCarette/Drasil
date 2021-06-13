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

qDefs :: [QDefinition]
qDefs = [waterMassQD, waterVolumeQD, tankVolumeQD, balanceDecayRateQD, 
  balanceDecayTimeQD, balanceSolidPCMQD, balanceLiquidPCMQD, ddHtFusionQD, 
  ddMeltFracQD, aspRatQD]

dataDefs :: [DataDefinition] 
dataDefs = [waterMass, waterVolume, tankVolume, balanceDecayRate, 
  balanceDecayTime, balanceSolidPCM, balanceLiquidPCM, ddHtFusion, ddMeltFrac, 
  aspRat]

-- FIXME? This section looks strange. Some data defs are created using
--    terms, some using defns, and some with a brand new description.
--    I think this will need an overhaul after we fix Data Definitions.

waterMassQD :: QDefinition
waterMassQD = mkQuantDef wMass waterMassEqn

waterMassEqn :: Expr
waterMassEqn = sy wVol `mulRe` sy wDensity

waterMassNotes :: Sentence
waterMassNotes = foldlSent [ch wVol, S "is defined in", makeRef2S waterVolume]

waterMass :: DataDefinition
waterMass = ddNoRefs waterMassQD Nothing "waterMass" []

----

waterVolumeQD :: QDefinition
waterVolumeQD = mkQuantDef wVol waterVolumeEqn

waterVolumeEqn :: Expr
waterVolumeEqn = sy tankVol $- sy pcmVol

waterVolumeNotes :: Sentence
waterVolumeNotes = foldlSent [S "Based on" +:+. makeRef2S assumpVCN, 
  ch tankVol, S "is defined in", makeRef2S tankVolume]

waterVolume :: DataDefinition
waterVolume = ddNoRefs waterVolumeQD Nothing "waterVolume_pcm" 
  [waterVolumeNotes]

----

tankVolumeQD :: QDefinition
tankVolumeQD = mkQuantDef tankVol tankVolumeEqn

tankVolumeEqn :: Expr
tankVolumeEqn = sy pi_ `mulRe` square (half $ sy diam) `mulRe` sy tankLength

tankVolume :: DataDefinition
tankVolume = ddNoRefs tankVolumeQD Nothing "tankVolume" []

----

balanceDecayRateQD :: QDefinition
balanceDecayRateQD = mkQuantDef tauW balanceDecayRateEqn

balanceDecayRateEqn :: Expr
balanceDecayRateEqn = sy wMass `mulRe` sy htCapW $/ (sy coilHTC `mulRe` sy coilSA)

balanceDecayRateNotes :: Sentence
balanceDecayRateNotes = foldlSent [ch wMass, S "is defined in", 
  makeRef2S waterMass]

balanceDecayRate :: DataDefinition
balanceDecayRate = dd balanceDecayRateQD [makeCite koothoor2013]
  Nothing "balanceDecayRate" []

----

balanceDecayTimeQD :: QDefinition
balanceDecayTimeQD = mkQuantDef eta balanceDecayTimeEqn

balanceDecayTimeEqn :: Expr
balanceDecayTimeEqn = sy pcmHTC `mulRe` sy pcmSA $/ (sy coilHTC `mulRe` sy coilSA)

balanceDecayTime :: DataDefinition
balanceDecayTime = dd balanceDecayTimeQD [makeCite koothoor2013]
  Nothing "balanceDecayTime" []

----

balanceSolidPCMQD :: QDefinition
balanceSolidPCMQD = mkQuantDef tauSP balanceSolidPCMEqn

balanceSolidPCMEqn :: Expr
balanceSolidPCMEqn = (sy pcmMass `mulRe` sy htCapSP) $/
  (sy pcmHTC `mulRe` sy pcmSA)

balanceSolidPCM :: DataDefinition
balanceSolidPCM = dd balanceSolidPCMQD [makeCite lightstone2012]
  Nothing "balanceSolidPCM" []

----

balanceLiquidPCMQD :: QDefinition
balanceLiquidPCMQD = mkQuantDef tauLP balanceLiquidPCMEqn

balanceLiquidPCMEqn :: Expr
balanceLiquidPCMEqn = (sy pcmMass `mulRe` sy htCapLP) $/
  (sy pcmHTC `mulRe` sy pcmSA)

balanceLiquidPCM :: DataDefinition
balanceLiquidPCM = dd balanceLiquidPCMQD [makeCite lightstone2012]
  Nothing "balanceLiquidPCM" []

----

ddHtFusionQD :: QDefinition
ddHtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = sy latentHeat $/ sy mass

ddHtFusion :: DataDefinition
ddHtFusion = dd ddHtFusionQD [makeCiteInfo bueche1986 $ Page [282]]
  Nothing "htFusion" [htFusionNote]

htFusionNote :: Sentence
htFusionNote = foldlSent [atStartNP (the htFusion),
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
meltFracEqn = sy latentEP $/ (sy htFusion `mulRe` sy pcmMass)

ddMeltFrac :: DataDefinition
ddMeltFrac = dd ddMeltFracQD [makeCite koothoor2013]
  Nothing "meltFrac" [meltFracConst, makeRef2S ddHtFusion]
  where meltFracConst = atStartNP (the value) `S.of_` eS meltFrac `S.is`
                        S "constrained to" +:+. eS (exactDbl 0 $<= sy meltFrac $<= exactDbl 1) -- TODO: This makes sense to us, but I think this wouldn't quite work with the current Expr language.

----

aspRatQD :: QDefinition
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRatEq :: Expr
aspRatEq = sy diam $/ sy tankLength

aspRat :: DataDefinition
aspRat = ddNoRefs aspRatQD Nothing "aspectRatio" []

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
