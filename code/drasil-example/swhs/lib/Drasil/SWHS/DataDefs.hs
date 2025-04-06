module Drasil.SWHS.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition, ddE, ddENoRefs)
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations (koothoor2013)

import Data.Drasil.Concepts.Documentation (value)
import Data.Drasil.Concepts.Thermodynamics (heat)

import Data.Drasil.Quantities.Math (pi_)
import Data.Drasil.Quantities.Physics (energy, pressure)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Thermodynamics (latentHeat)


import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.References (bueche1986, lightstone2012)
import Drasil.SWHS.Unitals (aspectRatio, coilHTC, coilSA, diam, eta, htCapLP,
  htCapSP, htCapW, htFusion, latentEP, meltFrac, pcmHTC, pcmMass, pcmSA, pcmVol,
  tankLength, tankVol, tauLP, tauSP, tauW, wDensity, wMass, wVol)

qDefs :: [SimpleQDef]
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

waterMassQD :: SimpleQDef
waterMassQD = mkQuantDef wMass waterMassEqn

waterMassEqn :: Expr
waterMassEqn = sy wVol $* sy wDensity

waterMassNotes :: Sentence
waterMassNotes = foldlSent [ch wVol, S "is defined in", refS waterVolume]

waterMass :: DataDefinition
waterMass = ddENoRefs waterMassQD Nothing "waterMass" []

----

waterVolumeQD :: SimpleQDef
waterVolumeQD = mkQuantDef wVol waterVolumeEqn

waterVolumeEqn :: Expr
waterVolumeEqn = sy tankVol $- sy pcmVol

waterVolumeNotes :: Sentence
waterVolumeNotes = foldlSent [S "Based on" +:+. refS assumpVCN, 
  ch tankVol, S "is defined in", refS tankVolume]

waterVolume :: DataDefinition
waterVolume = ddENoRefs waterVolumeQD Nothing "waterVolume_pcm" 
  [waterVolumeNotes]

----

tankVolumeQD :: SimpleQDef
tankVolumeQD = mkQuantDef tankVol tankVolumeEqn

tankVolumeEqn :: Expr
tankVolumeEqn = sy pi_ $* square (half $ sy diam) $* sy tankLength

tankVolume :: DataDefinition
tankVolume = ddENoRefs tankVolumeQD Nothing "tankVolume" []

----

balanceDecayRateQD :: SimpleQDef
balanceDecayRateQD = mkQuantDef tauW balanceDecayRateEqn

balanceDecayRateEqn :: Expr
balanceDecayRateEqn = sy wMass $* sy htCapW $/ (sy coilHTC $* sy coilSA)

balanceDecayRateNotes :: Sentence
balanceDecayRateNotes = foldlSent [ch wMass, S "is defined in", 
  refS waterMass]

balanceDecayRate :: DataDefinition
balanceDecayRate = ddE balanceDecayRateQD [dRef koothoor2013]
  Nothing "balanceDecayRate" []

----

balanceDecayTimeQD :: SimpleQDef
balanceDecayTimeQD = mkQuantDef eta balanceDecayTimeEqn

balanceDecayTimeEqn :: Expr
balanceDecayTimeEqn = sy pcmHTC $* sy pcmSA $/ (sy coilHTC $* sy coilSA)

balanceDecayTime :: DataDefinition
balanceDecayTime = ddE balanceDecayTimeQD [dRef koothoor2013]
  Nothing "balanceDecayTime" []

----

balanceSolidPCMQD :: SimpleQDef
balanceSolidPCMQD = mkQuantDef tauSP balanceSolidPCMEqn

balanceSolidPCMEqn :: Expr
balanceSolidPCMEqn = (sy pcmMass $* sy htCapSP) $/
  (sy pcmHTC $* sy pcmSA)

balanceSolidPCM :: DataDefinition
balanceSolidPCM = ddE balanceSolidPCMQD [dRef lightstone2012]
  Nothing "balanceSolidPCM" []

----

balanceLiquidPCMQD :: SimpleQDef
balanceLiquidPCMQD = mkQuantDef tauLP balanceLiquidPCMEqn

balanceLiquidPCMEqn :: Expr
balanceLiquidPCMEqn = (sy pcmMass $* sy htCapLP) $/
  (sy pcmHTC $* sy pcmSA)

balanceLiquidPCM :: DataDefinition
balanceLiquidPCM = ddE balanceLiquidPCMQD [dRef lightstone2012]
  Nothing "balanceLiquidPCM" []

----

ddHtFusionQD :: SimpleQDef
ddHtFusionQD = mkQuantDef htFusion htFusionEqn

htFusionEqn :: Expr
htFusionEqn = sy latentHeat $/ sy mass

ddHtFusion :: DataDefinition
ddHtFusion = ddE ddHtFusionQD [dRefInfo bueche1986 $ Page [282]]
  Nothing "htFusion" [htFusionNote]

htFusionNote :: Sentence
htFusionNote = foldlSent [atStartNP (the htFusion),
  sParen (S "also known as the enthalpy of fusion") `S.ofA` S "substance is the",
  phrase heat, phrase energy, S "required", sParen (ch latentHeat), S "to change the state" `S.ofA` S "unit of the",
  phrase mass, sParen (ch mass) `S.ofThe` S "substance from solid to liquid" `sC`
  S "at constant", phrase pressure]

----

ddMeltFracQD :: SimpleQDef
ddMeltFracQD = mkQuantDef meltFrac meltFracEqn

--FIXME: "Phi is the melt fraction" is produced; 
  --"Phi is the fraction of the PCM that is liquid" is what is supposed to be
  -- produced according to CaseStudies' original

meltFracEqn :: Expr
meltFracEqn = sy latentEP $/ (sy htFusion $* sy pcmMass)

ddMeltFrac :: DataDefinition
ddMeltFrac = ddE ddMeltFracQD [dRef koothoor2013]
  Nothing "meltFrac" [meltFracConst, refS ddHtFusion]
  where meltFracConst = atStartNP (the value) `S.of_` eS' meltFrac `S.is`
                        S "constrained to" +:+. eS (realInterval meltFrac (Bounded (Inc, exactDbl 0) (Inc, exactDbl 1)))

----

aspRatQD :: SimpleQDef
aspRatQD = mkQuantDef aspectRatio aspRatEq

aspRatEq :: Expr
aspRatEq = sy diam $/ sy tankLength

aspRat :: DataDefinition
aspRat = ddENoRefs aspRatQD Nothing "aspectRatio" []

--Need to add units to data definition descriptions

--Symbol appears as "Label"
--There is no actual label
--Units section doesn't appear
