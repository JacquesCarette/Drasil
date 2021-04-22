module Drasil.NoPCM.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition, ddNoRefs)
import Utils.Drasil

import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.DataDefs (balanceDecayRate, balanceDecayRateQD, tankVolume, 
  tankVolumeQD, waterMass, waterMassQD)
import Drasil.SWHS.Unitals (tankVol, wVol)

qDefs :: [QDefinition]
qDefs = [waterMassQD, waterVolumeQD, tankVolumeQD, balanceDecayRateQD]

dataDefs :: [DataDefinition] 
dataDefs = [waterMass, waterVolume, tankVolume, balanceDecayRate]

waterVolumeQD :: QDefinition
waterVolumeQD = mkQuantDef wVol waterVolumeEqn

waterVolumeEqn :: Expr
waterVolumeEqn = sy tankVol

waterVolumeNotes :: Sentence
waterVolumeNotes = foldlSent [S "Based on" +:+. makeRef2S assumpVCN, ch tankVol,
  S "is defined in", makeRef2S tankVolume]

waterVolume :: DataDefinition
waterVolume = ddNoRefs waterVolumeQD Nothing "waterVolume_nopcm" 
  [waterVolumeNotes]
