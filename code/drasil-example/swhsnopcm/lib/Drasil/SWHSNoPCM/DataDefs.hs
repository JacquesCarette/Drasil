module Drasil.SWHSNoPCM.DataDefs where --exports all of it

import Language.Drasil
import Theory.Drasil (DataDefinition, ddENoRefs)

import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.DataDefs (balanceDecayRate, balanceDecayRateQD, tankVolume, 
  tankVolumeQD, waterMass, waterMassQD)
import Drasil.SWHS.Unitals (tankVol, wVol)

qDefs :: [SimpleQDef]
qDefs = [waterMassQD, waterVolumeQD, tankVolumeQD, balanceDecayRateQD]

dataDefs :: [DataDefinition] 
dataDefs = [waterMass, waterVolume, tankVolume, balanceDecayRate]

waterVolumeQD :: SimpleQDef
waterVolumeQD = mkQuantDef wVol waterVolumeEqn

waterVolumeEqn :: Expr
waterVolumeEqn = sy tankVol

waterVolumeNotes :: Sentence
waterVolumeNotes = foldlSent [S "Based on" +:+. refS assumpVCN, ch tankVol,
  S "is defined in", refS tankVolume]

waterVolume :: DataDefinition
waterVolume = ddENoRefs waterVolumeQD Nothing "waterVolume_nopcm" 
  [waterVolumeNotes]
