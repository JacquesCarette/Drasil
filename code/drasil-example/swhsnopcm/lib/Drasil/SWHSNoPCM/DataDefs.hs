module Drasil.SWHSNoPCM.DataDefs (
  qDefs, dataDefs, waterVolumeQD, waterVolumeEqn, waterVolumeNotes, waterVolume
) where --exports all of it

import Language.Drasil
import Language.Drasil.Docs
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
