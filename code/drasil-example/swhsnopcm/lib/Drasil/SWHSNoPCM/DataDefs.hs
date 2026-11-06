module Drasil.SWHSNoPCM.DataDefs (
  dataDefs, waterVolumeQD, waterVolumeEqn, waterVolumeNotes, waterVolume
) where

import Language.Drasil
import Language.Drasil.Document
import Theory.Drasil (DataDefinition, ddENoRefs)

import Drasil.SWHS.Assumptions (assumpVCN)
import Drasil.SWHS.DataDefs (balanceDecayRate, tankVolume, waterMass)
import Drasil.SWHS.Unitals (tankVol, wVol)

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
