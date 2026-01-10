module Drasil.SWHSNoPCM.LabelledContent (
    labelledContent, figTank, sysCntxtFig
) where

import Language.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation as Doc (sysCont)
import Data.Drasil.Concepts.Thermodynamics (htFlux)

import Drasil.SWHS.Concepts (coil, sWHT)
import Drasil.SWHS.Unitals (htFluxC)

resourcePath :: String
resourcePath = "../../../../datafiles/swhsnopcm/"

labelledContent :: [LabelledContent]
labelledContent = [sysCntxtFig, figTank]

figTank :: LabelledContent
figTank = llccFig "Tank" $ fig (atStart sWHT `sC` S "with" +:+ phrase htFlux +:+
  S "from" +:+ phrase coil `S.of_` ch htFluxC)
  $ resourcePath ++ "TankWaterOnly.png"

sysCntxtFig :: LabelledContent
sysCntxtFig = llccFig "SysCon"
  $ fig (titleize sysCont)
  $ resourcePath ++ "SystemContextFigure.png"
