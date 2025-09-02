module Drasil.SWHS.LabelledContent (
  labelledContent, sysCntxtFig, figTank
) where

import Language.Drasil hiding (organization, section, variable)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation as Doc (sysCont)

import Drasil.SWHS.Concepts (sWHT)
import Drasil.SWHS.Unitals (htFluxC, htFluxP)

resourcePath :: String
resourcePath = "../../../../datafiles/swhs/"

labelledContent :: [LabelledContent]
labelledContent = [sysCntxtFig, figTank]

sysCntxtFig :: LabelledContent
sysCntxtFig = llcc (makeFigRef "SysCon")
  $ fig (titleize sysCont)
  $ resourcePath ++ "SystemContextFigure.png"

figTank :: LabelledContent
figTank = llcc (makeFigRef "Tank") $ fig (
  foldlSent_ [atStart sWHT `sC` S "with", phrase htFluxC `S.of_`
  ch htFluxC `S.and_` phrase htFluxP `S.of_` ch htFluxP])
  $ resourcePath ++ "Tank.png"
