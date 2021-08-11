module Drasil.DblPendulum.Figures (figMotion, sysCtxFig1) where

import Language.Drasil
import Utils.Drasil.Concepts

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/dblpendulum/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "dblpendulum") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "dblpendulum.png") 60

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "SystemContextFigure.png")
