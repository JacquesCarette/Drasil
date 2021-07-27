module Drasil.SglPendulum.Figures (figMotion, sysCtxFig1) where

import Language.Drasil
import Utils.Drasil.Concepts

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

resourcePath :: String
resourcePath = "../../../datafiles/SglPendulum/"

figMotion :: LabelledContent
figMotion = llcc (makeFigRef "sglpendulum") $ figWithWidth (atStartNP (the physicalSystem))
  (resourcePath ++ "sglpendulum.jpg") 70

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont) 
  (resourcePath ++ "SystemContextFigure.png")
