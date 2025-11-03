module Drasil.Projectile.LabelledContent (
  labelledContent, figLaunch, sysCtxFig1
) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Development as D

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

labelledContent :: [LabelledContent]
labelledContent = [figLaunch, sysCtxFig1]

resourcePath :: String
resourcePath = "../../../../datafiles/projectile/"

figLaunch :: LabelledContent
figLaunch = llcc (makeFigRef "Launch") $ figWithWidth (D.toSent $ atStartNP (the physicalSystem))
  (resourcePath ++ "Launch.jpg") 70

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont)
  (resourcePath ++ "SystemContextFigure.png")
