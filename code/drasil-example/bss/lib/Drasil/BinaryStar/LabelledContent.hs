module Drasil.BinaryStar.LabelledContent (
  labelledContent, figBSS, sysCtxFig1
) where

import Language.Drasil
import qualified Language.Drasil.Development as D
import Language.Drasil.Chunk.Concept.NamedCombinators

import Data.Drasil.Concepts.Documentation (physicalSystem, sysCont)

labelledContent :: [LabelledContent]
labelledContent = [figBSS, sysCtxFig1]

resourcePath :: String
resourcePath = "../../../../datafiles/bss/"

figBSS :: LabelledContent
figBSS = llccFig "bssPhysSys" $ figWithWidth (D.toSent $ atStartNP (the physicalSystem))
  (resourcePath ++ "bss.png") 60

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llccFig "sysCtxDiag" $ fig (titleize sysCont)
  (resourcePath ++ "SystemContextFigure.png")
