module Drasil.GamePhysics.LabelledContent (
  labelledContent, sysCtxFig1
) where

import Language.Drasil hiding (organization, section)

import Data.Drasil.Concepts.Documentation as Doc (sysCont)

resourcePath :: String
resourcePath = "../../../../datafiles/gamephysics/"

labelledContent :: [LabelledContent]
labelledContent = [sysCtxFig1]

sysCtxFig1 :: LabelledContent
sysCtxFig1 = llcc (makeFigRef "sysCtxDiag") $ fig (titleize sysCont)
  (resourcePath ++ "sysctx.png")
