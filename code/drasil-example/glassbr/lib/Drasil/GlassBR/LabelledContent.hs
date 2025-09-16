module Drasil.GlassBR.LabelledContent where

import Control.Lens((^.))

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumption, item, physicalSystem,
  requirement, section_, sysCont, traceyMatrix)

import Drasil.GlassBR.Concepts (aR, stdOffDist)
import Drasil.GlassBR.Unitals (aspectRatio, charWeight, demand, demandq,
  dimlessLoad, lateralLoad, sD, stressDistFac)

resourcePath :: String
resourcePath = "../../../../datafiles/glassbr/"

figures :: [LabelledContent]
figures = [sysCtxFig, physSystFig, traceItemSecsFig, traceReqsItemsFig,
  traceAssumpsOthersFig, demandVsSDFig, dimlessloadVsARFig]

sysCtxFig, physSystFig, traceItemSecsFig, traceReqsItemsFig,
  traceAssumpsOthersFig, demandVsSDFig, dimlessloadVsARFig :: LabelledContent

sysCtxFig = llcc (makeFigRef "sysCtxDiag") $ 
  fig (titleize sysCont) (resourcePath ++ "SystemContextFigure.png")

physSystFig = llcc (makeFigRef "physSystImage") $ figWithWidth 
  (atStartNP $ the physicalSystem) (resourcePath ++ "physicalsystimage.png") 30

traceItemSecsFig = llcc (makeFigRef "TraceyItemSecs") $ fig (showingCxnBw traceyMatrix $
  titleize' item +:+ S "of Different" +:+ titleize' section_)
  (resourcePath ++ "Trace.png")

traceReqsItemsFig = llcc (makeFigRef "TraceyReqsItems") $ fig (showingCxnBw traceyMatrix $
  titleize' requirement `S.and_` S "Other" +:+ titleize' item)
  (resourcePath ++ "RTrace.png")

traceAssumpsOthersFig = llcc (makeFigRef "TraceyAssumpsOthers") $ fig (showingCxnBw traceyMatrix $
  titleize' assumption `S.and_` S "Other" +:+ titleize' item)
  (resourcePath ++ "ATrace.png")

demandVsSDFig = llcc (makeFigRef "demandVSsod") $ fig ((demandq ^. defn) +:+
  sParen (ch demand) `S.versus` atStart sD +:+ sParen (short stdOffDist)
  `S.versus` atStart charWeight +:+ sParen (ch charWeight))
  (resourcePath ++ "ASTM_F2248-09.png")

dimlessloadVsARFig = llcc (makeFigRef "dimlessloadVSaspect") $ fig (S "Non dimensional" +:+
  phrase lateralLoad +:+ sParen (ch dimlessLoad)
  `S.versus` titleize aspectRatio +:+ sParen (short aR)
  `S.versus` atStart stressDistFac +:+ sParen (ch stressDistFac))
  (resourcePath ++ "ASTM_F2248-09_BeasonEtAl.png")
