module Drasil.PDController.SpSysDesc where

import Data.Drasil.Concepts.Documentation (goalStmtDom, physicalSystem)

import Drasil.PDController.Concepts
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

sysProblemDesc :: Sentence
sysProblemDesc
  = foldlSent_
      [S "provide a model of a", phrase pidC,
         S "that can be used for the tuning of the gain constants before",
         S "the deployment of the controller"]

sysParts :: [Sentence]
sysParts
  = map ((!.) . atStartNP . the)
      [summingPt, pidC, powerPlant]

sysFigure :: LabelledContent
sysFigure
  = llcc (makeFigRef "pidSysDiagram") $
      figWithWidth (atStartNP $ the physicalSystem)
        "../../../../datafiles/pdcontroller/Fig_PDController.png"
        70

sysGoalInput :: [Sentence]
sysGoalInput
  = [phrase setPoint, phrase simulationTime, phrase propGain, phrase derGain,
     phrase stepTime]

goals :: [ConceptInstance]
goals = [sysProcessVariable]

sysProcessVariable :: ConceptInstance
sysProcessVariable
  = cic "processVariable"
      (foldlSent
         [S "Calculate the output of the", phrase powerPlant,
            sParen (phrase processVariable),
            S "over time"])
      "Process-Variable"
      goalStmtDom