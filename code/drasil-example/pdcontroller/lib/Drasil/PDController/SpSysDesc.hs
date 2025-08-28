module Drasil.PDController.SpSysDesc where

import Data.Drasil.Concepts.Documentation (goalStmtDom)

import Drasil.PDController.Concepts
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

-- Introduction of the Problem Description section derives from purpose in
-- System (purp in Body.hs)

sysParts :: [Sentence]
sysParts
  = map ((!.) . atStartNP . the)
      [summingPt, pidC, powerPlant]

sysGoalInput :: [Sentence]
sysGoalInput
  = [phrase setPoint, phrase simulationTime, phrase propGain, phrase derGain,
     phrase stepTime]

goals :: [ConceptInstance]
goals = [sysProcessVariable]

sysProcessVariable :: ConceptInstance
sysProcessVariable
  = cic "calculateProcessVariable"
      (foldlSent
         [S "Calculate the output of the", phrase powerPlant,
            sParen (phrase processVariable),
            S "over time"])
      "Process-Variable"
      goalStmtDom
