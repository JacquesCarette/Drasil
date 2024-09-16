module Drasil.PDController.Assumptions where

import Data.Drasil.Concepts.Documentation (assumpDom)

import Data.Drasil.Concepts.PhysicalProperties (mass)
import Data.Drasil.SI_Units (kilogram)
import Drasil.PDController.Concepts
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

assumptions :: [ConceptInstance]
assumptions
  = [aPwrPlant, aDecoupled, aSP, aExtDisturb, aInitialValue, aParallelEq,
     aUnfilteredDerivative, apwrPlantTxFnx, aMass, aDampingCoeff,
     aStiffnessCoeff]

aPwrPlant, aDecoupled, aSP, aExtDisturb, aInitialValue, aParallelEq,
           aUnfilteredDerivative, apwrPlantTxFnx, aMass, aDampingCoeff,
           aStiffnessCoeff :: ConceptInstance

aPwrPlant = cic "pwrPlant" pwrPlantDesc "Power plant" assumpDom

aDecoupled = cic "decoupled" aDecoupledDesc "Decoupled equation" assumpDom

aSP = cic "setPoint" aSPDesc "Set-Point" assumpDom

aExtDisturb
  = cic "externalDisturb" aExtDisturbDesc "External disturbance" assumpDom

aInitialValue = cic "initialValue" aInitialValueDesc "Initial Value" assumpDom

aParallelEq = cic "parallelEq" aParallelEqDesc "Parallel Equation" assumpDom

apwrPlantTxFnx
  = cic "pwrPlantTxFnx" apwrPlantTxFnxDesc "Transfer Function" assumpDom

aUnfilteredDerivative
  = cic "unfilteredDerivative" aUnfilteredDerivativeDesc "Unfiltered Derivative"
      assumpDom

aMass = cic "massSpring" aMassDesc "Spring Mass" assumpDom

aDampingCoeff
  = cic "dampingCoeffSpring" aDampingCoeffDesc "Spring Damping Coefficient"
      assumpDom

aStiffnessCoeff
  = cic "stiffnessCoeffSpring" aStiffnessCoeffDesc "Spring Stiffness Coefficient"
      assumpDom

pwrPlantDesc, aDecoupledDesc, aSPDesc, aExtDisturbDesc, aManualTuningDesc,
              aInitialValueDesc, aParallelEqDesc, apwrPlantTxFnxDesc,
              aUnfilteredDerivativeDesc, aMassDesc, aDampingCoeffDesc,
              aStiffnessCoeffDesc :: Sentence
pwrPlantDesc
  = foldlSent
      [atStartNP (the powerPlant) `S.andThe` S "Sensor are coupled as a single unit"]

apwrPlantTxFnxDesc
  = foldlSent
      [S "The combined", phrase powerPlant `S.and_` S "Sensor",
         sParen (refS aPwrPlant),        
         S "are characterized by a Second Order mass-spring-damper System"]

aDecoupledDesc
  = foldlSent
      [S "The decoupled form" `S.ofThe` phrase pidC,
         S "equation used in this", phrase simulation]

aSPDesc
  = foldlSent
      [atStartNP (the setPoint), S "is constant throughout",
         phraseNP (the simulation)]

aExtDisturbDesc
  = foldlSent
      [S "There are no external disturbances to the", phrase powerPlant,
         S "during the", phrase simulation]

aManualTuningDesc
  = foldlSent
      [S "This model will be used for manual tuning" `S.ofThe` phrase pidC]

aInitialValueDesc
  = foldlSent
      [S "The initial value" `S.ofThe` phrase processVariable,
         S "is assumed to be zero"]

aParallelEqDesc
  = foldlSent
      [S "The Parallel form" `S.ofThe` S "equation is used for the", phrase pidC]

aUnfilteredDerivativeDesc
  = foldlSent
      [S "A pure derivative function" `S.is` S "used for this simulation;",
       S "there are no filters applied"]

aMassDesc
  = foldlSent
      [atStartNP (the mass) `S.ofThe` S "spring" `S.inThe` S "mass-spring-damper system",
       sParen (refS aPwrPlant),
       S "is assumed to be 1", 
       phrase kilogram]

aDampingCoeffDesc
  = foldlSent
      [atStartNP (the ccDampingCoeff) `S.ofThe` S "spring" `S.inThe` S "mass-spring-damper system",
       sParen (refS aPwrPlant), 
       S "is assumed to be 1"]

aStiffnessCoeffDesc
  = foldlSent
      [atStartNP (the ccStiffCoeff) `S.ofThe` S "spring" `S.inThe` S "mass-spring-damper system",
       sParen (refS aPwrPlant), 
       S "is assumed to be 20"]