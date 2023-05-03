module Drasil.PDController.Concepts where

import Data.Drasil.Concepts.Documentation
       (assumption, goalStmt, physSyst, requirement, srs, typUnc)
import Data.Drasil.TheoryConcepts
import Language.Drasil

acronyms :: [CI]
acronyms
  = [assumption, dataDefn, genDefn, goalStmt, inModel, physSyst, requirement,
     srs, thModel, typUnc, pdControllerCI, proportionalCI, derivativeCI,
     integralCI, pidCI]

pdControllerApp, pdControllerCI, proportionalCI, derivativeCI, integralCI,
                     pidCI :: CI

pdControllerApp = commonIdeaWithDict "pdControllerApp" (pn "PD Controller")                    "PD Controller" []
pdControllerCI  = commonIdeaWithDict "pdControllerCI"  (pn "Proportional Derivative")          "PD"            []
proportionalCI  = commonIdeaWithDict "proportionalCI"  (pn "Proportional")                     "P"             []
derivativeCI    = commonIdeaWithDict "derivativeCI"    (pn "Derivative")                       "D"             []
integralCI      = commonIdeaWithDict "integralCI"      (pn "Integral")                         "I"             []
pidCI           = commonIdeaWithDict "pidCI"           (pn "Proportional Integral Derivative") "PID"           []

pidC, pidCL, summingPt, powerPlant, secondOrderSystem, processError,
      simulationTime, processVariable, setPoint, propGain, derGain, 
      propControl, derControl, simulation,ccFrequencyDomain, ccTimeDomain,
      ccLaplaceTransform, controlVariable, stepTime, ccAbsTolerance, 
      ccRelTolerance, ccTransferFxn, ccDampingCoeff, ccStiffCoeff :: ConceptChunk
pidCL
  = dcc "pdCtrlLoop" (nounPhraseSP "PD Control Loop") ("Closed-Loop control " ++
        "system with PD Controller, Summing Point and Power Plant")

pidC
  = dcc "pdController" (nounPhraseSP "PD Controller") 
        "Proportional-Derivative Controller"

summingPt
  = dcc "summingPoint" (nounPhraseSP "Summing Point") ("Control block where " ++
        "the difference between the Set-Point and the Process Variable " ++
        "is computed")

powerPlant
  = dcc "powerPlant" (nounPhraseSP "Power Plant") 
      "A second order system to be controlled"

secondOrderSystem
  = dcc "secondOrderSystem" (nounPhraseSP "Second Order System") 
      ("A system whose input-output relationship is denoted by a second-order "
         ++ "differential equation")

processError
  = dcc "processError" (nounPhraseSP "Process Error") 
      ("Input to the PID controller. Process Error is the difference between the "
         ++ "Set-Point and the Process Variable")

stepTime = dcc "stepTime" (nounPhraseSP "Step Time") "Simulation step time"

simulationTime
  = dcc "simulationTime" (nounPhraseSP "Simulation Time") 
      "Total execution time of the PD simulation"

processVariable
  = dcc "processVariable" (nounPhraseSP "Process Variable") 
      "The output value from the power plant"

controlVariable
  = dcc "controlVariable" (nounPhraseSP "Control Variable") 
      "The Control Variable is the output of the PD controller"

setPoint
  = dcc "setPoint" (nounPhraseSP "Set-Point") 
      ("The desired value that the control system must reach. This also knows "
         ++ "as the reference variable")

propGain
  = dcc "propGain" (nounPhraseSP "Proportional Gain") 
      "Gain constant of the proportional controller"

derGain
  = dcc "derGain" (nounPhraseSP "Derivative Gain") 
      "Gain constant of the derivative controller"

propControl
  = dcc "propControl" (nounPhraseSP "Proportional control")
      ("A linear feedback control system where correction is applied to the controlled " ++
      "variable which is proportional to the difference between desired and measured values")

derControl
  = dcc "derControl" (nounPhraseSP "Derivative control")
      ("Monitors the rate of change of the error signal and contributes a component " ++ 
      "of the output signal (proportional to a derivative of the error signal)")

simulation
  = dcc "simulation" (nounPhraseSP "simulation") 
      "Simulation of the PD controller"

ccFrequencyDomain
  = dcc "frequencyDomain" (nounPhraseSP "frequency domain") 
      ("The analysis of mathematical functions in terms of frequency, instead "
         ++ "of time")

ccTimeDomain 
  = dcc "timeDomain" (nounPhraseSP "time domain")
      "The analysis of mathematical functions in terms of time"

ccLaplaceTransform
  = dcc "laplaceTransform" (cn' "Laplace transform") 
      ("An integral transform that converts a function of a real variable t " ++
         "(often time) to a function of a complex variable s (complex frequency)")

ccAbsTolerance
  = dcc "absoluteTolerance" (nounPhraseSP "Absolute Tolerance") 
      "Absolute tolerance for the integrator"

ccRelTolerance
  = dcc "relativeTolerance" (nounPhraseSP "Relative Tolerance") 
      "Relative tolerance for the integrator"

ccTransferFxn
  = dcc "transferFxn" (nounPhraseSP "Transfer Function")
      ("The Transfer Function of a system is the ratio of the output to the input"
         ++ " functions in the frequency domain")

ccDampingCoeff
  = dcc "dampingCoeff" (nounPhraseSP "Damping Coefficient")
      "Quantity that characterizes a second order system's oscillatory response"

ccStiffCoeff
  = dcc "stiffnessCoeff" (nounPhraseSP "Stiffness Coefficient")
      "Quantity that characterizes a spring's stiffness"

concepts :: [IdeaDict]
concepts = map nw defs

defs :: [ConceptChunk]
defs
  = [pidCL, pidC, summingPt, powerPlant, secondOrderSystem, processError,
     simulationTime, processVariable, setPoint, propGain, derGain, propControl,
    derControl, ccFrequencyDomain, ccTimeDomain, ccLaplaceTransform, controlVariable, stepTime,
     ccAbsTolerance, ccRelTolerance, ccTransferFxn, ccDampingCoeff,
     ccStiffCoeff]


