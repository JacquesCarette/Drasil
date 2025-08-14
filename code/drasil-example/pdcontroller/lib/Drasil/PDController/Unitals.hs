module Drasil.PDController.Unitals where

import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (second)
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.PDController.Concepts
import Control.Lens ((^.))

syms, symFS, symFt, syminvLaplace, symKd, symKp,
       symYT, symYS, symYrT, symYrS, symET, symES, symPS, symDS, symHS,
       symCT, symCS, symTStep, symTSim, symAbsTol, symRelTol,
       symDampingCoeff, symStifnessCoeff :: Symbol

symFS            = sub (variable "F") $ label "s"
syminvLaplace    = variable "L⁻¹[F(s)]"
syms             = variable "s"
symFt            = sub (variable "f") $ label "t"
symKd            = sub (variable "K") $ label "d"
symKp            = sub (variable "K") $ label "p"
symYrT           = sub (variable "r") $ label "t"
symYrS           = sub (variable "R") $ label "s"
symYT            = sub (variable "y") $ label "t"
symYS            = sub (variable "Y") $ label "s"
symET            = sub (variable "e") $ label "t"
symES            = sub (variable "E") $ label "s"
symPS            = sub (variable "P") $ label "s"
symDS            = sub (variable "D") $ label "s"
symHS            = sub (variable "H") $ label "s"
symCT            = sub (variable "c") $ label "t"
symCS            = sub (variable "C") $ label "s"
symTStep         = sub (variable "t") $ label "step"
symTSim          = sub (variable "t") $ label "sim"
symAbsTol        = variable "AbsTol"
symRelTol        = variable "RelTol"
symDampingCoeff  = variable "c"
symStifnessCoeff = variable "k"

symbols :: [DefinedQuantityDict]
symbols
  = [dqdLaplaceTransform, dqdFreqDomain, dqdFxnTDomain,
     dqdInvLaplaceTransform, dqdPropGain, dqdDerivGain, dqdSetPointTD, dqdSetPointFD,
     dqdProcessVariableTD, dqdProcessVariableFD, dqdProcessErrorTD,
     dqdProcessErrorFD, dqdDerivativeControlFD, dqdPropControlFD,
     dqdTransferFunctionFD, dqdCtrlVarTD, dqdCtrlVarFD, dqdStepTime, dqdSimTime,
     dqdDampingCoeff, dqdStiffnessCoeff]

dqdLaplaceTransform, dqdFreqDomain, dqdFxnTDomain,
                    dqdInvLaplaceTransform, dqdPropGain, dqdDerivGain,
                    dqdSetPointTD, dqdSetPointFD, dqdProcessVariableTD,
                    dqdProcessVariableFD, dqdProcessErrorTD, dqdProcessErrorFD,
                    dqdPropControlFD, dqdDerivativeControlFD,
                    dqdTransferFunctionFD, dqdCtrlVarFD, dqdCtrlVarTD, dqdStepTime,
                    dqdSimTime, dqdDampingCoeff, dqdStiffnessCoeff, dqdAbsTol, dqdRelTol :: DefinedQuantityDict

inputs :: [DefinedQuantityDict]
inputs = [dqdSetPointTD, dqdDerivGain, dqdPropGain, dqdStepTime, dqdSimTime]

outputs :: [DefinedQuantityDict]
outputs = [dqdProcessVariableTD]

inputsUC :: [UncertQ]
inputsUC
  = [ipSetPtUnc, ipPropGainUnc, ipDerGainUnc, ipStepTimeUnc, ipSimTimeUnc]

inpConstrained :: [ConstrConcept]
inpConstrained
  = [ipPropGain, ipDerivGain, ipSetPt, ipStepTime, ipSimTime, opProcessVariable]

ipPropGain, ipDerivGain, ipSetPt, ipStepTime, ipSimTime, opProcessVariable ::
            ConstrConcept

ipSetPtUnc, ipPropGainUnc, ipDerGainUnc, ipStepTimeUnc, ipSimTimeUnc :: UncertQ

ipPropGain
  = constrained' (dqdNoUnit propGain symKp Real) [gtZeroConstr] (exactDbl 20)
ipPropGainUnc = uq ipPropGain defaultUncrt
dqdPropGain = dqdWr ipPropGain

ipDerivGain
  = constrained' (dqdNoUnit derGain symKd Real) [physRange $ UpFrom (Inc, exactDbl 0)]
      (exactDbl 1)
ipDerGainUnc = uq ipDerivGain defaultUncrt
dqdDerivGain = dqdWr ipDerivGain

ipSetPt = constrained' (dqdNoUnit setPoint symYrT Real) [gtZeroConstr] (exactDbl 1)
ipSetPtUnc = uq ipSetPt defaultUncrt
dqdSetPointTD = dqdWr ipSetPt

--FIXME: the original timeStep is 0.01, this will trigger an error in Java ODE solver
--change it from 0.01 to 0.001 is a temporary fix to make ODE solver working
ipStepTime = constrained' (uc stepTime symTStep Real second)
  [physRange $ Bounded (Inc, frac 1 1000) (Exc, sy ipSimTime)]
  (dbl 0.001)
ipStepTimeUnc = uq ipStepTime defaultUncrt
dqdStepTime = dqdWr ipStepTime

ipSimTime
  = constrained' (uc simulationTime symTSim Real second)
      [physRange $ Bounded (Inc, exactDbl 1) (Inc, exactDbl 60)]
      (exactDbl 10)
ipSimTimeUnc = uq ipSimTime defaultUncrt
dqdSimTime = dqdWr ipSimTime

odeAbsTolConst, odeRelTolConst :: ConstQDef

pidConstants :: [ConstQDef]
pidConstants = [odeAbsTolConst, odeRelTolConst]

pidDqdConstants :: [DefinedQuantityDict]
pidDqdConstants = [dqdAbsTol, dqdRelTol]

dqdAbsTol = dqdNoUnit ccAbsTolerance symAbsTol Real
dqdRelTol = dqdNoUnit ccRelTolerance symRelTol Real

odeAbsTolConst = mkQuantDef dqdAbsTol (dbl 1.0e-10)
odeRelTolConst = mkQuantDef dqdRelTol (dbl 1.0e-10)

opProcessVariable
  = constrained' (dqdNoUnit processVariable symYT Real)
      [gtZeroConstr]
      (exactDbl 1)
dqdProcessVariableTD = dqdWr opProcessVariable

dqdSetPointFD
  = dqdNoUnit (dcc "dqdSetPointFD" (setPoint `inThe` ccFrequencyDomain)
    "the set point in the frequency domain") symYrS Real

dqdProcessVariableFD = dqdNoUnit (dcc "dqdProcessVariableFD"
  (processVariable `inThe` ccFrequencyDomain) "the process variable in the frequency domain") symYS Real

dqdProcessErrorTD
  = dqdNoUnit (dcc "dqdProcessErrorTD"
      (nounPhraseSent (S "Process Error in the time domain"))
      "the process error in the time domain") symET Real

dqdProcessErrorFD = dqdNoUnit (dcc "dqdProcessErrorFD" (processError `inThe`
  ccFrequencyDomain) "the process error in the time domain") symES Real

dqdPropControlFD  = dqdNoUnit (dcc "dqdPropControlFD" (propControl `inThe`
  ccFrequencyDomain) "the proportional control in the frequency domain") symPS Real

dqdDerivativeControlFD = dqdNoUnit (dcc "dqdDerivativeControlFD" (derControl `inThe`
  ccFrequencyDomain) "the derivative control in the frequency domain") symDS Real

dqdTransferFunctionFD = dqdNoUnit (dcc "dqdTransferFunctionFD" (ccTransferFxn `inThe`
  ccFrequencyDomain) "the transfer function ") symHS Real

dqdCtrlVarTD
  = dqdNoUnit (dcc "dqdCtrlVarTD" (nounPhraseSent (S "Control Variable in the time domain"))
      "the control variable in the time domain")
      symCT
      Real

dqdCtrlVarFD = dqdNoUnit (dcc "dqdCtrlVarFD" (controlVariable `inThe`
  ccFrequencyDomain) "the control variable in the frequency domain") symCS Real

dqdLaplaceTransform
  = dqdNoUnit (dcc "dqdLaplaceTransform"
      (nounPhraseSent (S "Laplace Transform of a function"))
      "the laplace transform of a function")
      symFS
      Real

dqdFreqDomain
  = dqdNoUnit (dcc "dqdFreqDomain" (nounPhraseSent (S "Complex frequency-domain parameter"))
      "the complex frequency-domain parameter")
      syms
      Real
      
dqdFxnTDomain
  = dqdNoUnit (dcc "dqdFxnTDomain" (nounPhraseSent (S "Function in the time domain"))
      "a function in the time domain") symFt
      Real

dqdInvLaplaceTransform
  = dqdNoUnit (dcc "dqdInvLaplaceTransform"
      (nounPhraseSent (S "Inverse Laplace Transform of a function"))
      "the inverse Laplace transform of a function")
      syminvLaplace
      Real

dqdDampingCoeff
  = dqdNoUnit (dcc "dqdDampingCoeff" (nounPhraseSent (S "Damping coefficient of the spring"))
      "the damping coefficient of the spring")
      symDampingCoeff
      Real

-- TODO: Create a separate description for the stiffness coefficient to state
-- that it is the "stiffness coefficient of the spring" (#4275)
dqdStiffnessCoeff
  = dqd (dccWDS "dqdStiffnessCoeff" (ccStiffCoeff ^. term) (ccStiffCoeff ^. defn))
      symStifnessCoeff
      Real
      second
