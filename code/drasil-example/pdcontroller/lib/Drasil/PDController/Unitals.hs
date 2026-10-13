module Drasil.PDController.Unitals (
  symbols, inputs, outputs, inputsUC, inpConstrained,
  pidConstants,
  symDampingCoeff, symStifnessCoeff,
  dqdSetPointTD, dqdPropGain, dqdDerivGain, dqdSimTime, dqdStepTime,
  dqdProcessErrorFD, dqdSetPointFD, dqdProcessVariableFD, dqdPropControlFD,
  dqdDerivativeControlFD, dqdFreqDomain, dqdCtrlVarFD, dqdProcessVariableTD,
  dqdInvLaplaceTransform, dqdLaplaceTransform, dqdFxnTDomain, dqdDampingCoeff,
  dqdStiffnessCoeff,
  odeRelTolConst, odeAbsTolConst, opProcessVariable
) where

import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE

import Drasil.Database (mkUid)
import Data.Drasil.Constraints (gtZeroConstr)
import Data.Drasil.SI_Units (second)
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators

import Drasil.PDController.Concepts
import Control.Lens ((^.))

syms, symFS, symFt, syminvLaplace, symKd, symKp,
       symYT, symYS, symYrT, symYrS, symES, symPS, symDS,
       symCS, symTStep, symTSim, symAbsTol, symRelTol,
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
symES            = sub (variable "E") $ label "s"
symPS            = sub (variable "P") $ label "s"
symDS            = sub (variable "D") $ label "s"
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
     dqdProcessVariableTD, dqdProcessVariableFD,
     dqdProcessErrorFD, dqdDerivativeControlFD, dqdPropControlFD,
     dqdCtrlVarFD, dqdStepTime, dqdSimTime,
     dqdDampingCoeff, dqdStiffnessCoeff, dqdAbsTol, dqdRelTol]

dqdLaplaceTransform, dqdFreqDomain, dqdFxnTDomain,
                    dqdInvLaplaceTransform, dqdPropGain, dqdDerivGain,
                    dqdSetPointTD, dqdSetPointFD, dqdProcessVariableTD,
                    dqdProcessVariableFD, dqdProcessErrorFD,
                    dqdPropControlFD, dqdDerivativeControlFD,
                    dqdCtrlVarFD, dqdStepTime,
                    dqdSimTime, dqdDampingCoeff, dqdStiffnessCoeff, dqdAbsTol, dqdRelTol :: DefinedQuantityDict

inputs :: NE.NonEmpty DefinedQuantityDict
inputs = dqdSetPointTD :| [dqdDerivGain, dqdPropGain, dqdStepTime, dqdSimTime]

outputs :: NE.NonEmpty DefinedQuantityDict
outputs = NE.singleton dqdProcessVariableTD

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
  = constrained' dqdPropGain [gtZeroConstr] (exactDbl 20)
ipPropGainUnc = uq ipPropGain defaultUncrt
dqdPropGain = dqdNoUnit propGain symKp Real

ipDerivGain
  = constrained' dqdDerivGain [physRange $ UpFrom (Inc, exactDbl 0)]
      (exactDbl 1)
ipDerGainUnc = uq ipDerivGain defaultUncrt
dqdDerivGain = dqdNoUnit derGain symKd Real

ipSetPt = constrained' dqdSetPointTD [gtZeroConstr] (exactDbl 1)
ipSetPtUnc = uq ipSetPt defaultUncrt
dqdSetPointTD = dqdNoUnit setPoint symYrT Real

--FIXME: the original timeStep is 0.01, this will trigger an error in Java ODE solver
--change it from 0.01 to 0.001 is a temporary fix to make ODE solver working
ipStepTime = constrained' dqdStepTime
  [physRange $ Bounded (Inc, frac 1 1000) (Exc, sy ipSimTime)]
  (dbl 0.001)
ipStepTimeUnc = uq ipStepTime defaultUncrt
dqdStepTime = dqd stepTime symTStep Real second

ipSimTime
  = constrained' dqdSimTime
      [physRange $ Bounded (Inc, exactDbl 1) (Inc, exactDbl 60)]
      (exactDbl 10)
ipSimTimeUnc = uq ipSimTime defaultUncrt
dqdSimTime = dqd simulationTime symTSim Real second

odeAbsTolConst, odeRelTolConst :: ConstQDef

pidConstants :: [ConstQDef]
pidConstants = [odeAbsTolConst, odeRelTolConst]

dqdAbsTol = dqdNoUnit ccAbsTolerance symAbsTol Real
dqdRelTol = dqdNoUnit ccRelTolerance symRelTol Real

odeAbsTolConst = mkQuantDef dqdAbsTol (dbl 1.0e-10)
odeRelTolConst = mkQuantDef dqdRelTol (dbl 1.0e-10)

opProcessVariable
  = constrained' dqdProcessVariableTD
      [gtZeroConstr]
      (exactDbl 1)
dqdProcessVariableTD = dqdNoUnit processVariable symYT (Vect Real)

dqdSetPointFD
  = quantNoUnit (mkUid "dqdSetPointFD") (setPoint `inThe` ccFrequencyDomain)
    (S "the set point in the frequency domain") symYrS Real

dqdProcessVariableFD = quantNoUnit (mkUid "dqdProcessVariableFD")
  (processVariable `inThe` ccFrequencyDomain) (S "the process variable in the frequency domain") symYS Real

dqdProcessErrorFD = quantNoUnit (mkUid "dqdProcessErrorFD") (processError `inThe`
  ccFrequencyDomain) (S "the process error in the time domain") symES Real

dqdPropControlFD  = quantNoUnit (mkUid "dqdPropControlFD") (propControl `inThe`
  ccFrequencyDomain) (S "the proportional control in the frequency domain") symPS Real

dqdDerivativeControlFD = quantNoUnit (mkUid "dqdDerivativeControlFD") (derControl `inThe`
  ccFrequencyDomain) (S "the derivative control in the frequency domain") symDS Real

dqdCtrlVarFD = quantNoUnit (mkUid "dqdCtrlVarFD") (controlVariable `inThe`
  ccFrequencyDomain) (S "the control variable in the frequency domain") symCS Real

dqdLaplaceTransform
  = quantNoUnit (mkUid "dqdLaplaceTransform")
      (pn "Laplace Transform of a function")
      (S "the laplace transform of a function")
      symFS
      Real

dqdFreqDomain
  = quantNoUnit (mkUid "dqdFreqDomain") (pn "Complex frequency-domain parameter")
      (S "the complex frequency-domain parameter")
      syms
      Real

dqdFxnTDomain
  = quantNoUnit (mkUid "dqdFxnTDomain") (pn "Function in the time domain")
      (S "a function in the time domain") symFt
      Real

dqdInvLaplaceTransform
  = quantNoUnit (mkUid "dqdInvLaplaceTransform")
      (pn "Inverse Laplace Transform of a function")
      (S "the inverse Laplace transform of a function")
      syminvLaplace
      Real

dqdDampingCoeff
  = quantNoUnit (mkUid "dqdDampingCoeff") (pn "Damping coefficient of the spring")
      (S "the damping coefficient of the spring")
      symDampingCoeff
      Real

-- TODO: Create a separate description for the stiffness coefficient to state
-- that it is the "stiffness coefficient of the spring" (#4275)
dqdStiffnessCoeff
  = quant (mkUid "dqdStiffnessCoeff") (ccStiffCoeff ^. term) (ccStiffCoeff ^. defn)
      symStifnessCoeff
      Real
      second
