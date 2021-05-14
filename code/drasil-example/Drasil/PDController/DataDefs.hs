{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.DataDefs where

import Drasil.PDController.Assumptions
import Drasil.PDController.Unitals
import Drasil.PDController.References
import Drasil.PDController.TModel
import Language.Drasil
import Theory.Drasil (DataDefinition, dd)
import Utils.Drasil

dataDefinitions :: [DataDefinition]
dataDefinitions = [ddErrSig, ddPropCtrl, ddDerivCtrl, ddCtrlVar]

----------------------------------------------

ddErrSig :: DataDefinition
ddErrSig
  = dd ddErrSigDefn [makeCite johnson2008] Nothing "ddProcessError"
      [ddErrSigNote]

ddErrSigDefn :: QDefinition
ddErrSigDefn = mkQuantDef qdProcessErrorFD ddErrSigEqn

ddErrSigEqn :: Expr
ddErrSigEqn = sy qdSetPointFD - sy qdProcessVariableFD

ddErrSigNote :: Sentence
ddErrSigNote
  = foldlSent
      [S "The Process Error is the difference between the Set-Point and " +:+.
         S "Process Variable",
       S "The equation is converted to the frequency" +:+
         S "domain by applying the Laplace transform" +:+. sParen (S "from" +:+
         makeRef2S tmLaplace),
       S "The Set-Point is assumed to be constant throughout the" +:+
         S "simulation" +:+. sParen (S "from" +:+
         makeRef2S aSP),
       S "The initial value of the Process Variable is assumed" +:+
         S "to be zero", sParen (S "from" +:+
         makeRef2S aInitialValue)]

----------------------------------------------

ddPropCtrl :: DataDefinition
ddPropCtrl
  = dd ddPropCtrlDefn [makeCite johnson2008] Nothing "ddPropCtrl"
      [ddPropCtrlNote]

ddPropCtrlDefn :: QDefinition
ddPropCtrlDefn = mkQuantDef qdPropControlFD ddPropCtrlEqn

ddPropCtrlEqn :: Expr
ddPropCtrlEqn = sy qdPropGain * sy qdProcessErrorFD

ddPropCtrlNote :: Sentence
ddPropCtrlNote
  = foldlSent
      [S "The Proportional Controller is the product of the Proportional Gain",
         S "and the Process Error" +:+. sParen (S "from" +:+
         makeRef2S ddErrSig),
       S "The equation is converted to the frequency",
         S "domain by applying the Laplace transform", sParen (S "from" +:+
         makeRef2S tmLaplace)]

----------------------------------------------

ddDerivCtrl :: DataDefinition
ddDerivCtrl
  = dd ddDerivCtrlDefn [makeCite johnson2008] Nothing "ddDerivCtrl"
      [ddDerivCtrlNote]

ddDerivCtrlDefn :: QDefinition
ddDerivCtrlDefn = mkQuantDef qdDerivativeControlFD ddDerivCtrlEqn

ddDerivCtrlEqn :: Expr
ddDerivCtrlEqn
  =  sy qdDerivGain * sy qdProcessErrorFD * sy qdFreqDomain

ddDerivCtrlNote :: Sentence
ddDerivCtrlNote
  = foldlSent
      [S "The Derivative Controller is the product of the Derivative Gain",
         S "and the differential of the Process Error" +:+. sParen (S "from" +:+
         makeRef2S ddErrSig),
       S "The equation is converted to the frequency",
         S "domain by applying the Laplace",
         S "transform" +:+. sParen (S "from" +:+
         makeRef2S tmLaplace),
       S "A pure form of the Derivative controller is used in this",
         S "application", sParen (S "from" +:+
         makeRef2S aUnfilteredDerivative)]

----------------------------------------------

ddCtrlVar :: DataDefinition
ddCtrlVar
  = dd ddCtrlVarDefn [makeCite johnson2008] Nothing "ddCtrlVar" [ddCtrlNote]

ddCtrlVarDefn :: QDefinition
ddCtrlVarDefn = mkQuantDef qdCtrlVarFD ddCtrlEqn

ddCtrlEqn :: Expr
ddCtrlEqn
  =  sy qdProcessErrorFD * (sy qdPropGain + 
        sy qdDerivGain * sy qdFreqDomain)

ddCtrlNote :: Sentence
ddCtrlNote
  = foldlSent
      [(S "The Control Variable is the output of the controller" !.),
       S "In this case" `sC` S "it is the sum of the Proportional", sParen (S "from" +:+
         makeRef2S ddPropCtrl),
         S "and Derivative", sParen (S "from" +:+
         makeRef2S ddDerivCtrl) +:+.
         S "controllers",
       S "The parallel", sParen (S "from" +:+ makeRef2S aParallelEq),
         S "and de-coupled", sParen (S "from" +:+
         makeRef2S aDecoupled),
         S "form of the PD equation is",
         S "used in this document"]

