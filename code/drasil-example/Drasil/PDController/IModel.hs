{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.IModel where

import Data.Drasil.Quantities.Physics (time)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.DataDefs
import Drasil.PDController.GenDefs
import Drasil.PDController.References
import Drasil.PDController.TModel
import Language.Drasil
import Theory.Drasil (InstanceModel, im, qwC, ModelKinds (OthModel))
import Utils.Drasil
import Drasil.PDController.Unitals

instanceModels :: [InstanceModel]
instanceModels = [imPD]

----------------------------------------------

imPD :: InstanceModel
imPD
  = im (OthModel imPDRC)
      [qwC qdSetPointTD $ UpFrom (Exc, 0), qwC qdPropGain $ UpFrom (Exc, 0),
       qwC qdDerivGain $ UpFrom (Exc, 0)]
      (qw qdProcessVariableTD)
      [UpFrom (Exc, 0)]
      [makeCite abbasi2015, makeCite johnson2008]
      (Just imDeriv)
      "pdEquationIM"
      []

imPDRC :: RelationConcept
imPDRC
  = makeRC "imPDRC"
      (nounPhraseSP
         "Computation of the Process Variable as a function of time.")
      EmptyS
      eqn

eqn :: Expr
eqn
  = deriv (deriv (sy qdProcessVariableTD) time) time +
      (1 + sy qdDerivGain) * deriv (sy qdProcessVariableTD) time
      + (20 + sy qdPropGain) * sy qdProcessVariableTD
      - sy qdSetPointTD * sy qdPropGain $= 0

imDeriv :: Derivation
imDeriv
  = mkDerivName (phrase processVariable)
      (weave [imDerivStmts, map E imDerivEqns])

imDerivStmts :: [Sentence]
imDerivStmts = [derivStmt1, derivStmt2, derivStmt3, derivStmt4]

imDerivEqns :: [Expr]
imDerivEqns = [derivEqn1, derivEqn2, derivEqn3, derivEqn4]

derivStmt1 :: Sentence
derivStmt1
  = foldlSent
      [S "The Process Variable (Y(S)) in a" +:+ phrase pidCL +:+
         S "is the product of the Process Error (from "
         <> makeRef2S ddErrSig
         <> S "), Control Variable (from "
         <> makeRef2S ddCtrlVar
         <> S "), and the Power-Plant (from "
         <> makeRef2S gdPowerPlant
         <> S ")"]

derivEqn1 :: Expr
derivEqn1
  = sy qdProcessVariableFD
      $= (sy qdSetPointFD - sy qdProcessVariableFD)
      * (sy qdPropGain + sy qdDerivGain * sy qdFreqDomain)
      * (1 / (square (sy qdFreqDomain) + sy qdFreqDomain + 20))

derivStmt2 :: Sentence
derivStmt2 = foldlSent [S "Substituting the values and rearranging the equation"]

derivEqn2 :: Expr
derivEqn2
  = square (sy qdFreqDomain) * sy qdProcessVariableFD 
      + (1 + sy qdDerivGain) * sy qdProcessVariableFD * sy qdFreqDomain
      + (20 + sy qdPropGain) * sy qdProcessVariableFD
      - sy qdSetPointFD * sy qdFreqDomain * sy qdDerivGain
      - sy qdSetPointFD * sy qdPropGain $= 0

derivStmt3 :: Sentence
derivStmt3
  = S "Computing the" +:+ phrase qdInvLaplaceTransform +:+ S "(from " <>
      makeRef2S tmInvLaplace
      <> (S ") of the equation" !.)

derivEqn3 :: Expr
derivEqn3
  = deriv (deriv (sy qdProcessVariableTD) time) time +
      ((1 + sy qdDerivGain) * deriv (sy qdProcessVariableTD) time)
      + ((20 + sy qdPropGain) * sy qdProcessVariableTD)
      - sy qdDerivGain * deriv (sy qdSetPointTD) time
      - sy qdSetPointTD * sy qdPropGain $= 0

derivStmt4 :: Sentence
derivStmt4
  = foldlSent_
      [S "The Set point (r(t)) is a step function, and a constant " +:+
         S "(from "
         <> makeRef2S aSP
         <> (S ")" !.),
       S "Therefore the" +:+
         S " differential of the set point is zero. Hence the equation "
         +:+ S "reduces to,"]

derivEqn4 :: Expr
derivEqn4
  = deriv (deriv (sy qdProcessVariableTD) time) time +
      (1 + sy qdDerivGain) * deriv (sy qdProcessVariableTD) time
      + (20 + sy qdPropGain) * sy qdProcessVariableTD
      - sy qdSetPointTD * sy qdPropGain $= 0
