module Data.Drasil.Theories.Physics where

import Language.Drasil
import Theory.Drasil (DataDefinition, GenDefn, TheoryModel, ddNoRefs, gd,
  tmNoRefs, ModelKinds (OthModel))
import Utils.Drasil

import Data.Drasil.Concepts.Documentation (body, component, constant, material_,
  value)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (gravity, twoD)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, 
  force, gravitationalAccel, pressure, torque, weight, positionVec)
import Data.Drasil.Equations.Defining.Physics (newtonSLRel, weightEqn,
  weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn, weightDerivSpecWeightEqn,
  hsPressureEqn)

physicsTMs :: [TheoryModel]
physicsTMs = [newtonSL]

newtonSL :: TheoryModel
newtonSL = tmNoRefs (cw newtonSLRC)
  [qw QP.force, qw QPP.mass, qw QP.acceleration] ([] :: [ConceptChunk])
  [] [sy QP.force $= sy QPP.mass * sy QP.acceleration] []
  "NewtonSecLawMot" [newtonSLDesc]

newtonSLRC :: RelationConcept
newtonSLRC = makeRC "newtonSL" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc newtonSLRel

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", getTandS QP.force, S "on a",
  phrase body `sIs` S "proportional to", getTandS QP.acceleration `ofThe`
  phrase body `sC` S "where", ch QPP.mass, S "denotes", phrase QPP.mass `ofThe`
  phrase body, S "as the", phrase constant `sOf` S "proportionality"]

--

weightGD :: GenDefn
weightGD = gd (OthModel weightRC) (getUnit QP.weight) (Just weightDeriv) [weightSrc] 
  "weight" [{-Notes-}]

weightRC :: RelationConcept
weightRC = makeRC "weight" (nounPhraseSP "weight") EmptyS weightEqn

weightSrc :: Reference
weightSrc = makeURI "weightSrc" "https://en.wikipedia.org/wiki/Weight" $
  shortname' "Definition of Weight"

weightDeriv :: Derivation
weightDeriv = mkDerivName (phrase QP.weight) $ weave [weightDerivSentences, weightDerivEqns]

weightDerivSentences, weightDerivEqns :: [Sentence]
weightDerivSentences = map foldlSentCol [weightDerivAccelSentence, 
  weightDerivNewtonSentence, weightDerivReplaceMassSentence, 
  weightDerivSpecWeightSentence]
weightDerivEqns = map E [weightDerivAccelEqn, weightDerivNewtonEqn, 
  weightDerivReplaceMassEqn, weightDerivSpecWeightEqn]

weightDerivAccelSentence :: [Sentence]
weightDerivAccelSentence = [S "Under the influence" `sOf` phrase gravity `sC` 
  S "and assuming a", short twoD, phrase cartesian, S "with down as positive" `sC`
  S "an object has an", phrase QP.acceleration, phrase vector, S "of"]

weightDerivNewtonSentence :: [Sentence]
weightDerivNewtonSentence = [S "Since there is only one non-zero", 
  phrase vector, phrase component `sC` S "the scalar", phrase value, 
  ch QP.weight, S "will be used for the" +:+. phrase QP.weight,
  S "In this scenario" `sC` phrase newtonSL, S "from", makeRef2S newtonSL, 
  S "can be expressed as"]

weightDerivReplaceMassSentence :: [Sentence]
weightDerivReplaceMassSentence = [atStart QPP.mass, S "can be expressed as",
  phrase QPP.density, S "multiplied by", phrase QPP.vol `sC` S "resulting in"]

weightDerivSpecWeightSentence :: [Sentence]
weightDerivSpecWeightSentence = [S "Substituting", phrase QPP.specWeight, 
  S "as the product" `sOf` phrase QPP.density `sAnd` phrase QP.gravitationalAccel,
  S "yields"]

--

hsPressureGD :: GenDefn
hsPressureGD = gd (OthModel hsPressureRC) (getUnit QP.pressure) Nothing
  [hsPressureSrc] "hsPressure" [hsPressureNotes]

hsPressureRC :: RelationConcept
hsPressureRC = makeRC "hsPressure" (nounPhraseSP "hydrostatic pressure") 
  hsPressureNotes hsPressureEqn

hsPressureSrc :: Reference
hsPressureSrc = makeURI "hsPressureSrc" "https://en.wikipedia.org/wiki/Pressure" $
  shortname' "Definition of Pressure"

hsPressureNotes :: Sentence
hsPressureNotes = S "This" +:+ phrase equation +:+ S "is derived from" +:+
  S "Bernoulli's" +:+ phrase equation +:+ S "for a slow moving fluid" +:+
  S "through a porous" +:+. phrase material_

--

torqueDD :: DataDefinition
torqueDD = ddNoRefs torque Nothing "torque" [torqueDesc] 

torque :: QDefinition
torque = mkQuantDef QP.torque torqueEqn

torqueEqn :: Expr
torqueEqn = cross (sy QP.positionVec) (sy QP.force)

torqueDesc :: Sentence
torqueDesc = foldlSent [S "The", phrase torque, 
  S "on a body measures the", S "the tendency" `sOf` S "a", phrase QP.force, 
  S "to rotate the body around an axis or pivot"]
