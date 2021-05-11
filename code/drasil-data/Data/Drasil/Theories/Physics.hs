module Data.Drasil.Theories.Physics where

import Language.Drasil
import Theory.Drasil (DataDefinition, GenDefn, TheoryModel, ddNoRefs, gd,
  tmNoRefs, ModelKinds (OthModel), tm)
import Utils.Drasil

import Data.Drasil.Citations (velocityWiki, accelerationWiki)
import Data.Drasil.Concepts.Documentation (component, material_, value, constant)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (gravity, twoD, rigidBody)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, velocity, position,
  force, gravitationalAccel, pressure, torque, weight, positionVec, time, momentOfInertia,
  angularAccel)
import Data.Drasil.Equations.Defining.Physics (newtonSLRel, newtonSLRC, newtonSLDesc, weightEqn,
  weightDerivAccelEqn, weightDerivNewtonEqn, weightDerivReplaceMassEqn, weightDerivSpecWeightEqn,
  hsPressureEqn, accelerationEqn, accelerationRC, velocityEqn, velocityRC)

physicsTMs :: [TheoryModel]
physicsTMs = [newtonSL]

newtonSL :: TheoryModel
newtonSL = tmNoRefs (OthModel newtonSLRC)
  [qw QP.force, qw QPP.mass, qw QP.acceleration] ([] :: [ConceptChunk])
  [] [newtonSLRel] [] "NewtonSecLawMot" [newtonSLDesc]

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
  S "on a body measures the", S "tendency" `sOf` S "a", phrase QP.force, 
  S "to rotate the body around an axis or pivot"]

--
newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (OthModel newtonSLRRC)
  [qw QP.torque, qw QP.momentOfInertia, qw QP.angularAccel] 
  ([] :: [ConceptChunk]) [] [newtonSLRRel] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRRC :: RelationConcept
newtonSLRRC = makeRC "newtonSLRRC" 
  (nounPhraseSP "Newton's second law for rotational motion") EmptyS newtonSLRRel

newtonSLRRel :: Relation
newtonSLRRel = sy QP.torque $= sy QP.momentOfInertia * sy QP.angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = map foldlSent [
  [S "The net", getTandS QP.torque, S "on a", phrase rigidBody `sIs`
   S "proportional to its", getTandS QP.angularAccel `sC` S "where",
   ch QP.momentOfInertia, S "denotes", phrase QP.momentOfInertia `the_ofThe`
   phrase rigidBody, S "as the", phrase constant `sOf` S "proportionality"]]
--

accelerationTM :: TheoryModel
accelerationTM = tm (OthModel accelerationRC)
  [qw QP.acceleration, qw QP.velocity, qw QP.time] ([] :: [ConceptChunk]) [] [accelerationEqn] []
  [makeCite accelerationWiki] "acceleration" []

----------

velocityTM :: TheoryModel
velocityTM = tm (OthModel velocityRC)
  [qw QP.velocity, qw QP.position, qw QP.time] ([] :: [ConceptChunk]) [] [velocityEqn] []
  [makeCite velocityWiki] "velocity" []
