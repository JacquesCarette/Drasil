-- | Defines theories in the field of Physics.
module Data.Drasil.Theories.Physics where

import Language.Drasil
import Utils.Drasil (weave)
import Theory.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Citations (velocityWiki, accelerationWiki)
import Data.Drasil.Concepts.Documentation (component, material_, value, constant)
import Data.Drasil.Concepts.Math (cartesian, equation, vector)
import Data.Drasil.Concepts.Physics (gravity, twoD, rigidBody)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, velocity,
  force, gravitationalAccel, pressure, torque, weight, positionVec, momentOfInertia,
  angularAccel, speed)
import Data.Drasil.Equations.Defining.Physics
import Data.Drasil.Equations.Defining.Derivations

-- | Collects theoretical models defined in this file.
physicsTMs :: [TheoryModel]
physicsTMs = [newtonSL]

-- * Newton's Second Law of Motion

newtonSL :: TheoryModel
newtonSL = tmNoRefs (equationalModelU "newtonSL" newtonSLQD)
  [dqdWr QP.force, dqdWr QPP.mass, dqdWr QP.acceleration] ([] :: [ConceptChunk])
  [newtonSLQD] [] [] "NewtonSecLawMot" [newtonSLDesc]

-- * Weight

weightGD :: GenDefn
weightGD = gd (equationalModel' weightQD) (getUnit QP.weight) (Just weightDeriv) [dRef weightSrc] 
  "weight" [{-Notes-}]

weightQD :: ModelQDef
weightQD = mkQuantDef' QP.weight (nounPhraseSP "weight") weightEqn

weightSrc :: Reference
weightSrc = makeURI "weightSrc" "https://en.wikipedia.org/wiki/Weight" $
  shortname' $ S "Definition of Weight"

weightDeriv :: Derivation
weightDeriv = mkDerivName (phrase QP.weight) $ weave [weightDerivSentences, weightDerivEqns]

weightDerivSentences, weightDerivEqns :: [Sentence]
weightDerivSentences = map foldlSentCol [weightDerivAccelSentence, 
  weightDerivNewtonSentence, weightDerivReplaceMassSentence, 
  weightDerivSpecWeightSentence]
weightDerivEqns = map eS [weightDerivAccelEqn, weightDerivNewtonEqn, 
  weightDerivReplaceMassEqn, weightDerivSpecWeightEqn]

weightDerivAccelSentence :: [Sentence]
weightDerivAccelSentence = [S "Under the influence" `S.of_` phrase gravity `sC` 
  S "and assuming a", short twoD, phrase cartesian, S "with down as positive" `sC`
  S "an object has an", phrase QP.acceleration, phrase vector, S "of"]

weightDerivNewtonSentence :: [Sentence]
weightDerivNewtonSentence = [S "Since there is only one non-zero", 
  phrase vector, phrase component `sC` S "the scalar", phrase value, 
  ch QP.weight, S "will be used for the" +:+. phrase QP.weight,
  S "In this scenario" `sC` phrase newtonSL, S "from", refS newtonSL, 
  S "can be expressed as"]

weightDerivReplaceMassSentence :: [Sentence]
weightDerivReplaceMassSentence = [atStart QPP.mass, S "can be expressed as",
  phrase QPP.density, S "multiplied by", phrase QPP.vol `sC` S "resulting in"]

weightDerivSpecWeightSentence :: [Sentence]
weightDerivSpecWeightSentence = [S "Substituting", phrase QPP.specWeight, 
  S "as the product" `S.of_` phrase QPP.density `S.and_` phrase QP.gravitationalAccel,
  S "yields"]

-- * Pressure

hsPressureGD :: GenDefn
hsPressureGD = gd (equationalModel' hsPressureQD) (getUnit QP.pressure) Nothing
  [dRef hsPressureSrc] "hsPressure" [hsPressureNotes]

hsPressureQD :: ModelQDef
hsPressureQD = mkQuantDef' QP.pressure (nounPhraseSP "hydrostatic pressure") hsPressureEqn

hsPressureSrc :: Reference
hsPressureSrc = makeURI "hsPressureSrc" "https://en.wikipedia.org/wiki/Pressure" $
  shortname' $ S "Definition of Pressure"

hsPressureNotes :: Sentence
hsPressureNotes = S "This" +:+ phrase equation +:+ S "is derived from" +:+
  S "Bernoulli's" +:+ phrase equation +:+ S "for a slow moving fluid" +:+
  S "through a porous" +:+. phrase material_

-- * Torque

torqueDD :: DataDefinition
torqueDD = ddENoRefs torque Nothing "torque" [torqueDesc] 

torque :: SimpleQDef
torque = mkQuantDef QP.torque torqueEqn

torqueEqn :: Expr
torqueEqn = sy QP.positionVec `cross` sy QP.force

torqueDesc :: Sentence
torqueDesc = foldlSent [S "The", phrase torque, 
  S "on a body measures the", S "tendency" `S.of_` S "a", phrase QP.force, 
  S "to rotate the body around an axis or pivot"]

-- * Vector Magnitude

vecMagQD :: SimpleQDef
vecMagQD = mkQuantDef QP.speed speedEqn

magNote :: Sentence
magNote = foldlSent [S "For a given", phrase QP.velocity, S "vector", ch QP.velocity `sC`
  S "the magnitude of the vector", sParen (eS speedEqn) `S.isThe`
  S "scalar called", phrase QP.speed]

vecMag :: DataDefinition
vecMag = ddENoRefs vecMagQD Nothing "vecMag" [magNote]

-- * Newton's Second Law of Rotational Motion

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (equationalModelU "newtonSLR" newtonSLRQD)
  [dqdWr QP.torque, dqdWr QP.momentOfInertia, dqdWr QP.angularAccel] 
  ([] :: [ConceptChunk]) [newtonSLRQD] [] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRQD :: ModelQDef
newtonSLRQD = mkQuantDef' QP.torque (nounPhraseSP "Newton's second law for rotational motion") newtonSLRExpr

newtonSLRExpr :: ExprC r => r
newtonSLRExpr = sy QP.momentOfInertia $* sy QP.angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = [foldlSent
  [S "The net", getTandS QP.torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS QP.angularAccel `sC` S "where",
   ch QP.momentOfInertia, S "denotes", phrase QP.momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"]]

-- * Acceleration

accelerationTM :: TheoryModel
accelerationTM = tm (equationalModelU "accelerationTM" accelerationQD)
  ([] :: [DefinedQuantityDict]) ([] :: [ConceptChunk]) [accelerationQD] [] []
  [dRef accelerationWiki] "acceleration" []

-- * Velocity

velocityTM :: TheoryModel
velocityTM = tm (equationalModelU "velocityTM" velocityQD)
  ([] :: [DefinedQuantityDict]) ([] :: [ConceptChunk]) [velocityQD] [] []
  [dRef velocityWiki] "velocity" []
