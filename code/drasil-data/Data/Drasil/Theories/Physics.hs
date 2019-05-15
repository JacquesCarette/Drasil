module Data.Drasil.Theories.Physics where

import Language.Drasil
import Theory.Drasil (GenDefn, TheoryModel, gd, tmNoRefs)
import Data.Drasil.Utils (weave)
import Data.Drasil.SentenceStructures (foldlSent, foldlSentCol, ofThe, sAnd, 
  sOf)
import Data.Drasil.Concepts.Documentation (body, component, constant, value)
import Data.Drasil.Concepts.Math (vector)
import Data.Drasil.Concepts.Physics (cartesian, twoD)
import qualified Data.Drasil.Quantities.Math as QM (unitVectj)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, force, 
  gravitationalAccel, weight)

physicsTMs :: [TheoryModel]
physicsTMs = [newtonSL]

newtonSL :: TheoryModel
newtonSL = tmNoRefs (cw newtonSLRC)
  [qw QP.force, qw QPP.mass, qw QP.acceleration] ([] :: [ConceptChunk])
  [] [(sy QP.force) $= (sy QPP.mass) * (sy QP.acceleration)] []
  "NewtonSecLawMot" [newtonSLDesc]

newtonSLRC :: RelationConcept
newtonSLRC = makeRC "newtonSL" (nounPhraseSP "Newton's second law of motion")
  newtonSLDesc newtonSLRel

newtonSLRel :: Relation
newtonSLRel = (sy QP.force) $= (sy QPP.mass) * (sy QP.acceleration)

newtonSLDesc :: Sentence
newtonSLDesc = foldlSent [S "The net", (phrase QP.force), (ch QP.force),
  (sParen $ Sy $ unit_symb QP.force), S "on a", phrase body,
  S "is proportional to the", (phrase QP.acceleration),
  (ch QP.acceleration), (sParen $ Sy $ unit_symb QP.acceleration),
  S "of the", phrase body `sC` S "where", (ch QPP.mass), 
  (sParen $ Sy $ unit_symb QPP.mass), S "denotes", (phrase QPP.mass) `ofThe` 
  phrase body, S "as the", phrase constant `sOf` S "proportionality"]

--

weightGD :: GenDefn
weightGD = gd weightRC (getUnit QP.weight) weightDeriv [weightSrc] 
  "weight" [{-Notes-}]

weightRC :: RelationConcept
weightRC = makeRC "weight" (nounPhraseSP "weight") EmptyS weightEqn

weightEqn :: Relation
weightEqn = sy QP.weight $= sy QPP.vol * sy QPP.specWeight

weightSrc :: Reference
weightSrc = makeURI "weightSrc" "https://en.wikipedia.org/wiki/Weight" $
  shortname' "Definition of Weight"

weightDeriv :: Derivation
weightDeriv = weave [weightDerivSentences, weightDerivEqns]

weightDerivSentences, weightDerivEqns :: [Sentence]
weightDerivSentences = map foldlSentCol [weightDerivAccelSentence, 
  weightDerivNewtonSentence, weightDerivReplaceMassSentence, 
  weightDerivSpecWeightSentence]
weightDerivEqns = map E [weightDerivAccelEqn, weightDerivNewtonEqn, 
  weightDerivReplaceMassEqn, weightDerivSpecWeightEqn]

weightDerivAccelSentence :: [Sentence]
weightDerivAccelSentence = [S "Under the influence of gravity" `sC` 
  S "and assuming a", short twoD, phrase cartesian, 
  S "with down as positive" `sC` S "an object has an", phrase QP.acceleration, 
  phrase vector, S "of"]

weightDerivNewtonSentence :: [Sentence]
weightDerivNewtonSentence = [S "Since there is only one non-zero", 
  phrase vector, phrase component `sC` S "the scalar", phrase value, 
  ch QP.weight, S "will be used for the" +:+. phrase QP.weight,
  S "In this scenario" `sC` phrase newtonSL, S "from", makeRef2S newtonSL, 
  S "can be expressed as"]

weightDerivReplaceMassSentence :: [Sentence]
weightDerivReplaceMassSentence = [at_start QPP.mass, S "can be expressed as",
  phrase QPP.density, S "multiplied by", phrase QPP.vol `sC` S "resulting in"]

weightDerivSpecWeightSentence :: [Sentence]
weightDerivSpecWeightSentence = [S "Substituting", phrase QPP.specWeight, 
  S "as the product of", phrase QPP.density `sAnd` phrase QP.gravitationalAccel,
  S "yields"]

weightDerivAccelEqn :: Expr
weightDerivAccelEqn = sy QP.acceleration $= vec2D 0 (sy QP.gravitationalAccel * 
  sy QM.unitVectj)

weightDerivNewtonEqn :: Expr
weightDerivNewtonEqn = sy QP.weight $= sy QPP.mass * sy QP.gravitationalAccel

weightDerivReplaceMassEqn :: Expr
weightDerivReplaceMassEqn = sy QP.weight $= sy QPP.density * sy QPP.vol * sy QP.gravitationalAccel

weightDerivSpecWeightEqn :: Expr
weightDerivSpecWeightEqn = sy QP.weight $= sy QPP.vol * sy QPP.specWeight

