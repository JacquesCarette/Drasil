module Data.Drasil.Theories.Physics where

import Language.Drasil
import Data.Drasil.Utils (weave)
import Data.Drasil.SentenceStructures (foldlSent, foldlSentCol, ofThe, sAnd, 
  sOf)
import Data.Drasil.Concepts.Documentation (body, constant)
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP (density, 
  mass, specWeight, vol)
import qualified Data.Drasil.Quantities.Physics as QP (acceleration, force, 
  gravitationalAccel, weight)

physicsTMs :: [TheoryModel]
physicsTMs = [newtonSL]

newtonSL :: TheoryModel
newtonSL = tm (cw newtonSLRC)
  [qw QP.force, qw QPP.mass, qw QP.acceleration] ([] :: [ConceptChunk])
  [] [(sy QP.force) $= (sy QPP.mass) * (sy QP.acceleration)] [] [] 
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
weightGD = gd' weightRC (getUnit QP.weight) weightDeriv [weightSrc] 
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
weightDerivSentences = map foldlSentCol [weightDerivNewtonSentence, weightDerivReplaceMassSentence, weightDerivSpecWeightSentence]
weightDerivEqns = map E [weightDerivNewtonEqn, weightDerivReplaceMassEqn, weightDerivSpecWeightEqn]

weightDerivNewtonSentence :: [Sentence]
weightDerivNewtonSentence = [S "Directly applying", phrase newtonSL, S "from",
  makeRef2S newtonSL, S "to the concept of", phrase QP.weight, S "yields"]

weightDerivReplaceMassSentence :: [Sentence]
weightDerivReplaceMassSentence = [at_start QPP.mass, S "can be expressed as",
  phrase QPP.density, S "multiplied by", phrase QPP.vol `sC` S "resulting in"]

weightDerivSpecWeightSentence :: [Sentence]
weightDerivSpecWeightSentence = [S "Substituting", phrase QPP.specWeight, 
  S "as the product of", phrase QPP.density `sAnd` phrase QP.gravitationalAccel,
  S "yields"]

weightDerivNewtonEqn :: Expr
weightDerivNewtonEqn = sy QP.weight $= sy QPP.mass * sy QP.gravitationalAccel

weightDerivReplaceMassEqn :: Expr
weightDerivReplaceMassEqn = sy QP.weight $= sy QPP.density * sy QPP.vol * sy QP.gravitationalAccel

weightDerivSpecWeightEqn :: Expr
weightDerivSpecWeightEqn = sy QP.weight $= sy QPP.vol * sy QPP.specWeight

