{-# LANGUAGE PostfixOperators #-}
module Drasil.GamePhysics.TMods (tMods, newtonSL, newtonSLR, newtonTL, newtonLUG) where

import qualified Data.List.NonEmpty as NE

import Language.Drasil
import Theory.Drasil
import qualified Language.Drasil.Sentence.Combinators as S

import Drasil.GamePhysics.Assumptions (assumpOD)
import Drasil.GamePhysics.Unitals (dispNorm, dVect, force_1, force_2,
  mass_1, mass_2, sqrDist, distMass)

import Data.Drasil.Concepts.Documentation (constant)
import Data.Drasil.Concepts.Physics (rigidBody, twoD)
import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (angularAccel,
  force, gravitationalConst, momentOfInertia, torque)
import Data.Drasil.Theories.Physics (newtonSL)

----- Theoretical Models -----

tMods :: [TheoryModel]
tMods = [newtonSL, newtonTL, newtonLUG, newtonSLR]

-- T1 : Newton's second law of motion --

-- T2 : Newton's third law of motion --

newtonTL :: TheoryModel
newtonTL = tmNoRefs (equationalModel' newtonTLQD) [dqdWr force_1, dqdWr force_2]
  ([] :: [ConceptChunk]) [newtonTLQD] [] [] "NewtonThirdLawMot" [newtonTLNote]

newtonTLQD :: ModelQDef
newtonTLQD = mkQuantDef' force_1 (nounPhraseSP "Newton's third law of motion") newtonTLExpr

newtonTLExpr :: PExpr
newtonTLExpr = neg (sy force_2)

newtonTLNote :: Sentence
newtonTLNote = foldlSent [(S "Every action has an equal and opposite reaction" !.),
  S "In other words, the", phrase force, ch force_1, S "exerted on the second",
  phrase rigidBody, S "by the first is equal in magnitude and" `S.inThe` S "opposite direction" `S.toThe`
  phrase force, ch force_2, S "exerted on the first", phrase rigidBody, S "by the second"]

-- T3 : Newton's law of universal gravitation --

-- FIXME: Missing ConceptDomain!
newtonLUGModel :: ModelKind ModelExpr
newtonLUGModel = equationalRealm' $ mkMultiDefnForQuant newtonForceQuant EmptyS $ NE.fromList [
    mkDefiningExpr "newtonLUGviaDeriv" [] EmptyS (sy gravitationalConst $* (sy mass_1 $* sy mass_2 $/ square (sy dispNorm)) $* sy dVect),
    mkDefiningExpr "newtonLUGviaForm"  [] EmptyS (sy gravitationalConst $* (sy mass_1 $* sy mass_2 $/ square (sy dispNorm)) $* (sy distMass $/ sy dispNorm))
  ]

newtonLUG :: TheoryModel
newtonLUG = tmNoRefs newtonLUGModel
  [dqdWr force, dqdWr gravitationalConst, dqdWr mass_1, dqdWr mass_2,
  dqdWr dispNorm, dqdWr dVect, dqdWr distMass] ([] :: [ConceptChunk])
  [] [express newtonLUGModel] [] "UniversalGravLaw" newtonLUGNotes

newtonForceQuant :: DefinedQuantityDict
newtonForceQuant = dqd' (dccA "force" (nounPhraseSP "Newton's law of universal gravitation") "Newton's law of universal gravitation" Nothing) (symbol force) Real Nothing

-- Can't include fractions within a sentence (in the part where 'r denotes the
-- unit displacement vector, equivalent to r/||r||' (line 184)). Changed to a
-- verbal description instead.

-- Can't properly include the gravitational constant in a sentence (in the last
-- sentence, supposed to include "6.673 $* 10^{-11} m/kgs^2" (line 187)).

newtonLUGNotes :: [Sentence]
newtonLUGNotes = [foldlSent
  [S "Two", plural rigidBody `S.inThe` S "universe attract each other with a",
   getTandS force, S "that is directly proportional" `S.toThe` S "product of their",
   plural mass `sC` ch mass_1 `S.and_` ch mass_2 `sC` EmptyS `S.and_`
   S "inversely proportional" `S.toThe` getTandS sqrDist, S "between them"]]

-- T4 : Newton's second law for rotational motion --

newtonSLR :: TheoryModel
newtonSLR = tmNoRefs (equationalModelU "newtonSLR" newtonSLRQD)
  [dqdWr torque, dqdWr momentOfInertia, dqdWr angularAccel]
  ([] :: [ConceptChunk]) [newtonSLRQD] [] [] "NewtonSecLawRotMot" newtonSLRNotes

newtonSLRQD :: ModelQDef
newtonSLRQD = mkQuantDef' torque (nounPhraseSP "Newton's second law for rotational motion") newtonSLRExpr

newtonSLRExpr :: PExpr
newtonSLRExpr = sy momentOfInertia $* sy angularAccel

newtonSLRNotes :: [Sentence]
newtonSLRNotes = map foldlSent [
  [S "The net", getTandS torque, S "on a", phrase rigidBody `S.is`
   S "proportional to its", getTandS angularAccel `sC` S "where",
   ch momentOfInertia, S "denotes", phrase momentOfInertia `S.the_ofThe`
   phrase rigidBody, S "as the", phrase constant `S.of_` S "proportionality"],
  [S "We also assume that all", plural rigidBody, S "involved" `S.are`
   phrase twoD, fromSource assumpOD]]
