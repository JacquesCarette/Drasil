module Drasil.GamePhysics.Changes where

--A list of likely and unlikely changes for GamePhysics

import Language.Drasil
import Drasil.DocumentLanguage (mkLklyChnk)
import Data.Drasil.SentenceStructures (foldlSent, foldlList, getES, foldlSP, maybeChanged, maybeExpanded, sAnd)
import Data.Drasil.Concepts.Documentation (section_, likelyChg, physics, game, library)
import qualified Data.Drasil.Concepts.Math as CM (ode, constraint)
import Data.Drasil.Concepts.Computation (algorithm)
import qualified Data.Drasil.Concepts.Physics as CP (collision, damping, joint)
import Data.Drasil.Utils (enumSimple)
import qualified Drasil.SRS as SRS (likeChg)




{--likelyChanges_SRS :: [Contents]
likelyChanges_SRS = [likelychg1{--, likelychg2, likelychg3, likelychg4--}]

likelychg1{--, likelychg2, likelychg3, likelychg4--} :: Contents
likelychg1 = mkLklyChnk "likelychg1" (lc1Desc) "ODE-Solving Algorithm"
--likelychg2 = mkLklyChnk "likelychg2" (lc2Desc) "Collisions"
--likelychg3 = mkLklyChnk "likelychg3" (lc3Desc) "Damping"
--likelychg4 = mkLklyChnk "likelychg4" (lc1Desc) "Joints and Constraints"

lc1Desc{--, 1c2Desc, 1c3Desc, 1c4Desc--} :: Sentence

lc1Desc = foldlSent[(S "internal" +:+ (getAcc CM.ode) :+: 
  S "-solving" +:+ phrase algorithm +:+ S "used by the" +:+
  (phrase library)) `maybeChanged` (S "in the future")]--}



s6 :: Section
s6_intro, s6_list :: Contents

s6 = SRS.likeChg [s6_intro, s6_list] []

s6_intro = foldlSP [S "This", (phrase section_), S "lists the", 
  (plural likelyChg), S "to be made to the", (phrase physics), (phrase game), 
  (phrase library)]

s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3, 
  s6_likelyChg_stmt4 :: Sentence

--these statements look like they could be parametrized
s6_likelyChg_stmt1 = (S "internal" +:+ (getAcc CM.ode) :+: 
  S "-solving" +:+ phrase algorithm +:+ S "used by the" +:+
  (phrase library)) `maybeChanged` (S "in the future")

s6_likelyChg_stmt2 = (phrase library) `maybeExpanded`
  (S "to deal with edge-to-edge and vertex-to-vertex" +:+ (plural CP.collision))

s6_likelyChg_stmt3 = (phrase library) `maybeExpanded` (
  S "to include motion with" +:+ (phrase CP.damping))

s6_likelyChg_stmt4 = (phrase library) `maybeExpanded` (S "to include" +:+ 
  (plural CP.joint) `sAnd` (plural CM.constraint))

s6_list' :: [Sentence]
s6_list' = [s6_likelyChg_stmt1, s6_likelyChg_stmt2, s6_likelyChg_stmt3,
  s6_likelyChg_stmt4]

s6_list = enumSimple 1 (getAcc likelyChg) s6_list'