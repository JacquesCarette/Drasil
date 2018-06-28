module Drasil.GamePhysics.Changes where

--A list of likely and unlikely changes for GamePhysics

import Language.Drasil
import Drasil.DocumentLanguage (mkLklyChnk, mkUnLklyChnk)
import Data.Drasil.SentenceStructures (foldlSent, foldlList, getES, foldlSP, maybeChanged, maybeExpanded, sAnd)
import Data.Drasil.Concepts.Documentation (section_, likelyChg, unlikelyChg, physics, game, library)
import qualified Data.Drasil.Concepts.Math as CM (ode, constraint)
import Data.Drasil.Concepts.Computation (algorithm)
import qualified Data.Drasil.Concepts.Physics as CP (collision, damping, joint)
import Data.Drasil.Utils (enumSimple)
import qualified Drasil.SRS as SRS (likeChg, unlikeChg)

--------------------------------
--  LIKELY CHANGES --
--------------------------------

s6 :: Section
s6_intro, s6_list :: Contents

s6 = SRS.likeChg [s6_intro, s6_list] []

s6_intro = foldlSP [S "This", (phrase section_), S "lists the", 
  (plural likelyChg), S "to be made to the", (phrase game), (phrase physics), 
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

--------------------------------
--UNLIKELY CHANGES --
--------------------------------

unlikelyChanges :: Section
unlikelyChanges_intro, unlikelyChanges_list :: Contents

unlikelyChanges = SRS.unlikeChg [unlikelyChanges_intro, unlikelyChanges_list] []

unlikelyChanges_intro = foldlSP [S "This", (phrase section_), S "lists the", 
  (plural unlikelyChg), S "to be made to the", (phrase game), (phrase physics), 
  (phrase library)]

unlikeChganges_unlikelyChg_stmt1, unlikeChganges_unlikelyChg_stmt2,
  unlikeChganges_unlikelyChg_stmt3, unlikeChganges_unlikelyChg_stmt4 :: Sentence

unlikeChganges_unlikelyChg_stmt1 = foldlSent [S "The goal of the system is to simulate",
  S "the interactions of rigid bodies"]
unlikeChganges_unlikelyChg_stmt2 = foldlSent [S "There will always be a sourse of input",
  S "data external to the software"]
unlikeChganges_unlikelyChg_stmt3 = foldlSent [S "A Cartesian Coordinate system is used"]
unlikeChganges_unlikelyChg_stmt4 = foldlSent [S "All objects are regid bodies"]
  
unlikelyChanges_list' :: [Sentence]
unlikelyChanges_list' = [unlikeChganges_unlikelyChg_stmt1, unlikeChganges_unlikelyChg_stmt2,
  unlikeChganges_unlikelyChg_stmt3, unlikeChganges_unlikelyChg_stmt4]

unlikelyChanges_list = enumSimple 1 (getAcc unlikelyChg) unlikelyChanges_list'
  