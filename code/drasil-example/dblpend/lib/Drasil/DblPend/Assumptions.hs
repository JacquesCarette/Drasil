{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.Assumptions (twoDMotion, cartSys, cartSysR,
  yAxisDir, assumpBasic, assumpDouble) where

import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Development as D
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumpDom)
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis, direction, positive)
import Data.Drasil.Concepts.Physics (gravity, twoD)
import Drasil.DblPend.Concepts (pendMotion)

assumpBasic :: [ConceptInstance]
assumpBasic = [twoDMotion, cartSys, cartSysR, yAxisDir]

assumpDouble :: [ConceptInstance]
assumpDouble = assumpBasic

twoDMotion, cartSys, cartSysR, yAxisDir :: ConceptInstance

twoDMotion        = cic "twoDMotion"    twoDMotionDesc          "twoDMotion"    assumpDom
cartSys           = cic "cartSys"       cartSysDesc             "cartSys"       assumpDom
cartSysR          = cic "cartSysR"      cartSysRDesc            "cartSysR"      assumpDom
yAxisDir          = cic "yAxisDir"      yAxisDirDesc            "yAxisDir"      assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = D.toSent (atStartNP (the pendMotion)) `S.is` phrase twoD +:+. sParen (short twoD)

cartSysDesc :: Sentence
cartSysDesc = D.toSent (atStartNP (a_ cartesian)) `S.is` (S "used" !.)

cartSysRDesc :: Sentence
cartSysRDesc = D.toSent (atStartNP (the cartesian)) `S.is` S "right-handed where" +:+
  D.toSent (phraseNP (combineNINP positive (xAxis `and_` yAxis))) +:+. S "point right up"

yAxisDirDesc :: Sentence
yAxisDirDesc = D.toSent (atStartNP (direction `the_ofThe` yAxis)) `S.is`
  S "directed opposite to" +:+. phrase gravity
