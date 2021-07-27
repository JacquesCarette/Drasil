{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPendulum.Assumptions (twoDMotion, cartSys, cartSysR,
  yAxisDir, startOrigin, firstPend, secondPend, assumptions) where
    
import Language.Drasil
import Utils.Drasil (foldlSent)
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S

import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis, direction, origin, positive)
import Data.Drasil.Concepts.Physics (gravity, twoD)
import Drasil.DblPendulum.Concepts (pendMotion, firstRod, secondRod, firstObject)


assumptions :: [ConceptInstance]
assumptions = [twoDMotion, cartSys, cartSysR, yAxisDir, startOrigin, firstPend, secondPend]

twoDMotion, cartSys, cartSysR, yAxisDir, startOrigin, firstPend, secondPend:: ConceptInstance 

twoDMotion  = cic "twoDMotion"      twoDMotionDesc    "twoDMotion"    assumpDom
cartSys     = cic "cartSys"         cartSysDesc       "cartSys"       assumpDom
cartSysR    = cic "cartSysR"    cartSysRDesc  "cartSysR"  assumpDom
yAxisDir    = cic "yAxisDir"          yAxisDirDesc        "yAxisDir"        assumpDom
startOrigin = cic "startOrigin"       startOriginDesc     "startOrigin"     assumpDom
firstPend   = cic "firstPend"       firstPendDesc     "firstPend"     assumpDom
secondPend   = cic "secondPend"       secondPendDesc     "secondPend"     assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = atStartNP (the pendMotion) `S.is` phrase twoD +:+. sParen (getAcc twoD)

cartSysDesc :: Sentence
cartSysDesc = atStartNP (a_ cartesian) `S.is` (S "used" !.)

cartSysRDesc :: Sentence
cartSysRDesc = atStartNP (the cartesian) `S.is` S "right-handed where" +:+ 
    phraseNP (combineNINP positive (xAxis `and_` yAxis)) +:+. S "point right up"

yAxisDirDesc :: Sentence
yAxisDirDesc = atStartNP (direction `the_ofThe` yAxis) `S.is` S "directed opposite to" +:+. phrase gravity

startOriginDesc :: Sentence
startOriginDesc = atStartNP (the firstRod) `S.is` S "attached" `S.toThe` (phrase origin !.)

firstPendDesc:: Sentence
firstPendDesc = foldlSent[
  atStartNP (the firstRod) +:+. S "has two sides", 
  S "One side attaches" `S.toThe` (phrase origin !.), 
  S "Another side attaches" `S.toThe` phrase firstObject]

secondPendDesc:: Sentence
secondPendDesc = atStartNP (the secondRod) `S.is` S "attached" `S.toThe` (phrase firstObject !.)
