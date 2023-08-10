{-# LANGUAGE PostfixOperators #-}
module Drasil.DblPend.Assumptions (twoDMotion, cartSys, cartSysR,
  yAxisDir, startOriginSingle, startOriginDouble, firstPend, secondPend, assumpSingle, assumpDouble) where
    
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis, direction, origin, positive)
import Data.Drasil.Concepts.Physics (gravity, twoD, pendulum)
import Drasil.DblPend.Concepts (pendMotion, firstRod, secondRod, firstObject, secondObject)

assumpBasic :: [ConceptInstance]
assumpBasic = [twoDMotion, cartSys, cartSysR, yAxisDir]

assumpSingle :: [ConceptInstance]
assumpSingle = assumpBasic ++ [startOriginSingle]

assumpDouble :: [ConceptInstance]
assumpDouble = assumpSingle ++ [startOriginDouble, firstPend, secondPend]

twoDMotion, cartSys, cartSysR, yAxisDir, startOriginSingle, startOriginDouble,
  firstPend, secondPend:: ConceptInstance 

twoDMotion        = cic "twoDMotion"    twoDMotionDesc          "twoDMotion"    assumpDom
cartSys           = cic "cartSys"       cartSysDesc             "cartSys"       assumpDom
cartSysR          = cic "cartSysR"      cartSysRDesc            "cartSysR"      assumpDom
yAxisDir          = cic "yAxisDir"      yAxisDirDesc            "yAxisDir"      assumpDom
startOriginSingle = cic "startOrigin"   startOriginDescSingle   "startOrigin"   assumpDom
startOriginDouble = cic "startOrigin"   startOriginDescDouble   "startOrigin"   assumpDom
firstPend         = cic "firstPend"     firstPendDesc           "firstPend"     assumpDom
secondPend        = cic "secondPend"    secondPendDesc          "secondPend"    assumpDom

twoDMotionDesc :: Sentence
twoDMotionDesc = atStartNP (the pendMotion) `S.is` phrase twoD +:+. sParen (getAcc twoD)

cartSysDesc :: Sentence
cartSysDesc = atStartNP (a_ cartesian) `S.is` (S "used" !.)

cartSysRDesc :: Sentence
cartSysRDesc = atStartNP (the cartesian) `S.is` S "right-handed where" +:+ 
    phraseNP (combineNINP positive (xAxis `and_` yAxis)) +:+. S "point right up"

yAxisDirDesc :: Sentence
yAxisDirDesc = atStartNP (direction `the_ofThe` yAxis) `S.is` S "directed opposite to" +:+. phrase gravity

startOriginDescDouble :: Sentence
startOriginDescDouble = atStartNP (the firstRod) `S.is` S "attached" `S.toThe` (phrase origin !.)

startOriginDescSingle :: Sentence
startOriginDescSingle = atStartNP (the pendulum) `S.is` S "attached" `S.toThe` (phrase origin !.)

firstPendDesc:: Sentence
firstPendDesc = foldlSent[
  atStartNP (the firstRod) +:+. S "has two sides", 
  S "One side attaches" `S.toThe` (phrase origin !.), 
  S "Another side attaches" `S.toThe` phrase firstObject]

secondPendDesc:: Sentence
secondPendDesc = foldlSent[
  atStartNP (the secondRod) +:+. S "has two sides", 
  S "One side attaches" `S.toThe` (phrase firstObject !.),
  S "Another side attaches" `S.toThe` phrase secondObject]
