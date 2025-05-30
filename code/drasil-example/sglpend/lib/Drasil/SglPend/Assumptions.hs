{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.Assumptions (assumpSingle) where
    
import Language.Drasil
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (cartesian, xAxis, yAxis, direction, origin, positive)
import Data.Drasil.Concepts.Physics (gravity, twoD, pendulum)

import Drasil.DblPend.Assumptions (assumpBasic)
import Drasil.DblPend.Concepts (pendMotion, firstRod, secondRod, firstObject, secondObject)

assumpSingle :: [ConceptInstance]
assumpSingle = startOriginSingle : assumpBasic

startOriginSingle = cic "startOrigin1x" startOriginDescSingle   "startOrigin"   assumpDom

startOriginDescSingle :: Sentence
startOriginDescSingle = atStartNP (the pendulum) `S.is` S "attached" `S.toThe` (phrase origin !.)
