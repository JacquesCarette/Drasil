{-# LANGUAGE PostfixOperators #-}
module Drasil.SglPend.Assumptions (assumpSingle) where
    
import Language.Drasil (cic, phrase, atStartNP, (!.), ConceptInstance, Sentence(S))
import Language.Drasil.Chunk.Concept.NamedCombinators (the)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (assumpDom) 
import Data.Drasil.Concepts.Math (origin)
import Data.Drasil.Concepts.Physics (pendulum)

import Drasil.DblPend.Assumptions (assumpBasic)

assumpSingle :: [ConceptInstance]
assumpSingle = startOriginSingle : assumpBasic

startOriginSingle :: ConceptInstance
startOriginSingle = cic "startOrigin1x" startOriginDescSingle "startOrigin" assumpDom

startOriginDescSingle :: Sentence
startOriginDescSingle = atStartNP (the pendulum) `S.is` S "attached" `S.toThe` (phrase origin !.)
