{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.TModel where

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (time)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.References
import Language.Drasil
import qualified Language.Drasil as DrasilLang
import Theory.Drasil (TheoryModel, tm, othModel')
import Language.Drasil.Chunk.Concept.NamedCombinators
import qualified Language.Drasil.Sentence.Combinators as S
import Data.Drasil.Citations(laplaceWiki)
import Drasil.PDController.Unitals
import Data.Drasil.Quantities.Math (posInf, negInf)

theoreticalModels :: [TheoryModel]
theoreticalModels = [tmLaplace, tmInvLaplace, tmSOSystem]

tmLaplace :: TheoryModel
tmLaplace
  = tm (othModel' laplaceRC)
      ([] :: [DefinedQuantityDict])
      ([] :: [ConceptChunk])
      []
      [express laplaceRel]
      []
      [dRef laplaceWiki]
      "laplaceTransform"
      [laplaceDesc]

laplaceRC :: RelationConcept
laplaceRC = makeRC "laplaceRC" (cn' "Laplace Transform") EmptyS laplaceRel

laplaceRel :: Relation
laplaceRel
  = sy dqdLaplaceTransform $=
      defint (eqSymb time) (sy negInf) (sy posInf) (sy dqdFxnTDomain 
      $* DrasilLang.exp (neg (sy dqdFreqDomain) $* sy time))

laplaceDesc :: Sentence
laplaceDesc
  = foldlSent
      [(S "Bilateral Laplace Transform" !.),
       atStartNP (theGen atStart' ccLaplaceTransform),
         S "are typically inferred from a pre-computed table of", titleize' ccLaplaceTransform,
         sParen (refS laplaceWiki)]

--------

tmInvLaplace :: TheoryModel
tmInvLaplace
  = tm (othModel' invlaplaceRC)
      ([] :: [DefinedQuantityDict])
      ([] :: [ConceptChunk])
      []
      [express invLaplaceRel]
      []
      [dRef laplaceWiki]
      "invLaplaceTransform"
      [invLaplaceDesc]

invlaplaceRC :: RelationConcept
invlaplaceRC
  = makeRC "invLaplaceRC" (cn' "Inverse Laplace Transform") EmptyS invLaplaceRel

invLaplaceRel :: Relation
invLaplaceRel = sy dqdFxnTDomain $= sy dqdInvLaplaceTransform

invLaplaceDesc :: Sentence
invLaplaceDesc
  = foldlSent
      [(S "Inverse Laplace Transform of F(S)" !.),
       S "The Inverse Laplace transforms are",
         S "typically inferred from a pre-computed table" `S.of_` S "Laplace Transforms",
         sParen (refS laplaceWiki)]

--------

tmSOSystem :: TheoryModel
tmSOSystem
  = tm (othModel' tmSOSystemRC)
      ([] :: [DefinedQuantityDict])
      ([] :: [ConceptChunk])
      []
      [express soSystemRel]
      []
      [dRef abbasi2015]
      "tmSOSystem"
      [soSystemDesc]

tmSOSystemRC :: RelationConcept
tmSOSystemRC
  = makeRC "tmSOSystemRC" (cn' "Second Order Mass-Spring-Damper System") EmptyS
      soSystemRel

soSystemRel :: Relation
soSystemRel
  = exactDbl 1 
    $/ (sy mass $* square (sy dqdFreqDomain) 
    $+ (sy dqdDampingCoeff $* sy dqdFreqDomain)
    $+ sy dqdStiffnessCoeff)

soSystemDesc :: Sentence
soSystemDesc
  = foldlSent
      [atStartNP (the ccTransferFxn), 
        fromSource apwrPlantTxFnx
        `S.ofA` phrase secondOrderSystem,
        sParen (S "mass-spring-damper"), 
        S "is characterized by this equation"]
