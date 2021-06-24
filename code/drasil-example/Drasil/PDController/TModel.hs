{-# LANGUAGE PostfixOperators #-}
module Drasil.PDController.TModel where

import Data.Drasil.Quantities.PhysicalProperties (mass)
import Data.Drasil.Quantities.Physics (time)
import Drasil.PDController.Assumptions
import Drasil.PDController.Concepts
import Drasil.PDController.References
import Language.Drasil
import qualified Language.Drasil as DrasilLang
import Theory.Drasil (TheoryModel, tm, ModelKinds(OthModel))
import Utils.Drasil
import Utils.Drasil.Concepts
import qualified Utils.Drasil.Sentence as S
import Data.Drasil.Citations(laplaceWiki)
import Drasil.PDController.Unitals

theoreticalModels :: [TheoryModel]
theoreticalModels = [tmLaplace, tmInvLaplace, tmSOSystem]

tmLaplace :: TheoryModel
tmLaplace
  = tm (OthModel laplaceRC)
      [qw qdLaplaceTransform, qw qdFreqDomain, qw time, qw qdPosInf,
       qw qdFxnTDomain]
      ([] :: [ConceptChunk])
      []
      [toDispExpr laplaceRel]
      []
      [makeCite laplaceWiki]
      "laplaceTransform"
      [laplaceDesc]

laplaceRC :: RelationConcept
laplaceRC = makeRC "laplaceRC" (cn' "Laplace Transform") EmptyS laplaceRel

laplaceRel :: Relation
laplaceRel
  = sy qdLaplaceTransform $=
      defint (eqSymb time) (sy qdNegInf) (sy qdPosInf) (sy qdFxnTDomain 
      `mulRe` DrasilLang.exp (neg (sy qdFreqDomain) `mulRe` sy time))

laplaceDesc :: Sentence
laplaceDesc
  = foldlSent
      [(S "Bilateral Laplace Transform" !.),
       atStartNP (theGen atStart' ccLaplaceTransform),
         S "are typically inferred from a pre-computed table of", titleize' ccLaplaceTransform,
         sParen (makeCiteS laplaceWiki)]

--------

tmInvLaplace :: TheoryModel
tmInvLaplace
  = tm (OthModel invlaplaceRC)
      [qw qdLaplaceTransform, qw qdFreqDomain, qw time, qw qdPosInf,
       qw qdFxnTDomain]
      ([] :: [ConceptChunk])
      []
      [toDispExpr invLaplaceRel]
      []
      [makeCite laplaceWiki]
      "invLaplaceTransform"
      [invLaplaceDesc]

invlaplaceRC :: RelationConcept
invlaplaceRC
  = makeRC "invLaplaceRC" (cn' "Inverse Laplace Transform") EmptyS invLaplaceRel

invLaplaceRel :: Relation
invLaplaceRel = sy qdFxnTDomain $= sy qdInvLaplaceTransform

invLaplaceDesc :: Sentence
invLaplaceDesc
  = foldlSent
      [(S "Inverse Laplace Transform of F(S)" !.),
       S "The Inverse Laplace transforms are",
         S "typically inferred from a pre-computed table of Laplace Transforms",
         sParen (makeCiteS laplaceWiki)]

--------

tmSOSystem :: TheoryModel
tmSOSystem
  = tm (OthModel tmSOSystemRC)
      [qw mass, qw qdDampingCoeff, qw qdStiffnessCoeff, qw qdFreqDomain]
      ([] :: [ConceptChunk])
      []
      [toDispExpr soSystemRel]
      []
      [makeCite abbasi2015]
      "tmSOSystem"
      [soSystemDesc]

tmSOSystemRC :: RelationConcept
tmSOSystemRC
  = makeRC "tmSOSystemRC" (cn' "Second Order Mass-Spring-Damper System") EmptyS
      soSystemRel

soSystemRel :: Relation
soSystemRel
  = exactDbl 1 
    $/ (sy mass `mulRe` square (sy qdFreqDomain) 
    `addRe` (sy qdDampingCoeff `mulRe` sy qdFreqDomain)
    `addRe` sy qdStiffnessCoeff)

soSystemDesc :: Sentence
soSystemDesc
  = foldlSent
      [atStartNP (the ccTransferFxn), 
        fromSource apwrPlantTxFnx
        `S.ofA` phrase secondOrderSystem,
        sParen (S "mass-spring-damper"), 
        S "is characterized by this equation"]

       --------
-- References --
tModRefs :: [Reference]
tModRefs = map rw theoreticalModels