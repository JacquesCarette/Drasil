module Drasil.BinaryStar.Changes (likelyChgs, unlikelyChgs) where

import Language.Drasil
import Language.Drasil.Document (refS)
import qualified Language.Drasil.Sentence.Combinators as S

import Data.Drasil.Concepts.Documentation (likeChgDom, unlikeChgDom, output_)

import Drasil.BinaryStar.Assumptions (twoBody, isolated, newtonianGravity,
  nonRelativistic, pointMass, constantMass, inertialFrame, planar,
  nonzeroSeparation)

likelyChgs :: [ConceptInstance]
likelyChgs = [lcDerivedOutputs]

unlikelyChgs :: [ConceptInstance]
unlikelyChgs = [ucNewtonianGravity]

lcDerivedOutputs :: ConceptInstance
lcDerivedOutputs = cic "lcDerivedOutputs"
  lcDerivedOutputsDesc "Derived-Output-Quantities" likeChgDom

lcDerivedOutputsDesc :: Sentence
lcDerivedOutputsDesc = foldlSent
  [S "Additional derived", plural output_ `sC`
   S "such as total energy or angular momentum" `sC`
   S "may be added to support validation and analysis of the simulation results.",
   S "This change could affect results derived under all current assumptions:",
   refS twoBody `sC` refS isolated `sC` refS newtonianGravity `sC`
   refS nonRelativistic `sC` refS pointMass `sC` refS constantMass `sC`
   refS inertialFrame `sC` refS planar `S.and_` refS nonzeroSeparation]

ucNewtonianGravity :: ConceptInstance
ucNewtonianGravity = cic "ucNewtonianGravity"
  ucNewtonianGravityDesc "Newtonian-Gravity-Model" unlikeChgDom

ucNewtonianGravityDesc :: Sentence
ucNewtonianGravityDesc = foldlSent
  [S "The gravitational interaction model",
   sParen (refS newtonianGravity),
   S "is unlikely to change" `sC`
   S "since BSS is specifically designed for Newtonian two-body dynamics"]
