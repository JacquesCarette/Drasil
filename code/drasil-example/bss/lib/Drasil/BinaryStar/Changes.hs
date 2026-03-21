module Drasil.BinaryStar.Changes (likelyChgs, unlikelyChgs) where

import Language.Drasil

import Data.Drasil.Concepts.Documentation (likeChgDom, unlikeChgDom, output_)

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
   S "may be added to support validation and analysis of the simulation results"]

ucNewtonianGravity :: ConceptInstance
ucNewtonianGravity = cic "ucNewtonianGravity"
  ucNewtonianGravityDesc "Newtonian-Gravity-Model" unlikeChgDom

ucNewtonianGravityDesc :: Sentence
ucNewtonianGravityDesc = foldlSent
  [S "The gravitational interaction model is unlikely to change" `sC`
   S "since BSS is specifically designed for Newtonian two-body dynamics"]
