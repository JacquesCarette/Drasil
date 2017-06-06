module Drasil.SSP.Modules where

import Language.Drasil

import Data.Drasil.Modules
import Data.Drasil.Quantities.SolidMechanics
import Data.Drasil.SentenceStructures

import Data.Drasil.Concepts.Math
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Computation
import Data.Drasil.Software.Products

import Drasil.SSP.Units
import Drasil.SSP.Defs
import Drasil.SSP.TMods

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_ctrl, mod_inputf, mod_outputf, mod_genalg,
           mod_kinadm, mod_slipslicer, mod_slipweight, mod_mp, mod_rfem,
           mod_sps, mod_sw, mod_sds, mod_rng, mod_plot]

-- HW Hiding Module imported from Drasil.Module
-- mod_hw :: ModuleChunk
-- mod_hw = M.mod_hw

-- Behaviour Hiding Module
-- mod_behav :: ModuleChunk
-- mod_behav = M.mod_behav


-- Control module
mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (S "The internal" +:+ (plural dataType') +:+ S "and" +:+ (plural algorithm))
  ssa [] [mod_inputf, mod_outputf, mod_genalg]

-- input format module
mod_inputf_desc :: ConceptChunk
mod_inputf_desc = dccWDS "mod_inputf_desc" (cn' "input format")
  (S "Reads the" +:+ (plural inDatum) +:+ S "from an" +:+ (phrase input_) +:+
   S "file, and/or prompted command line" +:+. (plural input_) +:+ (at_start' inDatum) +:+
   S "includes the x,y coordinates of the" +:+ (phrase slope) `sC` S "with a set of" +:+
   S "coordinates for each layer. Each layer's" +:+ (plural soilPrpty) +:+ S "of" +:+
   (foldlList (map (\x -> (phrase x)) $ (
    map cqs [fricAngle, cohesion, dryWeight, satWeight, elastMod]) ++ [cqs poissnsR])) +:+
   S "are stored in" +:+ (plural vector) +:+ S "of" +:+. (plural soilPrpty) +:+
   S "If a piezometric" +:+ (phrase surface_) +:+ S "exists in the" +:+ (phrase slope) +:+
   S "it's coordinates and the" +:+ (phrase waterWeight) +:+ S "are also" +:+
   S "included in the" +:+. (phrase input_) +:+ S "Lastly an expected range for" +:+
   S "the entrance and exit points of the" +:+ (phrase crtSlpSrf) +:+ S "are inputted.")

mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun ssa [] [mod_hw] (plural inDatum) mod_inputf_desc

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun ((phrase fs_rc) +:+
   S "for the critical" +:+ (phrase slip) +:+ S "calculated by the" +:+ (titleize morPrice) +:+ 
   S "Module and" +:+ (titleize rgFnElm) +:+ S "Method Module, and a" +:+
   S "plot of the" +:+ (phrase crtSlpSrf) +:+ S "on the" +:+ (phrase slope) +:+
   S "geometry, with the showing the" +:+ (phrase element) +:+
   S "displacements as calculated by the RFEM Module.")

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun ssa [] [mod_plot, mod_slipslicer, mod_mp, mod_rfem] 
  (plural outDatum) mod_outputf_desc

-- gen alg module
mod_genalg_desc :: ConceptChunk
mod_genalg_desc = dccWDS "mod_genalg_desc" (cn' "genetic algorithm")
  (S "Searches the" +:+ (phrase slope) +:+ S "for the" +:+ (phrase crtSlpSrf) +:+
   S "with the minimum" +:+ (phrase fs_rc))

mod_genalg :: ModuleChunk
mod_genalg = makeImpModule mod_genalg_desc
  ((at_start algorithm) +:+ S "to identify the" +:+ (phrase slpSrf) +:+
   S "that has the" +:+ S "minimum" +:+ (phrase fs_rc) `sC`
   S "based on the" +:+. (plural input_))
   ssa
   []
   []
   [mod_slipslicer, mod_kinadm, mod_rng, mod_slipweight, mod_mp]
   (Just mod_behav)

-- kin adm module
mod_kinadm_desc :: ConceptChunk
mod_kinadm_desc = dccWDS "mod_kinadm_desc" (cnIES "kinetic admissibility")
  (S "Some" +:+ (plural slpSrf) +:+ S "are physically unlikely or" +:+
   S "impossible to occur in a" +:+ (phrase slpSrf) `sC` S "such as" +:+
   (plural slpSrf) +:+ S "containing sharp angles, or going above the" +:+.
   (phrase slopeSrf) +:+ S "Ensures randomly generated or mutated" +:+
   (plural slope) +:+ S "from the" +:+ (titleize mod_genalg_desc) +:+
   S "Module are physically possible according to the" +:+
   S "criteria of the Kinematic Admissibility Module.")

mod_kinadm :: ModuleChunk
mod_kinadm = makeImpModule mod_kinadm_desc
  ((at_start algorithm) +:+ S "to determine if a given" +:+
   (phrase slpSrf) +:+ S "passes or fails a set of admissibility criteria.")
   ssa
   []
   []
   []
   (Just mod_behav)

-- slip slicer module
mod_slipslicer_desc :: ConceptChunk
mod_slipslicer_desc = dccWDS "mod_slipslicer_desc" (cn' "slip slicer")
  (S "When preparing a" +:+ (phrase slpSrf) +:+ S "for analysis by the" +:+
   (titleize morPrice) +:+ S "Module or the RFEM Module" `sC`
   S "the x-coordinates defining the boundaries of the" +:+
   (plural slice) +:+ S "are identified and stored in a" +:+. (phrase vector))

mod_slipslicer :: ModuleChunk
mod_slipslicer = makeImpModule mod_slipslicer_desc
  ((at_start algorithm) +:+ S "to determine the coordinates of where the" +:+
   (phrase slpSrf) +:+ (phrase intrslce) +:+ S "nodes occur.")
   ssa
   []
   []
   []
   (Just mod_behav)

-- slip weighting module
mod_slipweight_desc :: ConceptChunk
mod_slipweight_desc = dccWDS "mod_slipweight_desc" (cn' "slip weighting")
  (S "Weights a set of" +:+ (plural slpSrf) +:+ S "generated by the" +:+
   (titleize mod_genalg_desc) +:+ S "Module based on their" +:+.
   (plural fs_rc) +:+ S "A" +:+ (phrase slpSrf) +:+ S "with a low" +:+
   (phrase fs_rc) +:+ S "will have a high weight as it is more" +:+
   S "likely to be or to lead to generation of the" +:+. (phrase crtSlpSrf))

mod_slipweight :: ModuleChunk
mod_slipweight = makeImpModule mod_slipweight_desc
  (S "The weighting for each" +:+ (phrase slpSrf) +:+ S "in a set of" +:+
   (plural slpSrf) `sC` S "based on each" +:+ (phrase slpSrf) :+: S "'s" +:+. 
   (phrase fs_rc)) --FIXME: use possesive noun function in line above
  ssa
  []
  []
  []
  (Just mod_behav)

-- morg price solver module
mod_mp_desc :: ConceptChunk
mod_mp_desc = dccWDS "mod_mp_desc" (cn "morgenstern price solver")
  (S "Calculates the" +:+ (phrase fs_rc) +:+ S "of a given" +:+
   (phrase slpSrf) `sC`S "through implementation of a" +:+ (titleize morPrice) +:+
   (phrase ssa) +:+. (phrase method_))

mod_mp :: ModuleChunk
mod_mp = makeImpModule mod_mp_desc
  (S "The" +:+ (phrase fs_rc) +:+ S "of a given" +:+. (phrase slpSrf))
  ssa
  []
  []
  [mod_sps]
  (Just mod_behav)

-- rfem solver module
mod_rfem_desc :: ConceptChunk
mod_rfem_desc = dccWDS "mod_rfem_desc" (cn' "RFEM solver")
  (S "Calculate the global" +:+ (phrase fs_rc) `sC` S "local" +:+
   (phrase slice) +:+ (plural fs_rc) `sC` S "and local" +:+
   (phrase slice) +:+ S "displacements of a given" +:+ (phrase slpSrf) +:+
   S "under given conditions, through implementation of a" +:+
   (phrase rgFnElm) +:+ (phrase ssa) +:+. (phrase method_))

mod_rfem :: ModuleChunk
mod_rfem = makeImpModule mod_rfem_desc
  (S "The" +:+ (phrase algorithm) +:+ S "to perform a" +:+
   (titleize rgFnElm) +:+ S "Method analysis of the" +:+. (phrase slope))
   ssa
   []
   []
   [mod_sps]
   (Just mod_behav)

-- slice property sorter module
mod_sps_desc :: ConceptChunk
mod_sps_desc = dccWDS "mod_sps_desc" (cn' "slice property sorter")
  (S "When performing" +:+ (phrase slip) +:+ S "analysis with the RFEM Solver Module" +:+
   S "or" +:+ (titleize morPrice) +:+ S "Module" `sC` S "the base and" +:+ 
   (phrase intrslce) +:+ (plural surface) +:+ S "of each" +:+
   (phrase slice) +:+ S "in the" +:+ S "analysis requires a" +:+ (phrase soil) +:+
   S "constant." +:+ (titleize mod_sps_desc) +:+ S "Module identifies which" +:+
   (phrase soilLyr) +:+ S "the" +:+ (phrase surface) +:+ S "is in" +:+
   S "to assign" +:+ (plural property) +:+ S "from that" +:+ (phrase soilLyr) `sC`
   S "and uses a weighting scheme when the" +:+ (phrase surface) +:+
   S "crosses multiple" +:+. (plural soilLyr))

mod_sps :: ModuleChunk
mod_sps = makeImpModule mod_sps_desc
  ((at_start algorithm) +:+ S "to assigns" +:+ (plural soilPrpty) +:+
   S "to" +:+ (plural slice) +:+ S "based on the location of the" +:+
   (phrase slice) +:+ S "with respect" +:+S "to the different" +:+. (plural soilLyr))
   ssa
   []
   []
   []
   (Just mod_behav)

-- sfwr dec module
-- mod_sw :: ModuleChunk
-- mod_sw = M.mod_sw

-- sequence data structure module
-- mod_sds_desc :: ConceptChunk
-- mod_sds_desc = dccWDS "mod_sds_desc" (cn' "sequence data structure")
  -- (S "Provides array manipulation, including building an" +:+
   -- S "array, accessing a specific entry, slicing an array etc.")

mod_sds :: ModuleChunk
mod_sds = mod_seq_fun matlab []

-- rng module
-- mod_rng_desc :: ConceptChunk
-- mod_rng_desc = dccWDS "mod_rng_desc" (cn' "random number generator")
  -- (S "Randomly produces numbers between 0 and 1, using a" +:+
   -- S "chaotic function with an external seed. Used when generating" +:+
   -- (plural slpSrf) +:+ S "in the Genetic Algorithm Module.")

mod_rng :: ModuleChunk
mod_rng = mod_rng_fun matlab [] (S "Randomly produces numbers between 0 and 1" `sC`
  S "using a chaotic function with an external seed. Used when generating" +:+
  (plural slpSrf) +:+ S "in the" +:+ (titleize mod_genalg_desc) +:+ S "Module.")

-- plotting module
-- mod_plot_desc :: ConceptChunk
-- mod_plot_desc = dcc "mod_plot_desc" (cn' "plotting") "Provides a plot function."

mod_plot :: ModuleChunk
mod_plot = mod_plot_fun matlab [mod_hw]
