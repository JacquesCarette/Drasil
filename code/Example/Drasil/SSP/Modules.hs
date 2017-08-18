module Drasil.SSP.Modules where --All of it

import Language.Drasil

import Data.Drasil.Modules (mod_hw, mod_behav, mod_sw,
  mod_ctrl_fun, mod_io_fun, mod_seq_fun, mod_rng_fun,
  mod_plot_fun, mod_outputf_desc_fun)
import Data.Drasil.SentenceStructures (ofGiv, ofGiv',
  ofThe, foldlList, foldlSent, foldlSent_)

import Data.Drasil.Concepts.Math (surface, vector)
import Data.Drasil.Concepts.Documentation (property,
  method_, module_, analysis, input_, element)
import Data.Drasil.Concepts.Computation (algorithm,
  outDatum, inDatum, dataType')
import Data.Drasil.Software.Products (matlab)

import Drasil.SSP.Unitals (waterWeight, fs, sspInputs)
import Drasil.SSP.Defs (rgFnElm, soil, morPrice, slice,
  slip, slope, slopeSrf, slpSrf, crtSlpSrf, intrslce,
  ssa, soilLyr, soilPrpty)

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_ctrl, mod_inputf, mod_outputf, mod_genalg,
           mod_kinadm, mod_slipslicer, mod_slipweight, mod_mp, mod_rfem,
           mod_sps, mod_sw, mod_sds, mod_rng, mod_plot]

-- HW Hiding Module, Behaviour Hiding Module, and
-- Software Decision Module imported

-- Control module
mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (foldlSent_
  [S "The internal", plural dataType', S "and", plural algorithm])
  ssa [] [mod_inputf, mod_outputf, mod_genalg]

-- input format module
mod_inputf_desc :: ConceptChunk
mod_inputf_desc = dccWDS "mod_inputf_desc" (cn' "input format")
  (foldlSent [S "Reads the", plural inDatum, S "from an", phrase input_,
  S "file, and/or prompted command line" +:+. plural input_, at_start' inDatum,
  S "includes the x,y coordinates of the", phrase slope `sC` S "with a set of",
  S "coordinates for each layer. Each layer's", plural soilPrpty, S "of",
  (foldlList $ map phrase sspInputs),
  S "are stored in", plural vector, S "of" +:+. plural soilPrpty,
  S "If a piezometric", phrase surface, S "exists in the", phrase slope,
  S "it's coordinates and the", phrase waterWeight, S "are also",
  S "included in the" +:+. phrase input_, S "Lastly an expected range for",
  S "the entrance and exit points of the", phrase crtSlpSrf, S "are inputted"])

mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun ssa [] [mod_hw] (plural inDatum) mod_inputf_desc

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun (foldlSent [phrase fs,
  S "for the critical", phrase slip, S "calculated by the", titleize morPrice, 
  titleize module_, S "and", titleize rgFnElm, titleize method_,
  titleize module_ `sC` S "and a",
  S "plot of the", phrase crtSlpSrf, S "on the", phrase slope,
  S "geometry, with the showing the", phrase element,
  S "displacements as calculated by the RFEM", titleize module_])

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun ssa [] [mod_plot, mod_slipslicer, mod_mp, mod_rfem] 
  (plural outDatum) mod_outputf_desc

-- gen alg module
mod_genalg_desc :: ConceptChunk
mod_genalg_desc = dccWDS "mod_genalg_desc" (cn' "genetic algorithm")
  (foldlSent [S "Searches the", phrase slope, S "for the", phrase crtSlpSrf,
  S "with the minimum", phrase fs])

mod_genalg :: ModuleChunk
mod_genalg = makeImpModule mod_genalg_desc
  (foldlSent [at_start algorithm, S "to identify the", phrase slpSrf,
  S "that has the", S "minimum", phrase fs `sC`
  S "based on the", plural input_])
  ssa
  []
  []
  [mod_slipslicer, mod_kinadm, mod_rng, mod_slipweight, mod_mp]
  (Just mod_behav)

-- kin adm module
mod_kinadm_desc :: ConceptChunk
mod_kinadm_desc = dccWDS "mod_kinadm_desc" (cnIES "kinetic admissibility")
  (foldlSent [S "Some", plural slpSrf, S "are physically unlikely or",
  S "impossible to occur in a", phrase slpSrf `sC` S "such as",
  plural slpSrf, S "containing sharp angles, or going above the" +:+.
  phrase slopeSrf, S "Ensures randomly generated or mutated",
  plural slope, S "from the", titleize mod_genalg_desc,
  titleize module_, S "are physically possible according to the",
  S "criteria of the Kinematic Admissibility", titleize module_])

mod_kinadm :: ModuleChunk
mod_kinadm = makeImpModule mod_kinadm_desc
  (foldlSent [at_start algorithm, S "to determine if a given",
  phrase slpSrf, S "passes or fails a set of admissibility criteria"])
  ssa
  []
  []
  []
  (Just mod_behav)

-- slip slicer module
mod_slipslicer_desc :: ConceptChunk
mod_slipslicer_desc = dccWDS "mod_slipslicer_desc" (cn' "slip slicer")
  (foldlSent [S "When preparing a", phrase slpSrf, S "for analysis by the",
  titleize morPrice, titleize module_, S "or the RFEM",
  titleize module_ `sC` S "the x-coordinates defining", S "boundaries" `ofThe`
  plural slice, S "are identified and stored in a", phrase vector])

mod_slipslicer :: ModuleChunk
mod_slipslicer = makeImpModule mod_slipslicer_desc
  (foldlSent [at_start algorithm, S "to determine the coordinates of",
  S "where the", phrase slpSrf, phrase intrslce, S "nodes occur"])
  ssa
  []
  []
  []
  (Just mod_behav)

-- slip weighting module
mod_slipweight_desc :: ConceptChunk
mod_slipweight_desc = dccWDS "mod_slipweight_desc" (cn' "slip weighting")
  (foldlSent [S "Weights a set of", plural slpSrf, S "generated by the",
  titleize mod_genalg_desc, titleize module_, S "based on their" +:+.
  plural fs, S "A", phrase slpSrf, S "with a low",
  phrase fs, S "will have a high weight as it is more",
  S "likely to be or to lead to generation of the", phrase crtSlpSrf])

mod_slipweight :: ModuleChunk
mod_slipweight = makeImpModule mod_slipweight_desc
  (foldlSent [S "The weighting for each", phrase slpSrf, S "in a set of",
  plural slpSrf `sC` S "based on each", phrase's slpSrf, 
  phrase fs])
  ssa
  []
  []
  []
  (Just mod_behav)

-- morg price solver module
mod_mp_desc :: ConceptChunk
mod_mp_desc = dccWDS "mod_mp_desc" (cn "morgenstern price solver")
  (foldlSent [S "Calculates", phrase fs `ofGiv` phrase slpSrf `sC`
  S "through implementation of a", titleize morPrice,
  phrase ssa, phrase method_])

mod_mp :: ModuleChunk
mod_mp = makeImpModule mod_mp_desc
  (foldlSent [phrase fs `ofGiv'` phrase slpSrf])
  ssa
  []
  []
  [mod_sps]
  (Just mod_behav)

-- rfem solver module
mod_rfem_desc :: ConceptChunk
mod_rfem_desc = dccWDS "mod_rfem_desc" (cn' "RFEM solver")
  (foldlSent [S "Calculate", (S "global" +:+ phrase fs `sC` S "local" +:+
  phrase slice +:+ plural fs `sC` S "and local" +:+
  phrase slice +:+ S "displacements") `ofGiv` phrase slpSrf,
  S "under given conditions, through implementation of a",
  phrase rgFnElm, phrase ssa, phrase method_])

mod_rfem :: ModuleChunk
mod_rfem = makeImpModule mod_rfem_desc
  (foldlSent [S "The", phrase algorithm, S "to perform a", titleize rgFnElm,
  titleize method_, phrase analysis, S "of the", phrase slope])
  ssa
  []
  []
  [mod_sps]
  (Just mod_behav)

-- slice property sorter module
mod_sps_desc :: ConceptChunk
mod_sps_desc = dccWDS "mod_sps_desc" (cn' "slice property sorter")
  (foldlSent [S "When performing", phrase slip, phrase analysis,
  S "with the RFEM Solver", titleize module_,
  S "or", titleize morPrice, titleize module_ `sC` S "the base and", 
  phrase intrslce, plural surface, S "of each",
  phrase slice, S "in the", phrase analysis, S "requires a", phrase soil +:+.
  S "constant", titleize mod_sps_desc, titleize module_, S "identifies which",
  phrase soilLyr, S "the", phrase surface, S "is in",
  S "to assign", plural property, S "from that", phrase soilLyr `sC`
  S "and uses a weighting scheme when the", phrase surface,
  S "crosses multiple", plural soilLyr])

mod_sps :: ModuleChunk
mod_sps = makeImpModule mod_sps_desc
  (foldlSent [at_start algorithm, S "to assigns", plural soilPrpty,
  S "to", plural slice, S "based on the location of the",
  phrase slice, S "with respect", S "to the different", plural soilLyr])
  ssa
  []
  []
  []
  (Just mod_behav)

-- sequence data structure module
  -- "Provides array manipulation, including building an
  --  array, accessing a specific entry, slicing an array etc."
mod_sds :: ModuleChunk
mod_sds = mod_seq_fun matlab []

-- random number generator module
mod_rng :: ModuleChunk
mod_rng = mod_rng_fun matlab [] (
  foldlSent [S "Randomly produces numbers between", E 0, S "and", E 1 `sC`
  S "using a chaotic function with an external seed. Used when generating",
  plural slpSrf, S "in the", titleize mod_genalg_desc, titleize module_])

-- plotting module
mod_plot :: ModuleChunk
mod_plot = mod_plot_fun matlab [mod_hw]
