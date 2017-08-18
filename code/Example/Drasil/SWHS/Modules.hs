module Drasil.SWHS.Modules where --all of this file is exported

import Language.Drasil

import Drasil.SWHS.Concepts (swhsProg)
import Data.Drasil.Concepts.Software (modInputParam, modInputVerif,
  modInputFormat)
import Data.Drasil.Concepts.Math (parameter, equation, ode)
import Data.Drasil.Concepts.Computation (algorithm)
import Data.Drasil.Concepts.Documentation (input_, module_, datum,
  output_, constraint, physical, software)
import Data.Drasil.Modules (mod_plot_fun, mod_ode_fun, mod_hw,
  mod_seq_fun, mod_ctrl_fun, mod_param_fun, mod_behav, mod_io_fun,
  mod_outputf_desc_fun, mod_sw)
import Data.Drasil.Software.Products (matlab)
import Data.Drasil.Quantities.Thermodynamics (temp)
import Data.Drasil.Quantities.Physics (energy, time)
import Data.Drasil.Concepts.Thermodynamics (law_cons_energy, melting)
import Data.Drasil.SentenceStructures (foldlSent)

modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputv, mod_outputf,
          mod_outputv, mod_temp, mod_ener, mod_ctrl, mod_sw, mod_seq,
          mod_ode, mod_plot]

-- Input Format Module
mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun
  swhsProg [] [mod_hw, mod_inputp, mod_seq]
  (phrase input_ +:+ plural datum) modInputFormat

-- Input Parameters Module
mod_inputp :: ModuleChunk
mod_inputp = mod_io_fun
  swhsProg [] [mod_seq] (phrase input_ +:+ plural parameter) modInputParam

-- Input Verification Module
mod_inputv :: ModuleChunk
mod_inputv = mod_io_fun
  swhsProg [] [mod_inputp, mod_seq] (phrase physical +:+ S "and" +:+
  phrase software +:+ plural constraint) modInputVerif

-- Output Format Module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun $ foldlSent [phrase energy `sC`
  phrase input_, plural parameter `sC` plural temp `sC` S "and",
  plural time, S "when", phrase melting, S "starts and stops"]

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun
  swhsProg [] [mod_hw, mod_inputp, mod_seq]
  (phrase output_ +:+ plural datum) mod_outputf_desc

-- Output Verification Module
mod_outputv_desc :: ConceptChunk
mod_outputv_desc = dccWDS "mod_outputv_desc" (cn' "output verification") $
  foldlSent [S "Verifies that the", phrase output_, phrase energy,
  S "results follow the" +:+. phrase law_cons_energy,
  S "Throws a warning if the relative error exceeds the error threshold"]

mod_outputv :: ModuleChunk
mod_outputv = mod_param_fun swhsProg [mod_inputp, mod_seq]
  (S "The algorithm used to approximate expected results") mod_outputv_desc

-- Temperature ODEs Module
mod_temp_desc :: ConceptChunk
mod_temp_desc = dccWDS "mod_temp_desc" (cn' "temperature ODE") $
  foldlSent [S "Defines the", short ode :+: S "s",
  --FIXME uses a plural abbreviation?
  S "using the", plural parameter, S "in the",
  phrase input_, plural parameter, phrase module_]

mod_temp :: ModuleChunk
mod_temp = makeImpModule mod_temp_desc (foldlSent [S "The " :+:
  short ode :+: S "s", S "for solving the", phrase temp `sC`
  S "using the", phrase input_, plural parameter])
  swhsProg [] [] [mod_inputp, mod_seq] (Just mod_behav)

-- Energy Equations Module
mod_ener_desc :: ConceptChunk
mod_ener_desc = dccWDS "mod_ener_desc" (cn' "energy equation") $
  foldlSent [S "Defines the", phrase energy, plural equation,
  S "using the", plural parameter, S "in the", phrase input_,
  plural parameter, phrase module_]

mod_ener :: ModuleChunk
mod_ener = mod_param_fun swhsProg [mod_inputp, mod_seq]
  (S "The" +:+ plural equation +:+ S "for solving for the" +:+
  plural energy +:+ S "using the" +:+ phrase input_ +:+
  plural parameter) mod_ener_desc

-- Control Module
mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (S "The" +:+ phrase algorithm)
  swhsProg [] [mod_hw, mod_inputp, mod_inputf, mod_inputv, mod_temp, mod_ener,
  mod_ode, mod_plot, mod_outputv, mod_outputf, mod_seq]

-- Sequence Data Structure Module
mod_seq :: ModuleChunk
mod_seq = mod_seq_fun matlab []

-- ODE Solver Module
mod_ode :: ModuleChunk
mod_ode = mod_ode_fun matlab [mod_seq]

-- Plotting Module
mod_plot :: ModuleChunk
mod_plot = mod_plot_fun matlab [mod_seq]