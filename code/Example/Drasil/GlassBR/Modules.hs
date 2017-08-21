module Drasil.GlassBR.Modules where --whole file is used

import Language.Drasil

import Data.Drasil.Concepts.Software (modInputFormat, modInputParam,
  modInterpDatum)
import Data.Drasil.Concepts.Math (parameter, equation)
import Data.Drasil.Modules (mod_hw, mod_behav, mod_interp_fun, 
  mod_io_fun, mod_calc_fun, mod_derivedv_fun, mod_outputf_desc_fun,
  mod_ctrl_fun, mod_inputc_fun, mod_sw)
import Data.Drasil.Concepts.Documentation (input_, safetyReq, datum)
import Data.Drasil.Concepts.Computation (inDatum, algorithm, inParam,
  outDatum)
import Data.Drasil.SentenceStructures (foldlList, foldlSent)

import Drasil.GlassBR.Concepts (glassBRProg)
import Drasil.GlassBR.Unitals (demandq, capacity, prob_br)


modules :: [ModuleChunk]
modules = [mod_hw, mod_behav, mod_inputf, mod_inputp, mod_inputc,
  mod_outputf, mod_derivedv, mod_calc, mod_ctrl, mod_interpd, mod_sw,
  mod_interp]

-- input format module
mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun glassBRProg [] [mod_hw, mod_inputp] 
  (plural inDatum) modInputFormat

mod_inputp :: ModuleChunk
mod_inputp = mod_io_fun glassBRProg [] [mod_inputc] 
  (phrase input_ +:+ (plural parameter)) modInputParam --FIXME: Plural?

mod_inputc :: ModuleChunk
mod_inputc = mod_inputc_fun glassBRProg

mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (S "The" +:+ (phrase algorithm)) 
  glassBRProg [] 
  [mod_inputf, mod_inputp, mod_inputc, mod_derivedv, mod_calc,
  mod_interp, mod_outputf]

-- output format module
mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun (foldlList [plural inParam, 
  S "the" +:+ phrase demandq, S "the" +:+ phrase capacity,
  S "the" +:+ phrase prob_br, S "both" +:+. plural safetyReq])

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun glassBRProg [] [mod_hw, mod_inputp] 
  (plural outDatum) mod_outputf_desc

-- derived values module
mod_derivedv :: ModuleChunk
mod_derivedv = mod_derivedv_fun glassBRProg [mod_inputp]

-- calculations module
glassBR_calcDesc :: Sentence
glassBR_calcDesc =(foldlSent [S "Defines the", plural equation,
  S "for solving for the", foldlList [S "probability of glass breakage",
  phrase demandq, phrase capacity], S "using the", plural parameter, 
  S "in the", plural inParam, S "module"])

mod_calc :: ModuleChunk
mod_calc = mod_calc_fun (glassBR_calcDesc)
  (foldlSent [S "The", plural equation, S "for predicting the probability of", 
  S "glass breakage" `sC` phrase capacity `sC` S "and", phrase demandq `sC` 
  S "using the", plural inParam])
  glassBRProg
  []
  [mod_inputp]

-- interpolation data module
mod_interpd :: ModuleChunk
mod_interpd = mod_io_fun glassBRProg [] [] ((plural datum) +:+
  S "used for interpolation") modInterpDatum --FIXME: Plural?

mod_interp :: ModuleChunk
mod_interp = mod_interp_fun glassBRProg [mod_interpd]