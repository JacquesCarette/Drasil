module Drasil.HGHC.Modules where

import Language.Drasil
import Language.Drasil.Code

import Drasil.HGHC.HeatTransfer
import Data.Drasil.Concepts.Software
import Prelude hiding (id)
import Control.Lens ((^.))

self :: NamedChunk
self = ncWDS "HGHC" (S "HGHC")

executable :: NamedChunk
executable = ncWDS' (self ^. id) (self ^. term :+: (S " ") :+: program ^. term) ("HGHC")

-- HW Hiding Module
mod_hw :: ModuleChunk
mod_hw = makeImpModuleNoGen hwHiding
  (S "The data structure and algorithm used to implement the virtual hardware.")
  os
  []
  []
  []
  Nothing


-- Behaviour Hiding Module
mod_behav :: ModuleChunk
mod_behav = makeUnimpModule modBehavHiding
  (S "The contents of the required behaviors.")
  Nothing

-- input param module
mod_inputp :: ModuleChunk
mod_inputp = makeRecord modInputParams (S "The format and structure of " :+:
             S "the input parameters.") executable htVars [] (Just mod_behav)

--input format
meth_input :: MethodChunk
meth_input = makeFileInputMethod (ncWDS "read_input" (S "Reads and stores " :+:
             S "input from file.")) (makeVCObj "params" "input parameters" cP
             (mod_inputp ^. id)) "input"

mod_inputf :: ModuleChunk
mod_inputf = makeImpModule modInputFormat (S "The format and structure of " :+:
             S "the input data.") executable [] [meth_input] [mod_inputp]
             (Just mod_behav)


-- Calc Module
meth_htTransCladFuel, meth_htTransCladCool :: MethodChunk
meth_htTransCladFuel = fromEC htTransCladFuel
meth_htTransCladCool = fromEC htTransCladCool

mod_calc_desc :: ConceptChunk
mod_calc_desc = dcc "mod_calc_desc" "calc" "Calculates heat transfer coefficients"

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations used to calculate heat transfer coefficients")
  executable
  []
  [meth_htTransCladFuel, meth_htTransCladCool]
  []
  (Just mod_behav)


-- Output Format Module
meth_output :: MethodChunk
meth_output = makeFileOutputMethod (ncWDS "write_output" (S "Writes output to " :+:
        S "to file.")) [getVC htTransCladFuel, getVC htTransCladCool] "output"

mod_outputf_desc :: ConceptChunk
mod_outputf_desc = dccWDS "mod_outputf_desc" "output format" 
  (S "Outputs the results of the " :+:
  S "calculations, including the input parameters, " :+:
  S "temperatures, energies, and times when melting starts" :+:
  S " and stops.")

mod_outputf :: ModuleChunk
mod_outputf = makeImpModule mod_outputf_desc (S "The format and structure " :+:
              S "of the output data.")
              executable
              []
              [meth_output]
              [mod_hw, mod_inputp]
              (Just mod_behav)


-- Control Module
main_func :: Body
main_func =
  let labelParams = "params"
      typeParams = obj "input_parameters"
      labelIn = "in"
      typeIn = obj "input_format"
      labelCalc = "cal"
      typeCalc = obj "calc"
      labelOut = "out"
      typeOut = obj "output_format"
      labelCladThick = cladThick ^. id
      labelCoolFilm = coolFilmCond ^. id
      labelGapFilm = gapFilmCond ^. id
      labelCladCond = cladCond ^. id
      labelHg = htTransCladFuel ^. id
      labelHc = htTransCladCool ^. id
  in
  [ block
    [ objDecNewVoid labelParams typeParams,
      objDecNewVoid labelIn typeIn,
      valStmt $ objMethodCall (var labelIn) (meth_input ^. id) [var labelParams],
      objDecNewVoid labelCalc typeCalc,
      varDecDef labelHg float
        ( objMethodCall (var labelCalc) ("calc_" ++ labelHg)
          [ var labelParams $-> var labelCladCond,
            var labelParams $-> var labelGapFilm,
            var labelParams $-> var labelCladThick ] ),
      varDecDef labelHc float
        ( objMethodCall (var labelCalc) ("calc_" ++ labelHc)
          [ var labelParams $-> var labelCladCond,
            var labelParams $-> var labelCoolFilm,
            var labelParams $-> var labelCladThick ] ),
      objDecNewVoid labelOut typeOut,
      valStmt $ objMethodCall (var labelOut) (meth_output ^. id)
        [ var labelHg,
          var labelHc ]
    ]
  ]

meth_main :: MethodChunk
meth_main = makeMainMethod (ncWDS "main" (S "Main method")) main_func

mod_ctrl :: ModuleChunk
mod_ctrl = makeImpModule modControl (S "The algorithm for coordinating " :+:
           S "the running of the program.") executable [] [meth_main]
           [mod_hw, mod_inputp, mod_inputf, mod_calc, mod_outputf]
           (Just mod_behav)
