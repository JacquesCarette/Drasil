module Drasil.HGHC.Modules where

import Language.Drasil
import Drasil.HGHC.HeatTransfer
import Data.Drasil.Concepts.Software
import Prelude hiding (id)
import Control.Lens ((^.))

self :: NamedChunk
self = CC "HGHC" (S "HGHC")

executable :: NamedChunk
executable = CC (self ^. id) (self ^. term :+: (S " ") :+: program ^. term)

-- HW Hiding Module
mod_hw :: ModuleChunk
mod_hw = makeImpModuleNoGen modHWHiding
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
             S "the input parameters.") executable varChunks [] (Just mod_behav)

--input format
meth_input :: MethodChunk
meth_input = makeFileInputMethod (CC "read_input" (S "Reads and stores " :+:
             S "input from file.")) (makeVCObj "params" "input parameters" cP
             (mod_inputp ^. id)) "input"

mod_inputf :: ModuleChunk
mod_inputf = makeImpModule modInputFormat (S "The format and structure of " :+:
             S "the input data.") executable [] [meth_input] [mod_inputp]
             (Just mod_behav)


-- Calc Module
meth_h_g, meth_h_c :: MethodChunk
meth_h_g = fromEC h_g
meth_h_c = fromEC h_c

mod_calc_desc :: NamedChunk
mod_calc_desc = CC "calc" (S "Calculates heat transfer coefficients")

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations used to calculate heat transfer coefficients")
  executable
  []
  [meth_h_g, meth_h_c]
  []
  (Just mod_behav)


-- Output Format Module
meth_output :: MethodChunk
meth_output = makeFileOutputMethod (CC "write_output" (S "Writes output to " :+:
              S "to file.")) [getVC h_g, getVC h_c] "output"

mod_outputf_desc :: NamedChunk
mod_outputf_desc = CC "output format" (S "Outputs the results of the " :+:
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
  [ Block
    [ DeclState $ ObjDecDef "params" (Type "input_parameters")
        (StateObj (Type "input_parameters") []),
      DeclState $ ObjDecDef "in" (Type "input_format")
        (StateObj (Type "input_format") []),
      ValState $ ObjAccess (Var "in") (Func "read_input" [Var "params"]),
      DeclState $ ObjDecDef "cal" (Type "calc")
        (StateObj (Type "calc") []),
      DeclState $ VarDecDef "h_g" float (ObjAccess (Var "cal")
        (Func "calc_h_g" [
          ObjVar (Var "params") (Var "k_c"),
          ObjVar (Var "params") (Var "h_p"),
          ObjVar (Var "params") (Var "tau_c")])),
      DeclState $ VarDecDef "h_c" float (ObjAccess (Var "cal")
        (Func "calc_h_c" [
          ObjVar (Var "params") (Var "k_c"),
          ObjVar (Var "params") (Var "h_b"),
          ObjVar (Var "params") (Var "tau_c")])),
      DeclState $ ObjDecDef "out" (Type "output_format")
        (StateObj (Type "output_format") []),
      ValState $ ObjAccess (Var "out") (Func "write_output"
        [Var "h_g", Var "h_c"])
    ]
  ]

meth_main :: MethodChunk
meth_main = makeMainMethod (CC "main" (S "Main method")) main_func

mod_ctrl :: ModuleChunk
mod_ctrl = makeImpModule modControl (S "The algorithm for coordinating " :+:
           S "the running of the program.") executable [] [meth_main]
           [mod_hw, mod_inputp, mod_inputf, mod_calc, mod_outputf]
           (Just mod_behav)