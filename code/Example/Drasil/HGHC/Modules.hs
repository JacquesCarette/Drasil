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
mod_hw = makeImpModule modHWHiding
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
meth_input = makeStdInputMethod (makeVC "params" "input parameters" cP)
  --(QObj (mod_inputp ^. id)))

mod_inputf :: ModuleChunk
mod_inputf = makeImpModule modInputFormat (S "The format and structure of " :+:
             S "the input data.") executable [] [meth_input] [mod_inputp]
             (Just mod_behav)


-- Calc Module
meth_htTransCladFuel, meth_htTransCladCool :: MethodChunk
meth_htTransCladFuel = fromEC htTransCladFuel
meth_htTransCladCool = fromEC htTransCladCool

mod_calc_desc :: NamedChunk
mod_calc_desc = CC "calc" (S "Calculates heat transfer coefficients")

mod_calc :: ModuleChunk
mod_calc = makeImpModule mod_calc_desc
  (S "The equations used to calculate heat transfer coefficients")
  executable
  []
  [meth_htTransCladFuel, meth_htTransCladCool]
  []
  (Just mod_behav)
