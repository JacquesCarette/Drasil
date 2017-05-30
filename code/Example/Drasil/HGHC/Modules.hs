module Drasil.HGHC.Modules where

import Language.Drasil
import Language.Drasil.Code
import Drasil.HGHC.HeatTransfer
import Data.Drasil.Concepts.Software
import Prelude hiding (id)
import Control.Lens ((^.))

import Data.Drasil.Modules
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Concepts.Math
import Data.Drasil.Concepts.Computation
import Data.Drasil.Utils (foldlSent)

self :: NPNC
self = npnc "HGHC" (pn "HGHC")

executable :: NPNC
executable = npnc' (self ^. id) (tempCompoundPhrase self program) ("HGHC")

-- input param module
mod_inputp :: ModuleChunk
mod_inputp = makeRecord modInputParam 
             (foldlSent [S "The format and", (phrase structure), S "of the", 
             (phrase input_), (plural $ parameter ^. term)]) --FIXME: Plural?
             executable
             htVars
             []
             (Just mod_behav)

--input format
--FIXME: All the NP stuff here needs to be tweaked.
meth_input :: MethodChunk
meth_input = makeFileInputMethod
             (nc "read_input" (nounPhraseSP "Reads and stores input from file."))
             (makeVCObj "params" (cn "input parameters") cP (mod_inputp ^. id))
             "input"

mod_inputf :: ModuleChunk
mod_inputf = mod_io_fun executable
  [meth_input]
  [mod_inputp]
  (plural inDatum)
  modInputFormat

-- Calc Module
meth_htTransCladFuel, meth_htTransCladCool :: MethodChunk
meth_htTransCladFuel = fromEC htTransCladFuel hghcSymMap
meth_htTransCladCool = fromEC htTransCladCool hghcSymMap

hghc_calcDesc :: Sentence
hghc_calcDesc = S "Calculates heat transfer coefficients"

mod_calc :: ModuleChunk
mod_calc = mod_calc_fun 
  hghc_calcDesc
  (S "The equations used to calculate heat transfer coefficients")
  executable
  [meth_htTransCladFuel, meth_htTransCladCool]
  []

-- Output Format Module
meth_output :: MethodChunk
meth_output = makeFileOutputMethod (nc "write_output" (
  cn "Writes output to file.")) [getVC htTransCladFuel, getVC htTransCladCool] 
  "output"

mod_outputf_desc :: ConceptChunk
mod_outputf_desc = mod_outputf_desc_fun (foldlSent [S "input parameters,",
  S "temperatures, energies, and times when melting starts", S "and stops"])

mod_outputf :: ModuleChunk
mod_outputf = mod_io_fun executable
  [meth_output]
  [mod_hw, mod_inputp]
  (plural outDatum)
  mod_outputf_desc

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
meth_main = makeMainMethod (nc "main" (cn' "Main method")) main_func

mod_ctrl :: ModuleChunk
mod_ctrl = mod_ctrl_fun (S "The" +:+ (phrase $ algorithm ^. term))
  executable
  [meth_main] 
  [mod_hw, mod_inputp, mod_inputf, mod_calc, mod_outputf]