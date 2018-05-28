module Drasil.SWHS.UnitExtract where --all of this file is exported

import Language.Drasil --Language.Drasil.Chunk.AssumpChunk
import Control.Lens ((^.))

import Data.Drasil.Concepts.Documentation (system, simulation, model, 
  problem, acroNumGen)

import Drasil.SWHS.DataDefs (dd1HtFluxC, dd2HtFluxP)
import Drasil.SWHS.Concepts (coil, tank, phsChgMtrl, water, perfect_insul,
  charging, discharging)
import Drasil.SWHS.Unitals (w_vol, vol_ht_gen, temp_C, temp_init, temp_W,
  temp_PCM, htCap_L_P, htCap_W, htCap_S_P, w_density, pcm_density, pcm_vol)
import Drasil.SWHS.TMods (t1ConsThermE)

import Data.Drasil.Quantities.PhysicalProperties (vol)
import Data.Drasil.Quantities.Physics (time, energy)
import Data.Drasil.Quantities.Thermodynamics (temp, boil_pt, melt_pt)

import Data.Drasil.Concepts.Thermodynamics as CT (heat, melting,
  law_conv_cooling, heat_trans, thermal_energy)
import Data.Drasil.Concepts.PhysicalProperties (solid, liquid, gaseous)
import Data.Drasil.Concepts.Math (change)
import Data.Drasil.Concepts.Physics (mech_energy)

import Data.Drasil.SentenceStructures (acroGD, acroIM, foldlSent, ofThe,
  ofThe', sAnd, isThe)



findUnit :: UnitaryChunk -> [FundUnit]
findUnit a = 