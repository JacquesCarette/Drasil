module Drasil.PCM.Example where

import Language.Drasil

import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as U
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP
import Prelude hiding (id)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Math
import Data.Drasil.Concepts.Math (ode) 
import qualified Data.Drasil.Quantities.Physics as QP (time)

import Control.Lens ((^.))

pcmSymbols :: [UnitalChunk]
pcmSymbols = [coil_SA,hIn_SA,hOut_SA,htCap,htCap_Liq,htCap_W,tank_D,ht_gen_vol,
  ht_xfer_co,ht_xfer_CW,tank_L,mass,water_m, -- norm_vect, 
  ht_flux, thFluxVect,
  ht_flux_C,ht_flux_in,ht_flux_out,time,temp, --temp_boil,
  temp_coil,temp_env,time_final,temp_init,temp_water,temp_diff,vol, --tank_vol,
  water_vol,density,water_dense,dummyVar]

coil_SA, hIn_SA, hOut_SA, htCap, htCap_Liq, htCap_W, tank_D, ht_gen_vol,
  ht_xfer_co,ht_xfer_CW, tank_L,mass,water_m,ht_flux,thFluxVect,
  ht_flux_C,ht_flux_in,ht_flux_out,time,temp,--temp_boil,
  temp_coil,temp_env,time_final,temp_init,temp_water,temp_diff,vol,--tank_vol,
  water_vol,density,water_dense,dummyVar :: UnitalChunk

-- convenience
fixme :: String
fixme = "FIXME: Define this or remove the need for definitions"

coil_SA     = uc' "coil_SA" "coil surface area" fixme (sub cA cC) m_2
hIn_SA      = uc' "hIn_SA" "surface area over which heat is transferred in" 
  fixme (sub cA (Atomic "in")) m_2
hOut_SA     = uc' "hOut_SA" "surface area over which heat is transferred out" 
  fixme (sub cA (Atomic "out")) m_2
htCap       = ucFromVC heat_cap_spec U.heat_cap_spec
htCap_Liq   = uc' "htCap_Liq" "specific heat capacity of a liquid" 
  fixme (sup cC cL) U.heat_cap_spec
htCap_W     = uc' "htCap_W" "specific heat capacity of water" 
  fixme (sub cC cW) U.heat_cap_spec
tank_D      = uc' "tank_D" "diameter of tank" fixme cD metre
ht_gen_vol  = uc' "ht_gen_vol" "volumetric heat generation per unit volume" 
  fixme lG U.thermal_flux
ht_xfer_co  = uc' "ht_xfer_co" "convective heat transfer coefficient" 
  fixme lH U.heat_transfer_coef
ht_xfer_CW  = uc' "ht_xfer_CW" "convective heat transfer between coil and water" 
  fixme (sub lH cC) U.heat_transfer_coef
tank_L      = uc' "tank_L" "length of tank" fixme cL metre
mass        = ucFromVC QPP.mass kilogram
water_m     = uc' "water_m" ((QPP.mass ^. id) ++ " of water")
  fixme (sub (QPP.mass ^. symbol) cW) kilogram
  -- How do I make a symbol that needs one (or more) Accent? Add to Symbol or
  -- pull Accent out somehow?
ht_flux     = uc' "ht_flux" "heat flux" fixme lQ U.heat_transfer_coef
thFluxVect  = uc' "thFluxVect" "thermal flux vector" 
  fixme (vec lQ) U.thermal_flux
ht_flux_C   = uc' "ht_flux_C" "heat flux from coil" 
  fixme (sub lQ cC) U.thermal_flux
ht_flux_in  = uc' "ht_flux_in" "heat flux in" 
  fixme (sub lQ (Atomic "in")) U.thermal_flux
ht_flux_out = uc' "ht_flux_out" "heat flux out" 
  fixme (sub lQ (Atomic "out")) U.thermal_flux
time        = ucFromVC QP.time second
temp        = uc' "temp" "temperature" fixme cT centigrade
-- temp_boil   = uc' "T_boil" "temperature at boiling point" -- (sub cT (Atomic "boil")) centigrade
temp_coil   = uc' "temp_coil" "temperature of coil" fixme (sub cT cC) centigrade
temp_env    = uc' "temp_env" "temperature of environment" 
  fixme (sub cT (Atomic "env")) centigrade
time_final  = uc' "time_final" "time" 
  fixme (sub lT (Atomic "final")) second
temp_init   = uc' "temp_init" "initial temperature" 
  fixme (sub cT (Atomic "init")) centigrade
temp_water  = uc' "temp_water" "temperature of water" fixme (sub cT cW) centigrade
temp_diff   = uc' "temp_diff" "temperature difference" 
  fixme (Concat [Greek Delta, cT]) centigrade
vol         = uc' "vol" "volume" fixme cV m_3
--tank_vol    = uc' "V_tank" "volume of the cylindrical tank"   -- (sub cV (Atomic "tank")) m_3
water_vol   = uc' "water_vol" "volume of water" fixme (sub cV cW) m_3
density     = uc' "density" "density, mass per unit volume" 
  fixme (Greek Rho_L) densityU
water_dense = uc' "water_dense" "density of water" 
  fixme (sub (Greek Rho_L) cW) densityU
dummyVar    = uc' "dummyVar" "dummy variable for integration over time"
  fixme (Greek Tau_L) second
--melt_frac   = uc' "Phi" "melt fraction" (Greek Phi) unitless

----Acronyms-----
acronyms :: [CI]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ode,
  physSyst,requirement,srs,sWHS,thModel]
  
sWHS :: CI
sWHS = commonidea "sWHS" "Solar Water Heating System" "SWHS"

----EqChunks----
--Theoretical models--
t1consThermE :: RelationConcept
t1consThermE = makeRC "t1consThermE" "Conservation of thermal energy" 
  t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) := 
  (C density) * (C htCap) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = 
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+: 
  (P $ htCap ^. symbol) :+: S " and density " :+: (P $ density ^. symbol) :+:
  S ", where " :+: (P $ thFluxVect ^. symbol)) 
  --TODO: Finish this description and do it better. I need to
  --  figure out the best way to encode this information.
