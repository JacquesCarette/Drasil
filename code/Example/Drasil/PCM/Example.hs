module Drasil.PCM.Example where

import Language.Drasil

import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as U
import qualified Data.Drasil.Quantities.PhysicalProperties as QPP

import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Math

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

coil_SA     = makeUC "A_C" "coil surface area" (sub cA cC) m_2
hIn_SA      = makeUC "A_in" "surface area over which heat is transferred in" 
              (sub cA (Atomic "in")) m_2
hOut_SA     = makeUC "A_out" "surface area over which heat is transferred out" 
              (sub cA (Atomic "out")) m_2
htCap       = ucFromVC heat_cap_spec U.heat_cap_spec
htCap_Liq   = makeUC "C^L" "specific heat capacity of a liquid" (sup cC cL)
              U.heat_cap_spec
htCap_W     = makeUC "C_W" "specific heat capacity of water" (sub cC cW)
              U.heat_cap_spec
tank_D      = makeUC "D" "diameter of tank" cD metre
ht_gen_vol  = makeUC "g" "volumetric heat generation per unit volume" lG
              U.thermal_flux
ht_xfer_co  = makeUC "h" "convective heat transfer coefficient" lH U.heat_transfer
ht_xfer_CW  = makeUC "h_C" "convective heat transfer between coil and water" 
              (sub lH cC) U.heat_transfer
tank_L      = makeUC "L" "length of tank" cL metre
mass        = ucFromVC QPP.mass kilogram
water_m     = makeUC "m_W" ((QPP.mass ^. name) ++ " of water") 
                (sub (QPP.mass ^. symbol) cW) kilogram
  -- How do I make a symbol that needs one (or more) Accent? Add to Symbol or
  -- pull Accent out somehow?
ht_flux     = makeUC "q" "heat flux" lQ U.heat_transfer
thFluxVect  = makeUC "q_vect" "thermal flux vector" (vec lQ)
                  U.thermal_flux
ht_flux_C   = makeUC "q_C" "heat flux from coil" (sub lQ cC) U.thermal_flux
ht_flux_in  = makeUC "q_in" "heat flux in" (sub lQ (Atomic "in")) U.thermal_flux
ht_flux_out = makeUC "q_out" "heat flux out" (sub lQ (Atomic "out")) U.thermal_flux
time        = makeUC "t" "time" lT second
temp        = makeUC "T" "temperature" cT centigrade
-- temp_boil   = makeUC "T_boil" "temperature at boiling point" 
              -- (sub cT (Atomic "boil")) centigrade
temp_coil   = makeUC "T_coil" "temperature of coil" 
              (sub cT cC) centigrade
temp_env    = makeUC "T_env" "temperature of environment" 
              (sub cT (Atomic "env")) centigrade
time_final  = makeUC "t_final" "time" (sub lT (Atomic "final")) second
temp_init   = makeUC "T_init" "initial temperature" 
              (sub cT (Atomic "init")) centigrade
temp_water  = makeUC "T_W" "temperature of water" 
              (sub cT cW) centigrade
temp_diff   = makeUC "deltaT" "temperature difference" 
              (Concat [Greek Delta, cT]) centigrade
vol         = makeUC "V" "volume" cV m_3
--tank_vol    = makeUC "V_tank" "volume of the cylindrical tank" 
                -- (sub cV (Atomic "tank")) m_3
water_vol   = makeUC "V_W" "volume of water" (sub cV cW) m_3
density     = makeUC "rho" "density, mass per unit volume" (Greek Rho_L) 
              densityU
water_dense = makeUC "rho_W" "density of water" (sub (Greek Rho_L) cW) densityU
dummyVar    = makeUC "tau" "dummy variable for integration over time" 
                (Greek Tau_L) second
--melt_frac   = makeUC "Phi" "melt fraction" (Greek Phi) unitless

----Acronyms-----
acronyms :: [ConceptChunk]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,oDE,
  physSyst,requirement,srs,sWHS,thModel]
  
oDE, physSysDescr, sWHS :: ConceptChunk

oDE           = makeCC "ODE" "Ordinary Differential Equation"
physSysDescr  = makeCC "PS" "Physical System Description"
sWHS          = makeCC "SWHS" "Solar Water Heating System"

----EqChunks----
--Theoretical models--
t1consThermE :: RelationChunk
t1consThermE = makeRC "Conservation of thermal energy" t1descr cons_therm_rel

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
