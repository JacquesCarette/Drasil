module Drasil.NoPCM.Unitals where

import Language.Drasil

import Drasil.NoPCM.Definitions

import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as U
import Data.Drasil.Quantities.PhysicalProperties
import Prelude hiding (id)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Concepts.Math (surArea)
import Data.Drasil.Quantities.Math (diameter)
import Data.Drasil.Units.PhysicalProperties

import Control.Lens ((^.))

pcmSymbols :: [CQSWrapper]
pcmSymbols = map cqs pcmUnits

-- All values with constraints; built off of the Unital Chunks

pcmConstrained :: [ConstrConcept]
pcmConstrained = [coil_SA_con, htCap_W_con, ht_xfer_CW_con, tank_L_con, tank_V_con, 
  temp_coil_con, time_final_con, temp_init_con, water_dense_con]

coil_SA_con, htCap_W_con, ht_xfer_CW_con, tank_L_con, tank_V_con, 
  temp_coil_con, time_final_con, temp_init_con, water_dense_con:: ConstrConcept

coil_SA_con = constrained' coil_SA
  [physc $ \c -> c :> (Dbl 0),
  sfwrc $ \c -> c :<= c]
  
htCap_W_con = constrained' htCap_W
  [physc $ \c -> c :> (Dbl 0),
  sfwrc $ \c -> c :<= c :<= c]
  
ht_xfer_CW_con  = constrained' ht_xfer_CW 
  [physc $ \c -> c :> (Dbl 0),
  sfwrc $ \c -> c :< c :< c]
  
tank_L_con = constrained' tank_L
  [physc $ \c -> c :> (Dbl 0),
  sfwrc $ \c -> c :<= c :<= c]

tank_V_con = constrained' tank_V
  [physc $ \c -> c :> (Dbl 0),
  sfwrc $ \c -> c :<= c :<= c]
  
temp_coil_con = constrained' temp_coil
  [physc $ \c -> (Dbl 0) :< c :< (Dbl 100)]

time_final_con = constrained' time_final
  [physc $ \c -> c :> (Dbl 0),
  sfwrc $ \c -> c :<= c]
  
temp_init_con = constrained' temp_init
  [physc $ \c -> c :>= (Dbl 0),
  sfwrc $ \c -> c :<= c :<= c]
  
water_dense_con = constrained' water_dense
  [physc $ \c -> c :>= (Dbl 0),
  sfwrc $ \c -> c :< c :<= c]
  

pcmUnits :: [UCWrapper]
pcmUnits = map ucw [coil_SA, density, dummyVar, heat_cap_spec, hIn_SA, hOut_SA, 
  htCap_Liq, htCap_W, ht_flux, ht_flux_C, ht_flux_in, ht_flux_out, ht_gen_vol, 
  ht_xfer_co, ht_xfer_CW, mass, tank_D, tank_L, tank_V, temp, temp_coil, 
  temp_diff, temp_env, temp_init, temp_water, thFluxVect, time, time_final, 
  vol, water_dense, water_m, water_vol]

coil_SA, dummyVar, hIn_SA, hOut_SA, htCap_Liq, htCap_W, ht_flux_C, 
  ht_flux_in, ht_flux_out, ht_gen_vol, ht_xfer_co, ht_xfer_CW, tank_D, tank_L, 
  tank_V, temp_coil, temp_diff, temp_env, temp_init, temp_water, thFluxVect, 
  time_final, water_dense, water_m, water_vol :: UnitalChunk

-- convenience

coil_SA     = ucs "coil_SA" (compoundPhrase (nounPhrase'' (phrase coil) (phrase coil) 
              CapFirst CapWords) (nounPhrase'' (phrase surArea) (phrase surArea) 
              CapFirst CapWords)) "Area covered by the outermost layer of the coil" 
              (sub cA cC) m_2 Real
dummyVar    = ucs "dummyVar" (nounPhraseSP "dummy variable for integration over time")
              "Binary value representing the presence or absence of integration over time" 
              (Greek Tau_L) second Boolean
hIn_SA      = ucs "hIn_SA" (nounPhraseSP "surface area over which heat is transferred in")
              "Surface area over which thermal energy is transferred into an object" 
              (sub cA (Atomic "in")) m_2 Real
hOut_SA     = ucs "hOut_SA" (nounPhraseSP "surface area over which heat is transferred out")
              "Surface area over which thermal energy is transferred out of an object" 
              (sub cA (Atomic "out")) m_2 Real
htCap_Liq   = ucs "htCap_Liq" (nounPhraseSP "specific heat capacity of a liquid")
              ("The amount of energy required to raise the temperature of a given unit " ++ 
              "mass of a given liquid by a given amount")
              (sup cC cL) U.heat_cap_spec Real
htCap_W     = ucs "htCap_W" (heat_cap_spec `of_` water)
              ("The amount of energy required to raise the temperature of a given unit " ++ 
              "mass of water by a given amount") (sub cC cW)
              U.heat_cap_spec Real
ht_flux_C   = ucs "ht_flux_C" (nounPhraseSP "heat flux from coil")
              "The rate of thermal energy transfer through the coil per unit time" 
              (sub lQ cC) U.thermal_flux Real
ht_flux_in  = ucs "ht_flux_in" (nounPhraseSP "heat flux in")
              ("The rate of thermal energy transfer into an object through a " ++ 
              "given surface per unit time") (sub lQ (Atomic "in"))
              U.thermal_flux Real
ht_flux_out = ucs "ht_flux_out" (nounPhraseSP "heat flux out")
              ("The rate of thermal energy transfer out of an object through a " ++ 
              "given surface per unit time") (sub lQ (Atomic "out"))
              U.thermal_flux Real
ht_gen_vol  = ucs "ht_gen_vol" (nounPhraseSP "volumetric heat generation per unit volume")
              "Amount of thermal energy generated per unit volume" lG U.thermal_flux Real
ht_xfer_co  = ucs "ht_xfer_co" (nounPhraseSP "convective heat transfer coefficient")
              ("The proportionality constant between the heat flux and the " ++ 
              "thermodynamic driving force for the flow of thermal energy")
              lH U.heat_transfer_coef Real
ht_xfer_CW  = ucs "ht_xfer_CW" 
              (nounPhraseSP "convective heat transfer coefficient between coil and water")
              ("The convective heat transfer coefficient that models " ++
              "the thermal flux from the coil to the surrounding water") (sub lH cC) 
              U.heat_transfer_coef Real
  -- How do I make a symbol that needs one (or more) Accent? Add to Symbol or
  -- pull Accent out somehow?
tank_D      = ucs "tank_D" (diameter `of_` tank) "The diameter of the tank" 
              cD metre Real
tank_L      = ucs "tank_L" (len `of_` tank) "The length of the tank" cL metre 
              Real
tank_V      = ucs "V_tank" (nounPhraseSP "volume of the cylindrical tank") 
              "The amount of space encompassed by the tank" (sub cV (Atomic "tank")) 
              m_3 Real
temp_coil   = ucs "temp_coil" (temp `of_` coil)
              "The average kinetic energy of the particles within the coil" 
              (sub cT cC) centigrade Real
temp_diff   = ucs "temp_diff" (nounPhraseSP "temperature difference")
              "Measure of the relative amounts of internal energy within two bodies" 
              (Concat [Greek Delta, cT]) centigrade Real
temp_env    = ucs "temp_env" (temp `of_` environment)
              "The tempature of a given environment" (sub cT (Atomic "env")) 
              centigrade Real
thFluxVect  = ucs "thFluxVect" (cn' "thermal flux vector")
              "Vector denoting the direction of thermal flux through a surface" 
              (vec lQ) U.thermal_flux (Vect Real)
time_final  = ucs "time_final" (cn' "time")
              ("The amount of time elapsed from the beginning of the " ++
              "simulation to its conclusion") (sub lT (Atomic "final")) second Real
temp_init   = ucs "temp_init" (cn' "initial temperature")
              "The temperature at the beginning of the simulation" 
              (sub cT (Atomic "init")) centigrade Real
temp_water  = ucs "temp_water" (temp `of_` water)
              "The average kinetic energy of the particles within the water" 
              (sub cT cW) centigrade Real
water_dense = ucs "water_dense" (density `of_` water)
              "The amount of mass per unit volume of water" (sub (Greek Rho_L) cW) 
              densityU Real
water_m     = ucs "water_m" (mass `of_` water)
              "The amount of matter within a given quantity of water" 
              (sub (mass ^. symbol) cW) kilogram Real
water_vol   = ucs "water_vol" (vol `of_` water)
              "The amount of space occupied by a given quantity of water" 
              (sub cV cW) m_3 Real
--melt_frac   = uc' "Phi" "melt fraction" (Greek Phi) unitless