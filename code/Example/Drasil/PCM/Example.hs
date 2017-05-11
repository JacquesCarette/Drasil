module Drasil.PCM.Example where

import Language.Drasil

import Data.Drasil.SI_Units
import qualified Data.Drasil.Units.Thermodynamics as U
import Data.Drasil.Quantities.PhysicalProperties
import Prelude hiding (id)
import Data.Drasil.Concepts.Documentation
import Data.Drasil.Quantities.Thermodynamics
import Data.Drasil.Quantities.Physics (time)
import Data.Drasil.Concepts.Math (ode)
import Data.Drasil.Quantities.Math (gradient, diameter)

import Control.Lens ((^.))

pcmSymbols :: [CQSWrapper]
pcmSymbols = map cqs pcmUnits

pcmUnits :: [UWrapper]
pcmUnits = map uw [coil_SA, density, dummyVar, heat_cap_spec, hIn_SA, hOut_SA,
  htCap_Liq, htCap_W, ht_flux, ht_flux_C, ht_flux_in, ht_flux_out, ht_gen_vol, --tank_vol, norm_vect, temp_boil,
  ht_xfer_co, ht_xfer_CW, mass, tank_D, tank_L, temp, temp_coil, temp_diff, temp_env,
  temp_init, temp_water, thFluxVect, time, time_final, vol, water_dense, water_m,
  water_vol]

coil_SA, dummyVar, hIn_SA, hOut_SA, htCap_Liq, htCap_W, ht_flux_C,
  ht_flux_in, ht_flux_out, ht_gen_vol, ht_xfer_co, ht_xfer_CW, tank_D, tank_L,--temp_boil,
  temp_coil, temp_diff, temp_env, temp_init, temp_water, thFluxVect, time_final,--tank_vol,
  water_dense, water_m, water_vol :: UnitalChunk

-- convenience
fixme :: String
fixme = "FIXME: Define this or remove the need for definitions"

--FIXME: Anything that is "X of Y" should use the of combinator.
coil_SA     = uc' "coil_SA" (cn' "coil surface area") fixme (sub cA cC) m_2
dummyVar    = uc' "dummyVar" 
              (nounPhraseSP "dummy variable for integration over time")
              fixme (Greek Tau_L) second
hIn_SA      = uc' "hIn_SA" 
              (nounPhraseSP "surface area over which heat is transferred in")
              fixme (sub cA (Atomic "in")) m_2
hOut_SA     = uc' "hOut_SA" 
              (nounPhraseSP "surface area over which heat is transferred out")
              fixme (sub cA (Atomic "out")) m_2
htCap_Liq   = uc' "htCap_Liq" (nounPhraseSP "specific heat capacity of a liquid")
              fixme (sup cC cL) U.heat_cap_spec
htCap_W     = uc' "htCap_W" (heat_cap_spec `of_` water)
              fixme (sub cC cW) U.heat_cap_spec
ht_flux_C   = uc' "ht_flux_C" (nounPhraseSP "heat flux from coil")
              fixme (sub lQ cC) U.thermal_flux
ht_flux_in  = uc' "ht_flux_in" (nounPhraseSP "heat flux in")
              fixme (sub lQ (Atomic "in")) U.thermal_flux
ht_flux_out = uc' "ht_flux_out" (nounPhraseSP "heat flux out")
              fixme (sub lQ (Atomic "out")) U.thermal_flux
ht_gen_vol  = uc' "ht_gen_vol" 
              (nounPhraseSP "volumetric heat generation per unit volume")
              fixme lG U.thermal_flux
ht_xfer_co  = uc' "ht_xfer_co" 
              (nounPhraseSP "convective heat transfer coefficient")
              fixme lH U.heat_transfer_coef
ht_xfer_CW  = uc' "ht_xfer_CW" 
              (nounPhraseSP "convective heat transfer between coil and water")
              fixme (sub lH cC) U.heat_transfer_coef
  -- How do I make a symbol that needs one (or more) Accent? Add to Symbol or
  -- pull Accent out somehow?
-- temp_boil   = uc' "T_boil" "temperature at boiling point" -- (sub cT (Atomic "boil")) centigrade
tank_D      = uc' "tank_D" (diameter `of_` water) fixme cD metre
tank_L      = uc' "tank_L" (len `of_` tank) fixme cL metre
temp_coil   = uc' "temp_coil" (temp `of_` coil)
              fixme (sub cT cC) centigrade
temp_diff   = uc' "temp_diff" (nounPhraseSP "temperature difference")
              fixme (Concat [Greek Delta, cT]) centigrade
temp_env    = uc' "temp_env" (nounPhraseSP "temperature of environment")
              fixme (sub cT (Atomic "env")) centigrade
thFluxVect  = uc' "thFluxVect" (cn' "thermal flux vector")
              fixme (vec lQ) U.thermal_flux
time_final  = uc' "time_final" (cn' "time")
              fixme (sub lT (Atomic "final")) second
temp_init   = uc' "temp_init" (cn' "initial temperature")
              fixme (sub cT (Atomic "init")) centigrade
temp_water  = uc' "temp_water" (temp `of_` water)
              fixme (sub cT cW) centigrade
--tank_vol    = uc' "V_tank" "volume of the cylindrical tank"   -- (sub cV (Atomic "tank")) m_3
water_dense = uc' "water_dense" (density `of_` water)
              fixme (sub (Greek Rho_L) cW) densityU
water_m     = uc' "water_m" (mass `of_` water)
              fixme (sub (mass ^. symbol) cW) kilogram
water_vol   = uc' "water_vol" (vol `of_` water)
              fixme (sub cV cW) m_3
--melt_frac   = uc' "Phi" "melt fraction" (Greek Phi) unitless

--Common Terms
coil, tank, water, ht_trans :: NPNC

coil        = npnc "coil"           (cn' "coil")
tank        = npnc "tank"           (cn' "tank")
water       = npnc "water"          (cn "water")

ht_trans    = npnc "heat transfer"  (cn "heat transfer") --Not really a nounphase, just a hack to get RefSec to work

----Acronyms-----
acronyms :: [CINP]
acronyms = [assumption,dataDefn,genDefn,goalStmt,inModel,likelyChg,ode,
            physSyst,requirement,srs,sWHS,thModel]
  
sWHS, sWHT :: CINP

sWHS  = commonINP "sWHS"  (cn' "solar water heating system")  "SWHS"
sWHT  = commonINP "sWHT"  (cn' "solar water heating tank")    "SWHT"  

----EqChunks----
--Theoretical models--
t1consThermE :: RelationConcept
t1consThermE = makeRC "t1consThermE" 
  (nounPhraseSP "Conservation of thermal energy")
  t1descr cons_therm_rel

cons_therm_rel :: Relation
cons_therm_rel = (Neg (C gradient)) :. (C thFluxVect) + (C ht_gen_vol) := 
  (C density) * (C heat_cap_spec) * (Deriv Part (C temp) (C time))

t1descr :: Sentence
t1descr = 
  (S ("This equation gives the conservation of energy for time varying heat " ++
  "transfer in a material of specific heat capacity ") :+: 
  (P $ heat_cap_spec ^. symbol) :+: S " and density " :+: (P $ density ^. symbol) :+:
  S ", where " :+: (P $ thFluxVect ^. symbol)) 
  --TODO: Finish this description and do it better. I need to
  --  figure out the best way to encode this information.
  
srs_swhs :: ConceptChunk -- Used to make the title of the paper
  
srs_swhs = dcc "srs_swhs" (nounPhraseSP 
  "Solar Water Heating Systems")
  "SWHS"