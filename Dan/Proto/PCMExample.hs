{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module PCMExample where
-- import ASTInternal (Expr(..))
import SI_Units
-- import Unicode (Tau(..))
-- import EqChunk (EqChunk(..), fromEqn)
import Symbol
import UnitalChunk
import PCMUnits

-- import Control.Lens ((^.))
cA,cC :: Symbol
cA = Atomic "A"
cC = Atomic "C"

coil_SA, hIn_SA, hOut_SA, htCap_W, tank_D, g :: UnitalChunk
coil_SA = makeUC "A_C" "coil surface area" (sub cA cC) m_2
hIn_SA  = makeUC "A_in" "surface area over which heat is transferred in" 
            (sub cA (Atomic "in")) m_2
hOut_SA = makeUC "A_out" "surface area over which heat is transferred out" 
            (sub cA (Atomic "out")) m_2
htCap_W = makeUC "C_W" "specific heat capacity of water" (sub cC (Atomic "W"))
            heat_capacity
tank_D  = makeUC "D" "diameter of tank" (Atomic "D") metre
g       = makeUC "g" "volumetric heat generation per unit volume" (Atomic "g")
            thermFluxU

h,c :: Symbol
h = Atomic "h"
c = Atomic "c"
-- --------------- --------------- --------------- ---------------
-- {--------------- Begin tau_c ---------------}
-- --------------- --------------- --------------- ---------------
-- tau_c :: VarChunk
-- tau_c = VC "tau_c" "clad thickness" (sub (Special Tau_L) c)

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_c ---------------}
-- --------------- --------------- --------------- ---------------
-- h_c_eq :: Expr
-- h_c_eq = 2 * (C k_c) * (C h_b) / (2 * (C k_c) + (C tau_c) * (C h_b))

-- h_c :: EqChunk
-- h_c = fromEqn "h_c" 
  -- "convective heat transfer coefficient between clad and coolant"
  -- (sub h c) heat_transfer h_c_eq

-- -- --------------- --------------- --------------- ---------------
-- -- {--------------- Begin h_g ---------------}
-- -- --------------- --------------- --------------- ---------------
-- h_g_eq :: Expr
-- h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

-- h_g :: EqChunk
-- h_g = fromEqn "h_g" 
  -- "effective heat transfer coefficient between clad and fuel surface"
  -- (sub h (Atomic "g")) heat_transfer h_g_eq

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_b ---------------}
-- --------------- --------------- --------------- ---------------

-- h_b :: VarChunk
-- h_b = VC "h_b" "initial coolant film conductance" (sub h (Atomic "b"))

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_p ---------------}
-- --------------- --------------- --------------- ---------------

-- h_p :: VarChunk
-- h_p = VC "h_p" "initial gap film conductance" (sub h (Atomic "p"))

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin k_c ---------------}
-- --------------- --------------- --------------- ---------------

-- k_c :: VarChunk
-- k_c = VC "k_c" "clad conductivity" (sub h c)
