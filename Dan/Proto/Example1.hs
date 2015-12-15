{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example1 where
import ASTInternal (Expr(..))
-- import Spec (Spec(..))
-- import ExprTools (get_dep) // don't put dependencies in DS, compute it
import SI_Units
import Unicode (Tau(..))
import Unit (Unit(..))

import Chunk (VarChunk(..))
import UnitalChunk (UnitalChunk(..))
import EqChunk (EqChunk(..))

import Symbol

-----
-- Need some derived units.  For now, put them here, but need to think
-- about where they really ought to go.
heat_transfer :: Unit
heat_transfer = Derived (C kilogram :/ (C metre :^ (Int 2) :* C centigrade))

h,c :: Symbol
h = Atomic "h"
c = Atomic "c"

--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: VarChunk
tau_c = VC "tau_c" "clad thickness" (Composite (Ta Tau) [c] [])

--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}
--------------- --------------- --------------- ---------------
h_c_eq :: Expr
h_c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  :+((C tau_c):*(C h_b)))

h_c :: EqChunk
h_c = EC (UC 
  (VC "h_c" "convective heat transfer coefficient between clad and coolant"
      (Composite h [c] []))
  heat_transfer)
  h_c_eq

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_g ---------------}
-- --------------- --------------- --------------- ---------------
h_g_eq :: Expr
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

h_g :: EqChunk
h_g = EC (UC 
  (VC "h_g" "effective heat transfer coefficient between clad and fuel surface"
      (Composite h [Atomic "g"] [])) heat_transfer) h_g_eq

--------------- --------------- --------------- ---------------
{--------------- Begin h_b ---------------}
--------------- --------------- --------------- ---------------

h_b :: VarChunk
h_b = VC "h_b" "initial coolant film conductance" (Composite h [Atomic "b"] [])

--------------- --------------- --------------- ---------------
{--------------- Begin h_p ---------------}
--------------- --------------- --------------- ---------------

h_p :: VarChunk
h_p = VC "h_p" "initial gap film conductance" (Composite h [Atomic "p"] [])

--------------- --------------- --------------- ---------------
{--------------- Begin k_c ---------------}
--------------- --------------- --------------- ---------------

k_c :: VarChunk
k_c = VC "k_c" "clad conductivity" ((Composite h [Atomic "c"] []))
