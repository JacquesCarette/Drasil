{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleContexts #-} 
module Example1 where
import ASTInternal (Expr(..))
import Spec (Spec(..))
import ExprTools (get_dep)
import Chunk (Chunk(..))
import SI_Units
import Unicode (Circle(..), Unicode, Tau(..))
import Format (Format)

-- si_units :: (Format a, Unicode a Circle) => [Chunk a]
-- si_units = [metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              -- watt]
-- --------------- --------------- --------------- ---------------
-- {--------------- Begin tau_c ---------------}
-- --------------- --------------- --------------- ---------------
-- tau_c :: (Format mode, Unicode mode Tau) => Chunk mode
-- tau_c = newChunk "tau_c" $
  -- [(Symbol,U Tau_L :-: S "c"), --Formatted symbol for documentation
   -- (VarName,S "tau_c"),        --VarName if the symbol represents a variable
                                -- --Equation if the symbol can be calculated
   -- (Description,S "clad thickness") --Description
  -- ]

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_c ---------------}
-- --------------- --------------- --------------- ---------------
-- h_c_eq :: (Format mode, Unicode mode Tau) => Expr mode
-- h_c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  -- :+((C tau_c):*(C h_b)))

-- h_c :: (Format mode, Unicode mode Tau) => Chunk mode
-- h_c = newChunk "h_c" $
  -- [(Symbol, S "h" :-: S "c"),
   -- (Equation, E h_c_eq),
   -- (Description, S
    -- "convective heat transfer coefficient between clad and coolant"),
   -- (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   -- (Dependencies, D $ get_dep h_c_eq)
  -- ]

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_g ---------------}
-- --------------- --------------- --------------- ---------------
-- h_g_eq :: (Format mode, Unicode mode Tau) => Expr mode
-- h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

-- h_g :: (Format mode, Unicode mode Tau) => Chunk mode
-- h_g = newChunk "h_g" $
  -- [(Symbol, S "h" :-: S "g"),
   -- (Equation, E h_g_eq),
   -- (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   -- (Description, S
    -- "effective heat transfer coefficient between clad and fuel surface"),
   -- (Dependencies, D $ get_dep h_g_eq)
  -- ]

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_b ---------------}
-- --------------- --------------- --------------- ---------------

-- h_b :: Chunk mode
-- h_b = newChunk "h_b" $
  -- [(Symbol,S "h" :-: S "b"),
   -- (VarName,S "h_b"),
   -- (Description, S "initial coolant film conductance")
  -- ]
-- --------------- --------------- --------------- ---------------
-- {--------------- Begin h_p ---------------}
-- --------------- --------------- --------------- ---------------

-- h_p :: Chunk mode
-- h_p = newChunk "h_p" $
  -- [(Symbol, S "h":-: S "p"),
   -- (VarName, S "h_p"),
   -- (Description, S "initial gap film conductance")
  -- ]

-- --------------- --------------- --------------- ---------------
-- {--------------- Begin k_c ---------------}
-- --------------- --------------- --------------- ---------------

-- k_c :: Chunk mode
-- k_c = newChunk "k_c" $
  -- [(Symbol,S "k":-: S "c"),
   -- (VarName, S "k_c"),
   -- (Description, S "clad conductivity")
  -- ]
