{-# OPTIONS -Wall #-} 
module Example1 where
import ASTInternal
import Chunk
import SI_Units

si_units :: Chunks
si_units = [metre, kilogram, second, kelvin, centigrade, joule, calorie, mole,
              watt]
--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: Chunk
tau_c = newChunk $
  [(Symbol,U Tau_L :-: S "c"), --Formatted symbol for documentation
   (VarName,S "tau_c"),        --VarName if the symbol represents a variable
                                --Equation if the symbol can be calculated
   (Description,S "clad thickness") --Description
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}  
--------------- --------------- --------------- ---------------
h_c :: Chunk
h_c = newChunk $
  [(Symbol, S "h" :-: S "c"),
   (Equation, E h_c_eq),
   (Description, S 
    "convective heat transfer coefficient between clad and coolant"),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)")
  ]

h_c_dep :: Dependency
h_c_dep = get_dep h_c_eq

h_c_eq :: Expr           
h_c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  :+((C tau_c):*(C h_b)))


--------------- --------------- --------------- ---------------
{--------------- Begin h_g ---------------}
--------------- --------------- --------------- ---------------
h_g :: Chunk
h_g = newChunk $
  [(Symbol, S "h" :-: S "g"),
   (Equation, E h_g_eq),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   (Description, S
    "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = get_dep h_g_eq

h_g_eq :: Expr           
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+((C tau_c):*(C h_p)))

--------------- --------------- --------------- ---------------
{--------------- Begin h_b ---------------}
--------------- --------------- --------------- ---------------

h_b :: Chunk
h_b = newChunk $
  [(Symbol,S "h" :-: S "b"),
   (VarName,S "h_b"),
   (Description, S "initial coolant film conductance")
  ]
--------------- --------------- --------------- ---------------
{--------------- Begin h_p ---------------}
--------------- --------------- --------------- ---------------

h_p :: Chunk
h_p = newChunk $
  [(Symbol, S "h":-: S "p"),
   (VarName, S "h_p"),
   (Description, S "initial gap film conductance")
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin k_c ---------------}
--------------- --------------- --------------- ---------------

k_c :: Chunk
k_c = newChunk $
  [(Symbol,S "k":-: S "c"),
   (VarName, S "k_c"),
   (Description, S "clad conductivity")
  ]
