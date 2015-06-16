{-# OPTIONS -Wall #-} 
module Example2 where
import ASTInternal
import Chunk
import SI_Units

si_units :: Chunks
si_units = [metre, kilogram, second, centigrade, joule, watt]
--------------- --------------- --------------- ---------------
{--------------- Begin Surface Areas ---------------}
--------------- --------------- --------------- ---------------
a_C :: Chunk
a_C = newChunk $
  [(Symbol, S "A" :-: S "C"), --Formatted symbol for documentation
   (VarName,S "a_C"),        --VarName if the symbol represents a variable
                                --Equation if the symbol can be calculated
   (Description,S "coil surface area") --Description
  ]
----------------------------------------------------------------------
a_in :: Chunk
a_in = newChunk $
  [(Symbol, S "A" :-: S "in"),
   (VarName,S "a_in"),
   (Description,S "surface area over which heat is transferred in")
  ]
----------------------------------------------------------------------  
a_p :: Chunk
a_p = newChunk $
  [(Symbol, S "A" :-: S "P"),
   (VarName,S "a_P"),
   (Description,S "phase change material surface area")
  ]
----------------------------------------------------------------------
a_out :: Chunk
a_out = newChunk $
  [(Symbol, S "A" :-: S "out"),
   (VarName,S "a_out"),
   (Description,S "surface area over which heat is transferred out")
  ]
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Specific Heat Capacities ---------------}  
--------------- --------------- --------------- ---------------
c :: Chunk
c = newChunk $
  [(Symbol, S "C"),
   (VarName,S "c"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity")
  ]
-- c_dep :: Dependency
-- c_dep = get_dep h_c_eq

-- c_eq :: Expr           
-- c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  -- :+((C tau_c):*(C h_b)))

----------------------------------------------------------------------

c__L :: Chunk
c__L = newChunk $
  [(Symbol, S "C" :^: S "L"),
   (VarName,S "c__L"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of a liquid")
  ]

----------------------------------------------------------------------

c_P_L :: Chunk
c_P_L = newChunk $
  [(Symbol, S "C" :-: S "P" :^: S "L"),
   (VarName,S "c_P_L"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of PCM during latent heating phase")
  ]

----------------------------------------------------------------------

c__S :: Chunk
c__S = newChunk $
  [(Symbol, S "C" :^: S "S"),
   (VarName,S "c__S"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of a solid")
  ]

----------------------------------------------------------------------

c_P_S :: Chunk
c_P_S = newChunk $
  [(Symbol, S "C" :-: S "P" :^: S "S"),
   (VarName,S "c_P_S"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of PCM during sensible heating phase")
  ]

----------------------------------------------------------------------

c_V :: Chunk
c_V = newChunk $
  [(Symbol, S "C" :-: S "V"),
   (VarName,S "c_V"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of a vapour")
  ]

----------------------------------------------------------------------

c_W :: Chunk
c_W = newChunk $
  [(Symbol, S "C" :-: S "W"),
   (VarName,S "c_W"),
   -- (Equation, E h_c_eq),
   (Description, S "specific heat capacity of water")
  ]
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Dimensions ---------------}
--------------- --------------- --------------- ---------------
d :: Chunk
d = newChunk $
  [(Symbol, S "D"),
   (VarName,S "d"),
   -- (Equation, E h_c_eq),
   (Description, S "diameter of tank")
  ]
  
----------------------------------------------------------------------

l :: Chunk
l = newChunk $
  [(Symbol, S "L"),
   (VarName,S "l"),
   -- (Equation, E h_c_eq),
   (Description, S "length of tank")
  ]

--------------- --------------- --------------- ---------------
{--------------- Begin Heat Energies ---------------}
--------------- --------------- --------------- ---------------

e :: Chunk
e = newChunk $
  [(Symbol,S "E"),
   (VarName,S "e"),
-- (Equation, E h_c_eq),
   (Description, S "sensible heat energy")
  ]
  
----------------------------------------------------------------------

e_P :: Chunk
e_P = newChunk $
  [(Symbol,S "E" :-: P),
   (VarName,S "e_P"),
-- (Equation, E h_c_eq),
   (Description, S "heat energy in the PCM")
  ]
  
----------------------------------------------------------------------

e_Pmelt_init :: Chunk
e_Pmelt_init = newChunk $
  [(Symbol,S "E" :-: S "Pmelt" :^: S "init"),
   (VarName,S "e_Pmelt_init"),
-- (Equation, E h_c_eq),
   (Description, S "heat energy in the PCM at the instant when melting begins")
  ]
  
----------------------------------------------------------------------

e_W :: Chunk
e_W = newChunk $
  [(Symbol,S "E" :-: S "W"),
   (VarName,S "e_W"),
-- (Equation, E h_c_eq),
   (Description, S "heat energy in the water")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Heat Generations ---------------}
--------------- --------------- --------------- ---------------

g :: Chunk
g = newChunk $
  [(Symbol,S "g"),
   (VarName,S "g"),
-- (Equation, E h_c_eq),
   (Description, S "volumetric heat generation per unit volume")
  ]

--------------- --------------- --------------- ---------------
{--------------- Begin Heat Transfers ---------------}
--------------- --------------- --------------- ---------------

h :: Chunk
h = newChunk $
  [(Symbol,S "h"),
   (VarName,S "h"),
-- (Equation, E h_c_eq),
   (Description, S "convective heat transfer coefficient")
  ]

----------------------------------------------------------------------

h_C :: Chunk
h_C = newChunk $
  [(Symbol,S "h" :-: S "C"),
   (VarName,S "h_C"),
-- (Equation, E h_c_eq),
   (Description, S "convective heat transfer coefficient between coil and water")
  ]
  
----------------------------------------------------------------------

h_P :: Chunk
h_P = newChunk $
  [(Symbol,S "h" :-: S "P"),
   (VarName,S "h_P"),
-- (Equation, E h_c_eq),
   (Description, S "convective heat transfer coefficient between water and PCM")
  ]
  
----------------------------------------------------------------------

--------------- --------------- --------------- ---------------
{--------------- Begin Latent Heat-related ---------------}
--------------- --------------- --------------- ---------------

h_f :: Chunk
h_f = newChunk $
  [(Symbol,S "H":-: S "f"),
   (VarName, S "h_f"),
-- (Equation, E h_c_eq),
   (Description, S "specific latent heat of fusion")
  ]

--------------- --------------- --------------- ---------------
{--------------- Begin Masses ---------------}
--------------- --------------- --------------- ---------------
  
m :: Chunk
m = newChunk $
  [(Symbol, S "m"),
   (VarName,S "m"),
   -- (Equation, E h_c_eq),
   (Description, S "mass")
  ]