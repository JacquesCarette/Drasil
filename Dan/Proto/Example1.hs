{-# OPTIONS -Wall #-} 
module Example1 where
import ASTInternal
import Chunk
--------------- --------------- --------------- ---------------
{--------------- Begin tau_c ---------------}
--------------- --------------- --------------- ---------------
tau_c :: Chunk FName FDesc
tau_c = newChunk $
  [(Symbol,U Tau_L :- S "c"),
   (Equation,U Tau_L :- S "c"),
   (Description,S "clad thickness")
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin h_c ---------------}  
--------------- --------------- --------------- ---------------
h_c :: Chunk FName FDesc
h_c = newChunk $
  [(Symbol, S "h" :- S "c"),
   (Equation, E h_c_eq),
   (Description, S 
    "convective heat transfer coefficient between clad and coolant"),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)")
  ]

h_c_dep :: Dependency
h_c_dep = get_dep h_c_eq

h_c_eq :: Expr           
h_c_eq = ((Int 2):*(C k_c):*(C h_b)) :/ ((Int 2):*(C k_c)
  :+(C tau_c):*(C h_b))


--------------- --------------- --------------- ---------------
{--------------- Begin h_g ---------------}
--------------- --------------- --------------- ---------------
h_g :: Chunk FName FDesc
h_g = newChunk $
  [(Symbol, S "h" :- S "g"),
   (Equation, E h_g_eq),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   (Description, S
    "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = get_dep h_g_eq

h_g_eq :: Expr           
h_g_eq = ((Int 2):*(C k_c):*(C h_p)) :/ ((Int 2):*(C k_c):+(C tau_c):*(C h_p))

--------------- --------------- --------------- ---------------
{--------------- Begin h_b ---------------}
--------------- --------------- --------------- ---------------

h_b :: Chunk FName FDesc
h_b = newChunk $
  [(Symbol,S "h" :- S "b"),
   (Equation,S "h" :- S "b"),
   (Description, S "initial coolant film conductance")
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin h_p ---------------}
--------------- --------------- --------------- ---------------

h_p :: Chunk FName FDesc
h_p = newChunk $
  [(Symbol, S "h":- S "p"),
   (Equation, S "h":- S "p"),
   (Description, S "initial gap film conductance")
  ]
  
--------------- --------------- --------------- ---------------
{--------------- Begin k_c ---------------}
--------------- --------------- --------------- ---------------

k_c :: Chunk FName FDesc
k_c = newChunk $
  [(Symbol,S "k":- S "c"),
   (Equation, S "k":- S "c"),
   (Description, S "clad conductivity")
  ]