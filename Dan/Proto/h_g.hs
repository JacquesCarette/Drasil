module H_g where
import Chunk
import ASTInternal
import Text.PrettyPrint
import ToTex
import Tau_c
import H_p
import K_c
import Config

h_g :: Chunk FName FDesc
h_g = newChunk $
  [(Symbol, S "h" :- S "{g}"),
   (Equation, E h_g_eq),
   (SIU, S "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   (Description, S
    "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = get_dep h_g_eq
           
h_g_eq = ((Int 2):*(v k_c):*(v h_p)) :/ ((Int 2):*(v k_c):+(v tau_c):*(v h_p))