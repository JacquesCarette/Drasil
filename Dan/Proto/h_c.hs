module H_c where
import Chunk
import ASTInternal
import ToTex
import Text.PrettyPrint
import Tau_c
import H_b
import K_c
import Config

h_c :: Chunk FName FDesc
h_c = newChunk $
  [(Symbol,"$h_{c}$"),
   (Equation, expr h_c_eq),
   (Description, 
    "convective heat transfer coefficient between clad and coolant"),
   (SIU, "($\\mathrm{\\frac{kW}{m^2C}}$)")
  ]
  
h_c_dep :: Dependency
h_c_dep = get_dep h_c_eq
           
h_c_eq = ((Int 2):*(v k_c):*(v h_b)) :/ ((Int 2):*(v k_c):+(v tau_c):*(v h_b))