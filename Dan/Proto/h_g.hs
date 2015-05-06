module H_g where
import Chunk
import ASTInternal
import Text.PrettyPrint
import ToTex
import Tau_c
import H_p
import K_c

h_g :: Chunk FName FDesc
h_g = newChunk $
  [("Symbol",text "$h_g$"),
   ("Equation", expr h_g_eq),
   ("SIU", text "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   ("Description", 
    text "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = [tau_c,
           h_p,
           k_c]
           
h_g_eq = Frac ((Int 2):*(v k_c):*(v h_p)) ((Int 2):*(v k_c):+(v tau_c):*(v h_p))