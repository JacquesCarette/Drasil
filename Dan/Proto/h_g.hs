module H_g where
import Chunk
import ASTInternal
import Text.PrettyPrint
import Tau_c
import H_p
import K_c

h_g :: Chunk FName FDesc
h_g = newChunk $
  [("Symbol",text "$h_g$"),
   ("Equation", text "\\frac{2" <> get "Equation" k_c <> get "Equation" h_p <>
                  text "}{2" <> get "Equation" k_c <> text "+" <> 
                  get "Equation" tau_c <> get "Equation" h_p <> text "}"),
   ("SIU", text "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   ("Description", 
    text "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = [tau_c,
           h_p,
           k_c]
           
h_g_eq = Frac ((Dbl 2):*(c k_c):*(c h_p)) ((Dbl 2):*(c k_c):+(c tau_c):*(c h_p))
