module H_g where
import Chunk
import Text.PrettyPrint
import Tau_c
import H_p
import K_c

h_g :: Chunk FName FDesc
h_g = newChunk $
  [("Symbol",text "$h_g$"),
   ("Equation", text "\\frac{2k_{c}h_{p}}{2k_{c}+\\tau_c h_{p}}"),
   ("SIU", text "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   ("Description", 
    text "gap conductance")
  ]
  
h_g_dep :: Dependency
h_g_dep = [tau_c,
           h_p,
           k_c]