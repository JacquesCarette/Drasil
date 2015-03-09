module H_g where
import Chunk
import Text.PrettyPrint

h_g :: Chunk FName FDesc
h_g = newChunk $
  [("Symbol",text "$h_g$"),
   ("Equation", text "\\frac{2k_{c}h_{p}}{2k_{c}+\\tau_c h_{p}}"),
   ("SIU", text "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   ("Description", 
    text "effective heat transfer coefficient between clad and fuel surface")
  ]