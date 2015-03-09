module H_c where
import Chunk
import Text.PrettyPrint

h_c :: Chunk FName FDesc
h_c = newChunk $
  [("Symbol",text "$h_c$"),
   ("Equation", text "\\frac{ 2k_{c}h_{b}}{2k_{c}+\\tau_ch_{b}}"),
   ("Description", 
    text "convective heat transfer coefficient between clad and coolant"),
   ("SIU", text "($\\mathrm{\\frac{kW}{m^2C}}$)")
  ]