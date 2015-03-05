module H_c where
import Chunk
import Text.PrettyPrint

h_c = Chunk [("Symbol",text "$h_c$"),
             ("Equation", text "\\frac{ 2k_{c}h_{b}}{2k_{c}+\\tau_ch_{b}}")
             ]