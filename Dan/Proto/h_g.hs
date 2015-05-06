module H_g where
import Chunk
import ASTInternal
import Text.PrettyPrint
import ToTex
import Tau_c
import H_p
import K_c
import Data.List

h_g :: Chunk FName FDesc
h_g = newChunk $
  [("Symbol","$h_{g}$"),
   ("Equation", expr h_g_eq),
   ("SIU", "($\\mathrm{\\frac{kW}{m^2C}}$)"),
   ("Description", 
    "effective heat transfer coefficient between clad and fuel surface")
  ]
  
h_g_dep :: Dependency
h_g_dep = [tau_c,
           h_p,
           k_c]
           
h_g_eq = Frac ((Int 2):*(v k_c):*(v h_p)) ((Int 2):*(v k_c):+(v tau_c):*(v h_p))

get_dep :: Expr -> Dependency
get_dep (Frac a b) = nub (get_dep a ++ get_dep b)
get_dep (a :* b) = nub (get_dep a ++ get_dep b)
get_dep (a :+ b) = nub (get_dep a ++ get_dep b)
get_dep (Chnk c) = [c]
get_dep _ = []