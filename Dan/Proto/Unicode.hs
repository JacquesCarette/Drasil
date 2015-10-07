{-# OPTIONS -Wall #-} 
module Unicode where
import ASTInternal (OutFormat(..))

class Unicode a where
  render :: a -> OutFormat -> String

data Alpha  = Alpha_L
            | Alpha  
data Circle = Circle
data Delta  = Delta_L
            | Delta
data Phi    = Phi_L
            | Phi
data Rho    = Rho_L
            | Rho
data Tau    = Tau_L
            | Tau
            
instance Unicode Alpha where
  render Alpha_L TeX   = "\\alpha"
  render Alpha_L Plain = "alpha"
  render Alpha TeX     = "\\Alpha"
  render Alpha Plain   = "Alpha"
  
instance Unicode Circle where
  render Circle TeX    = "\\circle"
  render Circle Plain  = "o"

instance Unicode Delta where
  render Delta_L TeX   = "\\delta"
  render Delta_L Plain = "delta"
  render Delta TeX     = "\\Delta"
  render Delta Plain   = "uDelta"
  
instance Unicode Phi where
  render Phi_L TeX     = "\\phi"
  render Phi_L Plain   = "phi"
  render Phi TeX       = "\\Phi"
  render Phi Plain     = "uPhi"
  
instance Unicode Rho where
  render Rho_L TeX     = "\\rho"
  render Rho_L Plain   = "rho"
  render Rho TeX       = "\\Rho"
  render Rho Plain     = "uRho"
  
instance Unicode Tau where
  render Tau_L TeX     = "\\tau"
  render Tau_L Plain   = "tau"
  render Tau TeX       = "\\Tau"
  render Tau Plain     = "uTau"             
