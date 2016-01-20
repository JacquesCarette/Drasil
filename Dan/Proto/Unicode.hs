{-# OPTIONS -Wall #-} 
module Unicode where
import Format

class Render a where
  render :: Format -> a -> String

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
            
--

instance Render Alpha where
  render TeX Alpha_L = "\\alpha"
  render TeX Alpha   = "\\Alpha"
  render Plain Alpha_L = "alpha"
  render Plain Alpha   = "uAlpha"
  
-- 

instance Render Circle where
  render TeX Circle  = "${}^{\\circ}$" --temp $ workaround until unit printing's fixed.
  render Plain Circle  = "o"

--

instance Render Delta where
  render TeX Delta_L = "\\delta"
  render TeX Delta   = "\\Delta"
  render Plain Delta_L = "delta"
  render Plain Delta   = "uDelta"
  
--

instance Render Phi where
  render TeX Phi_L   = "\\phi"
  render TeX Phi     = "\\Phi"
  render Plain Phi_L   = "phi"
  render Plain Phi     = "uPhi"
  
--

instance Render Rho where
  render TeX Rho_L   = "\\rho"
  render TeX Rho     = "\\Rho"
  render Plain Rho_L   = "rho"
  render Plain Rho     = "uRho"
  
--

instance Render Tau where
  render TeX Tau_L   = "\\tau"
  render TeX Tau     = "\\Tau"
  render Plain Tau_L   = "tau"
  render Plain Tau     = "uTau"             
