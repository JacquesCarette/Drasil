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
data Nabla  = Nabla
data Phi    = Phi_L
            | Phi
data Rho    = Rho_L
            | Rho
data Tau    = Tau_L
            | Tau
            
--

instance Render Alpha where
  render TeX Alpha_L   = "\\alpha{}"
  render TeX Alpha     = "\\Alpha{}"
  render Plain Alpha_L = "alpha"
  render Plain Alpha   = "uAlpha"
  render HTML Alpha_L  = "alpha;"
  render HTML Alpha    = "Alpha;"
  
-- 

instance Render Circle where
  render TeX Circle   = "${}^{\\circ}$"--temp $ workaround until unit printing's fixed.
  render Plain Circle = "o"
  render HTML Circle  = "&deg;"

--

instance Render Delta where
  render TeX Delta_L   = "\\delta{}"
  render TeX Delta     = "\\Delta{}"
  render Plain Delta_L = "delta"
  render Plain Delta   = "uDelta"
  render HTML Delta_L  = "&delta;"
  render HTML Delta    = "&Delta;"
  
--

instance Render Nabla where
  render TeX Nabla   = "\\nabla{}"
  render Plain Nabla = "nabla"
  render HTML Nabla  = "&nabla;"

--

instance Render Phi where
  render TeX Phi_L   = "\\phi{}"
  render TeX Phi     = "\\Phi{}"
  render Plain Phi_L = "phi"
  render Plain Phi   = "uPhi"
  render HTML Phi_L  = "&phi;"
  render HTML Phi    = "&Phi;"
  
--

instance Render Rho where
  render TeX Rho_L   = "\\rho{}"
  render TeX Rho     = "\\Rho{}"
  render Plain Rho_L = "rho"
  render Plain Rho   = "uRho"
  render HTML Rho_L  = "&rho;"
  render HTML Rho    = "&Rho;"
  
--

instance Render Tau where
  render TeX Tau_L   = "\\tau{}"
  render TeX Tau     = "\\Tau{}"
  render Plain Tau_L = "tau"
  render Plain Tau   = "uTau"  
  render HTML Tau_L  = "&tau;"
  render HTML Tau    = "&Tau;"