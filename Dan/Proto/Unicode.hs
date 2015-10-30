{-# OPTIONS -Wall #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
module Unicode where
import Format

class Unicode mode a where
  render :: (Format mode) => mode -> a -> String

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

instance Unicode TeX Alpha where
  render _ Alpha_L = "\\alpha"
  render _ Alpha   = "\\Alpha"
  
instance Unicode Plain Alpha where
  render _ Alpha_L = "alpha"
  render _ Alpha   = "uAlpha"
  
-- 

instance Unicode TeX Circle where
  render _ Circle  = "^{\\circ}"

instance Unicode Plain Circle where
  render _ Circle  = "o"

--

instance Unicode TeX Delta where
  render _ Delta_L = "\\delta"
  render _ Delta   = "\\Delta"

instance Unicode Plain Delta where  
  render _ Delta_L = "delta"
  render _ Delta   = "uDelta"
  
--

instance Unicode TeX Phi where
  render _ Phi_L   = "\\phi"
  render _ Phi     = "\\Phi"
  
instance Unicode Plain Phi where
  render _ Phi_L   = "phi"
  render _ Phi     = "uPhi"
  
--

instance Unicode TeX Rho where
  render _ Rho_L   = "\\rho"
  render _ Rho     = "\\Rho"

instance Unicode Plain Rho where
  render _ Rho_L   = "rho"
  render _ Rho     = "uRho"
  
--

instance Unicode TeX Tau where
  render _ Tau_L   = "\\tau"
  render _ Tau     = "\\Tau"
  
instance Unicode Plain Tau where
  render _ Tau_L   = "tau"
  render _ Tau     = "uTau"             
