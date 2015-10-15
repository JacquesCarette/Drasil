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
            
instance Unicode TeX Alpha where
  render TeX Alpha_L   = "\\alpha"
  render TeX Alpha     = "\\Alpha"
  
instance Unicode Plain Alpha where
  render Plain Alpha_L = "alpha"
  render Plain Alpha   = "uAlpha"
  
instance Unicode TeX Circle where
  render TeX Circle    = "\\circle"

instance Unicode Plain Circle where
  render Plain Circle  = "o"

instance Unicode TeX Delta where
  render TeX Delta_L  = "\\delta"
  render TeX Delta    = "\\Delta"

instance Unicode Plain Delta where  
  render Plain Delta_L = "delta"
  render Plain Delta  = "uDelta"
  
instance Unicode TeX Phi where
  render TeX Phi_L    = "\\phi"
  render TeX Phi      = "\\Phi"
  
instance Unicode Plain Phi where
  render Plain Phi_L   = "phi"
  render Plain Phi     = "uPhi"
  
instance Unicode TeX Rho where
  render TeX Rho_L     = "\\rho"
  render TeX Rho       = "\\Rho"

instance Unicode Plain Rho where
  render Plain Rho_L   = "rho"
  render Plain Rho     = "uRho"
  
instance Unicode TeX Tau where
  render TeX Tau_L     = "\\tau"
  render TeX Tau       = "\\Tau"
  
instance Unicode Plain Tau where
  render Plain Tau_L   = "tau"
  render Plain Tau     = "uTau"             
