{-# OPTIONS -Wall #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
module Unicode where
import Format

class Unicode a where

class (Format mode, Unicode a) => Render mode a where
  render :: mode -> a -> String

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
            
instance Unicode Alpha
instance Unicode Circle
instance Unicode Delta
instance Unicode Phi
instance Unicode Rho
instance Unicode Tau
--

instance Render TeX Alpha where
  render _ Alpha_L = "\\alpha"
  render _ Alpha   = "\\Alpha"
  
instance Render Plain Alpha where
  render _ Alpha_L = "alpha"
  render _ Alpha   = "uAlpha"
  
-- 

instance Render TeX Circle where
  render _ Circle  = "^{\\circ}"

instance Render Plain Circle where
  render _ Circle  = "o"

--

instance Render TeX Delta where
  render _ Delta_L = "\\delta"
  render _ Delta   = "\\Delta"

instance Render Plain Delta where  
  render _ Delta_L = "delta"
  render _ Delta   = "uDelta"
  
--

instance Render TeX Phi where
  render _ Phi_L   = "\\phi"
  render _ Phi     = "\\Phi"
  
instance Render Plain Phi where
  render _ Phi_L   = "phi"
  render _ Phi     = "uPhi"
  
--

instance Render TeX Rho where
  render _ Rho_L   = "\\rho"
  render _ Rho     = "\\Rho"

instance Render Plain Rho where
  render _ Rho_L   = "rho"
  render _ Rho     = "uRho"
  
--

instance Render TeX Tau where
  render _ Tau_L   = "\\tau"
  render _ Tau     = "\\Tau"
  
instance Render Plain Tau where
  render _ Tau_L   = "tau"
  render _ Tau     = "uTau"             
