{-# Language FlexibleInstances, MultiParamTypeClasses #-}
module Language.Drasil.HTML.Monad where

import Language.Drasil.Unicode

-----------------------------------------------------------------------------
-- | Printing monad
--

-- first, start with a specific data type
-- note that this is just the Reader Monad for now, but we might need
-- to extend, so start there.

-- hack for now
newtype PrintHTML = PH {unPH :: String}

instance RenderGreek PrintHTML where
  greek Alpha_L  = PH "&alpha;"
  greek Alpha    = PH "&Alpha;"
  greek Beta_L  = PH "&beta;"
  greek Beta    = PH "&Beta;"
  greek Delta_L  = PH "&delta;"
  greek Delta    = PH "&Delta;"
  greek Ell   = PH "&#8467;"
  greek Eta_L  = PH "&eta;"
  greek Eta    = PH "&Eta;"
  greek Gamma_L  = PH "&gamma;"
  greek Gamma    = PH "&Gamma;"
  greek Lambda_L  = PH "&lambda;"
  greek Lambda    = PH "&Lambda;"
  greek Nabla  = PH "&nabla;"
  greek Nu_L  = PH "&nu;"
  greek Nu    = PH "&Nu;"
  greek Omega_L  = PH "&omega;"
  greek Omega    = PH "&Omega;"
  greek Pi_L   = PH "&pi;"
  greek Pi     = PH "&Pi;"
  greek Phi_L  = PH "&phi;"
  greek Phi_V  = PH "&phiv;"
  greek Phi    = PH "&Phi;"
  greek Rho_L  = PH "&rho;"
  greek Rho    = PH "&Rho;"
  greek Tau_L  = PH "&tau;"
  greek Tau    = PH "&Tau;"
  greek Theta_L = PH "&theta;"
  greek Theta   = PH "&Theta;"
  greek Upsilon_L  = PH "&upsilon;"
  greek Upsilon    = PH "&Upsilon;"

instance RenderSpecial PrintHTML where
  special Circle  = PH "&deg;"
  special Partial  = PH "&part;"
  special LEQ       = PH "&le;"
  special UScore  = PH "_"
