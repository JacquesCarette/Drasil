module Language.Drasil.HTML.Monad where

import Language.Drasil.Unicode

-----------------------------------------------------------------------------
-- | Printing "monad".  Don't need context, so Identity (under another name)
-- will do just fine.

newtype PrintHTML = PH {unPH :: String}

instance RenderGreek PrintHTML where
 
  greek Alpha     = PH "&Alpha;"
  greek Beta_L    = PH "&beta;"
  greek Beta      = PH "&Beta;"
  greek Chi_L     = PH "&chi"
  greek Chi       = PH "&Chi"
  greek Delta_L   = PH "&delta;"
  greek Delta     = PH "&Delta;"
  greek Ell       = PH "&#8467;"
  greek Epsilon_L = PH "&#1013;"
  greek Epsilon_V = PH "&epsilon;"
  greek Epsilon   = PH "&Epsilon;"
  greek Eta_L     = PH "&eta;"
  greek Eta       = PH "&Eta;"
  greek Gamma_L   = PH "&gamma;"
  greek Gamma     = PH "&Gamma;"
  greek Iota_L    = PH "&iota;"
  greek Iota      = PH "&Iota;"
  greek Kappa_L   = PH "&kappa;"
  greek Kappa     = PH "&Kappa;"
  greek Lambda_L  = PH "&lambda;"
  greek Lambda    = PH "&Lambda;"
  greek Mu_L      = PH "&mu;"
  greek Mu        = PH "&Mu;"  
  greek Nabla     = PH "&nabla;"
  greek Nu_L      = PH "&nu;"
  greek Nu        = PH "&Nu;"
  greek Omega_L   = PH "&omega;"
  greek Omega     = PH "&Omega;"
  greek Omicron_L = PH "&omicron;"
  greek Omicron   = PH "&Omicron;"
  greek Pi_L      = PH "&pi;"
  greek Pi        = PH "&Pi;"
  greek Phi_L     = PH "&phi;"
  greek Phi_V     = PH "&phiv;"
  greek Phi       = PH "&Phi;"
  greek Psi_L     = PH "&psi;"
  greek Psi       = PH "&Psi;"
  greek Rho_L     = PH "&rho;"
  greek Rho       = PH "&Rho;"
  greek Sigma_L   = PH "&sigma;"
  greek Sigma     = PH "&Sigma;"
  greek Tau_L     = PH "&tau;"
  greek Tau       = PH "&Tau;"
  greek Theta_L   = PH "&theta;"
  greek Theta     = PH "&Theta;"
  greek Upsilon_L = PH "&upsilon;"
  greek Upsilon   = PH "&Upsilon;"
  greek Xi_L      = PH "&xi;"
  greek Xi        = PH "&Xi;"
  greek Zeta_L    = PH "&zeta;"
  greek Zeta      = PH "&Zeta;"
  
instance RenderSpecial PrintHTML where
  special Circle       = PH "&deg;"
  special Partial      = PH "&part;"
  special UScore       = PH "_"
  special Percent      = PH "%"
  special Hash         = PH "#"
  special CurlyBrOpen  = PH "{"
  special CurlyBrClose = PH "}"
  special SqBrOpen     = PH "["
  special SqBrClose    = PH "]"
