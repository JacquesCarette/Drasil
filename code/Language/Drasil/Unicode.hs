module Language.Drasil.Unicode where

data Greek = Alpha_L
           | Alpha  
           | Beta_L
           | Beta
           | Delta_L
           | Delta
           | Ell
           | Eta_L
           | Eta
           | Gamma_L
           | Gamma
           | Lambda_L
           | Lambda
           | Nabla
           | Nu_L
           | Nu
           | Omega_L
           | Omega
           | Phi_L
           | Phi_V
           | Phi
           | Rho_L
           | Rho
           | Tau_L
           | Tau
           | Upsilon_L
           | Upsilon

data Special = LEQ | Partial | Circle | UScore --underscore

class RenderGreek r where
  greek :: Greek -> r

class RenderSpecial r where
  special :: Special -> r

{-
instance Render Alpha where
  render TeX Alpha_L   = "\\alpha{}"
  render TeX Alpha     = "\\Alpha{}"
  render Plain Alpha_L = "alpha"
  render Plain Alpha   = "uAlpha"
  render HTML Alpha_L  = "&alpha;"
  render HTML Alpha    = "&Alpha;"
  
-- 

instance Render Beta where
  render TeX Beta_L   = "\\beta{}"
  render TeX Beta     = "\\Beta{}"
  render Plain Beta_L = "beta"
  render Plain Beta   = "uBeta"  
  render HTML Beta_L  = "&beta;"
  render HTML Beta    = "&Beta;"

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

instance Render Ell where
  render TeX Ell    = "\\ell{}"
  render Plain Ell  = "ell"
  render HTML Ell   = "&#8467;"
  
--

instance Render Eta where
  render TeX Eta_L   = "\\eta{}"
  render TeX Eta     = "\\Eta{}"
  render Plain Eta_L = "eta"
  render Plain Eta   = "uEta"
  render HTML Eta_L  = "&eta;"
  render HTML Eta    = "&Eta;"
  
--

instance Render Gamma where
  render TeX Gamma_L   = "\\gamma{}"
  render TeX Gamma     = "\\Gamma{}"
  render Plain Gamma_L = "gamma"
  render Plain Gamma   = "uGamma"
  render HTML Gamma_L  = "&gamma;"
  render HTML Gamma    = "&Gamma;"

--

instance Render Lambda where
  render TeX Lambda_L   = "\\lambda{}"
  render TeX Lambda     = "\\Lambda{}"
  render Plain Lambda_L = "lambda"
  render Plain Lambda   = "uLambda"
  render HTML Lambda_L  = "&lambda;"
  render HTML Lambda    = "&Lambda;"

--

instance Render LEQ where
  render TeX LEQ        = "\\leq{}"
  render Plain LEQ      = "leq"
  render HTML LEQ       = "&le;"
  
--

instance Render Nabla where
  render TeX Nabla   = "\\nabla{}"
  render Plain Nabla = "nabla"
  render HTML Nabla  = "&nabla;"

--

instance Render Nu where
  render TeX Nu_L   = "\\nu{}"
  render TeX Nu     = "\\Nu{}"
  render Plain Nu_L = "nu"
  render Plain Nu   = "uNu"
  render HTML Nu_L  = "&nu;"
  render HTML Nu    = "&Nu;"

--

instance Render Omega where
  render TeX Omega_L   = "\\omega{}"
  render TeX Omega     = "\\Omega{}"
  render Plain Omega_L = "omega"
  render Plain Omega   = "uOmega"
  render HTML Omega_L  = "&omega;"
  render HTML Omega    = "&Omega;"

--

instance Render Partial where
  render TeX Partial   = "\\partial{}"
  render Plain Partial = "partial"
  render HTML Partial  = "&part;"
--

instance Render Phi where
  render TeX Phi_L   = "\\phi{}"
  render TeX Phi_V   = "\\varphi{}"
  render TeX Phi     = "\\Phi{}"
  render Plain Phi_L = "phi"
  render Plain Phi_V = "varphi"
  render Plain Phi   = "uPhi"
  render HTML Phi_L  = "&phi;"
  render HTML Phi_V  = "&phiv;"
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

--

instance Render Upsilon where
  render TeX Upsilon_L   = "\\upsilon{}"
  render TeX Upsilon     = "\\Upsilon{}"
  render Plain Upsilon_L = "upsilon"
  render Plain Upsilon   = "uUpsilon"  
  render HTML Upsilon_L  = "&upsilon;"
  render HTML Upsilon    = "&Upsilon;"
-}
