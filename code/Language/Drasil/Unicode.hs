-- | Special characters
module Language.Drasil.Unicode where

-- | Greek alphabet. @_L@ following the name represents lower case version.
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
           | Pi_L
           | Pi
           | Phi_L
           | Phi_V
           | Phi
           | Rho_L
           | Rho
           | Tau_L
           | Tau
           | Theta_L
           | Theta
           | Upsilon_L
           | Upsilon
  deriving (Eq, Ord)

-- | Special characters including @<=@, partial derivatives, degree circle, and
-- underscores
data Special = LEQ | Partial | Circle | UScore --underscore
  deriving (Eq, Ord)

-- Class for rendering greek characters
class RenderGreek r where
  greek :: Greek -> r

-- | Class for rendering special characters
class RenderSpecial r where
  special :: Special -> r
