-- | Special characters
module Language.Drasil.Unicode where

-- | Greek alphabet. @_L@ following the name represents lower case version.
data Greek = 
             Lambda
           | Mu_L
           | Mu
           | Nu_L
           | Nu
           | Xi_L
           | Xi
           | Omicron_L
           | Omicron
           | Pi_L
           | Pi
           | Rho_L
           | Rho
           | Sigma_L
           | Sigma
           | Tau_L
           | Tau
           | Upsilon_L
           | Upsilon
           | Phi_L
           | Phi_V
           | Phi
           | Chi_L
           | Chi
           | Psi_L
           | Psi
           | Omega_L
           | Omega
           --end of standard greek alphabet
           | Ell
           | Nabla
  deriving (Eq, Ord)

-- | Special characters including partial derivatives, degree circle, and
-- underscores
data Special = Partial
             | Circle
             | Percent
             | Hash
             | CurlyBrOpen
             | CurlyBrClose
             | SqBrOpen
             | SqBrClose
             | UScore
  deriving (Eq, Ord)

-- Class for rendering greek characters
class RenderGreek r where
  greek :: Greek -> r

-- | Class for rendering special characters
class RenderSpecial r where
  special :: Special -> r
