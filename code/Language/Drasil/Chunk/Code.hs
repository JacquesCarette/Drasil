{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeEntity(..), CodeName(..)
  ) where

import Control.Lens

import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk

import Language.Drasil.Code.Code (CodeType(..))

import Prelude hiding (id)

class (Chunk c) => CodeIdea c where
  codeName      :: Simple Lens c String

class (CodeIdea c, Quantity c) => CodeEntity c where
  codeType      :: Simple Lens c CodeType

data CodeName where
  SFCN :: (SymbolForm c) => c -> CodeName
  NICN :: (NamedIdea c)  => c -> CodeName
  
instance Chunk CodeName where
  id = cnlens id
instance CodeIdea CodeName where
  -- want to take symbol lens from SymbolForm and apply symbToCodeName to it
  -- to make codeName lens for CodeName
  codeName f sfcn@(SFCN _) = sfcnlens symbol f sfcn  
  -- want to take term lens from NamedIdea and apply sentenceToCodeName to it
  -- to make codeName lens for CodeName
  codeName f nicn@(NICN _) = nicnlens term f nicn
instance Eq CodeName where
  c1 == c2 = 
    (c1 ^. id) == (c2 ^. id)

sfcnlens :: (forall c. (SymbolForm c) => Simple Lens c a) 
             -> Simple Lens CodeName a
sfcnlens l f (SFCN a) = fmap (\x -> SFCN (set l x a)) (f (a ^. l))

nicnlens :: (forall c. (NamedIdea c) => Simple Lens c a) 
             -> Simple Lens CodeName a
nicnlens l f (NICN a) = fmap (\x -> NICN (set l x a)) (f (a ^. l))
  

sentenceToCodeName :: Sentence -> String
sentenceToCodeName (S s) = toCodeName s
sentenceToCodeName _ = error "fix"

symbToCodeName :: Symbol -> String
symbToCodeName (Atomic s) = toCodeName s
symbToCodeName (Special sp) = specialToCodeName sp
symbToCodeName (Greek g) = greekToCodeName g
symbToCodeName (Atop d sy) = decorate (symbToCodeName sy) d
symbToCodeName (Corners ul ll ur lr b) = 
  (cleft ul) ++ (cleft ll) ++ (symbToCodeName b)
    ++ (cright lr) ++ (cright ur)
  where cleft :: [Symbol] -> String
        cleft [] = ""
        cleft (s:syms) = symbToCodeName s ++ "_" ++ cleft syms
        cright :: [Symbol] -> String
        cright [] = ""
        cright (s:syms) = "_" ++ symbToCodeName s ++ cright syms
symbToCodeName (Concat sy) = concatMap symbToCodeName sy

decorate :: String -> Decoration -> String
decorate s Hat = s ++ "_hat"
decorate s Vector = s ++ "_vect"

greekToCodeName :: Greek -> String
greekToCodeName Alpha_L   = "alpha"
greekToCodeName Alpha     = "Alpha"
greekToCodeName Beta_L    = "beta"
greekToCodeName Beta      = "Beta"
greekToCodeName Chi_L     = "chi"
greekToCodeName Chi       = "Chi"
greekToCodeName Delta_L   = "delta"
greekToCodeName Delta     = "Delta"
greekToCodeName Ell       = "ell"
greekToCodeName Epsilon_L = "epsilon"
greekToCodeName Epsilon_V = "varepsilon"
greekToCodeName Epsilon   = "Epsilon"
greekToCodeName Eta_L     = "eta"
greekToCodeName Eta       = "Eta"
greekToCodeName Gamma_L   = "gamma"
greekToCodeName Gamma     = "Gamma"
greekToCodeName Iota_L    = "iota"
greekToCodeName Iota      = "Iota"
greekToCodeName Kappa_L   = "kappa"
greekToCodeName Kappa     = "Kappa"
greekToCodeName Lambda_L  = "lambda"
greekToCodeName Lambda    = "Lambda"
greekToCodeName Mu_L      = "mu"
greekToCodeName Mu        = "Mu"
greekToCodeName Nabla     = "nabla"
greekToCodeName Nu_L      = "nu"
greekToCodeName Nu        = "Nu"
greekToCodeName Omega_L   = "omega"
greekToCodeName Omega     = "Omega"
greekToCodeName Omicron_L = "omicron"
greekToCodeName Omicron   = "Omicron"
greekToCodeName Pi_L      = "pi"
greekToCodeName Pi        = "Pi"
greekToCodeName Phi_L     = "phi"
greekToCodeName Phi_V     = "varphi"
greekToCodeName Phi       = "Phi"
greekToCodeName Psi_L     = "psi"
greekToCodeName Psi       = "Psi"
greekToCodeName Rho_L     = "rho"
greekToCodeName Rho       = "Rho"
greekToCodeName Sigma_L   = "sigma"
greekToCodeName Sigma     = "Sigma"
greekToCodeName Tau_L     = "tau"
greekToCodeName Tau       = "Tau"
greekToCodeName Theta_L   = "theta"
greekToCodeName Theta     = "Theta"
greekToCodeName Upsilon_L = "upsilon"
greekToCodeName Upsilon   = "Upsilon"
greekToCodeName Xi_L      = "xi"
greekToCodeName Xi        = "Xi"
greekToCodeName Zeta_L    = "zeta"
greekToCodeName Zeta      = "Zeta"

specialToCodeName :: Special -> String
specialToCodeName Circle   = "circ"
specialToCodeName LEQ      = "leq"
specialToCodeName Partial  = "partial"
specialToCodeName UScore   = "_"

toCodeName :: String -> String
toCodeName s =
    let illegalChars = [
            "~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","{","}","|",":","\"","<",">","?"," "]
    in foldl varNameReplace s illegalChars
    where  varNameReplace :: String -> String -> String
           varNameReplace l old = replace old "_" l
           