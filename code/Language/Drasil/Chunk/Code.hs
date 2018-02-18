{-# LANGUAGE GADTs, Rank2Types #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeEntity(..), CodeName(..), CodeChunk(..), CodeDefinition(..),
    codevar, codefunc, qtoc, qtov, codeEquat, 
    ConstraintMap, constraintMap, physLookup, sfwrLookup, constraintLookup,
    symbToCodeName, CodeType(..),
    spaceToCodeType, toCodeName, funcPrefix
  ) where

import Control.Lens (Lens',(^.),set)

import Language.Drasil.Chunk.Constrained
import Language.Drasil.Chunk.Quantity
import Language.Drasil.Chunk.NamedIdea
import Language.Drasil.Chunk.Eq
import Language.Drasil.Chunk.SymbolForm
import Language.Drasil.Chunk

import Language.Drasil.Space as S
import Language.Drasil.Code.Code as G (CodeType(..))

import Language.Drasil.Expr
import Language.Drasil.Unicode
import Language.Drasil.Spec
import Language.Drasil.Symbol
import Language.Drasil.NounPhrase

import Data.String.Utils (replace)
import qualified Data.Map as Map

import Prelude hiding (id)

-- not using lenses for now
class (Chunk c) => CodeIdea c where
  codeName      :: c -> String

class (CodeIdea c, Quantity c) => CodeEntity c where
  codeType      :: c -> CodeType

data CodeName where
  SFCN :: (Quantity c) => c -> CodeName
  NICN :: (NamedIdea c)  => c -> CodeName
  
instance Chunk CodeName where
  id = cnlens id
instance CodeIdea CodeName where
  codeName (SFCN c) = symbToCodeName (codeSymb c)
  -- want to take term lens from NamedIdea and apply sentenceToCodeName to it
  -- to make codeName
  codeName (NICN c) = sentenceToCodeName (phrase $ c ^. term)
instance Eq CodeName where
  c1 == c2 = 
    (c1 ^. id) == (c2 ^. id)

cnlens :: (forall c. (Chunk c) => Lens' c a) 
           -> Lens' CodeName a
cnlens l f (SFCN a) = fmap (\x -> SFCN (set l x a)) (f (a ^. l))
cnlens l f (NICN a) = fmap (\x -> NICN (set l x a)) (f (a ^. l))
--sfcnlens :: (forall c. (SymbolForm c) => Lens' c a) 
--             -> Lens' CodeName a
--sfcnlens l f (SFCN a) = fmap (\x -> SFCN (set l x a)) (f (a ^. l))

--nicnlens :: (forall c. (NamedIdea c) => Lens' c a) 
--             -> Lens' CodeName a
--nicnlens l f (NICN a) = fmap (\x -> NICN (set l x a)) (f (a ^. l))
  

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
symbToCodeName Empty = ""

decorate :: String -> Decoration -> String
decorate s Hat = s ++ "_hat"
decorate s Vector = s ++ "_vect"
decorate s Prime = s ++ "'"

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
specialToCodeName Circle        = "circ"
specialToCodeName LEQ           = "leq"
specialToCodeName Partial       = "partial"
specialToCodeName UScore        = "_"
specialToCodeName Percent       = "%"
specialToCodeName CurlyBrOpen   = "{"
specialToCodeName CurlyBrClose  = "}"
specialToCodeName SqBrOpen      = "["
specialToCodeName SqBrClose     = "]"
specialToCodeName Hash          = "#" -- TODO: Double check that this is valid for 
                                      -- all of the output langs.

toCodeName :: String -> String
toCodeName s =
    let illegalChars = [
            ",","~","`","-","=","!","@","#","$","%","^","&","*","(",")","+",
            "[","]","\\",";","'",".","/","|",":","\"","<",">","?"," "]
    in foldl varNameReplace s illegalChars
    where  varNameReplace :: String -> String -> String
           varNameReplace l old = replace old "_" l


funcPrefix :: String
funcPrefix = "func_"
           
data CodeChunk where
  CodeVar :: (Quantity c) => c -> CodeChunk
  CodeFunc :: (Quantity c) => c -> CodeChunk
  
instance Chunk CodeChunk where
  id = qslens id
instance NamedIdea CodeChunk where
  term = qslens term
instance Idea CodeChunk where
  getA (CodeVar n) = getA n
  getA (CodeFunc n) = getA n
instance HasSpace CodeChunk where
  typ = qslens typ
instance HasSymbol CodeChunk where
  symbol s (CodeVar c)    = symbol s c
  symbol s (CodeFunc c)   = symbol s c
instance Quantity CodeChunk where
  getUnit (CodeVar c)     = getUnit c
  getUnit (CodeFunc c)    = getUnit c
instance CodeIdea CodeChunk where
  codeName (CodeVar c) = symbToCodeName (codeSymb c)
  codeName (CodeFunc c) = funcPrefix ++ symbToCodeName (codeSymb c)
instance CodeEntity CodeChunk where
  codeType (CodeVar c) = spaceToCodeType (c ^. typ)
  codeType (CodeFunc c) = spaceToCodeType (c ^. typ)
instance Eq CodeChunk where
  c1 == c2 = 
    (c1 ^. id) == (c2 ^. id)

qslens :: (forall c. (Quantity c) => Lens' c a) 
           -> Lens' CodeChunk a
qslens l f (CodeVar a) = 
  fmap (\x -> CodeVar (set l x a)) (f (a ^. l))
qslens l f (CodeFunc a) = 
  fmap (\x -> CodeFunc (set l x a)) (f (a ^. l))  
  
codevar :: (Quantity c) => c -> CodeChunk
codevar = CodeVar

codefunc :: (Quantity c) => c -> CodeChunk
codefunc = CodeFunc  
  
           
data CodeDefinition where
  CodeDefinition :: (CodeEntity c) => c -> Expr -> CodeDefinition
  
instance Chunk CodeDefinition where id = qscdlens id
instance NamedIdea CodeDefinition where term = qscdlens term
instance Idea CodeDefinition where getA (CodeDefinition n _) = getA n
instance HasSpace CodeDefinition where typ = qscdlens typ
instance HasSymbol CodeDefinition where symbol s (CodeDefinition c _)  = symbol s c
instance Quantity CodeDefinition where getUnit (CodeDefinition c _)    = getUnit c
instance CodeIdea CodeDefinition where codeName (CodeDefinition c _) = codeName c
instance CodeEntity CodeDefinition where codeType (CodeDefinition c _) = codeType c
instance Eq CodeDefinition where
  (CodeDefinition c1 _) == (CodeDefinition c2 _) = 
    (c1 ^. id) == (c2 ^. id)

qscdlens :: (forall c. (Quantity c) => Lens' c a) 
            -> Lens' CodeDefinition a
qscdlens l f (CodeDefinition a b) = 
  fmap (\x -> CodeDefinition (set l x a) b) (f (a ^. l)) 
  
qtoc :: QDefinition -> CodeDefinition
qtoc (EC q e _) = CodeDefinition (codefunc q) e

qtov :: QDefinition -> CodeDefinition
qtov (EC q e _) = CodeDefinition (codevar q) e

codeEquat :: CodeDefinition -> Expr
codeEquat (CodeDefinition _ e) = e 

spaceToCodeType :: Space -> CodeType
spaceToCodeType S.Integer = G.Integer
spaceToCodeType S.Natural = G.Integer
spaceToCodeType S.Radians = G.Float
spaceToCodeType S.Real = G.Float
spaceToCodeType S.Rational = G.Float
spaceToCodeType S.Boolean = G.Boolean
spaceToCodeType S.Char = G.Char
spaceToCodeType S.String = G.String
spaceToCodeType (S.Vect s) = G.List (spaceToCodeType s)
spaceToCodeType (S.Obj n) = G.Object (toCodeName n)
spaceToCodeType (S.DiscreteI _) = G.List (spaceToCodeType S.Integer)
spaceToCodeType (S.DiscreteD _) = G.List (spaceToCodeType S.Rational)
spaceToCodeType (S.DiscreteS _) = G.List (spaceToCodeType S.String)

type ConstraintMap = Map.Map String [Constraint]

constraintMap :: (Constrained c) => [c] -> ConstraintMap
constraintMap cs = Map.fromList (map (\x -> ((x ^. id), (x ^. constraints))) cs)

physLookup :: (Quantity q) => q -> ConstraintMap -> [Expr]
physLookup q m = constraintLookup' q m (filter isPhysC)

sfwrLookup :: (Quantity q) => q -> ConstraintMap -> [Expr]
sfwrLookup q m = constraintLookup' q m (filter isPhysC)

constraintLookup :: (Quantity q) => q -> ConstraintMap -> [Expr]
constraintLookup q m = constraintLookup' q m (\x -> x)

constraintLookup' :: (Quantity q) => q -> ConstraintMap 
                      -> ([Constraint] -> [Constraint]) -> [Expr]
constraintLookup' q m filt = 
  lookC (Map.lookup (q ^. id) m) q
  where lookC :: Quantity q => Maybe [Constraint] -> q -> [Expr]
        lookC (Just cs) s = map (\x -> renderC s x) (filt cs)
        lookC Nothing _ = []
