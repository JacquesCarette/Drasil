{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeChunk(..), CodeDefinition(..),
    programName,
    codeType, codevar, codefunc, qtoc, qtov, codeEquat,
    ConstraintMap, constraintMap, physLookup, sfwrLookup, constraintLookup,
    symbToCodeName, CodeType(..),
    spaceToCodeType, toCodeName, funcPrefix
  ) where

import Control.Lens ((^.),makeLenses,view)

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

programName :: NamedIdea c => c -> String
programName c = sentenceToCodeName (phrase $ c ^. term)

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
 
data VarOrFunc = Var | Func
data CodeChunk = CodeC {_qc :: QuantityDict, kind :: VarOrFunc}
makeLenses ''CodeChunk

instance Chunk CodeChunk where id = qc . id
instance NamedIdea CodeChunk where term = qc . term
instance Idea CodeChunk where getA = getA . view qc
instance HasSpace CodeChunk where typ = qc . typ
instance HasSymbol CodeChunk where symbol s c = symbol s (c ^. qc)
instance Quantity CodeChunk where getUnit = getUnit . view qc
instance CodeIdea CodeChunk where
  codeName (CodeC c Var) = symbToCodeName (codeSymb c)
  codeName (CodeC c Func) = funcPrefix ++ symbToCodeName (codeSymb c)
instance Eq CodeChunk where c1 == c2 = (c1 ^. id) == (c2 ^. id)

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
spaceToCodeType (S.DiscreteI _) = G.List (spaceToCodeType S.Integer)
spaceToCodeType (S.DiscreteD _) = G.List (spaceToCodeType S.Rational)
spaceToCodeType (S.DiscreteS _) = G.List (spaceToCodeType S.String)

codeType :: HasSpace c => c -> CodeType
codeType c = spaceToCodeType $ c ^. typ

codevar :: (Quantity c) => c -> CodeChunk
codevar c = CodeC (qw c) Var

codefunc :: (Quantity c) => c -> CodeChunk
codefunc c = CodeC (qw c) Func

data CodeDefinition = CD { _quant :: QuantityDict, _ci :: String, _def :: Expr }
makeLenses ''CodeDefinition

instance Chunk CodeDefinition where id = quant . id
instance NamedIdea CodeDefinition where term = quant . term
instance Idea CodeDefinition where getA = getA . view quant
instance HasSpace CodeDefinition where typ = quant . typ
instance HasSymbol CodeDefinition where symbol s c = symbol s $ c ^. quant
instance Quantity CodeDefinition where getUnit = getUnit . view quant
instance CodeIdea CodeDefinition where codeName = (^. ci)
instance Eq CodeDefinition where c1 == c2 = (c1 ^. id) == (c2 ^. id)

qtoc :: QDefinition -> CodeDefinition
qtoc (EC q e _) = CD (qw q) (funcPrefix ++ symbToCodeName (codeSymb q)) e

qtov :: QDefinition -> CodeDefinition
qtov (EC q e _) = CD (qw q) (symbToCodeName (codeSymb q)) e

codeEquat :: CodeDefinition -> Expr
codeEquat cd = cd ^. def

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
