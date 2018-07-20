{-# LANGUAGE TemplateHaskell #-}
module Language.Drasil.Chunk.Code (
    CodeIdea(..), CodeChunk(..), CodeDefinition(..),
    codeType, codevar, codefunc, qtoc, qtov, codeEquat,
    ConstraintMap, constraintMap, physLookup, sfwrLookup,
    programName, symbToCodeName, --CodeType(..), - not defined here
    spaceToCodeType, toCodeName, funcPrefix
  ) where

import Control.Lens ((^.),makeLenses,view)

import Language.Drasil-- hiding (CodeType(..))
import qualified Language.Drasil.Code.Code as G

import Data.String.Utils (replace)
import qualified Data.Map as Map

-- not using lenses for now
class CodeIdea c where
  codeName      :: c -> String

programName :: CommonIdea c => c -> String
programName = toCodeName . abrv

symbToCodeName :: Symbol -> String
symbToCodeName (Atomic s) = toCodeName s
symbToCodeName (Special sp) = specialToCodeName sp
--symbToCodeName (Greek g) = greekToCodeName g
symbToCodeName (Atop d s) = decorate (symbToCodeName s) d
symbToCodeName (Corners ul ll ur lr b) =
  cleft ul ++ cleft ll ++ symbToCodeName b
    ++ cright lr ++ cright ur
  where cleft :: [Symbol] -> String
        cleft [] = ""
        cleft (s:syms) = symbToCodeName s ++ "_" ++ cleft syms
        cright :: [Symbol] -> String
        cright [] = ""
        cright (s:syms) = "_" ++ symbToCodeName s ++ cright syms
symbToCodeName (Concat s) = concatMap symbToCodeName s
symbToCodeName Empty = ""

decorate :: String -> Decoration -> String
decorate s Hat = s ++ "_hat"
decorate s Vector = s ++ "_vect"
decorate s Prime = s ++ "'"

--greekToCodeName :: Greek -> String
--greekToCodeName Alpha_L   = "alpha"
--greekToCodeName Alpha     = "Alpha"
--greekToCodeName Beta_L    = "beta"
--greekToCodeName Beta      = "Beta"
--greekToCodeName Chi_L     = "chi"
--greekToCodeName Chi       = "Chi"
--greekToCodeName Delta_L   = "delta"
--greekToCodeName Delta     = "Delta"
--greekToCodeName Ell       = "ell"
--greekToCodeName Epsilon_L = "epsilon"
--greekToCodeName Epsilon_V = "varepsilon"
--greekToCodeName Epsilon   = "Epsilon"
--greekToCodeName Eta_L     = "eta"
--greekToCodeName Eta       = "Eta"
--greekToCodeName Gamma_L   = "gamma"
--greekToCodeName Gamma     = "Gamma"
--greekToCodeName Iota_L    = "iota"
--greekToCodeName Iota      = "Iota"
--greekToCodeName Kappa_L   = "kappa"
--greekToCodeName Kappa     = "Kappa"
--greekToCodeName Lambda_L  = "lambda"
--greekToCodeName Lambda    = "Lambda"
--greekToCodeName Mu_L      = "mu"
--greekToCodeName Mu        = "Mu"
--greekToCodeName Nabla     = "nabla"
--greekToCodeName Nu_L      = "nu"
--greekToCodeName Nu        = "Nu"
--greekToCodeName Omega_L   = "omega"
--greekToCodeName Omega     = "Omega"
--greekToCodeName Omicron_L = "omicron"
--greekToCodeName Omicron   = "Omicron"
--greekToCodeName Pi_L      = "pi"
--greekToCodeName Pi        = "Pi"
--greekToCodeName Phi_L     = "phi"
--greekToCodeName Phi_V     = "varphi"
--greekToCodeName Phi       = "Phi"
--greekToCodeName Psi_L     = "psi"
--greekToCodeName Psi       = "Psi"
--greekToCodeName Rho_L     = "rho"
--greekToCodeName Rho       = "Rho"
--greekToCodeName Sigma_L   = "sigma"
--greekToCodeName Sigma     = "Sigma"
--greekToCodeName Tau_L     = "tau"
--greekToCodeName Tau       = "Tau"
--greekToCodeName Theta_L   = "theta"
--greekToCodeName Theta     = "Theta"
--greekToCodeName Upsilon_L = "upsilon"
--greekToCodeName Upsilon   = "Upsilon"
--greekToCodeName Xi_L      = "xi"
--greekToCodeName Xi        = "Xi"
--greekToCodeName Zeta_L    = "zeta"
--greekToCodeName Zeta      = "Zeta"

specialToCodeName :: Special -> String
specialToCodeName Circle        = "circ"
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
data CodeChunk = CodeC { _qc :: QuantityDict
                       , kind :: VarOrFunc
                       }
makeLenses ''CodeChunk

instance HasUID    CodeChunk where uid = qc . uid
instance NamedIdea CodeChunk where term = qc . term
instance Idea      CodeChunk where getA = getA . view qc
instance HasSpace  CodeChunk where typ = qc . typ
instance HasSymbol CodeChunk where symbol c = symbol (c ^. qc)
instance Quantity  CodeChunk where getUnit = getUnit . view qc
instance CodeIdea  CodeChunk where
  codeName (CodeC c Var) = symbToCodeName (codeSymb c)
  codeName (CodeC c Func) = funcPrefix ++ symbToCodeName (codeSymb c)
instance Eq        CodeChunk where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)

spaceToCodeType :: Space -> G.CodeType
spaceToCodeType Integer       = G.Integer
spaceToCodeType Natural       = G.Integer
spaceToCodeType Radians       = G.Float
spaceToCodeType Real          = G.Float
spaceToCodeType Rational      = G.Float
spaceToCodeType Boolean       = G.Boolean
spaceToCodeType Char          = G.Char
spaceToCodeType String        = G.String
spaceToCodeType (Vect s)      = G.List (spaceToCodeType s)
spaceToCodeType (DiscreteI _) = G.List (spaceToCodeType Integer)
spaceToCodeType (DiscreteD _) = G.List (spaceToCodeType Rational)
spaceToCodeType (DiscreteS _) = G.List (spaceToCodeType String)

codeType :: HasSpace c => c -> G.CodeType
codeType c = spaceToCodeType $ c ^. typ

codevar :: (Quantity c) => c -> CodeChunk
codevar c = CodeC (qw c) Var

codefunc :: (Quantity c) => c -> CodeChunk
codefunc c = CodeC (qw c) Func

data CodeDefinition = CD { _quant :: QuantityDict
                         , _ci :: String
                         , _def :: Expr
                         }
makeLenses ''CodeDefinition

instance HasUID        CodeDefinition where uid = quant . uid
instance NamedIdea     CodeDefinition where term = quant . term
instance Idea          CodeDefinition where getA = getA . view quant
instance HasSpace      CodeDefinition where typ = quant . typ
instance HasSymbol     CodeDefinition where symbol c = symbol (c ^. quant)
instance Quantity      CodeDefinition where getUnit = getUnit . view quant
instance CodeIdea      CodeDefinition where codeName = (^. ci)
instance Eq            CodeDefinition where c1 == c2 = (c1 ^. uid) == (c2 ^. uid)

qtoc :: (Quantity q, ExprRelat q, HasSymbol q) => q -> CodeDefinition
qtoc q = CD (qw q) (funcPrefix ++ symbToCodeName (codeSymb q)) (q ^. relat)

qtov :: QDefinition -> CodeDefinition
qtov q = CD (qw q) (symbToCodeName (codeSymb q)) (q ^. relat)

codeEquat :: CodeDefinition -> Expr
codeEquat cd = cd ^. def

type ConstraintMap = Map.Map UID [Constraint]

constraintMap :: (HasUID c, Constrained c) => [c] -> ConstraintMap
constraintMap = Map.fromList . map (\x -> (x ^. uid, x ^. constraints))

physLookup :: (Quantity q) => ConstraintMap -> q -> (q,[Constraint])
physLookup m q = constraintLookup' q m (filter isPhysC)

sfwrLookup :: (Quantity q) => ConstraintMap -> q -> (q,[Constraint])
sfwrLookup m q = constraintLookup' q m (filter isPhysC)

constraintLookup' :: (Quantity q) => q -> ConstraintMap
                      -> ([Constraint] -> [Constraint]) -> (q , [Constraint])
constraintLookup' q m filt = (q, maybe [] filt (Map.lookup (q^.uid) m))
