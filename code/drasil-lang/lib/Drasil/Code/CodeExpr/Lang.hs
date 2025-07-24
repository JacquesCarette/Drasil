{-# LANGUAGE GADTs #-}

module Drasil.Code.CodeExpr.Lang where

import Prelude hiding (sqrt)
import Numeric.Natural

import Language.Drasil.Expr.Lang (Completeness, BasisBlades)
import Language.Drasil.Literal.Class (LiteralC(..))
import Language.Drasil.Literal.Lang (Literal(..))
import qualified Language.Drasil.Space as S
import Language.Drasil.Space (Space, RealInterval, DiscreteDomainDesc)
import Drasil.Database.UID (UID)

-- * Operators (mostly binary)

-- | Arithmetic operators (fractional, power, and subtraction).
data ArithBinOp = Frac | Pow | Subt
  deriving Eq

-- | Equality operators (equal or not equal).
data EqBinOp = Eq | NEq
  deriving Eq

-- | Conditional and Biconditional operators (Expressions can imply
-- one another, or exist if and only if another expression exists).
data BoolBinOp = Impl | Iff
  deriving Eq

-- | Index operator.
data LABinOp = Index | IndexOf
  deriving Eq

-- | Ordered binary operators (less than, greater than, less than or equal to, greater than or equal to).
data OrdBinOp = Lt | Gt | LEq | GEq
  deriving Eq

-- | @Clif x Clif -> Clif@ binary operations (cross product, clif addition, clif sub, wedge product, geometric product).
data CCCBinOp = Cross | CAdd | CSub | WedgeProd | GeometricProd
  deriving Eq

-- | @Clif x Clif -> Number@ binary operations (dot product).
data CCNBinOp = Dot
  deriving Eq

-- | @Number x Clif -> Clif@ binary operations (scaling).
data NCCBinOp = Scale
  deriving Eq

-- | @Natural x Clif -> Clif@ binary operations (grade selection).
data NatCCBinOp = GradeSelect
  deriving Eq

-- | Element + Set -> Set
data ESSBinOp = SAdd | SRemove
  deriving Eq

-- | Element + Set -> Bool
data ESBBinOp = SContains
  deriving Eq

data AssocConcatOper = SUnion
  deriving Eq
-- | Associative operators (adding and multiplication). Also specifies whether it is for integers or for real numbers.
data AssocArithOper = Add | Mul
  deriving Eq

-- | Associative boolean operators (and, or).
data AssocBoolOper = And | Or
  deriving Eq

-- | Unary functions (abs, log, ln, sin, etc.).
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving Eq

-- | @Bool -> Bool@ operators.
data UFuncB = Not
  deriving Eq

-- | @Clif -> Clif@ operators.
data UFuncCC = NegC
  deriving Eq

-- | @Clif -> Number@ operators (norm, dim, grade).
data UFuncCN = Norm | Dim | Grade
  deriving Eq

-- * CodeExpr

-- | Expression language where all terms also denote a term in GOOL
--   (i.e. translation is total and meaning preserving).
data CodeExpr where
  -- | Brings literals into the expression language.
  Lit      :: Literal -> CodeExpr

  -- | Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: AssocArithOper -> [CodeExpr] -> CodeExpr
  -- | Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [CodeExpr] -> CodeExpr

  AssocC :: AssocConcatOper -> [CodeExpr] -> CodeExpr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> CodeExpr
  -- | A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [CodeExpr] -> [(UID, CodeExpr)] -> CodeExpr
  -- | Actor creation given 'UID', parameters, and named parameters.
  New      :: UID -> [CodeExpr] -> [(UID, CodeExpr)] -> CodeExpr
  -- | Message an actor:
  --
  --   * 1st 'UID' is the actor,
  --   * 2nd 'UID' is the method.
  Message  :: UID -> UID -> [CodeExpr] -> [(UID, CodeExpr)] -> CodeExpr
  -- | Access a field of an actor:
  --
  --   * 1st 'UID' is the actor,
  --   * 2nd 'UID' is the field.
  Field    :: UID -> UID -> CodeExpr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(CodeExpr, CodeExpr)] -> CodeExpr
  -- | Represents a matrix of expressions.
  Matrix   :: [[CodeExpr]] -> CodeExpr
  -- | Represents a set of expressions
  Set      :: Space -> [CodeExpr] -> CodeExpr
  -- | used to refernce the (name + type = variable )
  Variable :: String -> CodeExpr -> CodeExpr
  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> CodeExpr -> CodeExpr
  -- | Unary operation for @Bool -> Bool@ operations.
  UnaryOpB      :: UFuncB -> CodeExpr -> CodeExpr
  -- | Unary operation for @Clif -> Clif@ operations.
  UnaryOpCC     :: UFuncCC -> CodeExpr -> CodeExpr
  -- | Unary operation for @Clif -> Number@ operations.
  UnaryOpCN     :: UFuncCN -> CodeExpr -> CodeExpr

  -- | Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: ArithBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for equality between expressions.
  EqBinaryOp    :: EqBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for indexing two expressions.
  LABinaryOp    :: LABinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: OrdBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Clif x Clif -> Clif@ operations (cross product).
  CCCBinaryOp   :: CCCBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Clif x Clif -> Number@ operations (dot product).
  CCNBinaryOp   :: CCNBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Number x Clif -> Clif@ operations (scaling).
  NCCBinaryOp   :: NCCBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Natural x Clif -> Clif@ operations (grade selection).
  NatCCBinaryOp   :: NatCCBinOp -> Natural -> CodeExpr -> CodeExpr
  -- | Set operator for Set + Set -> Set
  ESSBinaryOp :: ESSBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Set operator for Element + Set -> Bool
  ESBBinaryOp :: ESBBinOp -> CodeExpr -> CodeExpr -> CodeExpr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DiscreteDomainDesc CodeExpr CodeExpr -> CodeExpr -> CodeExpr
  -- | The expression is an element of a space.
  -- IsIn     :: Expr -> Space -> Expr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval CodeExpr CodeExpr -> CodeExpr

  Clif     :: S.Dimension -> BasisBlades CodeExpr -> CodeExpr

instance LiteralC CodeExpr where
  str      = Lit . str
  int      = Lit . int
  dbl      = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r
