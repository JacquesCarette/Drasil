{-# LANGUAGE GADTs #-}

-- | The Drasil Modelling Expression language
module Language.Drasil.ModelExpr.Lang where

import Prelude hiding (sqrt)

import Language.Drasil.Expr.Lang (Completeness)
import Language.Drasil.Literal.Lang (Literal(..))
import Language.Drasil.Space (Space, DomainDesc, RealInterval)
import Language.Drasil.UID (UID)
import Language.Drasil.Literal.Class (LiteralC(..))

-- Binary functions

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

-- | @Vector x Vector -> Vector@ binary operations (cross product, vector addition, subtraction).
data VVVBinOp = Cross | VAdd | VSub
  deriving Eq

-- | @Vector x Vector -> Number@ binary operations (dot product).
data VVNBinOp = Dot
  deriving Eq

-- | @Number x Vector -> Vector@ binary operations (scaling).
data NVVBinOp = Scale
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
data AssocBoolOper = And | Or | Equivalence
  deriving (Eq, Show)

-- | Unary functions (abs, log, ln, sin, etc.).
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving Eq

-- | @Bool -> Bool@ operators.
data UFuncB = Not
  deriving Eq

-- | @Vector -> Vector@ operators.
data UFuncVV = NegV
  deriving Eq

-- | @Vector -> Number@ operators.
data UFuncVN = Norm | Dim
  deriving Eq

-- | Statements involving 2 arguments.
data StatBinOp = Defines
  deriving Eq

-- | @Value -> Space -> Bool@ operators.
data SpaceBinOp = IsIn
  deriving Eq

-- | Determines the type of the derivative (either a partial derivative or a total derivative).
data DerivType = Part | Total
  deriving Eq

-- | Expression language where all terms are supposed to have a meaning, but
--   that meaning may not be that of a definite value. For example,
--   specification expressions, especially with quantifiers, belong here.
data ModelExpr where
  -- | Brings a literal into the expression language.
  Lit       :: Literal -> ModelExpr

  -- | Introduce Space values into the expression language.
  Spc       :: Space -> ModelExpr

  -- | Takes an associative arithmetic operator with a list of expressions.
  AssocA    :: AssocArithOper -> [ModelExpr] -> ModelExpr
  -- | Takes an associative boolean operator with a list of expressions.
  AssocB    :: AssocBoolOper  -> [ModelExpr] -> ModelExpr

  AssocC   :: AssocConcatOper -> [ModelExpr] -> ModelExpr
  -- | Derivative syntax is:
  --   Type ('Part'ial or 'Total') -> principal part of change -> with respect to
  --   For example: Deriv Part y x1 would be (dy/dx1).
  Deriv     :: Integer -> DerivType -> ModelExpr -> UID -> ModelExpr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C         :: UID -> ModelExpr
  -- | Function applications.
  FCall     :: UID -> [ModelExpr] -> ModelExpr
  -- | For multi-case expressions, each pair represents one case.
  Case      :: Completeness -> [(ModelExpr, ModelExpr)] -> ModelExpr
  -- | Represents a matrix of expressions.
  Matrix    :: [[ModelExpr]] -> ModelExpr
  -- | Represents a set of expressions
  Set :: [ModelExpr] -> ModelExpr
  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> ModelExpr -> ModelExpr
  -- | Unary operation for @Bool -> Bool@ operations.
  UnaryOpB      :: UFuncB -> ModelExpr -> ModelExpr
  -- | Unary operation for @Vector -> Vector@ operations.
  UnaryOpVV     :: UFuncVV -> ModelExpr -> ModelExpr
  -- | Unary operation for @Vector -> Number@ operations.
  UnaryOpVN     :: UFuncVN -> ModelExpr -> ModelExpr

  -- | Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: ArithBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for equality between expressions.
  EqBinaryOp    :: EqBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for indexing two expressions.
  LABinaryOp    :: LABinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: OrdBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Space-related binary operations.
  SpaceBinaryOp :: SpaceBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Statement-related binary operations.
  StatBinaryOp  :: StatBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: VVNBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Binary operator for @Number x Vector -> Vector@ operations (scaling).
  NVVBinaryOp   :: NVVBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Set operator for Element + Set -> Set
  ESSBinaryOp :: ESSBinOp -> ModelExpr -> ModelExpr -> ModelExpr
  -- | Set operator for Element + Set -> Bool
  ESBBinaryOp :: ESBBinOp -> ModelExpr -> ModelExpr -> ModelExpr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DomainDesc t ModelExpr ModelExpr -> ModelExpr -> ModelExpr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval ModelExpr ModelExpr -> ModelExpr
  
  -- | Universal quantification
  ForAll   :: UID -> Space -> ModelExpr -> ModelExpr

-- | The variable type is just a renamed 'String'.
type Variable = String

-- instance Num Expr where
--   (Int 0)        + b              = b
--   a              + (Int 0)        = a
--   (AssocA Add l) + (AssocA Add m) = AssocA Add (l ++ m)
--   (AssocA Add l) + b              = AssocA Add (l ++ [b])
--   a              + (AssocA Add l) = AssocA Add (a : l)
--   a              + b              = AssocA Add [a, b]

--   (AssocA Mul l) * (AssocA Mul m) = AssocA Mul (l ++ m)
--   (AssocA Mul l) * b              = AssocA Mul (l ++ [b])
--   a              * (AssocA Mul l) = AssocA Mul (a : l)
--   a              * b              = AssocA Mul [a, b]

--   a - b = ArithBinaryOp Subt a b
  
--   fromInteger = Int
--   abs         = UnaryOp Abs
--   negate      = UnaryOp Neg

--   -- this is a Num wart
--   signum _ = error "should not use signum in expressions"

-- | Expressions are equal if their constructors and contents are equal.
instance Eq ModelExpr where
  Lit l               == Lit r               =   l == r
  AssocA o1 l1        == AssocA o2 l2        =  o1 == o2 && l1 == l2
  AssocB o1 l1        == AssocB o2 l2        =  o1 == o2 && l1 == l2
  Deriv a t1 b c      == Deriv d t2 e f      =   a == d && t1 == t2 && b == e && c == f
  C a                 == C b                 =   a == b
  FCall a b           == FCall c d           =   a == c && b == d
  Case a b            == Case c d            =   a == c && b == d 
  UnaryOp a b         == UnaryOp c d         =   a == c && b == d
  UnaryOpB a b        == UnaryOpB c d        =   a == c && b == d
  UnaryOpVV a b       == UnaryOpVV c d       =   a == c && b == d
  UnaryOpVN a b       == UnaryOpVN c d       =   a == c && b == d
  ArithBinaryOp o a b == ArithBinaryOp p c d =   o == p && a == c && b == d
  BoolBinaryOp o a b  == BoolBinaryOp p c d  =   o == p && a == c && b == d
  EqBinaryOp o a b    == EqBinaryOp p c d    =   o == p && a == c && b == d
  OrdBinaryOp o a b   == OrdBinaryOp p c d   =   o == p && a == c && b == d
  SpaceBinaryOp o a b == SpaceBinaryOp p c d =   o == p && a == c && b == d
  StatBinaryOp o a b  == StatBinaryOp p c d  =   o == p && a == c && b == d
  LABinaryOp o a b    == LABinaryOp p c d    =   o == p && a == c && b == d
  VVVBinaryOp o a b   == VVVBinaryOp p c d   =   o == p && a == c && b == d
  VVNBinaryOp o a b   == VVNBinaryOp p c d   =   o == p && a == c && b == d
  ESSBinaryOp o a b   == ESSBinaryOp p c d   =   o == p && a == c && b == d
  ESBBinaryOp o a b   == ESBBinaryOp p c d   =   o == p && a == c && b == d
  _                   == _                   =   False
-- ^ TODO: This needs to add more equality checks

-- instance Fractional Expr where
--   a / b = ArithBinaryOp Frac a b
--   fromRational r = ArithBinaryOp Frac (fromInteger $ numerator   r)
--                                       (fromInteger $ denominator r)

instance LiteralC ModelExpr where
  int = Lit . int
  str = Lit . str
  dbl = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r
