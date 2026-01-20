{-# LANGUAGE GADTs #-}
module Drasil.Code.CodeExpr.Lang where

import Prelude hiding (sqrt)
import Control.Lens ((^.))

import Drasil.Database (UID, HasUID(..))

import Language.Drasil.Expr.Lang
  (Completeness(..), ArithBinOp(..), EqBinOp(..), BoolBinOp(..),
   LABinOp(..), OrdBinOp(..), EqBinOp(..),
   VVVBinOp(..), VVNBinOp(..), NVVBinOp(..), ESSBinOp(..), ESBBinOp(..),
   AssocArithOper(..), AssocBoolOper(..), AssocConcatOper(..),
   UFunc(..), UFuncB(..), UFuncVV(..), UFuncVN(..))
import Language.Drasil.Expr.Class (ExprC(..), square)
import Language.Drasil.Literal.Class (LiteralC(..))
import Language.Drasil.Literal.Lang (Literal(..))
import Language.Drasil.Space (Space, RealInterval, DiscreteDomainDesc,
  DomainDesc(BoundedDD), RTopology(..))

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
  -- | Unary operation for @Vector -> Vector@ operations.
  UnaryOpVV     :: UFuncVV -> CodeExpr -> CodeExpr
  -- | Unary operation for @Vector -> Number@ operations.
  UnaryOpVN     :: UFuncVN -> CodeExpr -> CodeExpr

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
  -- | Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: VVNBinOp -> CodeExpr -> CodeExpr -> CodeExpr
  -- | Binary operator for @Number x Vector -> Vector@ operations (scaling).
  NVVBinaryOp   :: NVVBinOp -> CodeExpr -> CodeExpr -> CodeExpr
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

instance LiteralC CodeExpr where
  str      = Lit . str
  int      = Lit . int
  dbl      = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r

instance ExprC CodeExpr where
  lit = Lit

  -- | Smart constructor for equating two expressions.
  ($=) = EqBinaryOp Eq
  -- | Smart constructor for showing that two expressions are not equal.
  ($!=) = EqBinaryOp NEq

  -- | Smart constructor for ordering two equations.
  -- | Less than.
  ($<) = OrdBinaryOp Lt
  -- | Greater than.
  ($>) = OrdBinaryOp Gt
  -- | Less than or equal to.
  ($<=) = OrdBinaryOp LEq
  -- | Greater than or equal to.
  ($>=) = OrdBinaryOp GEq

  -- | Smart constructor for the dot product of two equations.
  ($.) = VVNBinaryOp Dot

  -- | Add two expressions.
  ($+) (Lit (Int 0)) r = r
  ($+) l (Lit (Int 0)) = l
  ($+) (Lit (Dbl 0)) r = r
  ($+) l (Lit (Dbl 0)) = l
  ($+) l (Lit (ExactDbl 0)) = l
  ($+) (Lit (ExactDbl 0)) r = r
  ($+) (AssocA Add l) (AssocA Add r) = AssocA Add (l ++ r)
  ($+) (AssocA Add l) r = AssocA Add (l ++ [r])
  ($+) l (AssocA Add r) = AssocA Add (l : r)
  ($+) l r = AssocA Add [l, r]

  -- | Multiply two expressions.
  ($*) (Lit (Int 1)) r = r
  ($*) l (Lit (Int 1)) = l
  ($*) (Lit (Dbl 1.0)) r = r
  ($*) l (Lit (Dbl 1.0)) = l
  ($*) l (Lit (ExactDbl 1)) = l
  ($*) (Lit (ExactDbl 1)) r = r
  ($*) (AssocA Mul l) (AssocA Mul r) = AssocA Mul (l ++ r)
  ($*) (AssocA Mul l) r = AssocA Mul (l ++ [r])
  ($*) l (AssocA Mul r) = AssocA Mul (l : r)
  ($*) l r = AssocA Mul [l,r]

  -- | Smart constructor for subtracting two expressions.
  ($-) = ArithBinaryOp Subt
  -- | Smart constructor for dividing two expressions.
  ($/) = ArithBinaryOp Frac
  -- | Smart constructor for rasing the first expression to the power of the second.
  ($^) = ArithBinaryOp Pow

  -- | Smart constructor to show that one expression implies the other (conditional operator).
  ($=>) = BoolBinaryOp Impl
  -- | Smart constructor to show that an expression exists if and only if another expression exists (biconditional operator).
  ($<=>) = BoolBinaryOp Iff

  -- | Smart constructor for the boolean /and/ operator.
  a $&& b = AssocB And [a, b]
  -- | Smart constructor for the boolean /or/ operator.
  a $|| b = AssocB Or  [a, b]

  in' = ESBBinaryOp SContains

  -- | Smart constructor for taking the absolute value of an expression.
  abs_ = UnaryOp Abs

  -- | Smart constructor for negating an expression.
  neg = UnaryOp Neg

  -- | Smart constructor to take the log of an expression.
  log = UnaryOp Log

  -- | Smart constructor to take the ln of an expression.
  ln = UnaryOp Ln

  -- | Smart constructor to take the square root of an expression.
  sqrt = UnaryOp Sqrt

  -- | Smart constructor to apply sin to an expression.
  sin = UnaryOp Sin

  -- | Smart constructor to apply cos to an expression.
  cos = UnaryOp Cos

  -- | Smart constructor to apply tan to an expression.
  tan = UnaryOp Tan

  -- | Smart constructor to apply sec to an expression.
  sec = UnaryOp Sec

  -- | Smart constructor to apply csc to an expression.
  csc = UnaryOp Csc

  -- | Smart constructor to apply cot to an expression.
  cot = UnaryOp Cot

  -- | Smart constructor to apply arcsin to an expression.
  arcsin = UnaryOp Arcsin

  -- | Smart constructor to apply arccos to an expression.
  arccos = UnaryOp Arccos

  -- | Smart constructor to apply arctan to an expression.
  arctan = UnaryOp Arctan

  -- | Smart constructor for the exponential (base e) function.
  exp = UnaryOp Exp

  -- | Smart constructor for calculating the dimension of a vector.
  dim = UnaryOpVN Dim

  -- | Smart constructor for calculating the normal form of a vector.
  norm = UnaryOpVN Norm

  -- | Smart constructor for negating vectors.
  negVec = UnaryOpVV NegV
  -- | And more general scaling
  vScale = NVVBinaryOp Scale

  -- | Smart constructor for applying logical negation to an expression.
  not_ = UnaryOpB Not

  -- | Smart constructor for indexing.
  idx = LABinaryOp Index

  idxOf = LABinaryOp IndexOf
  -- | Integrate over some expression with bounds (∫).
  defint v low high = Operator Add (BoundedDD v Continuous low high)

  -- | Sum over some expression with bounds (∑).
  defsum v low high = Operator Add (BoundedDD v Discrete low high)

  -- | Product over some expression with bounds (∏).
  defprod v low high = Operator Mul (BoundedDD v Discrete low high)

  -- | Smart constructor for 'real interval' membership.
  realInterval c = RealI (c ^. uid)

  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean = sqrt . foldr1 ($+) . map square

  -- | Smart constructor to cross product two expressions.
  cross = VVVBinaryOp Cross

  -- | Adding vectors
  vAdd = VVVBinaryOp VAdd
  -- | Subtracting vectors
  vSub = VVVBinaryOp VSub

  -- | Smart constructor for case statements with a complete set of cases.
  completeCase = Case Complete

  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase = Case Incomplete

  matrix = Matrix

  set' = Set
  -- | Applies a given function with a list of parameters.
  apply f [] = sy f
  apply f ps = FCall (f ^. uid) ps []

  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy x = C (x ^. uid)
