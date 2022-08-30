{-# LANGUAGE GADTs #-}

module Language.Drasil.Code.Expr where

import Language.Drasil
import Language.Drasil.Literal.Development
import Prelude hiding (sqrt)
import Control.Lens
import Language.Drasil.Expr.Development (Completeness(Complete, Incomplete))

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
data LABinOp = Index
  deriving Eq

-- | Ordered binary operators (less than, greater than, less than or equal to, greater than or equal to).
data OrdBinOp = Lt | Gt | LEq | GEq
  deriving Eq

-- | @Vector x Vector -> Vector@ binary operations (cross product).
data VVVBinOp = Cross
  deriving Eq

-- | @Vector x Vector -> Number@ binary operations (dot product).
data VVNBinOp = Dot
  deriving Eq

-- | Associative operators (adding and multiplication). Also specifies whether it is for integers or for real numbers.
data AssocArithOper = AddI | AddRe | MulI | MulRe
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

-- | @Vector -> Vector@ operators.
data UFuncVV = NegV
  deriving Eq

-- | @Vector -> Number@ operators.
data UFuncVN = Norm | Dim
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
  ($=)  = EqBinaryOp Eq
  -- | Smart constructor for showing that two expressions are not equal.
  ($!=) = EqBinaryOp NEq
  
  -- | Smart constructor for ordering two equations.
  -- | Less than.
  ($<)  = OrdBinaryOp Lt
  -- | Greater than.
  ($>)  = OrdBinaryOp Gt
  -- | Less than or equal to.
  ($<=) = OrdBinaryOp LEq
  -- | Greater than or equal to.
  ($>=) = OrdBinaryOp GEq
  
  -- | Smart constructor for the dot product of two equations.
  ($.) = VVNBinaryOp Dot
  
  -- | Add two expressions (Integers).
  addI l (Lit (Int 0)) = l
  addI (Lit (Int 0)) r = r
  addI (AssocA AddI l) (AssocA AddI r) = AssocA AddI (l ++ r)
  addI (AssocA AddI l) r = AssocA AddI (l ++ [r])
  addI l (AssocA AddI r) = AssocA AddI (l : r)
  addI l r = AssocA AddI [l, r]
  
  -- | Add two expressions (Real numbers).
  addRe l (Lit (Dbl 0))= l
  addRe (Lit(Dbl 0)) r      = r
  addRe l (Lit (ExactDbl 0)) = l
  addRe (Lit (ExactDbl 0)) r = r
  addRe (AssocA AddRe l) (AssocA AddRe r) = AssocA AddRe (l ++ r)
  addRe (AssocA AddRe l) r = AssocA AddRe (l ++ [r])
  addRe l (AssocA AddRe r) = AssocA AddRe (l : r)
  addRe l r = AssocA AddRe [l, r]
  
  -- | Multiply two expressions (Integers).
  mulI l (Lit (Int 1)) = l
  mulI (Lit (Int 1)) r = r
  mulI (AssocA MulI l) (AssocA MulI r) = AssocA MulI (l ++ r)
  mulI (AssocA MulI l) r = AssocA MulI (l ++ [r])
  mulI l (AssocA MulI r) = AssocA MulI (l : r)
  mulI l r = AssocA MulI [l, r]
  
  -- | Multiply two expressions (Real numbers).
  mulRe l (Lit (Dbl 1))      = l
  mulRe (Lit (Dbl 1)) r      = r
  mulRe l (Lit (ExactDbl 1)) = l
  mulRe (Lit (ExactDbl 1)) r = r
  mulRe (AssocA MulRe l) (AssocA MulRe r) = AssocA MulRe (l ++ r)
  mulRe (AssocA MulRe l) r = AssocA MulRe (l ++ [r])
  mulRe l (AssocA MulRe r) = AssocA MulRe (l : r)
  mulRe l r = AssocA MulRe [l, r]
  
  -- | Smart constructor for subtracting two expressions.
  ($-) = ArithBinaryOp Subt
  -- | Smart constructor for dividing two expressions.
  ($/) = ArithBinaryOp Frac
  -- | Smart constructor for rasing the first expression to the power of the second.
  ($^) = ArithBinaryOp Pow
  
  -- | Smart constructor to show that one expression implies the other (conditional operator).
  ($=>)  = BoolBinaryOp Impl
  -- | Smart constructor to show that an expression exists if and only if another expression exists (biconditional operator).
  ($<=>) = BoolBinaryOp Iff
  
  -- | Smart constructor for the boolean /and/ operator.
  a $&& b = AssocB And [a, b]
  -- | Smart constructor for the boolean /or/ operator.
  a $|| b = AssocB Or  [a, b]
  
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
  
  -- | Smart constructor for applying logical negation to an expression.
  not_ = UnaryOpB Not
  
  -- | Smart constructor for indexing.
  idx = LABinaryOp Index
  
  -- | Integrate over some expression with bounds (∫).
  defint v low high = Operator AddRe (BoundedDD v Continuous low high)
  
  -- | Sum over some expression with bounds (∑).
  defsum v low high = Operator AddRe (BoundedDD v Discrete low high)
  
  -- | Product over some expression with bounds (∏).
  defprod v low high = Operator MulRe (BoundedDD v Discrete low high)
  
  -- | Smart constructor for 'real interval' membership.
  realInterval c = RealI (c ^. uid)
  
  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean = sqrt . foldr1 addRe . map square
  
  -- | Smart constructor to cross product two expressions.
  cross = VVVBinaryOp Cross
  
  -- | Smart constructor for case statements with a complete set of cases.
  completeCase = Case Complete
  
  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase = Case Incomplete
  
  matrix = Matrix

  -- | Create a two-by-two matrix from four given values. For example:
  --
  -- >>> m2x2 1 2 3 4
  -- [ [1,2],
  --   [3,4] ]
  m2x2 a b c d = matrix [[a,b],[c,d]]
  
  -- | Create a 2D vector (a matrix with two rows, one column). First argument is placed above the second.
  vec2D a b    = matrix [[a],[b]]
  
  -- | Creates a diagonal two-by-two matrix. For example:
  --
  -- >>> dgnl2x2 1 2
  -- [ [1, 0],
  --   [0, 2] ]
  dgnl2x2 a  = m2x2 a (int 0) (int 0)
  
  -- | Create a row vector
  rowVec a = matrix [a] 

  -- | Create a column vector
  columnVec a = matrix $ toColumn a

  -- | Change row vector to column vector
  toColumn [] = []
  toColumn (x:xs) = [x]:toColumn xs

  -- Some helper functions to do function application
  
  -- FIXME: These constructors should check that the UID is associated with a
  -- chunk that is actually callable.
  -- | Applies a given function with a list of parameters.
  apply f ps = FCall (f ^. uid) ps []
  
  -- | Similar to 'apply', but takes a relation to apply to 'FCall'.
  applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
    (map snd ns))
  
  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy x = C (x ^. uid)
