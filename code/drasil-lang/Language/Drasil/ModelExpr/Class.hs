{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.ModelExpr.Class where

import Prelude hiding (sqrt, log, sin, cos, tan, exp)

import Control.Lens ((^.))

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Expr.Lang (Completeness(..))
import Language.Drasil.Expr.Class (ExprC(..))
import Language.Drasil.ModelExpr.Lang
import Language.Drasil.Space (DomainDesc(..), RTopology(..), RealInterval, Space)
import Language.Drasil.Classes.Core (HasSymbol, HasUID(..))
import Language.Drasil.Classes (IsArgumentName)

-- pderiv, deriv,
-- space, isIn, defines, andMEs, equivMEs

  
-- | Helper for creating new smart constructors for Associative Binary
--   operations that require at least 1 expression.
assocCreate :: AssocBoolOper -> [ModelExpr] -> ModelExpr
assocCreate abo [] = error $ "Need at least 1 expression to create " ++ show abo
assocCreate _ [x]  = x
assocCreate b des  = AssocB b $ assocSanitize b des
  
-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: AssocBoolOper -> [ModelExpr] -> [ModelExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocB c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

class ModelExprC r where
  -- This also wants a symbol constraint.
  -- | Gets the derivative of an 'ModelExpr' with respect to a 'Symbol'.
  deriv, pderiv :: (HasUID c, HasSymbol c) => r -> c -> r
  
  -- TODO: This "defines" is odd, it should really be accepting a (HasUID c, HasSymbol c) => c as a first parameter
  -- | One expression is "defined" by another.
  defines :: r -> r -> r
  
  isIn :: r -> Space -> r
  
  -- | Binary associative "And".
  andMEs :: [r] -> r
  
  -- | Binary associative "Equivalence".
  equivMEs :: [r] -> r

instance ModelExprC ModelExpr where
  deriv e c  = Deriv Total e (c ^. uid)
  pderiv e c = Deriv Part  e (c ^. uid)
  
  defines = StatBinaryOp Defines

  isIn a s = SpaceBinaryOp IsIn a (Spc s)  

  andMEs = assocCreate And
  
  equivMEs des
    | length des >= 2 = assocCreate Equivalence des
    | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence

instance ExprC ModelExpr where
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
  addI l (Int 0) = l
  addI (Int 0) r = r
  addI (AssocA AddI l) (AssocA AddI r) = AssocA AddI (l ++ r)
  addI (AssocA AddI l) r = AssocA AddI (l ++ [r])
  addI l (AssocA AddI r) = AssocA AddI (l : r)
  addI l r = AssocA AddI [l, r]
  
  -- | Add two expressions (Real numbers).
  addRe l (Dbl 0)      = l
  addRe (Dbl 0) r      = r
  addRe l (ExactDbl 0) = l
  addRe (ExactDbl 0) r = r
  addRe (AssocA AddRe l) (AssocA AddRe r) = AssocA AddRe (l ++ r)
  addRe (AssocA AddRe l) r = AssocA AddRe (l ++ [r])
  addRe l (AssocA AddRe r) = AssocA AddRe (l : r)
  addRe l r = AssocA AddRe [l, r]
  
  -- | Multiply two expressions (Integers).
  mulI l (Int 1) = l
  mulI (Int 1) r = r
  mulI (AssocA MulI l) (AssocA MulI r) = AssocA MulI (l ++ r)
  mulI (AssocA MulI l) r = AssocA MulI (l ++ [r])
  mulI l (AssocA MulI r) = AssocA MulI (l : r)
  mulI l r = AssocA MulI [l, r]
  
  -- | Multiply two expressions (Real numbers).
  mulRe l (Dbl 1)      = l
  mulRe (Dbl 1) r      = r
  mulRe l (ExactDbl 1) = l
  mulRe (ExactDbl 1) r = r
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
  
  -- | Smart constructor for integers.
  int = Int
  
  -- | Smart constructor for doubles.
  dbl = Dbl
  
  -- | Smart constructor for exact doubles.
  exactDbl = ExactDbl
  
  -- | Smart constructor for fractions.
  frac l r = exactDbl l $/ exactDbl r
  
  -- | Smart constructor for rational expressions (only in 1/x form).
  recip_ denom = exactDbl 1 $/ denom
  
  -- | Smart constructor for strings.
  str = Str
  
  -- | Smart constructors for percents.
  perc = Perc
  
  -- | Integrate over some expression with bounds (∫).
  defint v low high = Operator AddRe (BoundedDD v Continuous low high)
  -- | Integrate over some expression (∫).
  intAll v = Operator AddRe (AllDD v Continuous)
  
  -- | Sum over some expression with bounds (∑).
  defsum v low high = Operator AddRe (BoundedDD v Discrete low high)
  -- | Sum over some expression (∑).
  sumAll v = Operator AddRe (AllDD v Discrete)
  
  -- | Product over some expression with bounds (∏).
  defprod v low high = Operator MulRe (BoundedDD v Discrete low high)
  -- | Product over some expression (∏).
  prodAll v = Operator MulRe (AllDD v Discrete)
  -- TODO: Above only does for Reals
  
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
  
  -- | Smart constructor to square a function.
  square x = x $^ exactDbl 2
  
  -- | Smart constructor to half a function exactly.
  half x = x $/ exactDbl 2
  
  -- | Constructs 1/2.
  oneHalf = frac 1 2
  
  -- | Constructs 1/3.
  oneThird = frac 1 3
  
  -- | Create a two-by-two matrix from four given values. For example:
  --
  -- >>> m2x2 1 2 3 4
  -- [ [1,2],
  --   [3,4] ]
  m2x2 a b c d = Matrix [[a,b],[c,d]]
  
  -- | Create a 2D vector (a matrix with two rows, one column). First argument is placed above the second.
  vec2D a b    = Matrix [[a],[b]]
  
  -- | Creates a diagonal two-by-two matrix. For example:
  --
  -- >>> dgnl2x2 1 2
  -- [ [1, 0],
  --   [0, 2] ]
  dgnl2x2 a  = m2x2 a (Int 0) (Int 0)
  
  -- Some helper functions to do function application
  
  -- FIXME: These constructors should check that the UID is associated with a
  -- chunk that is actually callable.
  -- | Applies a given function with a list of parameters.
  apply f ps = FCall (f ^. uid) ps []
  
  -- | Similar to 'apply', but converts second argument into 'Symbol's.
  apply1 f a = FCall (f ^. uid) [sy a] []
  
  -- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
  apply2 f a b = FCall (f ^. uid) [sy a, sy b] []
  
  -- | Similar to 'apply', but takes a relation to apply to 'FCall'.
  applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
    (map snd ns))
  
  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy x = C (x ^. uid)
  
