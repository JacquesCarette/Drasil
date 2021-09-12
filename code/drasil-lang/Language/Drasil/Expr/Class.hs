{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.Expr.Class (ExprC(..)) where

import Prelude hiding (sqrt, log, sin, cos, tan, exp)

import Control.Lens ((^.))

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Expr.Lang
import Language.Drasil.Space (DomainDesc(..), RTopology(..), RealInterval)
import Language.Drasil.Classes.Core (HasSymbol, HasUID(..))
import Language.Drasil.Classes (IsArgumentName)

class ExprC r where
  infixr 8 $^
  infixl 7 $/
  infixr 4 $=
  infixr 9 $&&
  infixr 9 $||
  
  -- * Binary Operators
  
  ($=), ($!=) :: r -> r -> r
  
  -- | Smart constructor for ordering two equations.
  ($<), ($>), ($<=), ($>=) :: r -> r -> r
  
  -- | Smart constructor for the dot product of two equations.
  ($.) :: r -> r -> r
  
  -- | Add two expressions (Integers).
  addI :: r -> r -> r
  
  -- | Add two expressions (Real numbers).
  addRe :: r -> r -> r
  
  -- | Multiply two expressions (Integers).
  mulI :: r -> r -> r
  
  -- | Multiply two expressions (Real numbers).
  mulRe :: r -> r -> r
  
  ($-), ($/), ($^) :: r -> r -> r
  
  ($=>), ($<=>) :: r -> r -> r
  
  ($&&), ($||) :: r -> r -> r
  
  -- | Smart constructor for taking the absolute value of an expression.
  abs_ :: r -> r
  
  -- | Smart constructor for negating an expression.
  neg :: r -> r 
  
  -- | Smart constructor to take the log of an expression.
  log :: r -> r
  
  -- | Smart constructor to take the ln of an expression.
  ln :: r -> r
  
  -- | Smart constructor to take the square root of an expression.
  sqrt :: r -> r
  
  -- | Smart constructor to apply sin to an expression.
  sin :: r -> r
  
  -- | Smart constructor to apply cos to an expression.
  cos :: r -> r 
  
  -- | Smart constructor to apply tan to an expression.
  tan :: r -> r
  
  -- | Smart constructor to apply sec to an expression.
  sec :: r -> r 
  
  -- | Smart constructor to apply csc to an expression.
  csc :: r -> r
  
  -- | Smart constructor to apply cot to an expression.
  cot :: r -> r 
  
  -- | Smart constructor to apply arcsin to an expression.
  arcsin :: r -> r 
  
  -- | Smart constructor to apply arccos to an expression.
  arccos :: r -> r 
  
  -- | Smart constructor to apply arctan to an expression.
  arctan :: r -> r 
  
  -- | Smart constructor for the exponential (base e) function.
  exp :: r -> r
  
  -- | Smart constructor for calculating the dimension of a vector.
  dim :: r -> r
  
  -- | Smart constructor for calculating the normal form of a vector.
  norm :: r -> r
  
  -- | Smart constructor for negating vectors.
  negVec :: r -> r
  
  -- | Smart constructor for applying logical negation to an expression.
  not_ :: r -> r
  
  -- | Smart constructor for indexing.
  idx :: r -> r -> r
  
  -- | Smart constructor for integers.
  int :: Integer -> r
  
  -- | Smart constructor for doubles.
  dbl :: Double -> r
  
  -- | Smart constructor for exact doubles.
  exactDbl :: Integer -> r
  
  -- | Smart constructor for fractions.
  frac :: Integer -> Integer -> r
  
  -- | Smart constructor for rational expressions (only in 1/x form).
  recip_ :: r -> r
  
  -- | Smart constructor for strings.
  str :: String -> r
  
  -- | Smart constructors for percents.
  perc :: Integer -> Integer -> r
  
  -- | Smart constructor for the summation, product, and integral functions over an interval.
  defint, defsum, defprod :: Symbol -> r -> r -> r -> r
  -- | Smart constructor for the summation, product, and integral functions over all Real numbers.
  intAll, sumAll, prodAll :: Symbol -> r -> r
  
  -- | Smart constructor for 'real interval' membership.
  realInterval :: HasUID c => c -> RealInterval r r -> r
  
  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean :: [r] -> r
  
-- TODO:  sum' :: (Num a, Foldable t) => t a -> a
-- TODO:  sum' = foldr1 (+)
    
  -- | Smart constructor to cross product two expressions.
  cross :: r -> r -> r
  
  -- | Smart constructor for case statements with a complete set of cases.
  completeCase :: [(r, r)] -> r
  
  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase :: [(r, r)] -> r
  
  -- | Smart constructor to square a function.
  square :: r -> r
  
  -- | Smart constructor to half a function exactly.
  half :: r -> r
  
  -- | Constructs 1/2.
  oneHalf :: r
  
  -- | Constructs 1/3.
  oneThird :: r
  
  -- | Create a two-by-two matrix from four given values. For example:
  --
  -- >>> m2x2 1 2 3 4
  -- [ [1,2],
  --   [3,4] ]
  m2x2 :: r -> r -> r -> r -> r
  
  -- | Create a 2D vector (a matrix with two rows, one column). First argument is placed above the second.
  vec2D :: r -> r -> r
  
  -- | Creates a diagonal two-by-two matrix. For example:
  --
  -- >>> dgnl2x2 1 2
  -- [ [1, 0],
  --   [0, 2] ]
  dgnl2x2 :: r -> r -> r
  
  -- Some helper functions to do function application
  
  -- FIXME: These constructors should check that the UID is associated with a
  -- chunk that is actually callable.
  -- | Applies a given function with a list of parameters.
  apply :: (HasUID f, HasSymbol f) => f -> [r] -> r
  
  -- | Similar to 'apply', but converts second argument into 'Symbol's.
  apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> r
  
  -- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
  apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) 
    => f -> a -> b -> r
  
  -- | Similar to 'apply', but takes a relation to apply to 'FCall'.
  applyWithNamedArgs :: (HasUID f, HasSymbol f, HasUID a, IsArgumentName a) => f 
    -> [r] -> [(a, r)] -> r
  
  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy :: (HasUID c, HasSymbol c) => c -> r

instance ExprC Expr where
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
  
