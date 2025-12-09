{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.Expr.Class (
  ExprC(..),
  frac, recip_,
  square, half,
  oneHalf, oneThird,
  apply1, apply2,
  m2x2, vec2D, dgnl2x2, rowVec, columnVec, mkSet,
  vScale, vAdd, vSub

) where

import Prelude hiding (sqrt, log, sin, cos, tan, exp)
import Control.Lens ((^.))

import Drasil.Database (HasUID(..))
import Utils.Drasil (toColumn)
import Language.Drasil.Symbol
import Language.Drasil.Expr.Lang
import Language.Drasil.Literal.Lang
import Language.Drasil.Space (DomainDesc(..), RTopology(..), RealInterval, Space)
import qualified Language.Drasil.ModelExpr.Lang as M
import qualified Drasil.Code.CodeExpr.Lang as C
import Language.Drasil.Literal.Class (LiteralC(..))
import Drasil.Database.UID (HasUID(..))
import qualified Language.Drasil.Space         as S
import           Numeric.Natural               (Natural)
import qualified Data.Map.Ordered              as OM

-- | Smart constructor for fractions.
frac :: (ExprC r, LiteralC r) => Integer -> Integer -> r
frac n d = exactDbl n $/ exactDbl d

-- | Smart constructor for rational expressions (only in 1/x form).
recip_ :: (ExprC r, LiteralC r) => r -> r
recip_ denom = exactDbl 1 $/ denom

-- | Smart constructor to square a function.
square :: (ExprC r, LiteralC r) => r -> r
square x = x $^ exactDbl 2

-- | Smart constructor to half a function exactly.
half :: (ExprC r, LiteralC r) => r -> r
half x = x $/ exactDbl 2

-- | 1/2, as an expression.
oneHalf :: (ExprC r, LiteralC r) => r
oneHalf = frac 1 2

-- | 1/3rd, as an expression.
oneThird :: (ExprC r, LiteralC r) => r
oneThird = frac 1 3

-- | Similar to 'apply', but converts second argument into 'Symbol's.
apply1 :: (ExprC r, HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> r
apply1 f a = apply f [sy a]

-- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
apply2 :: (ExprC r, HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b)
    => f -> a -> b -> r
apply2 f a b = apply f [sy a, sy b]

-- | Create a two-by-two matrix from four given values. For example:
--
-- >>> m2x2 1 2 3 4
-- [ [1,2],
--   [3,4] ]
m2x2 :: ExprC r => r -> r -> r -> r -> r
m2x2 a b c d = matrix [[a,b],[c,d]]

mkSet :: ExprC r => Space -> [r] -> r
mkSet = set'

-- | Create a 2D vector (a matrix with two rows, one column). First argument is placed above the second.
vec2D :: ExprC r => r -> r -> r
vec2D a b = vect [a, b]

-- | Creates a diagonal two-by-two matrix. For example:
--
-- >>> dgnl2x2 1 2
-- [ [1, 0],
--   [0, 2] ]
dgnl2x2 :: (ExprC r, LiteralC r) => r -> r -> r
dgnl2x2 a = m2x2 a (int 0) (int 0)

-- | Create a row vector
rowVec :: ExprC r => [r] -> r
rowVec a = matrix [a]

-- | Create a column vector
columnVec :: ExprC r => [r] -> r
columnVec a = matrix $ toColumn a

-- | Alias for `cScale`
vScale :: (ExprC r) => r -> r -> r
vScale = cScale

-- | Alias for `cAdd`
vAdd :: (ExprC r) => r -> r -> r
vAdd = cAdd

-- | Alias for `cSub`
vSub :: (ExprC r) => r -> r -> r
vSub = cSub

class ExprC r where
  infixr 8 $^
  infixl 7 $/
  infixr 4 $=
  infixr 9 $&&
  infixr 9 $||
  lit :: Literal -> r

  -- * Binary Operators

  ($=), ($!=) :: r -> r -> r

  -- | Smart constructor for ordering two equations.
  ($<), ($>), ($<=), ($>=) :: r -> r -> r

  -- | Smart constructor for the dot product of two equations.
  ($.) :: r -> r -> r

  -- | Add two expressions.
  ($+) :: r -> r -> r

  -- | Multiply two expressions.
  ($*) :: r -> r -> r

  ($-), ($/), ($^) :: r -> r -> r

  ($=>), ($<=>) :: r -> r -> r

  ($&&), ($||) :: r -> r -> r

  -- | Smart constructor for set-theoretic membership relation. Added ' to avoid conflict.
  in' :: r -> r -> r

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

<<<<<<< HEAD
  -- | Smart constructor for calculating the dimension of a vector (or general clif).
  dim :: r -> r

  -- | Smart constructor for calculating the normal form of a vector (or general clif).
  norm :: r -> r

  -- | Smart constructor for negating vectors (or general clifs).
  negClif :: r -> r

  -- | Smart constructor for applying logical negation to an expression.
  not_ :: r -> r

  -- | Smart constructor for indexing.
  idx :: r -> r -> r

  -- | Smart constructor for indexOf. Finds the index of the first occurrence of a value in a list.
  idxOf :: r -> r -> r

  -- | Smart constructor for the summation, product, and integral functions over an interval.
  defint, defsum, defprod :: Symbol -> r -> r -> r -> r

  -- | Smart constructor for 'real interval' membership.
  realInterval :: HasUID c => c -> RealInterval r r -> r

  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean :: [r] -> r

  -- | Smart constructor to cross product two expressions.
  cross :: r -> r -> r

  -- | Smart constructor for clif scaling
  cScale :: r -> r -> r

  -- | Smart constructor for clif addition 
  cAdd :: r -> r -> r

  -- | Smart constructor for clif subtraction
  cSub :: r -> r -> r

  -- | Smart constructor for case statements with a complete set of cases.
  completeCase :: [(r, r)] -> r

  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase :: [(r, r)] -> r

  -- | Create a matrix.
  matrix :: [[r]] -> r

  -- | Create a Set.
  set' :: Space -> [r] -> r

  -- | Applies a given function with a list of parameters.
  apply :: (HasUID f, HasSymbol f) => f -> [r] -> r

  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy :: (HasUID c, HasSymbol c) => c -> r

  -- | Vectors with fixed components, of a given fixed dimension
  vect :: [r] -> r

  -- | Smart constructor for the geometric product of two Clifford objects.
  geometricProd :: r -> r -> r

  -- | Smart constructor for the wedge (outer) product.
  wedgeProd :: r -> r -> r

  -- | Smart constructor for grade selection.
  gradeSelect :: Natural -> r -> r

instance ExprC Expr where
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
  ($.) = CCNBinaryOp Dot

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
  ($*) l r = AssocA Mul [l, r]

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

<<<<<<< HEAD
  -- | Smart constructor for calculating the dimension of a clif.
  dim = UnaryOpCN Dim

  -- | Smart constructor for calculating the normal form of a clif.
  norm = UnaryOpCN Norm
=======
  -- | Smart constructor for calculating the dimension of a vector.
  dim = UnaryOpVN Dim

  -- | Smart constructor for calculating the normal form of a vector.
  norm = UnaryOpVN Norm
>>>>>>> origin/main

  -- | Smart constructor for negating vectors.
  negClif = UnaryOpCC NegC

  -- | And more general scaling
<<<<<<< HEAD
  cScale = NCCBinaryOp Scale
=======
  vScale = NVVBinaryOp Scale
>>>>>>> origin/main

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

  -- TODO: Move euclidean to smart constructor
  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean = sqrt . foldr1 ($+) . map square

  -- | Smart constructor to cross product two expressions.
  cross = CCCBinaryOp Cross
  -- | Adding vectors
  cAdd  = CCCBinaryOp CAdd
  -- | Subtracting vectors
<<<<<<< HEAD
  cSub  = CCCBinaryOp CSub

  geometricProd = CCCBinaryOp GeometricProd
  wedgeProd     = CCCBinaryOp WedgeProd
  gradeSelect   = NatCCBinaryOp GradeSelect
=======
  vSub  = VVVBinaryOp VSub
>>>>>>> origin/main

  -- | Smart constructor for case statements with a complete set of cases.
  completeCase = Case Complete

  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase = Case Incomplete

  matrix = Matrix

  set' = Set
  -- | Applies a given function with a list of parameters.
  apply f [] = sy f
  apply f ps = FCall (f ^. uid) ps

  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy x = C (x ^. uid)

<<<<<<< HEAD
  -- | Vectors with known components
  --   This will create a Clifford space with dimension equal to the length of the list
  vect es =
    let
      d = fromIntegral $ length es
      vectComp n e = (vectorKey n d, e)
    in
      Clif (S.Fixed d) $ OM.fromList $ mapWithIndex vectComp es

mapWithIndex :: (Natural -> a -> b) -> [a] -> [b]
mapWithIndex f = go 0
  where
    go _ [] = []
    go n (x:xs) = f n x : go (n+1) xs

=======
>>>>>>> origin/main
instance ExprC M.ModelExpr where
  lit = M.Lit

  -- | Smart constructor for equating two expressions.
  ($=)  = M.EqBinaryOp M.Eq
  -- | Smart constructor for showing that two expressions are not equal.
  ($!=) = M.EqBinaryOp M.NEq

  -- | Smart constructor for ordering two equations.
  -- | Less than.
  ($<)  = M.OrdBinaryOp M.Lt
  -- | Greater than.
  ($>)  = M.OrdBinaryOp M.Gt
  -- | Less than or equal to.
  ($<=) = M.OrdBinaryOp M.LEq
  -- | Greater than or equal to.
  ($>=) = M.OrdBinaryOp M.GEq

  -- | Smart constructor for the dot product of two equations.
  ($.) = M.CCNBinaryOp M.Dot

  -- | Add two expressions.
  ($+) (M.Lit (Int 0)) r = r
  ($+) l (M.Lit (Int 0)) = l
  ($+) (M.Lit (Dbl 0)) r = r
  ($+) l (M.Lit (Dbl 0)) = l
  ($+) l (M.Lit (ExactDbl 0)) = l
  ($+) (M.Lit (ExactDbl 0)) r = r
  ($+) (M.AssocA M.Add l) (M.AssocA M.Add r) = M.AssocA M.Add (l ++ r)
  ($+) (M.AssocA M.Add l) r = M.AssocA M.Add (l ++ [r])
  ($+) l (M.AssocA M.Add r) = M.AssocA M.Add (l : r)
  ($+) l r = M.AssocA M.Add [l, r]

  -- | Multiply two expressions.
  ($*) (M.Lit (Int 1)) r = r
  ($*) l (M.Lit (Int 1)) = l
  ($*) (M.Lit (Dbl 1.0)) r = r
  ($*) l (M.Lit (Dbl 1.0)) = l
  ($*) l (M.Lit (ExactDbl 1)) = l
  ($*) (M.Lit (ExactDbl 1)) r = r
  ($*) (M.AssocA M.Mul l) (M.AssocA M.Mul r) = M.AssocA M.Mul (l ++ r)
  ($*) (M.AssocA M.Mul l) r = M.AssocA M.Mul (l ++ [r])
  ($*) l (M.AssocA M.Mul r) = M.AssocA M.Mul (l : r)
  ($*) l r = M.AssocA M.Mul [l,r]
  -- | Smart constructor for subtracting two expressions.
  ($-) = M.ArithBinaryOp M.Subt
  -- | Smart constructor for dividing two expressions.
  ($/) = M.ArithBinaryOp M.Frac
  -- | Smart constructor for rasing the first expression to the power of the second.
  ($^) = M.ArithBinaryOp M.Pow

  -- | Smart constructor to show that one expression implies the other (conditional operator).
  ($=>)  = M.BoolBinaryOp M.Impl
  -- | Smart constructor to show that an expression exists if and only if another expression exists (biconditional operator).
  ($<=>) = M.BoolBinaryOp M.Iff

  -- | Smart constructor for the boolean /and/ operator.
  a $&& b = M.AssocB M.And [a, b]
  -- | Smart constructor for the boolean /or/ operator.
  a $|| b = M.AssocB M.Or  [a, b]

  in' = M.ESBBinaryOp M.SContains

  -- | Smart constructor for taking the absolute value of an expression.
  abs_ = M.UnaryOp M.Abs

  -- | Smart constructor for negating an expression.
  neg = M.UnaryOp M.Neg

  -- | Smart constructor to take the log of an expression.
  log = M.UnaryOp M.Log

  -- | Smart constructor to take the ln of an expression.
  ln = M.UnaryOp M.Ln

  -- | Smart constructor to take the square root of an expression.
  sqrt = M.UnaryOp M.Sqrt

  -- | Smart constructor to apply sin to an expression.
  sin = M.UnaryOp M.Sin

  -- | Smart constructor to apply cos to an expression.
  cos = M.UnaryOp M.Cos

  -- | Smart constructor to apply tan to an expression.
  tan = M.UnaryOp M.Tan

  -- | Smart constructor to apply sec to an expression.
  sec = M.UnaryOp M.Sec

  -- | Smart constructor to apply csc to an expression.
  csc = M.UnaryOp M.Csc

  -- | Smart constructor to apply cot to an expression.
  cot = M.UnaryOp M.Cot

  -- | Smart constructor to apply arcsin to an expression.
  arcsin = M.UnaryOp M.Arcsin

  -- | Smart constructor to apply arccos to an expression.
  arccos = M.UnaryOp M.Arccos

  -- | Smart constructor to apply arctan to an expression.
  arctan = M.UnaryOp M.Arctan

  -- | Smart constructor for the exponential (base e) function.
  exp = M.UnaryOp M.Exp

  -- | Smart constructor for calculating the dimension of a clif.
  dim = M.UnaryOpCN M.Dim

  -- | Smart constructor for calculating the normal form of a clif.
  norm = M.UnaryOpCN M.Norm

  -- | Smart constructor for negating vectors.
  negClif = M.UnaryOpCC M.NegC

  -- | More general scaling
  cScale = M.NCCBinaryOp M.Scale

  -- | Smart constructor for applying logical negation to an expression.
  not_ = M.UnaryOpB M.Not

  -- | Smart constructor for indexing.
  idx = M.LABinaryOp M.Index

  -- | Smart constructor for indexing.
  idxOf = M.LABinaryOp M.IndexOf

  -- | Integrate over some expression with bounds (∫).
  defint v low high = M.Operator M.Add (BoundedDD v Continuous low high)

  -- | Sum over some expression with bounds (∑).
  defsum v low high = M.Operator M.Add (BoundedDD v Discrete low high)

  -- | Product over some expression with bounds (∏).
  defprod v low high = M.Operator M.Mul (BoundedDD v Discrete low high)

  -- | Smart constructor for 'real interval' membership.
  realInterval c = M.RealI (c ^. uid)

  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean = sqrt . foldr1 ($+) . map square

  -- | Smart constructor to cross product two expressions.
  cross = M.CCCBinaryOp M.Cross

  -- | Adding vectors
  cAdd  = M.CCCBinaryOp M.CAdd
  -- | Subtracting vectors
<<<<<<< HEAD
  cSub  = M.CCCBinaryOp M.CSub
=======
  vSub  = M.VVVBinaryOp M.VSub
>>>>>>> origin/main

  -- | Smart constructor for case statements with a complete set of cases.
  completeCase = M.Case Complete

  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase = M.Case Incomplete

  matrix = M.Matrix

  set' = M.Set
  -- | Applies a given function with a list of parameters.
  apply f [] = sy f
  apply f ps = M.FCall (f ^. uid) ps

  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy x = M.C (x ^. uid)
<<<<<<< HEAD

  -- | Vectors with known components
  --   This will create a Clifford space with dimension equal to the length of the list
  --   TODO: does this have to change in ModelExpr?
  vect es =
    let
      d = fromIntegral $ length es
      vectComp n e = (vectorKey n d, e)
    in
      M.Clif (S.Fixed d) $ OM.fromList $ mapWithIndex vectComp es

  geometricProd = M.CCCBinaryOp M.GeometricProd
  wedgeProd     = M.CCCBinaryOp M.WedgeProd
  gradeSelect   = M.NatCCBinaryOp M.GradeSelect

instance ExprC C.CodeExpr where
  lit = C.Lit

  -- | Smart constructor for equating two expressions.
  ($=)  = C.EqBinaryOp C.Eq
  -- | Smart constructor for showing that two expressions are not equal.
  ($!=) = C.EqBinaryOp C.NEq

  -- | Smart constructor for ordering two equations.
  -- | Less than.
  ($<)  = C.OrdBinaryOp C.Lt
  -- | Greater than.
  ($>)  = C.OrdBinaryOp C.Gt
  -- | Less than or equal to.
  ($<=) = C.OrdBinaryOp C.LEq
  -- | Greater than or equal to.
  ($>=) = C.OrdBinaryOp C.GEq

  -- | Smart constructor for the dot product of two equations.
  ($.) = C.CCNBinaryOp C.Dot

  -- | Add two expressions.
  ($+) (C.Lit (Int 0)) r = r
  ($+) l (C.Lit (Int 0)) = l
  ($+) (C.Lit (Dbl 0)) r = r
  ($+) l (C.Lit (Dbl 0)) = l
  ($+) l (C.Lit (ExactDbl 0)) = l
  ($+) (C.Lit (ExactDbl 0)) r = r
  ($+) (C.AssocA C.Add l) (C.AssocA C.Add r) = C.AssocA C.Add (l ++ r)
  ($+) (C.AssocA C.Add l) r = C.AssocA C.Add (l ++ [r])
  ($+) l (C.AssocA C.Add r) = C.AssocA C.Add (l : r)
  ($+) l r = C.AssocA C.Add [l, r]

  -- | Multiply two expressions.
  ($*) (C.Lit (Int 1)) r = r
  ($*) l (C.Lit (Int 1)) = l
  ($*) (C.Lit (Dbl 1.0)) r = r
  ($*) l (C.Lit (Dbl 1.0)) = l
  ($*) l (C.Lit (ExactDbl 1)) = l
  ($*) (C.Lit (ExactDbl 1)) r = r
  ($*) (C.AssocA C.Mul l) (C.AssocA C.Mul r) = C.AssocA C.Mul (l ++ r)
  ($*) (C.AssocA C.Mul l) r = C.AssocA C.Mul (l ++ [r])
  ($*) l (C.AssocA C.Mul r) = C.AssocA C.Mul (l : r)
  ($*) l r = C.AssocA C.Mul [l,r]

  -- | Smart constructor for subtracting two expressions.
  ($-) = C.ArithBinaryOp C.Subt
  -- | Smart constructor for dividing two expressions.
  ($/) = C.ArithBinaryOp C.Frac
  -- | Smart constructor for rasing the first expression to the power of the second.
  ($^) = C.ArithBinaryOp C.Pow

  -- | Smart constructor to show that one expression implies the other (conditional operator).
  ($=>)  = C.BoolBinaryOp C.Impl
  -- | Smart constructor to show that an expression exists if and only if another expression exists (biconditional operator).
  ($<=>) = C.BoolBinaryOp C.Iff

  -- | Smart constructor for the boolean /and/ operator.
  a $&& b = C.AssocB C.And [a, b]
  -- | Smart constructor for the boolean /or/ operator.
  a $|| b = C.AssocB C.Or  [a, b]

  in' = C.ESBBinaryOp C.SContains

  -- | Smart constructor for taking the absolute value of an expression.
  abs_ = C.UnaryOp C.Abs

  -- | Smart constructor for negating an expression.
  neg = C.UnaryOp C.Neg

  -- | Smart constructor to take the log of an expression.
  log = C.UnaryOp C.Log

  -- | Smart constructor to take the ln of an expression.
  ln = C.UnaryOp C.Ln

  -- | Smart constructor to take the square root of an expression.
  sqrt = C.UnaryOp C.Sqrt

  -- | Smart constructor to apply sin to an expression.
  sin = C.UnaryOp C.Sin

  -- | Smart constructor to apply cos to an expression.
  cos = C.UnaryOp C.Cos

  -- | Smart constructor to apply tan to an expression.
  tan = C.UnaryOp C.Tan

  -- | Smart constructor to apply sec to an expression.
  sec = C.UnaryOp C.Sec

  -- | Smart constructor to apply csc to an expression.
  csc = C.UnaryOp C.Csc

  -- | Smart constructor to apply cot to an expression.
  cot = C.UnaryOp C.Cot

  -- | Smart constructor to apply arcsin to an expression.
  arcsin = C.UnaryOp C.Arcsin

  -- | Smart constructor to apply arccos to an expression.
  arccos = C.UnaryOp C.Arccos

  -- | Smart constructor to apply arctan to an expression.
  arctan = C.UnaryOp C.Arctan

  -- | Smart constructor for the exponential (base e) function.
  exp = C.UnaryOp C.Exp

  -- | Smart constructor for calculating the dimension of a vector.
  dim = C.UnaryOpCN C.Dim

  -- | Smart constructor for calculating the normal form of a vector.
  norm = C.UnaryOpCN C.Norm

  -- | Smart constructor for negating vectors.
  negClif = C.UnaryOpCC C.NegC

  -- | And more general scaling
  cScale = C.NCCBinaryOp C.Scale

  -- | Smart constructor for applying logical negation to an expression.
  not_ = C.UnaryOpB C.Not

  -- | Smart constructor for indexing.
  idx = C.LABinaryOp C.Index

  idxOf = C.LABinaryOp C.IndexOf
  -- | Integrate over some expression with bounds (∫).
  defint v low high = C.Operator C.Add (BoundedDD v Continuous low high)

  -- | Sum over some expression with bounds (∑).
  defsum v low high = C.Operator C.Add (BoundedDD v Discrete low high)

  -- | Product over some expression with bounds (∏).
  defprod v low high = C.Operator C.Mul (BoundedDD v Discrete low high)

  -- | Smart constructor for 'real interval' membership.
  realInterval c = C.RealI (c ^. uid)

  -- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
  euclidean = sqrt . foldr1 ($+) . map square

  -- | Smart constructor to cross product two expressions.
  cross = C.CCCBinaryOp C.Cross

  -- | Adding clifs
  cAdd  = C.CCCBinaryOp C.CAdd

  -- | Subtracting clifs
  cSub  = C.CCCBinaryOp C.CSub

  -- | Smart constructor for case statements with a complete set of cases.
  completeCase = C.Case Complete

  -- | Smart constructor for case statements with an incomplete set of cases.
  incompleteCase = C.Case Incomplete

  matrix = C.Matrix

  set' = C.Set
  -- | Applies a given function with a list of parameters.
  apply f [] = sy f
  apply f ps = C.FCall (f ^. uid) ps []

  -- Note how |sy| 'enforces' having a symbol
  -- | Create an 'Expr' from a 'Symbol'ic Chunk.
  sy x = C.C (x ^. uid)

  -- | Vectors with known components
  --   This will create a Clifford space with dimension equal to the length of the list
  --   TODO: does this have to change in CodeExpr?
  vect es =
    let
      d = fromIntegral $ length es
      vectComp n e = (vectorKey n d, e)
    in
      C.Clif (S.Fixed d) $ OM.fromList $ mapWithIndex vectComp es

  geometricProd = C.CCCBinaryOp C.GeometricProd
  wedgeProd     = C.CCCBinaryOp C.WedgeProd
  gradeSelect   = C.NatCCBinaryOp C.GradeSelect
=======
>>>>>>> origin/main
