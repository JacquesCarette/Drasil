{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
module Language.Drasil.ModelExpr.Math where

import Prelude hiding (sqrt)
import Control.Lens ((^.))
import Language.Drasil.Expr (Completeness(..), DerivType(..))
import Language.Drasil.ExprClasses (Express(express))
import Language.Drasil.ModelExpr (ModelExpr(..), ($^), ($/), AssocArithOper(..),
  LABinOp(..), VVVBinOp(..), UFunc(..), UFuncB(..), UFuncVN(..), UFuncVV(..), addRe,
  AssocBoolOper(Equivalence, And), SpaceBinOp(IsIn), StatBinOp(Defines))
import Language.Drasil.Space (Space, RTopology(..), DomainDesc(..), RealInterval)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Classes.Core (HasUID(uid), HasSymbol)
import Language.Drasil.Classes (IsArgumentName)

-- | Smart constructor for taking the absolute value of an expression.
abs_ :: ModelExpr -> ModelExpr
abs_ = UnaryOp Abs

-- | Smart constructor for negating an expression.
neg :: ModelExpr -> ModelExpr 
neg = UnaryOp Neg

-- | Smart constructor to take the log of an expression.
log :: ModelExpr -> ModelExpr
log = UnaryOp Log

-- | Smart constructor to take the ln of an expression.
ln :: ModelExpr -> ModelExpr
ln = UnaryOp Ln

-- | Smart constructor to take the square root of an expression.
sqrt :: ModelExpr -> ModelExpr
sqrt = UnaryOp Sqrt

-- | Smart constructor to apply sin to an expression.
sin :: ModelExpr -> ModelExpr
sin = UnaryOp Sin

-- | Smart constructor to apply cos to an expression.
cos :: ModelExpr -> ModelExpr 
cos = UnaryOp Cos

-- | Smart constructor to apply tan to an expression.
tan :: ModelExpr -> ModelExpr
tan = UnaryOp Tan

-- | Smart constructor to apply sec to an expression.
sec :: ModelExpr -> ModelExpr 
sec = UnaryOp Sec

-- | Smart constructor to apply csc to an expression.
csc :: ModelExpr -> ModelExpr
csc = UnaryOp Csc

-- | Smart constructor to apply cot to an expression.
cot :: ModelExpr -> ModelExpr 
cot = UnaryOp Cot

-- | Smart constructor to apply arcsin to an expression.
arcsin :: ModelExpr -> ModelExpr 
arcsin = UnaryOp Arcsin

-- | Smart constructor to apply arccos to an expression.
arccos :: ModelExpr -> ModelExpr 
arccos = UnaryOp Arccos

-- | Smart constructor to apply arctan to an expression.
arctan :: ModelExpr -> ModelExpr 
arctan = UnaryOp Arctan

-- | Smart constructor for the exponential (base e) function.
exp :: ModelExpr -> ModelExpr
exp = UnaryOp Exp

-- | Smart constructor for calculating the dimension of a vector.
dim :: ModelExpr -> ModelExpr
dim = UnaryOpVN Dim

-- | Smart constructor for calculating the normal form of a vector.
norm :: ModelExpr -> ModelExpr
norm = UnaryOpVN Norm

-- | Smart constructor for negating vectors.
negVec :: ModelExpr -> ModelExpr
negVec = UnaryOpVV NegV

-- | Smart constructor for applying logical negation to an expression.
not_ :: ModelExpr -> ModelExpr
not_ = UnaryOpB Not

-- | Smart constructor for indexing.
idx :: ModelExpr -> ModelExpr -> ModelExpr
idx = LABinaryOp Index

-- | Smart constructor for integers.
int :: Integer -> ModelExpr
int = Int

-- | Smart constructor for doubles.
dbl :: Double -> ModelExpr
dbl = Dbl

-- | Smart constructor for exact doubles.
exactDbl :: Integer -> ModelExpr
exactDbl = ExactDbl

-- | Bring a Space into the ModelExpr.
space :: Space -> ModelExpr
space = Spc

-- | Smart constructor for fractions.
frac :: Integer -> Integer -> ModelExpr
frac l r = exactDbl l $/ exactDbl r

-- | Smart constructor for rational expressions (only in 1/x form).
recip_ :: ModelExpr -> ModelExpr
recip_ denom = exactDbl 1 $/ denom

-- | Smart constructor for strings.
str :: String -> ModelExpr
str = Str

-- | Smart constructors for percents.
perc :: Integer -> Integer -> ModelExpr
perc = Perc

-- | Smart constructor for the summation, product, and integral functions over an interval.
defint, defsum, defprod :: Symbol -> ModelExpr -> ModelExpr -> ModelExpr -> ModelExpr
-- | Smart constructor for the summation, product, and integral functions over all Real numbers.
intAll, sumAll, prodAll :: Symbol -> ModelExpr -> ModelExpr

defint v low high = Operator AddRe (BoundedDD v Continuous low high)
intAll v = Operator AddRe (AllDD v Continuous)

defsum v low high = Operator AddRe (BoundedDD v Discrete low high)
sumAll v = Operator AddRe (AllDD v Discrete)

defprod v low high = Operator MulRe (BoundedDD v Discrete low high)
prodAll v = Operator MulRe (AllDD v Discrete)
-- TODO: Above only does for Reals

-- | Smart constructor for 'real interval' membership.
realInterval :: HasUID c => c -> RealInterval ModelExpr ModelExpr -> ModelExpr
realInterval c = RealI (c ^. uid)

-- | Euclidean function : takes a vector and returns the sqrt of the sum-of-squares.
euclidean :: [ModelExpr] -> ModelExpr
euclidean = sqrt . foldr1 addRe . map square

{-# ANN sum' "HLint: ignore Use sum" #-}
-- | Used by 'euclidean' function (in place of 'sum') to fix representation of computation.
sum' :: (Num a, Foldable t) => t a -> a
sum' = foldr1 (+)
  
-- | Smart constructor to cross product two expressions.
cross :: ModelExpr -> ModelExpr -> ModelExpr
cross = VVVBinaryOp Cross

-- | Smart constructor for case statement with complete set of cases.
completeCase :: [(ModelExpr,ModelExpr)] -> ModelExpr
completeCase = Case Complete

-- | Smart constructor for case statement with incomplete set of cases.
incompleteCase :: [(ModelExpr,ModelExpr)] -> ModelExpr
incompleteCase = Case Incomplete

-- | Smart constructor to square a function.
square :: ModelExpr -> ModelExpr
square x = x $^ exactDbl 2

-- | Smart constructor to half a function exactly.
half :: ModelExpr -> ModelExpr
half x = x $/ exactDbl 2

-- | Constructs 1/2.
oneHalf :: ModelExpr
oneHalf = frac 1 2

-- | Constructs 1/3.
oneThird :: ModelExpr
oneThird = frac 1 3

-- | Matrix helper function.
m2x2 :: ModelExpr -> ModelExpr -> ModelExpr -> ModelExpr -> ModelExpr
m2x2 a b c d = Matrix [[a,b],[c,d]]
-- | Matrix helper function.
vec2D :: ModelExpr -> ModelExpr -> ModelExpr
vec2D a b    = Matrix [[a],[b]]
-- | Matrix helper function.
dgnl2x2 :: ModelExpr -> ModelExpr -> ModelExpr
dgnl2x2 a  = m2x2 a (Int 0) (Int 0)

-- Some helper functions to do function application

-- FIXME: These constructors should check that the UID is associated with a
-- chunk that is actually callable.
-- | Applies a given function with a list of parameters.
apply :: (HasUID f, HasSymbol f) => f -> [ModelExpr] -> ModelExpr
apply f ps = FCall (f ^. uid) ps []

-- | Similar to 'apply', but converts second argument into 'Symbol's.
apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> ModelExpr
apply1 f a = FCall (f ^. uid) [sy a] []

-- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) 
  => f -> a -> b -> ModelExpr
apply2 f a b = FCall (f ^. uid) [sy a, sy b] []

-- | Similar to 'apply', but takes a relation to apply to 'FCall'.
applyWithNamedArgs :: (HasUID f, HasSymbol f, HasUID a, IsArgumentName a) => f 
  -> [ModelExpr] -> [(a, ModelExpr)] -> ModelExpr
applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- Note how |sy| 'enforces' having a symbol
-- | Get an 'ModelExpr' from a 'Symbol'.
sy :: (HasUID c, HasSymbol c) => c -> ModelExpr
sy x = C (x ^. uid)

-- This also wants a symbol constraint.
-- | Gets the derivative of an 'ModelExpr' with respect to a 'Symbol'.
deriv, pderiv :: (HasUID c, HasSymbol c) => ModelExpr -> c -> ModelExpr
deriv e c = Deriv Total e (c^.uid)
pderiv e c = Deriv Part e (c^.uid)

-- | One expression is "defined" by another.
defines :: (Express a, Express b) => a -> b -> ModelExpr
defines a b = StatBinaryOp Defines (express a) (express b)

isIn :: Express a => a -> ModelExpr -> ModelExpr
isIn a s@(Spc _) = SpaceBinaryOp IsIn (express a) s
isIn _ _         = error "isIn target must be a Space"


-- | Helper for creating new smart constructors for Associative Binary
--   operations that require at least 1 expression.
assocCreate :: Express d => AssocBoolOper -> [d] -> ModelExpr
assocCreate abo [] = error $ "Need at least 1 expression to create " ++ show abo
assocCreate _ [x]  = express x
assocCreate b des  = AssocB b $ assocSanitize b $ map express des

-- | Helper for associative operations, removes embedded variants of the same kind
assocSanitize :: AssocBoolOper -> [ModelExpr] -> [ModelExpr]
assocSanitize _ [] = []
assocSanitize b (it@(AssocB c des):r)
  | b == c    = assocSanitize b des ++ assocSanitize b r
  | otherwise = it : assocSanitize b r
assocSanitize b (de:des) = de : assocSanitize b des

-- | Binary associative "And".
andDEs :: Express d => [d] -> ModelExpr
andDEs = assocCreate And

-- | Binary associative "Equivalence".
equivDEs :: Express a => [a] -> ModelExpr
equivDEs des
  | length des >= 2 = assocCreate Equivalence des
  | otherwise       = error $ "Need at least 2 expressions to create " ++ show Equivalence
