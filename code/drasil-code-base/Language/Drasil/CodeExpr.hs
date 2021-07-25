{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- Defines constructors for CodeExprs often used in relation to code generation.
module Language.Drasil.CodeExpr (CodeExpr, 
  sy, str, int, dbl, exactDbl, matrix,
  frac, recip_, abs_, neg, log,  ln, sqrt,
  sin, cos, tan, sec, csc, cot, arcsin, arccos, arctan,
  exp, dim, norm, not_,
  new, newWithNamedArgs, message, msgWithNamedArgs,
  field, apply, apply1, apply2, applyWithNamedArgs,
  ($&&), ($-), ($/), ($^), ($=>), ($<=>), ($=), ($!=), ($<), ($>),
  ($<=), ($>=), 
  addI, addRe, mulI, mulRe, idx,
  expr) where

import Prelude hiding (exp, sin, cos, tan, sqrt, log)

import Language.Drasil (Space(Actor), Callable, HasSpace(..), HasSymbol,
  HasUID(..), IsArgumentName)

import Language.Drasil.Chunk.CodeBase (CodeIdea, CodeVarChunk)
import Language.Drasil.Code.Expr
import Language.Drasil.Code.Expr.Convert (expr)

import Control.Lens ((^.))

-- | Smart constructor for chunk symbols.
sy :: (HasUID u, HasSymbol u) => u -> CodeExpr
sy = C . (^. uid)

-- | Smart constructor for strings.
str :: String -> CodeExpr
str = Str

-- | Smart constructor for integers.
int :: Integer -> CodeExpr
int = Int

-- | Smart constructor for doubles.
dbl :: Double -> CodeExpr
dbl = Dbl

-- | Smart constructor for exact doubles.
exactDbl :: Integer -> CodeExpr
exactDbl = ExactDbl

-- | Smart constructor for matrices.
matrix :: [[CodeExpr]] -> CodeExpr
matrix = Matrix

-- | Smart constructor for fractions.
frac :: Integer -> Integer -> CodeExpr
frac l r = exactDbl l $/ exactDbl r

-- | Smart constructor for rational expressions (only in 1/x form).
recip_ :: CodeExpr -> CodeExpr
recip_ denom = exactDbl 1 $/ denom


-- | Smart constructor for taking the absolute value of an expression.
abs_ :: CodeExpr -> CodeExpr
abs_ = UnaryOp Abs

-- | Smart constructor for negating an expression.
neg :: CodeExpr -> CodeExpr 
neg = UnaryOp Neg

-- | Smart constructor to take the log of an expression.
log :: CodeExpr -> CodeExpr
log = UnaryOp Log

-- | Smart constructor to take the ln of an expression.
ln :: CodeExpr -> CodeExpr
ln = UnaryOp Ln

-- | Smart constructor to take the square root of an expression.
sqrt :: CodeExpr -> CodeExpr
sqrt = UnaryOp Sqrt

-- | Smart constructor to apply sin to an expression.
sin :: CodeExpr -> CodeExpr
sin = UnaryOp Sin

-- | Smart constructor to apply cos to an expression.
cos :: CodeExpr -> CodeExpr 
cos = UnaryOp Cos

-- | Smart constructor to apply tan to an expression.
tan :: CodeExpr -> CodeExpr
tan = UnaryOp Tan

-- | Smart constructor to apply sec to an expression.
sec :: CodeExpr -> CodeExpr 
sec = UnaryOp Sec

-- | Smart constructor to apply csc to an expression.
csc :: CodeExpr -> CodeExpr
csc = UnaryOp Csc

-- | Smart constructor to apply cot to an expression.
cot :: CodeExpr -> CodeExpr 
cot = UnaryOp Cot

-- | Smart constructor to apply arcsin to an expression.
arcsin :: CodeExpr -> CodeExpr 
arcsin = UnaryOp Arcsin

-- | Smart constructor to apply arccos to an expression.
arccos :: CodeExpr -> CodeExpr 
arccos = UnaryOp Arccos

-- | Smart constructor to apply arctan to an expression.
arctan :: CodeExpr -> CodeExpr 
arctan = UnaryOp Arctan

-- | Smart constructor for the exponential (base e) function.
exp :: CodeExpr -> CodeExpr
exp = UnaryOp Exp

-- | Smart constructor for calculating the dimension of a vector.
dim :: CodeExpr -> CodeExpr
dim = UnaryOpVN Dim

-- | Smart constructor for calculating the normal form of a vector.
norm :: CodeExpr -> CodeExpr
norm = UnaryOpVN Norm

-- | Smart constructor for applying logical negation to an expression.
not_ :: CodeExpr -> CodeExpr
not_ = UnaryOpB Not

-- | Smart constructor for indexing.
idx :: CodeExpr -> CodeExpr -> CodeExpr
idx = LABinaryOp Index

-- | Constructs a CodeExpr for actor creation (constructor call)
new :: (Callable f, HasUID f, CodeIdea f) => f -> [CodeExpr] -> CodeExpr
new c ps = New (c ^. uid) ps []

-- | Constructs a CodeExpr for actor creation (constructor call) that uses named arguments
newWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID a, 
  IsArgumentName a) => f -> [CodeExpr] -> [(a, CodeExpr)] -> CodeExpr
newWithNamedArgs c ps ns = New (c ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- | Constructs a CodeExpr for actor messaging (method call)
message :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, CodeIdea c) 
  => c -> f -> [CodeExpr] -> CodeExpr
message o m ps = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps []
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"

-- | Constructs a CodeExpr for actor messaging (method call) that uses named arguments
msgWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, 
  CodeIdea c, HasUID a, IsArgumentName a) => c -> f -> [CodeExpr] -> [(a, CodeExpr)] -> 
  CodeExpr
msgWithNamedArgs o m ps as = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps 
          (zip (map ((^. uid) . fst) as) (map snd as))
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"

-- | Constructs a CodeExpr representing the field of an actor
field :: CodeVarChunk -> CodeVarChunk -> CodeExpr
field o f = checkObj (o ^. typ)
  where checkObj (Actor _) = Field (o ^. uid) (f ^. uid)
        checkObj _ = error $ "Invalid actor field: Actor should have " ++
          "Actor space"


-- FIXME: These constructors should check that the UID is associated with a
-- chunk that is actually callable.
-- | Applies a given function with a list of parameters.
apply :: (HasUID f, HasSymbol f) => f -> [CodeExpr] -> CodeExpr
apply f ps = FCall (f ^. uid) ps []

-- | Similar to 'apply', but converts second argument into 'Symbol's.
apply1 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a) => f -> a -> CodeExpr
apply1 f a = FCall (f ^. uid) [sy a] []

-- | Similar to 'apply', but the applied function takes two parameters (which are both 'Symbol's).
apply2 :: (HasUID f, HasSymbol f, HasUID a, HasSymbol a, HasUID b, HasSymbol b) 
  => f -> a -> b -> CodeExpr
apply2 f a b = FCall (f ^. uid) [sy a, sy b] []

-- | Similar to 'apply', but takes a relation to apply to 'FCall'.
applyWithNamedArgs :: (HasUID f, HasSymbol f, HasUID a, IsArgumentName a) => f 
  -> [CodeExpr] -> [(a, CodeExpr)] -> CodeExpr
applyWithNamedArgs f ps ns = FCall (f ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

($&&), ($-), ($/), ($^), ($=>), ($<=>), ($=), ($!=), ($<), ($>), ($<=), ($>=)
  :: CodeExpr -> CodeExpr -> CodeExpr
-- | Smart constructor for the boolean /and/ operator.
a $&& b = AssocB And [a, b]
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
-- | Smart constructor for equating two expressions.
($=)  = EqBinaryOp Eq
-- | Smart constructor for showing that two expressions are not equal.
($!=) = EqBinaryOp NEq
-- | Less than.
($<)  = OrdBinaryOp Lt
-- | Greater than.
($>)  = OrdBinaryOp Gt
-- | Less than or equal to.
($<=) = OrdBinaryOp LEq
-- | Greater than or equal to.
($>=) = OrdBinaryOp GEq

-- | Add two expressions (Integers).
addI :: CodeExpr -> CodeExpr -> CodeExpr
addI l (Int 0) = l
addI (Int 0) r = r
addI (AssocA AddI l) (AssocA AddI r) = AssocA AddI (l ++ r)
addI (AssocA AddI l) r = AssocA AddI (l ++ [r])
addI l (AssocA AddI r) = AssocA AddI (l : r)
addI l r = AssocA AddI [l, r]

-- | Add two expressions (Real numbers).
addRe :: CodeExpr -> CodeExpr -> CodeExpr
addRe l (Dbl 0)      = l
addRe (Dbl 0) r      = r
addRe l (ExactDbl 0) = l
addRe (ExactDbl 0) r = r
addRe (AssocA AddRe l) (AssocA AddRe r) = AssocA AddRe (l ++ r)
addRe (AssocA AddRe l) r = AssocA AddRe (l ++ [r])
addRe l (AssocA AddRe r) = AssocA AddRe (l : r)
addRe l r = AssocA AddRe [l, r]

-- | Multiply two expressions (Integers).
mulI :: CodeExpr -> CodeExpr -> CodeExpr
mulI l (Int 1) = l
mulI (Int 1) r = r
mulI (AssocA MulI l) (AssocA MulI r) = AssocA MulI (l ++ r)
mulI (AssocA MulI l) r = AssocA MulI (l ++ [r])
mulI l (AssocA MulI r) = AssocA MulI (l : r)
mulI l r = AssocA MulI [l, r]

-- | Multiply two expressions (Real numbers).
mulRe :: CodeExpr -> CodeExpr -> CodeExpr
mulRe l (Dbl 1)      = l
mulRe (Dbl 1) r      = r
mulRe l (ExactDbl 1) = l
mulRe (ExactDbl 1) r = r
mulRe (AssocA MulRe l) (AssocA MulRe r) = AssocA MulRe (l ++ r)
mulRe (AssocA MulRe l) r = AssocA MulRe (l ++ [r])
mulRe l (AssocA MulRe r) = AssocA MulRe (l : r)
mulRe l r = AssocA MulRe [l, r]
