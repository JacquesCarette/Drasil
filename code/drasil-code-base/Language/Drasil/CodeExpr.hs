{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
-- Defines constructors for Exprs often used in relation to code generation.
module Language.Drasil.CodeExpr (CodeExpr, 
  sy, str, int, dbl, exactDbl, matrix,
  new, newWithNamedArgs, message, msgWithNamedArgs,
  field, apply, apply1, apply2, applyWithNamedArgs,
  ($&&), ($-), ($/), ($^), ($=>), ($<=>), ($=), ($!=), ($<), ($>),
  ($<=), ($>=), 
  addI, addRe, mulI, mulRe, dim, idx,
  expr) where

import Language.Drasil (Space(Actor), Callable, HasSpace(..), HasSymbol,
  HasUID(..), IsArgumentName)

import Language.Drasil.Chunk.CodeBase (CodeIdea, CodeVarChunk)
import Language.Drasil.Code.Expr
import Language.Drasil.Code.Expr.Convert (expr)

import Control.Lens ((^.))

sy :: (HasUID u, HasSymbol u) => u -> CodeExpr
sy = C . (^. uid)

str :: String -> CodeExpr
str = Str

int :: Integer -> CodeExpr
int = Int

dbl :: Double -> CodeExpr
dbl = Dbl

exactDbl :: Integer -> CodeExpr
exactDbl = ExactDbl

matrix :: [[CodeExpr]] -> CodeExpr
matrix = Matrix

-- | Constructs an Expr for actor creation (constructor call)
new :: (Callable f, HasUID f, CodeIdea f) => f -> [CodeExpr] -> CodeExpr
new c ps = New (c ^. uid) ps []

-- | Constructs an Expr for actor creation (constructor call) that uses named arguments
newWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID a, 
  IsArgumentName a) => f -> [CodeExpr] -> [(a, CodeExpr)] -> CodeExpr
newWithNamedArgs c ps ns = New (c ^. uid) ps (zip (map ((^. uid) . fst) ns) 
  (map snd ns))

-- | Constructs an Expr for actor messaging (method call)
message :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, CodeIdea c) 
  => c -> f -> [CodeExpr] -> CodeExpr
message o m ps = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps []
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"

-- | Constructs an Expr for actor messaging (method call) that uses named arguments
msgWithNamedArgs :: (Callable f, HasUID f, CodeIdea f, HasUID c, HasSpace c, 
  CodeIdea c, HasUID a, IsArgumentName a) => c -> f -> [CodeExpr] -> [(a, CodeExpr)] -> 
  CodeExpr
msgWithNamedArgs o m ps as = checkObj (o ^. typ)
  where checkObj (Actor _) = Message (o ^. uid) (m ^. uid) ps 
          (zip (map ((^. uid) . fst) as) (map snd as))
        checkObj _ = error $ "Invalid actor message: Actor should have " ++ 
          "Actor space"

-- | Constructs an Expr representing the field of an actor
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

dim :: CodeExpr -> CodeExpr
dim = UnaryOpVec Dim

idx :: CodeExpr -> CodeExpr -> CodeExpr
idx = LABinaryOp Index

-- Generate below 4 functions with TH?
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
addRe l (Int 0) = l
addRe (Int 0) r = r
addRe (AssocA AddRe l) (AssocA AddRe r) = AssocA AddRe (l ++ r)
addRe (AssocA AddRe l) r = AssocA AddRe (l ++ [r])
addRe l (AssocA AddRe r) = AssocA AddRe (l : r)
addRe l r = AssocA AddRe [l, r]

-- | Multiply two expressions (Integers).
mulI :: CodeExpr -> CodeExpr -> CodeExpr
mulI (AssocA MulI l) (AssocA MulI r) = AssocA MulI (l ++ r)
mulI (AssocA MulI l) r = AssocA MulI (l ++ [r])
mulI l (AssocA MulI r) = AssocA MulI (l : r)
mulI l r = AssocA MulI [l, r]

-- | Multiply two expressions (Real numbers).
mulRe :: CodeExpr -> CodeExpr -> CodeExpr
mulRe (AssocA MulRe l) (AssocA MulRe r) = AssocA MulRe (l ++ r)
mulRe (AssocA MulRe l) r = AssocA MulRe (l ++ [r])
mulRe l (AssocA MulRe r) = AssocA MulRe (l : r)
mulRe l r = AssocA MulRe [l, r]
