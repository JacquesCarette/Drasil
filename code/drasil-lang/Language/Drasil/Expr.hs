{-# LANGUAGE GADTs #-}
-- | The Drasil Expression language
module Language.Drasil.Expr where

import Data.Ratio (denominator, numerator)
import Prelude hiding (sqrt)

import Language.Drasil.Space (Space(..), DomainDesc, RealInterval)
import Language.Drasil.UID (UID)

-- FIXME: Haddock open issue #43 seems to make it so GADT constructors cannot
-- be documented properly

type Relation = Expr

infixr 8 $^
infixl 7 $/
infixr 4 $=
infixr 9 $&&
infixr 9 $||

-- Known math functions.
-- TODO: Move the below to a separate file somehow. How to go about it?

{- Expectations of usage (through types), so I know how I should be grouping them together...

-- TODO: What is the type of Vectors? How are they instantiated in the expression language?
--       What about things like proj_i for values of a vector?

Add     :: numType -> numType -> numType
Mul     :: numType -> numType -> numType
Subt    :: numType -> numType -> numType

-- TODO: Bitwise operations?

-- TODO: Do we want to do integer and floating point division?
Frac    :: numType -> numType -> numType

Pow     :: numType -> numType -> numType

-- TODO: Do we want all things to be "equatable"?
Eq      :: x -> x -> Bool
NEq     :: x -> x -> Bool

Lt      :: x -> x -> Bool
Gt      :: x -> x -> Bool

LEq     :: x -> x -> Bool
GEq     :: x -> x -> Bool

And     :: Bool -> Bool -> Bool
Or      :: Bool -> Bool -> Bool
Impl    :: Bool -> Bool -> Bool
Iff     :: Bool -> Bool -> Bool

-- List indexing or Vector indexing? Both?
Index   :: 


Dot     :: Vector numType -> Vector numType -> numType        -- TODO: ?
Cross   :: Vector numType -> Vector numType -> Vector numType

-- TODO: Set logic?
-- TODO: Numeric type casting?

-- TODO: Do we ever need to worry about side-effects with function calls?
--       If so, it might be a bit odd to implement _some_ things like `xor` which require 
--              observing at least one of the values twice?

-- TODO: Some things are not yet implemented (e.g., vector norm, cross, and dot, and, logical implication, iff).
--       Should we be "sticking to the walls" and only doing the things that are implemented to start off with
--       first, and then add later, or should we be adding as we go along, creating default "not implemented errors"
--       as we go along (just essentially laying out the typing information)?

-}

data BinOp = Frac | Pow | Subt | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Index
  | Dot | Cross
  deriving Eq

data ArithOper = Add | Mul
  deriving Eq

data BoolOper = And | Or
  deriving Eq

-- | Unary functions
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg

data UFuncB = Not

data UFuncVec = Norm | Dim

-- | For case expressions
data Completeness = Complete | Incomplete
  deriving Eq

-- | Drasil Expressions
data Expr where
  Dbl      :: Double -> Expr
  Int      :: Integer -> Expr
  Str      :: String -> Expr
  Perc     :: Integer -> Integer -> Expr
  AssocA   :: ArithOper -> [Expr] -> Expr
  AssocB   :: BoolOper  -> [Expr] -> Expr
  -- | Derivative, syntax is:
  --   Type (Partial or total) -> principal part of change -> with respect to
  --   For example: Deriv Part y x1 would be (dy/dx1)
  Deriv    :: DerivType -> Expr -> UID -> Expr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes has a symbol.
  C        :: UID -> Expr
  -- | F(x) is (FCall F [x] []) or similar.
  --   FCall accepts a list of params and a list of named params.
  --   F(x,y) would be (FCall F [x,y]) or sim.
  --   F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | Actor creation given UID and parameters 
  New      :: UID -> [Expr] -> [(UID, Expr)] -> Expr 
  -- | Message an actor: 
  --   1st UID is the actor, 
  --   2nd UID is the method
  Message  :: UID -> UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | Access a field of an actor:
  --   1st UID is the actor,
  --   2nd UID is the field
  Field :: UID -> UID -> Expr
  -- | For multi-case expressions, each pair represents one case
  Case     :: Completeness -> [(Expr,Relation)] -> Expr
  Matrix   :: [[Expr]] -> Expr
  
  -- | Unary functions/operations
  UnaryOp    :: UFunc -> Expr -> Expr
  UnaryOpB   :: UFuncB -> Expr -> Expr
  UnaryOpVec :: UFuncVec -> Expr -> Expr

  BinaryOp :: BinOp -> Expr -> Expr -> Expr
  
  -- | Operators are generalized arithmetic operators over a |DomainDesc|
  --   of an |Expr|.  Could be called |BigOp|.
  --   ex: Summation is represented via |Add| over a discrete domain
  Operator :: ArithOper -> DomainDesc Expr Expr -> Expr -> Expr
  -- | element of
  IsIn     :: Expr -> Space -> Expr 
  -- | a different kind of 'element of'
  RealI    :: UID -> RealInterval Expr Expr -> Expr 

($=), ($!=), ($<), ($>), ($<=), ($>=), ($=>), ($<=>), ($.), ($-),
  ($/), ($^) :: Expr -> Expr -> Expr
($=)   = BinaryOp Eq
($!=)  = BinaryOp NEq
($<)   = BinaryOp Lt
($>)   = BinaryOp Gt
($<=)  = BinaryOp LEq
($>=)  = BinaryOp GEq
($=>)  = BinaryOp Impl
($<=>) = BinaryOp Iff
($.)   = BinaryOp Dot
($-)   = BinaryOp Subt
($/)   = BinaryOp Frac
($^)   = BinaryOp Pow

($&&), ($||) :: Expr -> Expr -> Expr
a $&& b = AssocB And [a,b]
a $|| b = AssocB Or  [a,b]

type Variable = String

data DerivType = Part | Total
  deriving Eq

instance Num Expr where
  (Int 0)        + b              = b
  a              + (Int 0)        = a
  (AssocA Add l) + (AssocA Add m) = AssocA Add (l ++ m)
  (AssocA Add l) + b              = AssocA Add (l ++ [b])
  a              + (AssocA Add l) = AssocA Add (a : l)
  a              + b              = AssocA Add [a, b]

  (AssocA Mul l) * (AssocA Mul m) = AssocA Mul (l ++ m)
  (AssocA Mul l) * b              = AssocA Mul (l ++ [b])
  a              * (AssocA Mul l) = AssocA Mul (a : l)
  a              * b              = AssocA Mul [a, b]

  a - b = BinaryOp Subt a b
  
  fromInteger = Int
  abs         = UnaryOp Abs
  negate      = UnaryOp Neg

  -- this is a Num wart
  signum _ = error "should not use signum in expressions"

instance Eq Expr where
  Dbl a          == Dbl b          =   a == b
  Int a          == Int b          =   a == b
  Str a          == Str b          =   a == b
  AssocA o1 l1   == AssocA o2 l2   =  o1 == o2 && l1 == l2
  AssocB o1 l1   == AssocB o2 l2   =  o1 == o2 && l1 == l2
  Deriv t1 a b   == Deriv t2 c d   =  t1 == t2 && a == c && b == d
  C a            == C b            =   a == b
  FCall a b c    == FCall d e f    =   a == d && b == e && c == f
  Case a b       == Case c d       =   a == c && b == d 
  IsIn  a b      == IsIn  c d      =   a == c && b == d
  BinaryOp o a b == BinaryOp p c d =   o == p && a == c && b == d
  _              == _              =   False

instance Fractional Expr where
  a / b = BinaryOp Frac a b
  fromRational r = BinaryOp Frac (fromInteger $ numerator   r)
                                 (fromInteger $ denominator r)
