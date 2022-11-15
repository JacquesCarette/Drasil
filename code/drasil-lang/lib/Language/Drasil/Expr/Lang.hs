{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The Drasil Expression language
module Language.Drasil.Expr.Lang where

import           Language.Drasil.Literal.Class (LiteralC (..))
import           Language.Drasil.Literal.Lang  (Literal (..))
import           Language.Drasil.Space         (DiscreteDomainDesc,
                                                RealInterval, Space)
import qualified Language.Drasil.Space         as S
import           Language.Drasil.UID           (UID)
import           Language.Drasil.WellTyped
import qualified Language.Drasil.WellTyped     as S
import Data.Either (lefts)
import qualified Data.Foldable as NE

-- * Expression Types

-- | A relation is just an expression ('Expr').
type Relation = Expr

-- | The variable type is just a renamed 'String'.
type Variable = String

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

-- TODO: I suppose these can be merged to just Add and Mul?
-- | Associative operators (adding and multiplication). Also specifies whether it is for integers or for real numbers.
data AssocArithOper = AddI | AddRe | MulI | MulRe
  deriving Eq

-- | Associative boolean operators (and, or).
data AssocBoolOper = And | Or
  deriving Eq

-- | Unary functions (abs, log, ln, sin, etc.).
data UFunc = Abs | Log | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin
  | Arccos | Arctan | Exp | Sqrt | Neg
  deriving (Eq, Show)

-- | @Bool -> Bool@ operators.
data UFuncB = Not
  deriving Eq

-- | @Vector -> Vector@ operators.
data UFuncVV = NegV
  deriving Eq

-- | @Vector -> Number@ operators.
data UFuncVN = Norm | Dim
  deriving Eq

-- | For case expressions (either complete or incomplete).
data Completeness = Complete | Incomplete
  deriving Eq

-- ** Expr

-- | Expression language where all terms are supposed to be 'well understood'
--   (i.e., have a definite meaning). Right now, this coincides with
--   "having a definite value", but should not be restricted to that.
data Expr where
  -- | Brings a literal into the expression language.
  Lit :: Literal -> Expr
  -- | Takes an associative arithmetic operator with a list of expressions.
  AssocA   :: AssocArithOper -> [Expr] -> Expr
  -- | Takes an associative boolean operator with a list of expressions.
  AssocB   :: AssocBoolOper  -> [Expr] -> Expr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> Expr
  -- | A function call accepts a list of parameters and a list of named parameters.
  --   For example
  --
  --   * F(x) is (FCall F [x] []).
  --   * F(x,y) would be (FCall F [x,y]).
  --   * F(x,n=y) would be (FCall F [x] [(n,y)]).
  FCall    :: UID -> [Expr] -> [(UID, Expr)] -> Expr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(Expr, Relation)] -> Expr
  -- | Represents a matrix of expressions.
  Matrix   :: [[Expr]] -> Expr

  -- | Unary operation for most functions (eg. sin, cos, log, etc.).
  UnaryOp       :: UFunc -> Expr -> Expr
  -- | Unary operation for @Bool -> Bool@ operations.
  UnaryOpB      :: UFuncB -> Expr -> Expr
  -- | Unary operation for @Vector -> Vector@ operations.
  UnaryOpVV     :: UFuncVV -> Expr -> Expr
  -- | Unary operation for @Vector -> Number@ operations.
  UnaryOpVN     :: UFuncVN -> Expr -> Expr

  -- | Binary operator for arithmetic between expressions (fractional, power, and subtraction).
  ArithBinaryOp :: ArithBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for boolean operators (implies, iff).
  BoolBinaryOp  :: BoolBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for equality between expressions.
  EqBinaryOp    :: EqBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for indexing two expressions.
  LABinaryOp    :: LABinOp -> Expr -> Expr -> Expr
  -- | Binary operator for ordering expressions (less than, greater than, etc.).
  OrdBinaryOp   :: OrdBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Vector x Vector -> Vector@ operations (cross product).
  VVVBinaryOp   :: VVVBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Vector x Vector -> Number@ operations (dot product).
  VVNBinaryOp   :: VVNBinOp -> Expr -> Expr -> Expr

  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DiscreteDomainDesc Expr Expr -> Expr -> Expr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval Expr Expr -> Expr

-- | Expressions are equal if their constructors and contents are equal.
instance Eq Expr where
  Lit (Int l)         == Lit (Int r)         =  l == r
  Lit (Str l)         == Lit (Str r)         =  l == r
  Lit (Dbl l)         == Lit (Dbl r)         =  l == r
  Lit (ExactDbl l)    == Lit (ExactDbl r)    =  l == r
  Lit (Perc l1 l2)    == Lit (Perc r1 r2)    =  l1 == r1 && l2 == r2
  -- Lit a               == Lit b               =   a == b -- TODO: When we have typed expressions, I think this will be possible.
  AssocA o1 l1        == AssocA o2 l2        =  o1 == o2 && l1 == l2
  AssocB o1 l1        == AssocB o2 l2        =  o1 == o2 && l1 == l2
  C a                 == C b                 =   a == b
  FCall a b c         == FCall d e f         =   a == d && b == e && c == f
  Case a b            == Case c d            =   a == c && b == d
  UnaryOp a b         == UnaryOp c d         =   a == c && b == d
  UnaryOpB a b        == UnaryOpB c d        =   a == c && b == d
  UnaryOpVV a b       == UnaryOpVV c d       =   a == c && b == d
  UnaryOpVN a b       == UnaryOpVN c d       =   a == c && b == d
  ArithBinaryOp o a b == ArithBinaryOp p c d =   o == p && a == c && b == d
  BoolBinaryOp o a b  == BoolBinaryOp p c d  =   o == p && a == c && b == d
  EqBinaryOp o a b    == EqBinaryOp p c d    =   o == p && a == c && b == d
  OrdBinaryOp o a b   == OrdBinaryOp p c d   =   o == p && a == c && b == d
  LABinaryOp o a b    == LABinaryOp p c d    =   o == p && a == c && b == d
  VVVBinaryOp o a b   == VVVBinaryOp p c d   =   o == p && a == c && b == d
  VVNBinaryOp o a b   == VVNBinaryOp p c d   =   o == p && a == c && b == d
  _                   == _                   =   False
-- ^ TODO: This needs to add more equality checks

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

-- instance Fractional Expr where
--   a / b = ArithBinaryOp Frac a b
--   fromRational r = ArithBinaryOp Frac (fromInteger $ numerator   r)
--                                       (fromInteger $ denominator r)

instance LiteralC Expr where
  int = Lit . int
  str = Lit . str
  dbl = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r

assocArithOperToTy :: AssocArithOper -> Space
assocArithOperToTy AddI = S.Integer
assocArithOperToTy MulI = S.Integer
assocArithOperToTy _    = S.Real

instance Typed Expr Space where
  infer :: TypingContext Space -> Expr -> Either Space TypeError
  infer cxt (Lit lit) = infer cxt lit

  infer cxt (AssocA op exs)
    | allOfType cxt exs t = Left t
    | otherwise = Right "Associative arithmetic operation does not contain strictly the same numeric type."
      where t = assocArithOperToTy op

  infer cxt (AssocB _ exs)
    | allOfType cxt exs S.Boolean = Left S.Boolean
    | otherwise = Right "Associative boolean operation does not contain strictly boolean operands."

  infer cxt (C uid) = inferFromContext cxt uid

  -- FIXME: It seems odd having named arguments here. Should we remove it? For
  -- now, I'm not type checking them.
  infer cxt (FCall uid exs _) = case (inferFromContext cxt uid, map (infer cxt) exs) of
    (Left (S.Function params out), exst) -> if NE.toList params == lefts exst
      then Left out
      else Right $ "Function `" ++ show uid ++ "` expects parameters of types: " ++ show params
    (Left _, _) -> Right $ "Function application on non-function: " ++ show uid
    (Right x, _) -> Right x

  infer cxt (Case _ ers) -- = _ -- all (\(e, r) -> infer cxt e) ers
    | null ers = Right "Case contains no expressions, no type to infer."
    | all (\(ne, _) -> infer cxt ne == eT) (tail ers) = eT
    | otherwise = Right "Expressions in case statement contain different types."
      where
        (fe, _) = head ers
        eT = infer cxt fe

  infer cxt (Matrix exss)
    | null exss = Right "Matrix has no rows."
    | null $ head exss = Right "Matrix has no columns."
    | allRowsHaveSameColumnsAndSpace = Left $ S.Matrix rows columns t
    | otherwise = Right "Not all rows have the same number of columns or the same value types."
    where
        rows = length exss
        columns = if rows > 0 then length $ head exss else 0
        sss = map (map (infer cxt)) exss
        expT = head $ head sss
        allRowsHaveSameColumnsAndSpace
          = either
              (\_ -> all (\ r -> length r == columns && all (== expT) r) sss)
              (const False) expT
        (Left t) = expT

  infer cxt (UnaryOp uf ex) = case infer cxt ex of
    Left sp -> case uf of
      Abs -> if S.isNumericSpace sp && sp /= S.Natural
        then Left sp
        else Right "numeric 'absolute' value operator only applies to, non-natural, numeric types"
      Neg -> if S.isNumericSpace sp && sp /= S.Natural
        then Left sp
        else Right "negation only applies to, non-natural, numeric types"
      Exp -> if sp == S.Real || sp == S.Integer then Left S.Real else Right $ show Exp ++ " only applies to reals"
      x -> if sp == S.Real then Left S.Real else Right $ show x ++ " only applies to reals"
    x       -> x

  infer cxt (UnaryOpB Not ex) = case infer cxt ex of
    Left S.Boolean -> Left S.Boolean
    Left _         -> Right "¬ on non-boolean operand"
    x              -> x

  -- TODO: What about "Vect Vect ... Vect X"?
  infer cxt (UnaryOpVV NegV e) = case infer cxt e of
    Left (S.Vect sp) -> if S.isNumericSpace sp && sp /= S.Natural
      then Left $ S.Vect sp
      else Right "Vector negation only applies to, non-natural, numbered vectors"
    Left _ -> Right "Vector negation should only be applied to numeric vectors."
    Right ex -> Right ex

  infer cxt (UnaryOpVN Norm e) = case infer cxt e of
    Left (S.Vect sp) -> if sp == S.Real
      then Left S.Real
      else Right "Vector norm only applies to vectors of real numbers"
    Left _ -> Right "Vector norm only applies to vectors of real numbers"
    ex -> ex

  infer cxt (UnaryOpVN Dim e) = case infer cxt e of
    Left _ -> Left S.Integer -- FIXME: I feel like Integer would be more usable, but S.Natural is the 'real' expectation here
    ex -> ex

  infer cxt (ArithBinaryOp Frac l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isNumericSpace lt && lt == rt -- FIXME: What do we want here?
      then Left lt
      else Right "Fractions/divisions should only be applied to the same numeric typed operands"
    (_      , Right e) -> Right e
    (Right e, _      ) -> Right e

  infer cxt (ArithBinaryOp Pow l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isNumericSpace lt && (lt == rt || (lt == S.Real && rt == S.Integer))
      then Left lt
      else Right "Powers only be applied to the same numeric type in both operands, or real base with integer exponent"
    (_      , Right x) -> Right x
    (Right x, _      ) -> Right x

  infer cxt (ArithBinaryOp Subt l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isNumericSpace lt && lt == rt
      then Left lt
      else Right "Both operands of a subtraction must be the same numeric type"
    (_, Right re) -> Right re
    (Right le, _) -> Right le

  infer cxt (BoolBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left S.Boolean, Left S.Boolean) -> Left S.Boolean
    (Left _, Left _) -> Right "Boolean expression contains non-boolean operand"
    (_     , Right er) -> Right er
    (Right el, _     ) -> Right el

  infer cxt (EqBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if lt == rt
      then Left S.Boolean
      else Right "Both operands of an (in)equality (=/≠) must be of the same type"
    (_, Right re) -> Right re
    (Right le, _) -> Right le

  infer cxt (LABinaryOp Index l n) = case (infer cxt l, infer cxt n) of
    (Left (S.Vect lt), Left nt) -> if nt == S.Integer || nt == S.Natural -- I guess we should only want it to be natural numbers, but integers or naturals is fine for now
      then Left lt
      else Right "List accessor not of type integer nor natural"
    (Left _          , _      ) -> Right "List accessor expects a list/vector"
    (_               , Right e) -> Right e
    (Right e         , _      ) -> Right e

  infer cxt (OrdBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isNumericSpace lt && lt == rt
      then Left S.Boolean
      else Right "Both operands of a numeric comparison must be the same numeric type"
    (_, Right re) -> Right re
    (Right le, _) -> Right le

  infer cxt (VVVBinaryOp Cross l r) = case (infer cxt l, infer cxt r) of
    (Left lTy, Left rTy) -> if lTy == rTy
      then Left lTy
      else Right "Vector cross product expects both operands to have the same time"
    (_       , Right re) -> Right re
    (Right le, _       ) -> Right le

  infer cxt (VVNBinaryOp Dot l r) = case (infer cxt l, infer cxt r) of
    (Left (S.Vect lsp), Left (S.Vect rsp)) -> if lsp == rsp && S.isNumericSpace lsp
      then Left lsp
      else Right "Vector dot product expects numeric vector operands"
    (Left _, Left _) -> Right "Vector dot product expects vector operands"
    (_, Right rx) -> Right rx
    (Right lx, _) -> Right lx

  infer cxt (Operator aao (S.BoundedDD _ _ bot top) body) = let expTy = assocArithOperToTy aao
    in case (infer cxt bot, infer cxt top, infer cxt body) of
      (Left botTy, Left topTy, Left bodyTy) -> if expTy == botTy
        then if expTy == topTy
          then if expTy == bodyTy
            then Left expTy
            else Right $ "'Big' operator range body not of expected type: " ++ show expTy
          else Right $ "'Big' operator range top not of expected type: " ++ show expTy
        else Right $ "'Big' operator range bottom not of expected type: " ++ show expTy
      (_         , _         , Right x    ) -> Right x
      (_         , Right x   , _          ) -> Right x
      (Right x   , _         , _          ) -> Right x

  infer cxt (RealI uid ri) = case inferFromContext cxt uid of
    Left uidSp -> case riOfTy uidSp ri of
      Left True -> Left S.Boolean
      Left _    -> Right "Interval not of same type as variable"
      Right s   -> Right s
    x -> x
    where
      riOfTy :: Space -> RealInterval Expr Expr -> Either Bool TypeError
      riOfTy sp (S.Bounded (_, lx) (_, rx)) = case (isOfTy sp cxt lx, isOfTy sp cxt rx) of
        (Left l, Left r)  -> Left $ l && r
        (Left _, Right r) -> Right r
        (Right l, _)      -> Right l
      riOfTy sp (S.UpTo (_, x)) = isOfTy sp cxt x
      riOfTy sp (S.UpFrom (_, x)) = isOfTy sp cxt x

      isOfTy :: S.Typed e t => t -> TypingContext t -> e -> Either Bool TypeError
      isOfTy sp cxt' e = case infer cxt' e of
        Left x  -> Left $ x == sp
        Right x -> Right x -- This can't be "x -> x" due to type of "x" in this case conflicting!


-- instance (Typed a Space) => Typed (RealInterval a a) Space where
--   infer :: Typed a Space => TypingContext Space -> RealInterval a a -> Either Space TypeError
--   infer cxt (Bounded (_, l) (_, r)) = case (infer cxt l, infer cxt r) of
--     (Left Real, Right Real) -> Left Boolean
--   infer cxt (UpTo (_, x0)) = case infer cxt x0 of
--     Left Real -> Left Boolean
--     Left sp   -> Right $ "Expression in 'real interval' not Real-typed, but `" ++ show sp ++ "`-typed"
--     x         -> x
--   infer cxt (UpFrom (_, x0)) = case infer cxt x0 of
--     Left Real -> Left Boolean
--     Left sp   -> Right $ "Expression in 'real interval' not Real-typed, but `" ++ show sp ++ "`-typed"
--     x         -> x
