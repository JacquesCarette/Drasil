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
import Data.Either (lefts, fromLeft)
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

-- | @Vector x Vector -> Vector@ binary operations (cross product, addition, subtraction).
data VVVBinOp = Cross | VAdd | VSub
  deriving Eq

-- | @Vector x Vector -> Number@ binary operations (dot product).
data VVNBinOp = Dot
  deriving Eq

-- | @Number x Vector -> Vector@ binary operations (scaling).
data NVVBinOp = Scale
  deriving Eq

-- | Element + Set -> Set
data ESSBinOp = SAdd | SRemove
  deriving Eq

-- | Element + Set -> Bool
data ESBBinOp = SContains
  deriving Eq

data AssocConcatOper = SUnion
  deriving Eq

-- | Associative operators (adding and multiplication). Also specifies whether it is for integers or for real numbers.
data AssocArithOper = Add | Mul
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

  AssocC   :: AssocConcatOper -> [Expr] -> Expr
  -- | C stands for "Chunk", for referring to a chunk in an expression.
  --   Implicitly assumes that the chunk has a symbol.
  C        :: UID -> Expr
  -- | Function applications.
  FCall    :: UID -> [Expr] -> Expr
  -- | For multi-case expressions, each pair represents one case.
  Case     :: Completeness -> [(Expr, Relation)] -> Expr
  -- | Represents a matrix of expressions.
  Matrix   :: [[Expr]] -> Expr
  -- | Represents a set of expressions
  Set :: [Expr] -> Expr
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
  -- | Binary operator for @Expr x Vector -> Vector@ operations (scaling).
  NVVBinaryOp   :: NVVBinOp -> Expr -> Expr -> Expr
  -- | Set operator for Element + Set -> Set
  ESSBinaryOp :: ESSBinOp -> Expr -> Expr -> Expr
  -- | Set operator for Element + Set -> Bool
  ESBBinaryOp :: ESBBinOp -> Expr -> Expr -> Expr
  -- | Operators are generalized arithmetic operators over a 'DomainDesc'
  --   of an 'Expr'.  Could be called BigOp.
  --   ex: Summation is represented via 'Add' over a discrete domain.
  Operator :: AssocArithOper -> DiscreteDomainDesc Expr Expr -> Expr -> Expr
  -- | A different kind of 'IsIn'. A 'UID' is an element of an interval.
  RealI    :: UID -> RealInterval Expr Expr -> Expr

-- | Expressions are equal if their constructors and contents are equal.
instance Eq Expr where
  Lit a               == Lit b               =   a == b
  AssocA o1 l1        == AssocA o2 l2        =  o1 == o2 && l1 == l2
  AssocB o1 l1        == AssocB o2 l2        =  o1 == o2 && l1 == l2
  C a                 == C b                 =   a == b
  FCall a b           == FCall c d           =   a == c && b == d
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
  NVVBinaryOp o a b   == NVVBinaryOp p c d   =   o == p && a == c && b == d
  ESSBinaryOp o a b   == ESSBinaryOp p c d   =   o == p && a == c && b == d
  ESBBinaryOp o a b   == ESBBinaryOp p c d   =   o == p && a == c && b == d
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

-- Helper class for pretty-printing errors (should move from here)
-- We don't want to (ab)use Show for this
class Pretty p where
  pretty :: p -> String

instance Pretty VVVBinOp where
  pretty Cross = "cross product"
  pretty VAdd  = "vector addition"
  pretty VSub  = "vector subtraction"

instance LiteralC Expr where
  int = Lit . int
  str = Lit . str
  dbl = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r


-- helper function for typechecking to help reduce duplication
vvvInfer :: TypingContext Space -> VVVBinOp -> Expr -> Expr -> Either Space TypeError
vvvInfer ctx op l r = case (infer ctx l, infer ctx r) of
    (Left lt@(S.Vect lsp), Left (S.Vect rsp)) ->
      if lsp == rsp && S.isBasicNumSpace lsp then
        if op == VSub && (lsp == S.Natural || rsp == S.Natural) then
          Right $ "Vector subtraction expects both operands to be vectors of non-natural numbers. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
        else Left lt
      else Right $ "Vector " ++ pretty op ++ " expects both operands to be vectors of non-natural numbers. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
    (Left lsp, Left rsp) -> Right $ "Vector operation " ++ pretty op ++ " expects vector operands. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
    (_       , Right re) -> Right re
    (Right le, _       ) -> Right le


instance Typed Expr Space where
  check :: TypingContext Space -> Expr -> Space -> Either Space TypeError
  check = typeCheckByInfer

  infer :: TypingContext Space -> Expr -> Either Space TypeError
  infer cxt (Lit lit) = infer cxt lit

  infer cxt (AssocA _ (e:exs)) = 
    case infer cxt e of
      Left spaceValue | S.isBasicNumSpace spaceValue -> 
          -- If the inferred type of e is a valid Space, call allOfType with spaceValue
          allOfType cxt exs spaceValue spaceValue 
              "Associative arithmetic operation expects all operands to be of the same expected type."
      Left l ->
          -- Handle the case when sp is a Left value but spaceValue is invalid
          Right ("Expected all operands in addition/multiplication to be numeric, but found " ++ show l)
      Right r ->
          -- If sp is a Right value containing a TypeError
          Right r
  infer _ (AssocA Add _) = Right "Associative addition requires at least one operand."
  infer _ (AssocA Mul _) = Right "Associative multiplication requires at least one operand."

  infer cxt (AssocB _ exs) = allOfType cxt exs S.Boolean S.Boolean
    $ "Associative boolean operation expects all operands to be of the same type (" ++ show S.Boolean ++ ")."

  infer cxt (AssocC _ (e:exs)) = 
    case infer cxt e of
      Left spaceValue | spaceValue /= S.Void -> 
          -- If the inferred type of e is a valid Space, call allOfType with spaceValue
          allOfType cxt exs spaceValue spaceValue 
              "Associative arithmetic operation expects all operands to be of the same expected type."
      Left l ->
          -- Handle the case when sp is a Left value but spaceValue is invalid
          Right ("Expected all operands in addition/multiplication to be numeric, but found " ++ show l)
      Right r ->
          -- If sp is a Right value containing a TypeError
          Right r
  infer _ (AssocC SUnion _) = Right "Associative addition requires at least one operand."
    
  infer cxt (C uid) = inferFromContext cxt uid

  infer cxt (FCall uid exs) = case (inferFromContext cxt uid, map (infer cxt) exs) of
    (Left (S.Function params out), exst) -> if NE.toList params == lefts exst
      then Left out
      else Right $ "Function `" ++ show uid ++ "` expects parameters of types: " ++ show params ++ ", but received: " ++ show (lefts exst) ++ "."
    (Left s, _) -> Right $ "Function application on non-function `" ++ show uid ++ "` (" ++ show s ++ ")."
    (Right x, _) -> Right x

  infer cxt (Case _ ers)
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
        t = fromLeft (error "Infer on Matrix had a strong expectation of Left-valued data.") expT -- This error should never occur.

  infer cxt (Set (e:exs)) =
    case infer cxt e of
        Left sp -> if S.isBasicNumSpace sp then Left sp else Right (show sp)
        Right err -> Right "Expressions in case"
  infer cxt (Set _) = Right "Expressions in case"

  infer cxt (UnaryOp uf ex) = case infer cxt ex of
    Left sp -> case uf of
      Abs -> if S.isBasicNumSpace sp && sp /= S.Natural
        then Left sp
        else Right $ "Numeric 'absolute' value operator only applies to, non-natural, numeric types. Received `" ++ show sp ++ "`."
      Neg -> if S.isBasicNumSpace sp && sp /= S.Natural
        then Left sp
        else Right $ "Negation only applies to, non-natural, numeric types. Received `" ++ show sp ++ "`."
      Exp -> if sp == S.Real || sp == S.Integer then Left S.Real else Right $ show Exp ++ " only applies to reals."
      x -> if sp == S.Real
        then Left S.Real
        else Right $ show x ++ " only applies to Reals. Received `" ++ show sp ++ "`."
    x       -> x

  infer cxt (UnaryOpB Not ex) = case infer cxt ex of
    Left S.Boolean -> Left S.Boolean
    Left sp        -> Right $ "¬ on non-boolean operand, " ++ show sp ++ "."
    x              -> x

  infer cxt (UnaryOpVV NegV e) = case infer cxt e of
    Left (S.Vect sp) -> if S.isBasicNumSpace sp && sp /= S.Natural
      then Left $ S.Vect sp
      else Right $ "Vector negation only applies to, non-natural, numbered vectors. Received `" ++ show sp ++ "`."
    Left sp -> Right $ "Vector negation should only be applied to numeric vectors. Received `" ++ show sp ++ "`."
    x -> x

  infer cxt (UnaryOpVN Norm e) = case infer cxt e of
    Left (S.Vect S.Real) -> Left S.Real
    Left sp -> Right $ "Vector norm only applies to vectors of real numbers. Received `" ++ show sp ++ "`."
    x -> x

  infer cxt (UnaryOpVN Dim e) = case infer cxt e of
    Left (S.Vect _) -> Left S.Integer -- FIXME: I feel like Integer would be more usable, but S.Natural is the 'real' expectation here
    Left sp -> Right $ "Vector 'dim' only applies to vectors. Received `" ++ show sp ++ "`."
    x -> x

  infer cxt (ArithBinaryOp Frac l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isBasicNumSpace lt && lt == rt
      then Left lt
      else Right $ "Fractions/divisions should only be applied to the same numeric typed operands. Received `" ++ show lt ++ "` / `" ++ show rt ++ "`."
    (_      , Right e) -> Right e
    (Right e, _      ) -> Right e

  infer cxt (ArithBinaryOp Pow l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isBasicNumSpace lt && (lt == rt || (lt == S.Real && rt == S.Integer))
      then Left lt
      else Right $
        "Powers should only be applied to the same numeric type in both operands, or real base with integer exponent. Received `" ++ show lt ++ "` ^ `" ++ show rt ++ "`."
    (_      , Right x) -> Right x
    (Right x, _      ) -> Right x

  infer cxt (ArithBinaryOp Subt l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isBasicNumSpace lt && lt == rt
      then Left lt
      else Right $ "Both operands of a subtraction must be the same numeric type. Received `" ++ show lt ++ "` - `" ++ show rt ++ "`."
    (_, Right re) -> Right re
    (Right le, _) -> Right le

  infer cxt (BoolBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left S.Boolean, Left S.Boolean) -> Left S.Boolean
    (Left lt, Left rt) -> Right $ "Boolean expression contains non-boolean operand. Received `" ++ show lt ++ "` & `" ++ show rt ++ "`."
    (_     , Right er) -> Right er
    (Right el, _     ) -> Right el

  infer cxt (EqBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if lt == rt
      then Left S.Boolean
      else Right $ "Both operands of an (in)equality (=/≠) must be of the same type. Received `" ++ show lt ++ "` & `" ++ show rt ++ "`."
    (_, Right re) -> Right re
    (Right le, _) -> Right le

  infer cxt (LABinaryOp Index l n) = case (infer cxt l, infer cxt n) of
    (Left (S.Vect lt), Left nt) -> if nt == S.Integer || nt == S.Natural -- I guess we should only want it to be natural numbers, but integers or naturals is fine for now
      then Left lt
      else Right $ "List accessor not of type Integer nor Natural, but of type `" ++ show nt ++ "`"
    (Left lt         , Left _)  -> Right $ "List accessor expects a list/vector, but received `" ++ show lt ++ "`."
    (_               , Right e) -> Right e
    (Right e         , _      ) -> Right e

  infer cxt (OrdBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt) -> if S.isBasicNumSpace lt && lt == rt
      then Left S.Boolean
      else Right $ "Both operands of a numeric comparison must be the same numeric type, got: " ++ show lt ++ ", " ++ show rt ++ "."
    (_, Right re) -> Right re
    (Right le, _) -> Right le

  infer cxt (NVVBinaryOp Scale l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left (S.Vect rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Left rsp
      else if lt /= rsp then
        Right $ "Vector scaling expects a scaling by the same kind as the vector's but found scaling by`" ++ show lt ++ "` over vectors of type `" ++ show rsp ++ "`."
      else
        Right $ "Vector scaling expects a numeric scaling, but found `" ++ show lt ++ "`."
    (Left _, Left rsp) -> Right $ "Vector scaling expects vector as second operand. Received `" ++ show rsp ++ "`."
    (_, Right rx) -> Right rx
    (Right lx, _) -> Right lx

  infer cxt (VVVBinaryOp o l r) = vvvInfer cxt o l r
    {- case (infer cxt l, infer cxt r) of
    (Left lTy, Left rTy) -> if lTy == rTy && S.isBasicNumSpace lTy && lTy /= S.Natural
      then Left lTy
      else Right $ "Vector cross product expects both operands to be vectors of non-natural numbers. Received `" ++ show lTy ++ "` X `" ++ show rTy ++ "`."
    (_       , Right re) -> Right re
    (Right le, _       ) -> Right le
    -}

  infer cxt (VVNBinaryOp Dot l r) = case (infer cxt l, infer cxt r) of
    (Left lt@(S.Vect lsp), Left rt@(S.Vect rsp)) -> if lsp == rsp && S.isBasicNumSpace lsp
      then Left lsp
      else Right $ "Vector dot product expects same numeric vector types, but found `" ++ show lt ++ "` · `" ++ show rt ++ "`."
    (Left lsp, Left rsp) -> Right $ "Vector dot product expects vector operands. Received `" ++ show lsp ++ "` · `" ++ show rsp ++ "`."
    (_, Right rx) -> Right rx
    (Right lx, _) -> Right lx

  infer cxt (ESSBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt@(S.Set rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Left lt
      else Right $ "Set Add/Sub should only be applied to Set of same space. Received `" ++ show lt ++ "` / `" ++ show rt ++ "`."
    (_      , Right e) -> Right e
    (Right e, _      ) -> Right e
    (Left lt, Left rsp) -> Right $ "Set union expects set operands. Received `" ++ show lt ++ "` · `" ++ show rsp ++ "`."

  infer cxt (ESBBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left rt@(S.Set rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Left lt
      else Right $ "Set contains should only be applied to Set of same space. Received `" ++ show lt ++ "` / `" ++ show rt ++ "`."
    (_      , Right e) -> Right e
    (Right e, _      ) -> Right e
    (Left lt, Left rsp) -> Right $ "Set union expects set operands. Received `" ++ show lt ++ "` · `" ++ show rsp ++ "`."

  infer cxt (Operator _ (S.BoundedDD _ _ bot top) body) =
    let expTy = S.Integer in
    case (infer cxt bot, infer cxt top, infer cxt body) of
      (Left botTy, Left topTy, Left bodyTy) -> if botTy == S.Integer
        then if topTy == S.Integer
          then if expTy == bodyTy
            then Left expTy
            else Right $ "'Big' operator range body not Integer, found: " ++ show bodyTy ++ "."
          else Right $ "'Big' operator range top not Integer, found: " ++ show topTy ++ "."
        else Right $ "'Big' operator range bottom not of expected type: " ++ show expTy ++ ", found: " ++ show botTy ++ "."
      (_         , _         , Right x    ) -> Right x
      (_         , Right x   , _          ) -> Right x
      (Right x   , _         , _          ) -> Right x

  infer cxt (RealI uid ri) =
    case (inferFromContext cxt uid, riTy ri) of
      (Left S.Real, Left riSp) -> if riSp == S.Real
        then Left S.Boolean
        else Right $
          "Real interval expects interval bounds to be of type Real, but received: " ++ show riSp ++ "."
      (Left uidSp, _         ) -> Right $
        "Real interval expects variable to be of type Real, but received `" ++ show uid ++ "` of type `" ++ show uidSp ++ "`."
      (_          , Right x  ) -> Right x
      (Right x    , _        ) -> Right x
    where
      riTy :: RealInterval Expr Expr -> Either Space TypeError
      riTy (S.Bounded (_, lx) (_, rx)) = case (infer cxt lx, infer cxt rx) of
        (Left lt, Left rt) -> if lt == rt
          then Left lt
          else Right $
            "Bounded real interval contains mismatched types for bottom and top. Received `" ++ show lt ++ "` to `" ++ show rt ++ "`."
        (_      , Right x) -> Right x
        (Right x, _      ) -> Right x
      riTy (S.UpTo (_, x)) = infer cxt x
      riTy (S.UpFrom (_, x)) = infer cxt x