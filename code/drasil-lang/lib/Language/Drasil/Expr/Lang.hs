{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The Drasil Expression language
module Language.Drasil.Expr.Lang where

import Data.Either (fromRight, rights)
import qualified Data.Foldable as NE
import Data.List (nub)

import Drasil.Database (UID)

import Language.Drasil.Literal.Class (LiteralC (..))
import Language.Drasil.Literal.Lang (Literal (..))
import Language.Drasil.Space (DiscreteDomainDesc, RealInterval, Space,
  assertVector, assertNumericVector, assertNumeric, assertFunction,
  assertNonNatNumVector, assertRealVector, assertEquivNumeric, assertNonNatNumeric,
  assertIndexLike, assertSet, assertReal, assertBoolean)
import qualified Language.Drasil.Space as S
import Language.Drasil.WellTyped

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

-- | Index operator. `Index` represents accessing an element at a specific
-- index, while `IndexOf` represents finding the index of a specific element.
data LABinOp = Index | IndexOf
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
  Set      :: Space -> [Expr] -> Expr
  -- | used to refernce the (name + type = variable )
  Variable :: String -> Expr -> Expr
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
vvvInfer :: TypingContext Space -> VVVBinOp -> Expr -> Expr -> Either TypeError Space
vvvInfer ctx op l r = do
  lt <- infer ctx l
  rt <- infer ctx r

  let msg dir sp = "Vector operation " ++ pretty op ++ " expects numeric vectors, but found `" ++ sp ++ "` on the " ++ dir ++ "-hand side."

  lsp <- assertNumericVector lt $ msg "left"
  rsp <- assertNumericVector rt $ msg "right"

  if op == VSub then
    assertNonNatNumeric lsp $ \sp ->
      "Vector subtraction expects both operands to be vectors of non-natural numbers. Received `" ++ sp ++ "`."
  else Right ()

  lsp ~== rsp $ \lt' rt' -> "Vector " ++ pretty op ++ " expects both operands to be of the same numeric type. Received `" ++ lt' ++ "` and `" ++ rt' ++ "`."

  pure lt

instance Typed Expr Space where
  check :: TypingContext Space -> Expr -> Space -> Either TypeError Space
  check = typeCheckByInfer

  infer :: TypingContext Space -> Expr -> Either TypeError Space
  infer cxt (Lit lit) = infer cxt lit

  infer cxt (AssocA _ (e:exs)) = do
    et <- infer cxt e
    assertNumeric et $
      \sp -> "Associative arithmetic operation expects numeric operands, but found `" ++ sp ++ "`."
    assertAllEq cxt exs et
        "Associative arithmetic operation expects all operands to be of the same type."
    pure et
  infer _ (AssocA Add _) = Left "Associative addition requires at least one operand."
  infer _ (AssocA Mul _) = Left "Associative multiplication requires at least one operand."

  infer cxt (AssocB _ exs) = do
    assertAllEq cxt exs S.Boolean $ "Associative boolean operation expects all operands to be of the same type (" ++ show S.Boolean ++ ")."
    pure S.Boolean

  infer cxt (AssocC _ (e:exs)) =
    case infer cxt e of
      Right spaceValue | spaceValue /= S.Void -> do
          assertAllEq cxt exs spaceValue
              "Associative arithmetic operation expects all operands to be of the same type."
          pure spaceValue
      Right r ->
          -- Handle the case when sp is a Left value but spaceValue is invalid
          Left ("Expected all operands in addition/multiplication to be numeric, but found " ++ show r)
      Left l ->
          -- If sp is a Right value containing a TypeError
          Left l

  infer _ (AssocC SUnion _) = Left "Associative addition requires at least one operand."

  infer cxt (C uid) = inferFromContext cxt uid

  infer cxt (Variable _ n) = infer cxt n

  infer cxt (FCall uid exs) = do
    ft <- inferFromContext cxt uid
    (params, out) <- assertFunction ft $ \t -> "Function application on non-function `" ++ show uid ++ "` (" ++ t ++ ")."
    let exst = map (infer cxt) exs
    if NE.toList params == rights exst
      then pure out
      else Left $ "Function `" ++ show uid ++ "` expects parameters of types: " ++ show params ++ ", but received: " ++ show (rights exst) ++ "."

  infer   _ (Case _ []) = Left "Case contains no expressions, no type to infer."
  infer cxt (Case _ ers) = do
    let inferPair (e, r) = (,) <$> infer cxt e <*> infer cxt r
    ers' <- traverse inferPair ers -- fail and return immediately on first Left
    let (ets, rts) = unzip ers'
        rt = nub rts
        et = nub ets

    if rt /= [S.Boolean] then
      Left $ "Case contains expressions of different types: " ++ show rt
    else if length et /= 1 then
      Left $ "Case contains expressions of different types: " ++ show et
    else
      pure $ head et

  infer cxt (Matrix exss)
    | null exss = Left "Matrix has no rows."
    | null $ head exss = Left "Matrix has no columns."
    | allRowsHaveSameColumnsAndSpace = Right $ S.Matrix rows columns t
    | otherwise = Left "Not all rows have the same number of columns or the same value types."
    where
        rows = length exss
        columns = if rows > 0 then length $ head exss else 0
        sss = map (map (infer cxt)) exss
        expT = head $ head sss
        allRowsHaveSameColumnsAndSpace
          = either
              (\_ -> all (\ r -> length r == columns && all (== expT) r) sss)
              (const False) expT
        t = fromRight (error "Infer on Matrix had a strong expectation of Right-valued data.") expT -- This error should never occur.

  infer cxt (Set s es) = do
    ets <- traverse (infer cxt) es
    if all (== s) ets
      then pure s
      else Left $ "Set contains expressions of unexpected type: `" ++ show (filter (/= s) ets) ++ "`. Expected type: `" ++ show s ++ "`."

  infer cxt (UnaryOp uf e) = do
    et <- infer cxt e
    case uf of
      Abs -> do
        assertNonNatNumeric et (\sp -> "'Absolute value' operator only applies to non-natural numeric types. Received `" ++ sp ++ "`.")
        pure et
      Neg -> do
        assertNonNatNumeric et (\sp -> "'Negation' operator only applies to non-natural numeric types. Received `" ++ sp ++ "`.")
        pure et
      Exp -> do
        if et == S.Real || et == S.Integer
          then pure S.Real
          else Left $ "'Exponentiation' operator only applies to reals and integers. Received `" ++ show et ++ "`."
      x -> do
        if et == S.Real
          then pure S.Real
          else Left $ show x ++ " operator only applies to Reals. Received `" ++ show et ++ "`."

  infer cxt (UnaryOpB Not e) = do
    et <- infer cxt e
    assertBoolean et (\sp -> "¬ on non-boolean operand of type: " ++ sp ++ ".")
    pure S.Boolean

  infer cxt (UnaryOpVV NegV e) = do
    et <- infer cxt e
    vet <- assertNonNatNumVector (\sp -> "Vector negation only applies to non-natural numeric vectors. Received `" ++ sp ++ "`.") et
    pure $ S.Vect vet

  infer cxt (UnaryOpVN Norm e) = do
    et <- infer cxt e
    assertRealVector et (\sp -> "Vector norm only applies to vectors of real numbers. Received `" ++ sp ++ "`.")
    pure et

  infer cxt (UnaryOpVN Dim e) = do
    et <- infer cxt e
    _ <- assertVector et (\sp -> "Vector dimension only applies to vectors. Received `" ++ sp ++ "`.")
    pure S.Integer

  infer cxt (ArithBinaryOp Frac n d) = do
    nt <- infer cxt n
    dt <- infer cxt d
    assertEquivNumeric nt dt
      (\lt rt -> "Fractions/divisions should only be applied to the same numeric typed operands. Received `" ++ lt ++ "` / `" ++ rt ++ "`.")
    pure nt

  infer cxt (ArithBinaryOp Pow l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    if S.isBasicNumSpace lt && (lt == rt || (lt == S.Real && rt == S.Integer))
      then Right lt
      else Left $
        "Powers should only be applied to the same numeric type in both operands, or real base with integer exponent. Received `" ++ show lt ++ "` ^ `" ++ show rt ++ "`."

  infer cxt (ArithBinaryOp Subt l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    assertEquivNumeric
      lt rt
      (\ls rs -> "Subtraction should only be applied to the same numeric typed operands. Received `" ++ ls ++ "` - `" ++ rs ++ "`.")
    pure lt

  infer cxt (BoolBinaryOp _ l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    let msg = const $ "Boolean expression contains non-boolean operand. Received `" ++ show lt ++ "` & `" ++ show rt ++ "`."
    assertBoolean lt msg
    assertBoolean rt msg
    pure S.Boolean

  infer cxt (EqBinaryOp _ l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    lt ~== rt $ \lsp rsp -> "Both operands of an (in)equality (=/≠) must be of the same type. Received `" ++ lsp ++ "` & `" ++ rsp ++ "`."
    pure S.Boolean

  infer cxt (LABinaryOp Index l n) = do
    lt <- infer cxt l
    vet <- assertVector lt (\sp -> "List accessor expects a vector, but received `" ++ sp ++ "`.")

    nt <- infer cxt n
    assertIndexLike nt
      (\sp -> "List accessor expects an index-like type (Integer or Natural), but received `" ++ sp ++ "`.")

    pure vet

  infer cxt (LABinaryOp IndexOf l e) = do
    lt <- infer cxt l
    vet <- assertVector lt (\sp -> "List index-of expects a vector, but received `" ++ sp ++ "`.")

    et <- infer cxt e
    vet ~== et
      $ \ls rs -> "List index-of expects an element of the same type as the vector, but received `" ++ ls ++ "` and `" ++ rs ++ "`."

    pure S.Integer -- TODO: This can also be `S.Natural`, but we don't express that in the type system yet.

  infer cxt (OrdBinaryOp _ l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    let msg ls rs = "Ordering expression contains non-numeric operand. Received `" ++ ls ++ "` & `" ++ rs ++ "`."
    assertEquivNumeric lt rt msg
    pure S.Boolean

  infer cxt (NVVBinaryOp Scale l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    vet <- assertNumericVector rt
      (\sp -> "Vector scaling expects a numeric vector on the right-hand side, but found `" ++ sp ++ "`.")
    assertEquivNumeric lt vet
      (\ls rs -> "Vector scaling expects scalar and vector of scalars of the same type, but found `" ++ ls ++ "` over vector of `" ++ rs ++ "`s.")
    pure rt

  infer cxt (VVVBinaryOp o l r) = vvvInfer cxt o l r

  infer cxt (VVNBinaryOp Dot l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    let msg hand sp = "Vector dot product expects a numeric vector on the " ++ hand ++ "-hand side, but found `" ++ sp ++ "`."
    lvet <- assertNumericVector lt (msg "left")
    rvet <- assertNumericVector rt (msg "right")
    assertEquivNumeric lvet rvet
      (\ls rs -> "Vector dot product expects vectors of the same numeric type, but found `" ++ ls ++ "` and `" ++ rs ++ "`.")
    pure lvet

  infer cxt (ESSBinaryOp _ l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    set <- assertSet rt
      (\sp -> "Set add/subtract expects a set on the right-hand side, but found `" ++ sp ++ "`.")
    assertEquivNumeric lt set
      (\ls rs -> "Set add/subtract expects numeric set operands. Received `" ++ ls ++ "` / `" ++ rs ++ "`.")
    pure rt

  infer cxt (ESBBinaryOp SContains l r) = do
    lt <- infer cxt l
    rt <- infer cxt r
    set <- assertSet rt
      (\sp -> "Set contains expects a set on the right-hand side, but found `" ++ sp ++ "`.")
    assertEquivNumeric lt set
      (\ls rs -> "Set contains should only be applied to Set of numeric type. Received `" ++ ls ++ "` / `" ++ rs ++ "`.")
    pure S.Boolean

  infer cxt (Operator _ (S.BoundedDD _ _ bot top) body) = do
    botTy <- infer cxt bot
    topTy <- infer cxt top
    bodyTy <- infer cxt body

    assertNumeric bodyTy
      (\sp -> "'Big' operator body is not numeric, found: " ++ sp ++ ".")

    let msg dir sp = "'Big' operator range " ++ dir ++ " is not an index-like type (Integer or Natural), found: " ++ sp ++ "."

    assertIndexLike botTy (msg "start")
    assertIndexLike topTy (msg "stop")

    assertEquivNumeric botTy topTy
      (\ls rs -> "'Big' operator range expects start and stop to be of the same numeric type, but found `" ++ ls ++ "` and `" ++ rs ++ "`.")

    -- FIXME: We have a `Symbol` in the `S.BoundedDD` but it's not used in type-checking.
    pure bodyTy

  infer cxt (RealI uid ri) = do
    uidT <- inferFromContext cxt uid
    riT <- riTy ri
    assertReal uidT $
      \sp -> "Real interval expects variable to be of type Real, but received `" ++ show uid ++ "` of type `" ++ sp ++ "`."
    assertReal riT $
      \sp -> "Real interval expects interval bounds to be of type Real, but received: " ++ sp ++ "."
    pure S.Boolean
    where
      riTy :: RealInterval Expr Expr -> Either TypeError Space
      riTy (S.Bounded (_, lx) (_, rx)) = do
        lt <- infer cxt lx
        rt <- infer cxt rx
        let msg dir sp = "Bounded real interval " ++ dir ++ " is not a real number, found: " ++ sp ++ "."
        assertReal lt (msg "lower bound")
        assertReal rt (msg "upper bound")
        pure S.Real
      riTy (S.UpTo (_, x)) = infer cxt x
      riTy (S.UpFrom (_, x)) = infer cxt x
