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

import           Data.Either                  (lefts, fromLeft)
import qualified Data.Foldable as NE
import           Numeric.Natural              (Natural)
import           Data.Map                     (Map)
import qualified Data.Map as Map

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
data LABinOp = Index | IndexOf
  deriving Eq

-- | Ordered binary operators (less than, greater than, less than or equal to, greater than or equal to).
data OrdBinOp = Lt | Gt | LEq | GEq
  deriving Eq

-- | @Clif x Clif -> Clif@ binary operations (cross product, addition, subtraction).
data CCCBinOp = Cross | CAdd | CSub | WedgeProd | GeometricProd
  deriving Eq

-- | @Clif x Clif -> Number@ binary operations (dot product).
data CCNBinOp = Dot
  deriving Eq

-- | @Number x Clif -> Clif@ binary operations (scaling).
data NCCBinOp = Scale
  deriving Eq

-- | @Natural x Clif -> Clif@ binary operations (grade selection).
data NatCCBinOp = GradeSelect
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

-- | @Clif -> Clif@ operators.
data UFuncCC = NegC
  deriving Eq

-- | @Clif -> Number@ operators (norm, dim, grade).
data UFuncCN = Norm | Dim | Grade
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
  -- | Unary operation for @Clif -> Clif@ operations.
  UnaryOpCC     :: UFuncCC -> Expr -> Expr
  -- | Unary operation for @Clif -> Clif@ operations.
  UnaryOpCN     :: UFuncCN -> Expr -> Expr

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
  -- | Binary operator for @Clif x Clif -> Clif@ operations (cross product).
  CCCBinaryOp   :: CCCBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Clif x Clif -> Number@ operations (dot product).
  CCNBinaryOp   :: CCNBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Expr x Clif -> Clif@ operations (scaling).
  NCCBinaryOp   :: NCCBinOp -> Expr -> Expr -> Expr
  -- | Binary operator for @Natural x Clif -> Clif@ operations (grade selection).
  NatCCBinaryOp :: NatCCBinOp -> Natural -> Expr -> Expr
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
  -- | A clif of arbitrary dimension. The Maybe [Expr] determines the
  --   components of the clif projected in a basis. If this is `Nothing`,
  --   then the clif has not been projected into a particular basis. 
  --   If this `isJust`, the number of components must be 2 ^ d where
  --   d is the dimension of the clifford space.
  -- All Clifs are currently assumed to be embedded in a space defined by spacelike 
  -- basis vectors (e.g. Euclidean space) for now.
  Clif     :: S.Dimension -> BasisBlades -> Expr
  -- | Indexing into an expression (clifs only for now)
  -- The list of indexes correspond to the index in each grade
  -- SubSup determines if it is a superscript or a subscript
  -- The Expression must be a clif with the right grade and where the indexes are ≤ the dimension
--   IndexC   :: [Index] -> SubSup -> Expr -> Expr

-- -- | An index will use the same definition as dimension for now, renamed for clarity
-- type Index = S.Dimension

-- -- | Whether an index is a superscript or a subscript
-- data SubSup = 
--   Super | Sub
--   deriving (Eq)

-- | Basis Keys are represented by binary numbers (per Roefls, 2025)
--   Basis elements are ordered by grade, then sorted lexiographically
--   Y means basis vector is included
--   N means basis vector is not included
--   C is for concatenation
--   E is for empty
--   Example: in dimension 3, the basis vectors are e0, e1, and e2
--    Here are some examples for `BasisKey`:
--     - e0     : N (N (Y E))
--     - e2     : Y (N (N E))
--     - e1e2   : Y (Y (N E))
--     - 1      : N (N (N E))
--     - e0e1e2 : Y (Y (Y E))
data BasisKey =
    Y BasisKey
  | N BasisKey
  | E
  deriving (Eq, Show)

-- | A mapping from basis blades to their expressions
type BasisBlades =
  Map BasisKey Expr

-- | A scalar key. E.g., for d=2: `scalarKey 2 = N (N E)`
scalarKey :: Natural -> BasisKey
scalarKey d = elemKey [] d

-- | A vector key. E.g., for d=2, basis element e1: `vectorKey 1 2 = Y (N E)`
vectorKey :: Natural -> Natural -> BasisKey
vectorKey n d = elemKey [n] d

-- | A bivector key. E.g., for d=3, basis element e0e1: `bivectorKey 0 1 3 = N (Y (Y E))`
bivectorKey :: Natural -> Natural -> Natural -> BasisKey
bivectorKey m n d = elemKey [m,n] d

-- | Create a general element key. E.g. for d=4, e0e2e3: `elemKey [0,2,3] 4 = (Y (Y (N (Y E))))`
--   This function does not care about the order of the list.
elemKey :: Foldable t => t Natural -> Natural -> BasisKey
elemKey ns 0 = E
elemKey ns d 
  | d - 1 `elem` ns = Y (elemKey ns $ d-1)
  | otherwise   = N (elemKey ns $ d-1)


-- | The basis in which to project clifs
-- TODO: Generalize this to other cliff spaces
data Basis where
  -- | ℝⁿ
  Rn :: Natural -> Basis

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
  UnaryOpCC a b       == UnaryOpCC c d       =   a == c && b == d
  UnaryOpCN a b       == UnaryOpCN c d       =   a == c && b == d
  ArithBinaryOp o a b == ArithBinaryOp p c d =   o == p && a == c && b == d
  BoolBinaryOp o a b  == BoolBinaryOp p c d  =   o == p && a == c && b == d
  EqBinaryOp o a b    == EqBinaryOp p c d    =   o == p && a == c && b == d
  OrdBinaryOp o a b   == OrdBinaryOp p c d   =   o == p && a == c && b == d
  LABinaryOp o a b    == LABinaryOp p c d    =   o == p && a == c && b == d
  CCCBinaryOp o a b   == CCCBinaryOp p c d   =   o == p && a == c && b == d
  CCNBinaryOp o a b   == CCNBinaryOp p c d   =   o == p && a == c && b == d
  NCCBinaryOp o a b   == NCCBinaryOp p c d   =   o == p && a == c && b == d
  ESSBinaryOp o a b   == ESSBinaryOp p c d   =   o == p && a == c && b == d
  ESBBinaryOp o a b   == ESBBinaryOp p c d   =   o == p && a == c && b == d
  Clif a b            == Clif c d            =             a == c && b == d
  -- IndexC a b c        == IndexC d e f        =   a == d && b == e && c == f
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

instance Pretty CCCBinOp where
  pretty Cross = "cross product"
  pretty CAdd  = "clif addition"
  pretty CSub  = "clif subtraction"

instance LiteralC Expr where
  int = Lit . int
  str = Lit . str
  dbl = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r


-- helper function for typechecking to help reduce duplication
-- TODO: refactor these if expressions so they're more readable (the else is too far from the if)
cccInfer :: TypingContext Space -> CCCBinOp -> Expr -> Expr -> Either Space TypeError
cccInfer ctx op l r = case (infer ctx l, infer ctx r) of
    (Left lt@(S.ClifS lD lsp), Left (S.ClifS rD rsp)) ->
      if lD == rD then -- The dimension in which the clif is embedded must match
        if lsp == rsp && S.isBasicNumSpace lsp then
          if op == CSub && (lsp == S.Natural || rsp == S.Natural) then
            Right $ "Clif subtraction expects both operands to be clifs of non-natural numbers. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
          else Left lt
        else Right $ "Clif " ++ pretty op ++ " expects both operands to be clifs of non-natural numbers. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
      else Right $ "Clif " ++ pretty op ++ " expects both Clifs to be of the same dimension. Received `" ++ show lD ++ "` and `" ++ show rD ++ "`."
    (Left lsp, Left rsp) -> Right $ "Vector operation " ++ pretty op ++ " expects clif operands. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
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

  infer cxt (Variable _ n) = infer cxt n

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

  infer _ (Set s _) = Left s

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

  infer cxt (UnaryOpCC NegC e) = case infer cxt e of
    Left c@(S.ClifS dim sp) -> if S.isBasicNumSpace sp && sp /= S.Natural
      then Left c
      else Right $ "Clif negation only applies to, non-natural, numbered clifs. Received `" ++ show sp ++ "`."
    Left sp -> Right $ "Clif negation should only be applied to numeric clifs. Received `" ++ show sp ++ "`."
    x -> x

  -- TODO: support generalized clif norm
  infer cxt (UnaryOpCN Norm e) = case infer cxt e of
    Left (S.ClifS dim S.Real) -> Left S.Real
    Left sp -> Right $ "Vector norm only applies to vectors (or clifs) of real numbers. Received `" ++ show sp ++ "`."
    x -> x

  infer cxt (UnaryOpCN Dim e) = case infer cxt e of
    Left (S.ClifS _ _) -> Left S.Integer -- FIXME: I feel like Integer would be more usable, but S.Natural is the 'real' expectation here
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
    (Left (S.ClifS d lt), Left nt) -> if nt == S.Integer || nt == S.Natural -- I guess we should only want it to be natural numbers, but integers or naturals is fine for now
      then Left lt
      else Right $ "List accessor not of type Integer nor Natural, but of type `" ++ show nt ++ "`"
    (Left lt         , Left _)  -> Right $ "List accessor expects a list/vector, but received `" ++ show lt ++ "`."
    (_               , Right e) -> Right e
    (Right e         , _      ) -> Right e
  infer cxt (LABinaryOp IndexOf l n) = case (infer cxt l, infer cxt n) of
    (Left (S.Set lt), Left nt) -> if S.isBasicNumSpace lt && nt == lt-- I guess we should only want it to be natural numbers, but integers or naturals is fine for now
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

  infer cxt (NCCBinaryOp Scale l r) = case (infer cxt l, infer cxt r) of
    (Left lt, Left (S.ClifS d rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Left rsp
      else if lt /= rsp then
        Right $ "Vector scaling expects a scaling by the same kind as the vector's but found scaling by`" ++ show lt ++ "` over vectors of type `" ++ show rsp ++ "`."
      else
        Right $ "Vector scaling expects a numeric scaling, but found `" ++ show lt ++ "`."
    (Left _, Left rsp) -> Right $ "Vector scaling expects vector as second operand. Received `" ++ show rsp ++ "`."
    (_, Right rx) -> Right rx
    (Right lx, _) -> Right lx

  -- If you select grade N of a Clif, you get a Clif of grade N
  infer cxt (NatCCBinaryOp GradeSelect n c) = case infer cxt c of
    Left (S.ClifS d sp) -> 
      Left $ S.ClifS (S.Fixed n) sp
    Left rsp -> Right $ "Grade selection expects clif as second operand. Received `" ++ show rsp ++ "`."
    Right x -> Right x

  infer cxt (CCCBinaryOp o l r) = cccInfer cxt o l r
    {- case (infer cxt l, infer cxt r) of
    (Left lTy, Left rTy) -> if lTy == rTy && S.isBasicNumSpace lTy && lTy /= S.Natural
      then Left lTy
      else Right $ "Vector cross product expects both operands to be vectors of non-natural numbers. Received `" ++ show lTy ++ "` X `" ++ show rTy ++ "`."
    (_       , Right re) -> Right re
    (Right le, _       ) -> Right le
    -}

  infer cxt (CCNBinaryOp Dot l r) = case (infer cxt l, infer cxt r) of
    (Left lt@(S.ClifS lD lsp), Left rt@(S.ClifS rD rsp)) -> 
      if lD == rD then
        if lsp == rsp && S.isBasicNumSpace lsp
        then Left lsp
        else Right $ "Vector dot product expects same numeric vector types, but found `" ++ show lt ++ "` · `" ++ show rt ++ "`."
      else Right $ "Clif dot product expects both Clifs to be of the same dimension. Received `" ++ show lD ++ "` and `" ++ show rD ++ "`."
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
  
  -- For a clif to be well-typed it must:
  -- 1. Contain only basic numeric types inside it
  -- 2. Have a dimension of at least the grade (a 0-dimensional vector makes no sense)
  infer ctx (Clif d es) = 
      -- A clif with no explicit compile/"specification"-time expressions in the components
    if es == Map.empty then Left $ S.ClifS d S.Real
    else
      case eitherLists (fmap (infer ctx) $ Map.elems es) of
        Left ts -> 
          let
            allReal = all (== S.Real) ts
            -- Check the dimensions of a clif to ensure it makes sense
            isValidDim =
              case d of
                -- If it's a fixed dimension, the number of expressions must be dimension ^ 2
                S.Fixed fD -> length es == fromIntegral (2 ^ fD)
                -- We don't know enough to say for sure
                S.VDim _  -> True
          in
          -- `Clif`s must store a basic number space, not things like other clifs
          -- if S.isBasicNumSpace t then
            if isValidDim then
              Left $ S.ClifS d S.Real
            else Right $ "The number of components in a clif of dimension " ++ show d ++ " must be 2 ^ " ++ show d
          -- else Right $ "Clifs must contain basic number spaces. Received " ++ show t
        Left t -> Right $ "Clifs currently only support Real-numbered components. Received " ++ show t
        Right x -> Right x

-- verify :: Bool -> Space -> TypeError -> Either TypeError Space
-- verify b s t =
--   if b then
--     Right s
--   else
--     Left x

-- inferAndVerify :: 

eitherLists :: [Either a b] -> Either [a] b
eitherLists es =
  eitherLists' (Left []) es
  where
    eitherLists' (Left ls) (Left l : es) =
      eitherLists' (Left $ l : ls) es
    eitherLists' _ (Right r : _) = Right r
    eitherLists' ls [] = ls