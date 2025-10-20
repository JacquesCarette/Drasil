{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | The Drasil Expression language
module Language.Drasil.Expr.Lang where

import Drasil.Database.UID (UID)

import Language.Drasil.Literal.Class (LiteralC (..))
import Language.Drasil.Literal.Lang (Literal (..))
import Language.Drasil.Space (DiscreteDomainDesc, RealInterval, Space)
import qualified Language.Drasil.Space as S
import Language.Drasil.WellTyped

import Data.Either (fromRight, rights)
import qualified Data.Foldable as NE
import Data.List (nub)

import Numeric.Natural (Natural)
import Data.Map.Ordered (OrderedMap)

import qualified Data.Map.Ordered as OM

-- * Helper Functions for Type Checking

-- | Helper function to check that all expressions have the same expected type
allOfType :: TypingContext Space -> [Expr] -> Space -> Space -> String -> Either TypeError Space
allOfType cxt exs expected returnType errorMsg
  | null exs = Right returnType
  | otherwise = 
    let types = map (infer cxt) exs
        successes = rights types
    in if all (== expected) successes && length successes == length types
        then Right returnType
        else Left $ errorMsg ++ " Expected " ++ show expected ++ " but found: " ++ show types

-- | Helper function to assert that a space is a function and extract its parameters and return type
assertFunction :: Space -> (String -> String) -> Either TypeError ([Space], Space)
assertFunction (S.Function params ret) _ = Right (NE.toList params, ret)
assertFunction space mkError = Left $ mkError (show space)

-- | Helper function to assert that a space is Real
assertReal :: Space -> (String -> String) -> Either TypeError ()
assertReal S.Real _ = Right ()
assertReal space mkError = Left $ mkError (show space)

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
  Clif     :: S.Dimension -> BasisBlades Expr -> Expr
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

-- TODO: Move this -- where?
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
  deriving (Eq, Ord, Show)

-- | A mapping from basis blades to their expressions
type BasisBlades e = OrderedMap BasisKey e

-- | A scalar key. E.g., for d=2: `scalarKey 2 = N (N E)`
scalarKey :: Natural -> BasisKey
scalarKey = elemKey []

-- | A vector key. E.g., for d=2, basis element e1: `vectorKey 1 2 = Y (N E)`
vectorKey :: Natural -> Natural -> BasisKey
vectorKey n = elemKey [n]

-- | A bivector key. E.g., for d=3, basis element e0e1: `bivectorKey 0 1 3 = N (Y (Y E))`
bivectorKey :: Natural -> Natural -> Natural -> BasisKey
bivectorKey m n = elemKey [m,n]

-- | A bivector key. E.g., for d=3, basis element e0e1: `bivectorKey 0 1 2 3 = Y (Y (Y E))`
trivectorKey :: Natural -> Natural -> Natural -> Natural -> BasisKey
trivectorKey m n p = elemKey [m,n,p]

-- | Create a general element key. E.g. for d=4, basis element e0e2e3: `elemKey [0,2,3] 4 = (Y (Y (N (Y E))))`
--   This function does not care about the order or cardinlaity of the objects in the Foldable 
--   value. That is, it is treated as "set-like". Consider using Data.Set if you're interested 
--   in enforcing these properties yourself.
--   If you give it numbers that are "out of scope", i.e. n >= d, or duplicates, they will be ignored
elemKey :: Foldable t => t Natural -> Natural -> BasisKey
elemKey _  0 = E
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
  pretty Cross         = "cross product"
  pretty CAdd          = "clif addition"
  pretty CSub          = "clif subtraction"
  pretty WedgeProd     = "clif wedge"
  pretty GeometricProd = "clif geometric product"

instance LiteralC Expr where
  int = Lit . int
  str = Lit . str
  dbl = Lit . dbl
  exactDbl = Lit . exactDbl
  perc l r = Lit $ perc l r


-- helper function for typechecking to help reduce duplication
-- TODO: refactor these if expressions so they're more readable (the else is too far from the if)
-- helper function for typechecking to help reduce duplication
cccInfer :: TypingContext Space -> CCCBinOp -> Expr -> Expr -> Either TypeError Space
cccInfer ctx op l r = case (infer ctx l, infer ctx r) of
    (Right lt@(S.ClifS lD lKind lsp), Right (S.ClifS rD rKind rsp)) ->
      if lD == rD && lKind == rKind then -- The dimension and kind must match
        if lsp == rsp && S.isBasicNumSpace lsp then
          if op == CSub && (lsp == S.Natural || rsp == S.Natural) then
            Left $ "Clif subtraction expects both operands to be clifs of non-natural numbers. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
          else Right lt
        else Left $ "Clif " ++ pretty op ++ " expects both operands to be clifs of non-natural numbers. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
      else Left $ "Clif " ++ pretty op ++ " expects both Clifs to be of the same dimension and kind. Received `" ++ show lD ++ ", " ++ show lKind ++ "` and `" ++ show rD ++ ", " ++ show rKind ++ "`."
    (Right lsp, Right rsp) -> Left $ "Vector operation " ++ pretty op ++ " expects clif operands. Received `" ++ show lsp ++ "` and `" ++ show rsp ++ "`."
    (Left re, _       ) -> Left re
    (_       , Left re) -> Left re

instance Typed Expr Space where
  check :: TypingContext Space -> Expr -> Space -> Either TypeError Space
  check = typeCheckByInfer

  infer :: TypingContext Space -> Expr -> Either TypeError Space
  infer cxt (Lit lit) = infer cxt lit

  infer cxt (AssocA _ (e:exs)) =
    case infer cxt e of
      Right spaceValue | S.isBasicNumSpace spaceValue ->
          -- If the inferred type of e is a valid Space, call allOfType with spaceValue
          allOfType cxt exs spaceValue spaceValue
              "Associative arithmetic operation expects all operands to be of the same expected type."
      Right l ->
          -- Handle the case when sp is a Right value but spaceValue is invalid
          Left ("Expected all operands in addition/multiplication to be numeric, but found " ++ show l)
      Left r ->
          -- Handle the case when sp is a Left value containing a TypeError
          Left r

  infer _ (AssocA Add _) = Left "Associative addition requires at least one operand."
  infer _ (AssocA Mul _) = Left "Associative multiplication requires at least one operand."

  infer cxt (AssocB _ exs) = allOfType cxt exs S.Boolean S.Boolean
    "Associative boolean operation expects all operands to be of the same type (Boolean)."

  infer _ (AssocC SUnion []) = Left "Associative addition requires at least one operand."

  infer cxt (AssocC SUnion (e:exs)) =
    case infer cxt e of
      Right spaceValue | spaceValue /= S.Void ->
          -- If the inferred type of e is a valid Space, call allOfType with spaceValue
          allOfType cxt exs spaceValue spaceValue
              "Associative arithmetic operation expects all operands to be of the same expected type."
      Right l ->
          -- Handle the case when sp is a Right value but spaceValue is invalid
          Left ("Expected all operands in addition/multiplication to be numeric, but found " ++ show l)
      Left r ->
          -- If sp is a Left value containing a TypeError
          Left r

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
              (const False) 
              (\_ -> all (\ r -> length r == columns && all (== expT) r) sss) expT
        t = fromRight (error "Infer on Matrix had a strong expectation of Right-valued data.") expT -- This error should never occur.

  infer _ (Set s _) = Right s

  infer cxt (UnaryOp uf ex) = case infer cxt ex of
    Right sp -> case uf of
      Abs -> if S.isBasicNumSpace sp && sp /= S.Natural
        then Right sp
        else Left $ "Numeric 'absolute' value operator only applies to, non-natural, numeric types. Received `" ++ show sp ++ "`."
      Neg -> if S.isBasicNumSpace sp && sp /= S.Natural
        then Right sp
        else Left $ "Negation only applies to, non-natural, numeric types. Received `" ++ show sp ++ "`."
      Exp -> if sp == S.Real || sp == S.Integer then Right S.Real else Left $ show Exp ++ " only applies to reals."
      x -> if sp == S.Real
        then Right S.Real
        else Left $ show x ++ " only applies to Reals. Received `" ++ show sp ++ "`."
    x       -> x

  infer cxt (UnaryOpB Not ex) = case infer cxt ex of
    Right S.Boolean -> Right S.Boolean
    Right sp        -> Left $ "¬ on non-boolean operand, " ++ show sp ++ "."
    x              -> x

  infer cxt (UnaryOpCC NegC e) = case infer cxt e of
    Right c@(S.ClifS _ _ sp) -> if S.isBasicNumSpace sp && sp /= S.Natural
      then Right c
      else Left $ "Clif negation only applies to, non-natural, numbered clifs. Received `" ++ show sp ++ "`."
    Right sp -> Left $ "Clif negation should only be applied to numeric clifs. Received `" ++ show sp ++ "`."
    x -> x

  -- TODO: support generalized clif norm
  infer cxt (UnaryOpCN Norm e) = case infer cxt e of
    Right (S.ClifS _ _ S.Real) -> Right S.Real
    Right sp -> Left $ "Vector norm only applies to vectors (or clifs) of real numbers. Received `" ++ show sp ++ "`."
    x -> x

  infer cxt (UnaryOpCN Dim e) = case infer cxt e of
    Right (S.ClifS {}) -> Right S.Integer -- FIXME: I feel like Integer would be more usable, but S.Natural is the 'real' expectation here
    Right sp -> Left $ "Vector 'dim' only applies to vectors. Received `" ++ show sp ++ "`."
    x -> x
  
  infer cxt (UnaryOpCN Grade e) = case infer cxt e of
    Right (S.ClifS {}) -> Right S.Integer -- FIXME: I feel like Integer would be more usable, but S.Natural is the 'real' expectation here
    Right sp -> Left $ "Vector 'grade' only applies to vectors. Received `" ++ show sp ++ "`."
    x -> x

  infer cxt (ArithBinaryOp Frac l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt) -> if S.isBasicNumSpace lt && lt == rt
      then Right lt
      else Left $ "Fractions/divisions should only be applied to the same numeric typed operands. Received `" ++ show lt ++ "` / `" ++ show rt ++ "`."
    (_, Left e) -> Left e
    (Left e, _      ) -> Left e

  infer cxt (ArithBinaryOp Pow l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt) -> if S.isBasicNumSpace lt && (lt == rt || (lt == S.Real && rt == S.Integer))
      then Right lt
      else Left $
        "Powers should only be applied to the same numeric type in both operands, or real base with integer exponent. Received `" ++ show lt ++ "` ^ `" ++ show rt ++ "`."
    (_, Left x) -> Left x
    (Left x, _      ) -> Left x

  infer cxt (ArithBinaryOp Subt l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt) -> if S.isBasicNumSpace lt && lt == rt
      then Right lt
      else Left $ "Both operands of a subtraction must be the same numeric type. Received `" ++ show lt ++ "` - `" ++ show rt ++ "`."
    (_, Left re) -> Left re
    (Left le, _) -> Left le

  infer cxt (BoolBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Right S.Boolean, Right S.Boolean) -> Right S.Boolean
    (Right lt, Right rt) -> Left $ "Boolean expression contains non-boolean operand. Received `" ++ show lt ++ "` & `" ++ show rt ++ "`."
    (_, Left er) -> Left er
    (Left el, _     ) -> Left el

  infer cxt (EqBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt) -> if lt == rt
      then Right S.Boolean
      else Left $ "Both operands of an (in)equality (=/≠) must be of the same type. Received `" ++ show lt ++ "` & `" ++ show rt ++ "`."
    (_, Left re) -> Left re
    (Left le, _) -> Left le

  infer cxt (LABinaryOp Index l n) = case (infer cxt l, infer cxt n) of
    (Right (S.ClifS _ _ lt), Right nt) -> if nt == S.Integer || nt == S.Natural -- I guess we should only want it to be natural numbers, but integers or naturals is fine for now
      then Right lt
      else Left $ "List accessor not of type Integer nor Natural, but of type `" ++ show nt ++ "`"
    (Right lt         , Right _)  -> Left $ "List accessor expects a list/vector, but received `" ++ show lt ++ "`."
    (_, Left e) -> Left e
    (Left e         , _      ) -> Left e
  infer cxt (LABinaryOp IndexOf l n) = case (infer cxt l, infer cxt n) of
    (Right (S.Set lt), Right nt) -> if S.isBasicNumSpace lt && nt == lt-- I guess we should only want it to be natural numbers, but integers or naturals is fine for now
      then Right lt
      else Left $ "List accessor not of type Integer nor Natural, but of type `" ++ show nt ++ "`"
    (Right lt         , Right _)  -> Left $ "List accessor expects a list/vector, but received `" ++ show lt ++ "`."
    (_, Left e) -> Left e
    (Left e         , _      ) -> Left e
  infer cxt (OrdBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt) -> if S.isBasicNumSpace lt && lt == rt
      then Right S.Boolean
      else Left $ "Both operands of a numeric comparison must be the same numeric type, got: " ++ show lt ++ ", " ++ show rt ++ "."
    (_, Left re) -> Left re
    (Left le, _) -> Left le

  infer cxt (NCCBinaryOp Scale l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right (S.ClifS _ _ rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Right rsp
      else if lt /= rsp then
        Left $ "Vector scaling expects a scaling by the same kind as the vector's but found scaling by`" ++ show lt ++ "` over vectors of type `" ++ show rsp ++ "`."
      else
        Left $ "Vector scaling expects a numeric scaling, but found `" ++ show lt ++ "`."
    (Right _, Right rsp) -> Left $ "Vector scaling expects vector as second operand. Received `" ++ show rsp ++ "`."
    (_, Left rx) -> Left rx
    (Left lx, _) -> Left lx

  -- If you select grade N of a Clif, you get a Clif of grade N
  infer cxt (NatCCBinaryOp GradeSelect n c) = case infer cxt c of
    Right (S.ClifS _ k sp) -> Right $ S.ClifS (S.Fixed n) k sp
    Right rsp -> Left $ "Grade selection expects clif as second operand. Received `" ++ show rsp ++ "`."
    Left x -> Left x
  
  infer cxt (CCCBinaryOp o l r) = cccInfer cxt o l r

  infer cxt (CCNBinaryOp Dot l r) = case (infer cxt l, infer cxt r) of
    (Right lt@(S.ClifS lD lKind lsp), Right rt@(S.ClifS rD rKind rsp)) ->
      if lD == rD && lKind == rKind then
        if lsp == rsp && S.isBasicNumSpace lsp
        then Right lsp
        else Left $ "Vector dot product expects same numeric vector types, but found `" ++ show lt ++ "` · `" ++ show rt ++ "`."
      else Left $ "Clif dot product expects both Clifs to be of the same dimension and kind. Received `" ++ show lD ++ ", " ++ show lKind ++ "` and `" ++ show rD ++ ", " ++ show rKind ++ "`."
    (Right lsp, Right rsp) -> Left $ "Vector dot product expects vector operands. Received `" ++ show lsp ++ "` · `" ++ show rsp ++ "`."
    (_, Left rx) -> Left rx
    (Left lx, _) -> Left lx

  infer cxt (ESSBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt@(S.Set rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Right lt
      else Left $ "Set Add/Sub should only be applied to Set of same space. Received `" ++ show lt ++ "` / `" ++ show rt ++ "`."
    (_, Left e) -> Left e
    (Left e, _      ) -> Left e
    (Right lt, Right rsp) -> Left $ "Set union expects set operands. Received `" ++ show lt ++ "` · `" ++ show rsp ++ "`."

  infer cxt (ESBBinaryOp _ l r) = case (infer cxt l, infer cxt r) of
    (Right lt, Right rt@(S.Set rsp)) -> if S.isBasicNumSpace lt && lt == rsp
      then Right lt
      else Left $ "Set contains should only be applied to Set of same space. Received `" ++ show lt ++ "` / `" ++ show rt ++ "`."
    (_, Left e) -> Left e
    (Left e, _      ) -> Left e
    (Right lt, Right rsp) -> Left $ "Set union expects set operands. Received `" ++ show lt ++ "` · `" ++ show rsp ++ "`."

  infer cxt (Operator _ (S.BoundedDD _ _ bot top) body) =
    let expTy = S.Integer in
    case (infer cxt bot, infer cxt top, infer cxt body) of
      (Right botTy, Right topTy, Right bodyTy) -> if botTy == S.Integer
        then if topTy == S.Integer
          then if expTy == bodyTy
            then Right expTy
            else Left $ "'Big' operator range body not Integer, found: " ++ show bodyTy ++ "."
          else Left $ "'Big' operator range top not Integer, found: " ++ show topTy ++ "."
        else Left $ "'Big' operator range bottom not of expected type: " ++ show expTy ++ ", found: " ++ show botTy ++ "."
      (_         , _         , Left x    ) -> Left x
      (_         , Left x   , _          ) -> Left x
      (Left x   , _         , _          ) -> Left x

  infer cxt (RealI uid ri) =
    case (inferFromContext cxt uid, riTy ri) of
      (Right S.Real, Right riSp) -> if riSp == S.Real
        then Right S.Boolean
        else Left $
          "Real interval expects interval bounds to be of type Real, but received: " ++ show riSp ++ "."
      (Right uidSp, _         ) -> Left $
        "Real interval expects variable to be of type Real, but received `" ++ show uid ++ "` of type `" ++ show uidSp ++ "`."
      (_          , Left x  ) -> Left x
      (Left x    , _        ) -> Left x

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

  -- For a clif to be well-typed it must:
  -- 1. Contain only basic numeric types inside it
  -- 2. Have a dimension of at least the grade (a 0-dimensional vector makes no sense)
  infer ctx (Clif d es) =
    -- A clif with no explicit compile/"specification"-time expressions in the components
    if OM.null es then Right $ S.ClifS d S.Vector S.Real
    else
      case eitherLists (infer ctx <$> OM.elems es) of
        Left _ ->
          let
            -- Check the dimensions of a clif to ensure it makes sense
            isValidDim =
              case d of
                -- If it's a fixed dimension, the number of expressions must be dimension ^ 2
                S.Fixed fD -> OM.size es == fromIntegral ((2 :: Integer) ^ fD)
                -- We don't know enough to say for sure
                S.VDim _  -> True
          in
          -- `Clif`s must store a basic number space, not things like other clifs
          if isValidDim then
            Right $ S.ClifS d S.Vector S.Real
          else Left $ "The number of components in a clif of dimension " ++ show d ++ " must be 2 ^ " ++ show d
        Right x -> Right x


eitherLists :: [Either a b] -> Either [a] b
eitherLists = eitherLists' (Left [])
  where
    eitherLists' :: Either [a] b -> [Either a b] -> Either [a] b
    eitherLists' (Left ls) (Left l : es') = eitherLists' (Left $ l : ls) es'
    eitherLists' _ (Right r : _) = Right r
    eitherLists' _ (Left _ : _) = error "eitherLists impl. non-exhaustive pattern: _ [Left, ...]"
    eitherLists' ls [] = ls