module Language.Drasil.Printing.Import where

import Language.Drasil.Expr
import Language.Drasil.Space
import qualified Language.Drasil.Printing.AST as P

import Data.List (intersperse)

-- | translating operations
oper :: Oper -> P.Oper
oper And = P.And
oper Or = P.Or
oper Add = P.Add
oper Mul = P.Mul

ufunc :: UFunc -> P.UFunc
ufunc Norm = error "ufunc@Norm should no longer be used"
ufunc Abs = error "ufunc@Abs should no longer be used"
ufunc Log = error "ufunc@Log should no longer be used"
ufunc Sin = error "ufunc@Sin should no longer be used"
ufunc Cos = error "ufunc@Cos should no longer be used"
ufunc Tan = error "ufunc@Tan should no longer be used"
ufunc Sec = error "ufunc@Sec should no longer be used"
ufunc Csc = error "ufunc@Csc should no longer be used"
ufunc Cot = error "ufunc@Cot should no longer be used"
ufunc Exp = error "ufunc@Exp should no longer be used"
ufunc Sqrt = P.Sqrt
ufunc Not = error "ufunc@Not should no longer be used"
ufunc Neg = P.Neg
ufunc Dim = error "ufunc@Dim should no longer be used"

binop :: BinOp -> P.BinOp
binop Frac = P.Frac
binop Div = P.Div
binop Pow = P.Pow
binop Subt = P.Subt
binop Eq = P.Eq
binop NEq = P.NEq
binop Lt = P.Lt
binop Gt = P.Gt
binop LEq = P.LEq
binop GEq = P.GEq
binop Impl = P.Impl
binop Iff = P.Iff
binop Index = P.Index
binop Dot = P.Dot
binop Cross = P.Cross

space :: Space -> P.Expr
space Integer = P.MO P.Integer
space Rational = P.MO P.Rational
space Real = P.MO P.Real
space Natural = P.MO P.Natural
space Boolean = P.MO P.Boolean
space Char = P.Ident "Char"
space String = P.Ident "String"
space Radians = error "Radians not translated"
space (Vect _) = error "Vector space not translated"
space (DiscreteI _) = error "DiscreteI" --ex. let A = {1, 2, 4, 7}
space (DiscreteD _) = error "DiscreteD" -- [Double]
space (DiscreteS l) = P.Fenced P.Curly P.Curly $ P.Row $ intersperse (P.MO P.Comma) $ map P.Ident l --ex. let Meal = {"breakfast", "lunch", "dinner"}

{-
p_space :: Space -> String
p_space Radians  = "rad"
p_space (Vect a) = "V" ++ p_space a
p_space (DiscreteI a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteD a)  = "{" ++ (concat $ intersperse ", " (map show a)) ++ "}"
p_space (DiscreteS a)  = "{" ++ (concat $ intersperse ", " a) ++ "}"
-}
