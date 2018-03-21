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

binop :: BinOp -> P.BinOp
binop Frac = P.Div
binop Pow = error "Print.Import.binop: Pow"
binop Subt = P.Subt
binop Eq = error "Printing.Import.binop: Eq"
binop NEq = error "Printing.Import.binop: NEq"
binop Lt = error "Printing.Import.binop: Lt"
binop Gt = error "Printing.Import.binop: Gt"
binop LEq = error "Printing.Import.binop: LEq"
binop GEq = error "Printing.Import.binop: GEq"
binop Impl = error "Printing.Import.binop: Impl"
binop Iff = error "Printing.Import.binop: Iff"
binop Index = error "Printing.Import.bin: Index"
binop Dot = error "Printing.Import.binop: Dot"
binop Cross = error "Printing.Import.binop: Cross"

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
