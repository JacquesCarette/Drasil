module Drasil.Document.Layout.Expr (
  -- * Types
  Expr(..), Ops(..)
) where

import Drasil.Document.Layout.Typography

-- | Different operators.
data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma | Prime | Log
  | Ln | Sin | Cos | Tan | Sec | Csc | Cot | Arcsin | Arccos | Arctan | Not
  | Dim | Exp | Neg | Cross | Dot | Scale | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff
  | Subt | And | Or | Add | Mul | Summ | Inte | Prod | Point | Perc | LArrow | RArrow | ForAll
  | VAdd | VSub | Partial | SAdd | SRemove | SUnion | SContains
  deriving Eq

-- | Redefine the 'Expr' type from Language.Drasil to be more suitable to printing.
data Expr
  = Dbl    Double
  | Int    Integer
  | Str    String
  | Case   [(Expr, Expr)] -- ^ Case expressions
  | Mtx    [[Expr]] -- ^ Matrix.
  | Set    [Expr]
  | Row    [Expr]
  | Ident  String
  | Label  String
  | Spec   Special -- ^ Special characters.
  | Sub    Expr -- ^ Subscript.
  | Sup    Expr -- ^ Superscript.
  | MO     Ops
  | Over   OverSymb Expr -- ^ Holds an expression that needs a hat symbol "^"
  | Fenced Fence Fence Expr -- ^ Holds an expression that is surrounded with a 'Fence'.
  | Font   Fonts Expr -- ^ Holds an expression with a font.
  | Div    Expr Expr -- ^ Fractions are a layout thing.
  | Sqrt   Expr      -- ^ Roots are also a layout thing. Just sqrt for now.
  | Spc    Spacing -- ^ Holds the 'Spacing'.
