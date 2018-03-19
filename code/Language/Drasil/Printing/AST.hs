module Language.Drasil.Printing.AST where

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefType, RefAdd)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.People (People)
import Language.Drasil.Chunk.Citation (Month, EntryID, CitationKind)

data Oper = Add | Mul | And | Or

data UFunc = Neg

data BinOp = Frac | Div | Pow | Subt | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Index
  | Dot | Cross

data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma | Prime | Log
  | Sin | Cos | Tan | Sec | Csc | Cot | Not | Dim | Exp | Sqrt

data Fence = Paren | Curly | Norm | Abs
data OverSymb = Hat
data Fonts = Bold | Emph

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Assoc Oper [Expr]
          | BOp   BinOp Expr Expr
          | Case  [(Expr,Expr)]
          | UOp   UFunc Expr
          | Funct Functional Expr
          | Mtx [[Expr]]
          | Row   [Expr]
          | Ident String
          | Spec  Special
          | Gr    Greek
          | Sub   Expr
          | Sup   Expr
          | MO    Ops
          | Over  OverSymb Expr
          | Fenced Fence Fence Expr
          | Font  Fonts Expr
          
data Functional = 
            Summation (Maybe ((Symbol, Expr),Expr))
          | Integral ((Maybe Expr),(Maybe Expr)) Symbol
          | Product (Maybe ((Symbol, Expr), Expr))

-- These precedences are inspired from Haskell/F# 
-- as documented at http://kevincantu.org/code/operators.html
-- They are all multiplied by 10, to leave room to weave things in between

prec :: Oper -> Int
prec Mul = 190
prec Add = 180
prec And = 120
prec Or = 110

prec2 :: BinOp -> Int
prec2 Frac = 190
prec2 Div = 190
prec2 Pow = 150
prec2 Subt = 220
prec2 Eq = 130
prec2 NEq  = 130
prec2 Lt  = 130
prec2 Gt  = 130
prec2 LEq  = 130
prec2 GEq  = 130
prec2 Impl = 130
prec2 Iff = 130
prec2 Index = 250
prec2 Dot = 190
prec2 Cross = 190

infixr 5 :+:

data Spec = E Expr
          | S String
          | Spec :+: Spec -- concat
          | Sy USymb
          | N Symbol
          | G Greek
          | Sp Special
          | Ref RefType RefAdd Spec
          | EmptyS
          | HARDNL        -- newline. Temp fix for multi-line descriptions; 
                          -- May move to a new LayoutObj, but only exists in TeX
                          -- so it's not really a big deal ATM.
type Title    = Spec

data ListType = Ordered [ItemType] 
              | Unordered [ItemType]
              | Simple      [(Title,ItemType)]
              | Desc        [(Title,ItemType)]
              | Definitions  [(Title,ItemType)]

data ItemType = Flat Spec
              | Nested Spec ListType

type BibRef = [Citation]

data Citation = Cite EntryID CitationKind [CiteField]

-- | Fields used in citations.
data CiteField = Address      Spec
               | Author       People
               | BookTitle    Spec -- Used for 'InCollection' references only.
               | Chapter      Int
               | Edition      Int
               | Editor       People
               | HowPublished HP
               | Institution  Spec
               | Journal      Spec
               | Month        Month
               | Note         Spec
               | Number       Int
               | Organization Spec
               | Pages        [Int] -- Range of pages (ex1. 1-32; ex2. 7,31,52-55)
               | Publisher    Spec
               | School       Spec
               | Series       Spec
               | Title        Spec
               | Type         Spec -- BibTeX "type" field
               | Volume       Int
               | Year         Int

-- | How Published. Necessary for URLs to work properly.
data HP = URL Spec
        | Verb Spec
