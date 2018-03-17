module Language.Drasil.Printing.AST where

import Language.Drasil.Symbol (Symbol, Decoration)
import Language.Drasil.Spec (USymb, RefType, RefAdd)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.People (People)
import Language.Drasil.Chunk.Citation (Month, EntryID, CitationKind)

data Oper = Add | Mul | And | Or

data UFunc = Norm | Abs | Log | Sin | Cos | Tan | Sec | Csc | Cot | Exp
  | Sqrt | Not | Neg | Dim

data BinOp = Frac | Div | Pow | Subt | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Index
  | Dot | Cross

data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma

data Fence = Paren | Curly

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Assoc Oper [Expr]
          | BOp   BinOp Expr Expr
          | Sym   Symbol
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
          | Over  Decoration Expr
          | MO    Ops
          | Fenced Fence Fence Expr
          
data Functional = 
            Summation (Maybe ((Symbol, Expr),Expr))
          | Integral ((Maybe Expr),(Maybe Expr)) Symbol
          | Product (Maybe ((Symbol, Expr), Expr))

prec :: Oper -> Int
prec Mul = 3
prec Add = 4
prec And = 11
prec Or = 12

prec2 :: BinOp -> Int
prec2 Frac = 3
prec2 Div = 3
prec2 Pow = 2
prec2 Subt = 4
prec2 Eq = 9
prec2 NEq  = 9
prec2 Lt  = 9
prec2 Gt  = 9
prec2 LEq  = 9
prec2 GEq  = 9
prec2 Impl = 13
prec2 Iff = 13
prec2 Index = 1
prec2 Dot = 3
prec2 Cross = 3

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
