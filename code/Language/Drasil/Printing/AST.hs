module Language.Drasil.Printing.AST where

import Language.Drasil.Expr (Oper(..),UFunc,BinOp(..))
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Space (Space)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.People (People)
import Language.Drasil.Citations (Month)

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Assoc Oper [Expr]
          | BOp   BinOp Expr Expr
          | Sym   Symbol
          | Call  Expr [Expr]
          | Case  [(Expr,Expr)]
          | UOp   UFunc Expr
          | Grouping Expr
          | Funct Functional Expr
          | IsIn  Expr Space
          | Mtx [[Expr]]
          
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
          | Spec :^: Spec -- superscript
          | Spec :-: Spec -- subscript
          | Spec :/: Spec -- frac
          | Sy USymb
          | N Symbol
          | G Greek
          | Sp Special
          | Ref RefType Spec
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

data Citation = Book [CiteField] | Article [CiteField]
              | MThesis [CiteField] | PhDThesis [CiteField]
              | Misc [CiteField] | Online [CiteField]

type City   = Spec
type State  = Spec

data CiteField = Author     People
               | Title      Spec
               | Series     Spec
               | Collection Spec
               | Volume     Integer
               | Edition    Integer
               | Place    (City, State) --State can also mean country
               | Publisher  Spec
               | Journal    Spec
               | Year       Integer
               | Date Integer Month Integer
               | Page       Integer
               | Pages    (Integer, Integer)
               | Note       Spec
               | Issue      Integer
               | School     Spec
               | URL        Spec
               | HowPub     Spec
               | URLdate Integer Month Integer
               | Editor     People
