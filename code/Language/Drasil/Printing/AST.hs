module Language.Drasil.Printing.AST where

import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefType, RefAdd)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.People (People)
import Language.Drasil.Chunk.Citation (Month, EntryID, CitationKind)

data Ops = IsIn | Integer | Real | Rational | Natural | Boolean | Comma | Prime | Log
  | Sin | Cos | Tan | Sec | Csc | Cot | Not | Dim | Exp | Neg | Cross
  | Dot | Eq | NEq | Lt | Gt | LEq | GEq | Impl | Iff | Subt | And | Or
  | Add | Mul

data Fence = Paren | Curly | Norm | Abs
data OverSymb = Hat
data Fonts = Bold | Emph
data Spacing = Thin

data Expr = Dbl   Double
          | Int   Integer
          | Str   String
          | Case  [(Expr,Expr)]
          | Funct Functional Expr -- FIXME, this still needs to go!
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
          | Div   Expr Expr -- actually, fractions are a layout thing!
          | Sqrt  Expr      -- as are roots. Just sqrt for now.
          | Spc   Spacing
          
data Functional = 
            Summation (Maybe ((Symbol, Expr),Expr))
          | Integral ((Maybe Expr),(Maybe Expr)) Symbol
          | Product (Maybe ((Symbol, Expr), Expr))

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
