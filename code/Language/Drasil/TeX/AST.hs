module Language.Drasil.TeX.AST where

import Data.List (intersperse)

import Language.Drasil.Expr (Variable)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Unicode (Greek,Special)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.Citations (Month(..))
import Language.Drasil.People (People)
import Language.Drasil.Document (MaxWidthPercent, DType)

data Expr = Var  Variable
          | Dbl  Double
          | Int  Integer
          | Bln  Bool
          | Mul  Expr Expr
          | Add  Expr Expr
          | Frac Expr Expr
          | Div  Expr Expr
          | Pow  Expr Expr
          | Sub  Expr Expr
          | And  Expr Expr
          | Or   Expr Expr
          | Sym  Symbol
          | Eq   Expr Expr
          | NEq  Expr Expr
          | Lt   Expr Expr
          | Gt   Expr Expr
          | LEq   Expr Expr
          | GEq   Expr Expr
          | Dot  Expr Expr
          | Not  Expr 
          | Neg  Expr
          | Call Expr [Expr]
          | Case [(Expr,Expr)]
          | Op Function [Expr]
          | Grouping Expr
          | IsIn  Expr Set
          | Forall Symbol Expr
          | Exists Symbol Expr
          | Impl Expr Expr
          | Iff  Expr Expr
          | Mtx [[Expr]]
          | Index Expr Expr
          
data Function = Log
           | Summation (Maybe ((Symbol, Expr),Expr))
           | Product (Maybe ((Symbol, Expr),Expr))
           | Abs
           | Norm
           | Integral ((Maybe Expr),(Maybe Expr)) Symbol
           | Sin
           | Cos
           | Tan
           | Sec
           | Csc
           | Cot
           | Cross
           | Exp
           | Sqrt           

data Set = Integer
         | Rational
         | Real
         | Natural
         | Boolean
         | Char
         | String
         | Radians
         | Vect Set
         | Obj String
         | DiscreteI [Int]
         | DiscreteD [Double]
         | DiscreteS [String]

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

data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [LayoutObj]
type Depth    = Int
type Width    = Float
type Height   = Float
type Label    = Spec
type Filepath = String
type Caption  = Spec

data LayoutObj = Table [[Spec]] Label Bool Title
               | Section Depth Title [LayoutObj] Label
               | Paragraph Contents
               | EqnBlock Contents
             --  | CodeBlock Code
               | Definition [(String,[LayoutObj])] Label
               | List ListType
               | Figure Label Caption Filepath MaxWidthPercent
               | Requirement Contents Label
               | Assumption Contents Label
               | LikelyChange Contents Label
               | UnlikelyChange Contents Label
               | Graph [(Spec, Spec)] (Maybe Width) (Maybe Height) Caption Label
               | Bib BibRef
               | Defnt DType [(String,[LayoutObj])] Label -- To replace Definition eventually
               
data ListType = Item [ItemType]
              | Enum [ItemType]
              | Simple [(Spec,ItemType)]
              | Desc [(Spec,ItemType)]
              | Definitions [(Spec, ItemType)]

data ItemType = Flat Spec
              | Nested Spec ListType

instance Show Function where
  show Log            = "\\log"
  show (Summation _)  = "\\displaystyle\\sum"
  show (Product _)    = "\\displaystyle\\prod"
  show Abs            = ""
  show Norm           = ""
  show (Integral _ _) = "\\int"
  show Sin            = "\\sin"
  show Cos            = "\\cos"
  show Tan            = "\\tan"
  show Sec            = "\\sec"
  show Csc            = "\\csc"
  show Cot            = "\\cot"
  show Cross          = "\\times"
  show Exp            = "e"
  show Sqrt           = "\\sqrt"
  
instance Show Set where
  show Integer  = "\\mathbb{Z}"
  show Rational = "\\mathbb{Q}"
  show Real     = "\\mathbb{R}"
  show Natural  = "\\mathbb{N}"
  show Boolean  = "\\mathbb{B}"
  show Char     = "Char"
  show String   = "String"
  show Radians  = "rad"
  show (Vect a) = "V" ++ show a
  show (Obj a)  = a
  show (DiscreteI a)  = "\\{" ++ (concat $ intersperse ", " (map show a)) ++ "\\}"
  show (DiscreteD a)  = "\\{" ++ (concat $ intersperse ", " (map show a)) ++ "\\}"
  show (DiscreteS a)  = "\\{" ++ (concat $ intersperse ", " a) ++ "\\}"

type BibRef = [Citation]
type City   = Spec
type State  = Spec

data Citation = Book [CiteField] | Article [CiteField]
              | MThesis [CiteField] | PhDThesis [CiteField]
              | Misc [CiteField] | Online [CiteField]

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

instance Show Citation where
  show (Book      _) = "book"
  show (Article   _) = "article"
  show (MThesis   _) = "mastersthesis"
  show (PhDThesis _) = "phdthesis"
  show (Misc      _) = "misc"
  show (Online    _) = "online"
