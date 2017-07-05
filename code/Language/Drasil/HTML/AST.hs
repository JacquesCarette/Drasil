{-# Language GADTs #-}
module Language.Drasil.HTML.AST where

import Language.Drasil.Expr (Variable)
import Language.Drasil.Symbol (Symbol)
import Language.Drasil.Spec (USymb, RefType)
import Language.Drasil.Unicode (Greek, Special)
import Language.Drasil.Document (DType (..))

-- | Internal HTML version of Expr 
-- (for converting 'Language.Drasil.Expr.Expr')
data Expr = Var   Variable
          | Dbl   Double
          | Int   Integer
          | Bln   Bool
          | Mul   Expr Expr
          | Add   Expr Expr
          | Frac  Expr Expr
          | Div   Expr Expr
          | Pow   Expr Expr
          | Sub   Expr Expr
          | And   Expr Expr
          | Or    Expr Expr
          | Sym   Symbol
          | Eq    Expr Expr
          | NEq   Expr Expr
          | Lt    Expr Expr
          | Gt    Expr Expr
          | LEq   Expr Expr
          | GEq   Expr Expr
          | Dot   Expr Expr
          | Not   Expr
          | Neg   Expr
          | Call  Expr [Expr]
          | Case  [(Expr,Expr)]
          | Op Function [Expr]
          | Grouping Expr
          | IsIn  [Expr] Set
          | NotIn [Expr] Set
          | State [Quantifier] Expr
          | Impl Expr Expr
          | Iff  Expr Expr
          
-- | Internal HTML version of Function 
-- (for converting Functions from 'Language.Drasil.Expr')
data Function = Log
           | Summation (Maybe ((Symbol, Expr),Expr))
           | Abs
           | Norm
           | Integral ((Maybe Expr),(Maybe Expr)) Expr
           | Sin
           | Cos
           | Tan
           | Sec
           | Csc
           | Cot
           | Cross
           | Product (Maybe ((Symbol, Expr), Expr))
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

data Quantifier = Forall Expr | Exists Expr

-- | Internal HTML version of Sentence 
-- (for converting 'Language.Drasil.Spec.Sentence')
infixr 5 :+:
data Spec where
  E :: Expr -> Spec
  S :: String -> Spec
  (:+:) :: Spec -> Spec -> Spec -- concat
  (:^:) :: Spec -> Spec -> Spec -- superscript
  (:-:) :: Spec -> Spec -> Spec -- subscript
  (:/:) :: Spec -> Spec -> Spec -- frac
  Sy :: USymb -> Spec
  N :: Symbol -> Spec
  G :: Greek -> Spec 
  Sp :: Special -> Spec
  HARDNL :: Spec
  Ref :: RefType -> Spec -> Spec
  EmptyS :: Spec

-- | Internal HTML version of Document
-- (for converting 'Language.Drasil.Document.Document')
data Document = Document Title Author [LayoutObj]
type Title    = Spec
type Author   = Spec
type Contents = Spec
type Items    = [LayoutObj]
type Tags     = [String]
type Label    = Spec
type Filepath = String
type Caption  = Spec

-- | Internal HTML version of LayoutObj 
-- (for converting 'Language.Drasil.LayoutObj.LayoutObj')
data LayoutObj = Table Tags [[Spec]] Label Bool Caption
               | Header Int Contents
               | Paragraph Contents
               | HDiv Tags [LayoutObj] Label
               | Tagless Contents
             --  CodeBlock Code
               | Definition DType [(String,[LayoutObj])] Label
               | List ListType
               | Figure Label Caption Filepath
               | Module String Label
               | Assumption Contents Label
               -- Span Tags Contents
               
data ListType = Ordered [ItemType] | Unordered [ItemType]
              | Simple      [(Title,ItemType)]
              | Desc        [(Title,ItemType)]
              | Definitions  [(Title,ItemType)]

data ItemType = Flat Spec | Nested Spec ListType

instance Show ListType where
  show (Ordered _)   = "o"
  show (Unordered _) = "u"
  show (Desc _)    = error "Printing descriptive list failed"
  show (Simple _)  = error "Printing Simple list failed, see ASTHTML/PrintHTML"

instance Show Function where
  show Log = "log"
  show (Summation _) = "&sum;"
  show (Product _) = "&prod;"
  show Abs = ""
  show Norm = ""
  show (Integral _ _) = "&int;"
  show Sin = "sin"
  show Cos = "cos"
  show Tan = "tan"
  show Sec = "sec"
  show Csc = "csc"
  show Cot = "cot"
  show Cross = "&#10799;"
  show Exp = "e"
  show Sqrt = "&radic;"
  
instance Show Set where
  show Integer = "&#8484;"
  show Rational = "&#8474;"
  show Real = "&#8477;"
  show Natural = "&#8469;"
  show Boolean = "&#120121;"
  show Char = "Char"
  show String = "String"
  show Radians = "rad"
  show (Vect a) = "V" ++ show a
  show (Obj a) = a